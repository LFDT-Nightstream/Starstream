//! Starstream ledger client.

use core::iter::zip;
use core::pin::pin;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{Context as _, bail, ensure};
use bytes::{Bytes, BytesMut};
use clap::{Parser, Subcommand};
use ed25519_dalek::{SigningKey, VerifyingKey};
use http::Uri;
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::TokioExecutor;
use rand_core::OsRng;
use sha2::{Digest as _, Sha256};
use starstream_ledger::client::encode_transaction;
use starstream_ledger::client::http::ClientBuilder;
use starstream_ledger::client::runtime::{
    Client, call_coordination_script, compile_component, new_contract,
};
use starstream_ledger::{TransactionInput, TransactionOutput, encode_digest, parse_digest};
use tokio::fs;
use tokio::io::{AsyncRead, AsyncWriteExt as _, stdout};
use tokio_util::codec::Encoder as _;
use tracing::info;
use wasm_wave::wasm::WasmFunc as _;
use wasmtime::component::{Val, types};
use zeroize::Zeroizing;

#[derive(Debug, Parser)]
#[command(version, about)]
struct Args {
    /// Base URL of the ledger HTTP API.
    #[arg(
        long,
        global = true,
        value_name = "URL",
        default_value = "http://[::1]:9000/"
    )]
    url: Uri,

    /// Network identifier the transaction is bound to.
    #[arg(long, global = true, default_value = "dev")]
    network: Box<str>,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Manage accounts.
    #[command(subcommand)]
    Account(AccountCommand),

    /// Query ledger blocks.
    #[command(subcommand)]
    Block(BlockCommand),

    /// Manage published contracts.
    #[command(subcommand)]
    Contract(ContractCommand),

    /// Compute the digest of a file.
    Digest {
        /// Path to the file.
        path: PathBuf,
    },

    /// Manage signing keys.
    #[command(subcommand)]
    Key(KeyCommand),

    /// Interact with UTXOs.
    #[command(subcommand)]
    Utxo(UtxoCommand),
}

/// Arguments common to every signed transaction.
#[derive(Debug, clap::Args)]
struct SigningArgs {
    /// Path to a file containing the hex-encoded Ed25519 signing key.
    #[arg(long, value_name = "PATH")]
    key: PathBuf,

    /// Transaction nonce.
    #[arg(long)]
    nonce: u64,
}

#[derive(Debug, Subcommand)]
enum AccountCommand {
    /// Fund an account with a credit signed by the admin key.
    Fund {
        #[command(flatten)]
        signing: SigningArgs,

        /// Hex-encoded Ed25519 public key of the account to fund.
        #[arg(value_parser = parse_verifying_key)]
        account: VerifyingKey,

        /// Amount to credit the account with.
        amount: u64,
    },
}

#[derive(Debug, Subcommand)]
enum BlockCommand {
    /// Get the height of the latest ledger block.
    Height,
}

#[derive(Debug, Subcommand)]
#[allow(clippy::large_enum_variant)]
enum ContractCommand {
    /// Sign and publish a contract.
    Publish {
        #[command(flatten)]
        signing: SigningArgs,

        /// Path to the contract.
        wasm: PathBuf,
    },
    /// Interact with contract coordination scripts.
    #[command(subcommand)]
    Script(ScriptCommand),
}

#[derive(Debug, Subcommand)]
enum ScriptCommand {
    /// Call a coordination script exported by a published contract and submit the resulting transaction.
    Call {
        /// Path to a file containing the hex-encoded Ed25519 signing key.
        #[arg(long, value_name = "PATH", required_unless_present = "simulate")]
        key: Option<PathBuf>,

        /// Simulate the call instead of submitting the transaction.
        #[arg(long)]
        simulate: bool,

        /// Write the encoded transaction to a file.
        #[arg(long, value_name = "PATH")]
        output_transaction: Option<PathBuf>,

        /// Paths to contracts to resolve imports from instead of the ledger.
        #[arg(long = "import", value_name = "PATH")]
        imports: Vec<PathBuf>,

        /// Digest of the published contract.
        #[arg(value_parser = parse_digest)]
        digest: [u8; 32],

        /// Script to call.
        script: Box<str>,

        /// Script arguments. UTXO parameters are given as `[TRANSACTION]:INDEX` input
        /// references, where an empty `TRANSACTION` refers to the genesis block.
        /// All other parameters are WAVE-encoded.
        args: Vec<Box<str>>,
    },
}

struct ImportClient<'a, T> {
    imports: HashMap<[u8; 32], Bytes>,
    client: &'a T,
}

impl<T: Client> Client for ImportClient<'_, T> {
    async fn get_contract_wasm(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        if let Some(wasm) = self.imports.get(&digest) {
            return Ok(wasm.clone());
        }
        self.client.get_contract_wasm(digest).await
    }

    async fn get_input_utxo(&self, input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        self.client.get_input_utxo(input).await
    }
}

#[derive(Debug, Subcommand)]
enum KeyCommand {
    /// Generate a new Ed25519 key pair.
    Generate,
}

#[derive(Debug, Subcommand)]
enum UtxoCommand {
    /// Call a method of a UTXO on the ledger, discarding the resulting state.
    Call {
        /// Digest of the transaction that produced the UTXO, defaults to the genesis block.
        #[arg(long, value_name = "DIGEST", value_parser = parse_digest)]
        transaction: Option<[u8; 32]>,

        /// Index of the UTXO in the transaction outputs.
        index: usize,

        /// Method to call.
        method: Box<str>,

        /// WAVE-encoded method arguments.
        args: Vec<Box<str>>,
    },
}

fn decode_component(wasm: &[u8]) -> anyhow::Result<(wit_parser::Resolve, wit_parser::WorldId)> {
    let wasm = wit_parser::decoding::decode(wasm).context("failed to decode Wasm")?;
    let wit_parser::decoding::DecodedWasm::Component(resolve, world) = wasm else {
        bail!("Wasm is not a component")
    };
    Ok((resolve, world))
}

/// Encode WAVE-encoded `args` as parameters of type `ty`.
fn encode_args(
    ty: &wasm_wave::value::FuncType,
    args: impl IntoIterator<Item = Box<str>>,
) -> anyhow::Result<Bytes> {
    let params_ty = zip(ty.param_names(), ty.params());
    let mut args = args.into_iter();
    let mut buf = BytesMut::new();
    for (name, ty) in params_ty {
        let v = args
            .next()
            .with_context(|| format!("missing value for parameter `{name}`"))?;
        let v = wasm_wave::from_str::<wasm_wave::value::Value>(&ty, &v)
            .with_context(|| format!("failed to parse value for parameter `{name}`"))?;
        wrpc_wave::WaveEncoder::new(&ty)
            .encode(&v, &mut buf)
            .with_context(|| format!("failed to encode value of parameter `{name}`"))?;
    }
    ensure!(args.next().is_none(), "trailing arguments");
    Ok(buf.freeze())
}

async fn write_results(
    ty: &wasm_wave::value::FuncType,
    results: impl AsyncRead + Unpin,
) -> anyhow::Result<()> {
    let mut results = pin!(results);
    if let Some(ty) = wasm_wave::value::Type::tuple(ty.results().collect::<Box<_>>()) {
        let v = wrpc_wave::read_value(&mut results, &ty)
            .await
            .context("failed to read result tuple")?;
        let s = wasm_wave::to_string(&v).context("failed to encode result tuple")?;
        stdout().write_all(s.as_bytes()).await
    } else {
        stdout().write_all(b"()").await
    }
    .context("failed to write result tuple to stdout")
}

async fn read_signing_key(path: &Path) -> anyhow::Result<SigningKey> {
    let key = fs::read_to_string(path)
        .await
        .with_context(|| format!("failed to read signing key from `{}`", path.display()))?;
    let key = Zeroizing::new(key);
    let mut buf = Zeroizing::new([0u8; 32]);
    hex::decode_to_slice(key.trim(), &mut *buf).context("key hex is not valid")?;
    Ok(SigningKey::from_bytes(&buf))
}

fn parse_input(s: &str) -> anyhow::Result<TransactionInput> {
    let (transaction, index) = s
        .rsplit_once(':')
        .context("input must be formatted as `[TRANSACTION]:INDEX`")?;
    if !transaction.is_empty() {
        parse_digest(transaction).context("transaction digest is not valid")?;
    }
    let index = index.parse().context("input index is not valid")?;
    Ok(TransactionInput {
        transaction: transaction.into(),
        index,
    })
}

fn parse_verifying_key(s: &str) -> anyhow::Result<VerifyingKey> {
    let mut buf = [0u8; 32];
    hex::decode_to_slice(s, &mut buf).context("key hex is not valid")?;
    let key = VerifyingKey::from_bytes(&buf)?;
    Ok(key)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Args {
        url,
        network,
        command,
    } = Args::parse();

    tracing_subscriber::fmt()
        .without_time()
        .with_writer(std::io::stderr)
        .with_env_filter(
            tracing_subscriber::EnvFilter::builder()
                .with_env_var("STARSTREAM_LEDGER_LOG")
                .with_default_directive(tracing::level_filters::LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let http = hyper_util::client::legacy::Client::builder(TokioExecutor::new());
    let client = ClientBuilder::new(http, HttpConnector::new(), url)
        .network(network.as_ref())
        .build();
    match command {
        Command::Account(AccountCommand::Fund {
            signing: SigningArgs { key, nonce },
            account,
            amount,
        }) => {
            let key = read_signing_key(&key).await?;
            client.fund(key, nonce, &account, amount).await
        }
        Command::Block(BlockCommand::Height) => {
            let height = client.block_height().await?;
            stdout()
                .write_all(height.to_string().as_bytes())
                .await
                .context("failed to write height to stdout")
        }
        Command::Digest { path } => {
            let buf = fs::read(&path)
                .await
                .with_context(|| format!("failed to read `{}`", path.display()))?;
            let digest = Sha256::digest(&buf);
            stdout()
                .write_all(encode_digest(&digest.into()).as_bytes())
                .await
                .context("failed to write digest to stdout")
        }
        Command::Contract(ContractCommand::Publish {
            signing: SigningArgs { key, nonce },
            wasm,
        }) => {
            let key = read_signing_key(&key).await?;
            let wasm = fs::read(&wasm)
                .await
                .with_context(|| format!("failed to read `{}`", wasm.display()))?;
            client.publish_contract(key, nonce, wasm).await
        }
        Command::Contract(ContractCommand::Script(ScriptCommand::Call {
            key,
            simulate,
            output_transaction,
            imports,
            digest,
            script,
            args,
        })) => {
            let key = if simulate {
                None
            } else {
                let key = key.context("`--key` is required to submit the transaction")?;
                let key = read_signing_key(&key).await?;
                Some(key)
            };
            let mut imported = HashMap::with_capacity(imports.len());
            for path in imports {
                let wasm = fs::read(&path)
                    .await
                    .with_context(|| format!("failed to read `{}`", path.display()))?;
                imported.insert(Sha256::digest(&wasm).into(), Bytes::from(wasm));
            }
            let imports = ImportClient {
                imports: imported,
                client: &client,
            };
            let mut contracts = HashMap::default();
            let wasm = imports.get_contract_wasm(digest).await?;
            let component = compile_component(client.engine(), client.wizer(), &wasm)?;
            let contract =
                new_contract(&imports, client.wizer(), &component, None, &mut contracts).await?;

            let script = contract.get_coordination_script(&script)?;

            let ty = script.ty();
            let params_ty = ty.params();
            let mut params = Vec::with_capacity(params_ty.len());
            let mut args = args.into_iter();
            for (name, ty) in params_ty {
                let v = args
                    .next()
                    .with_context(|| format!("missing value for parameter `{name}`"))?;
                let v = v.as_ref();
                let v = match ty {
                    types::Type::Own(..) | types::Type::Borrow(..) => {
                        let utxo = parse_input(v).with_context(|| {
                            format!("failed to parse UTXO input for parameter `{name}`")
                        })?;
                        utxo.into()
                    }
                    ty => {
                        let v = wasm_wave::from_str::<Val>(&ty, v).with_context(|| {
                            format!("failed to parse value for parameter `{name}`")
                        })?;
                        v.into()
                    }
                };
                params.push(v);
            }
            ensure!(args.next().is_none(), "trailing arguments");
            let mut results = vec![Val::Bool(false); ty.results().len()];

            let tx = call_coordination_script(
                &imports,
                client.wizer(),
                &contract,
                &wasm,
                &script,
                &mut contracts,
                params,
                &mut results,
            )
            .await?;
            let results = wasm_wave::to_string(&Val::Tuple(results))
                .context("failed to encode result tuple")?;
            if let Some(path) = output_transaction {
                let tx = encode_transaction(network, tx.clone())?;
                fs::write(&path, &tx)
                    .await
                    .with_context(|| format!("failed to write `{}`", path.display()))?;
            }
            if let Some(key) = key {
                client.transact(key, tx).await?;
            }
            stdout()
                .write_all(results.as_bytes())
                .await
                .context("failed to write result tuple to stdout")
        }
        Command::Key(KeyCommand::Generate) => {
            let key = SigningKey::generate(&mut OsRng);
            info!(
                public_key = hex::encode(key.verifying_key().to_bytes()),
                "generated key pair"
            );
            stdout()
                .write_all(hex::encode(key.to_bytes()).as_bytes())
                .await
                .context("failed to write signing key to stdout")
        }
        Command::Utxo(UtxoCommand::Call {
            transaction,
            index,
            method,
            args,
        }) => {
            let TransactionOutput {
                instance,
                methods,
                storage,
                wasm,
                ..
            } = if let Some(transaction) = transaction {
                client.get_transaction_utxo(transaction, index).await?
            } else {
                client.get_genesis_utxo(index).await?
            };
            let (resolve, world) = decode_component(&wasm)?;
            let world = &resolve.worlds[world];
            let ty = world
                .exports
                .iter()
                .find_map(|(name, item)| {
                    let wit_parser::WorldKey::Name(name) = name else {
                        return None;
                    };
                    let wit_parser::WorldItem::Interface { id, .. } = item else {
                        return None;
                    };
                    if name == instance.as_ref() {
                        Some(id)
                    } else {
                        None
                    }
                })
                .with_context(|| format!("UTXO instance `{instance}` not found"))?;
            let ty = resolve.interfaces[*ty]
                .functions
                .get(&format!("[method]utxo.{method}"))
                .with_context(|| format!("method `{method}` not found"))?;
            let ty = wit_parser::Function {
                params: ty.params.iter().skip(1).cloned().collect(),
                ..ty.clone()
            };
            let ty = wasm_wave::value::resolve_wit_func_type(&resolve, &ty)
                .context("failed to resolve method type")?;
            let args = encode_args(&ty, args)?;
            let digest = Sha256::digest(&wasm).into();
            let rx = client
                .call_utxo_method(&digest, &instance, &method, &methods, &storage, &args)
                .await?;
            write_results(&ty, rx).await
        }
    }
}
