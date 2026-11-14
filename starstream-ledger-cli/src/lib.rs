//! Starstream ledger client.

use core::pin::pin;
use core::time::Duration;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::Context as _;
use bytes::Bytes;
use clap::{Parser, Subcommand};
use ed25519_dalek::{SigningKey, VerifyingKey};
use http::Uri;
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::{TokioExecutor, TokioTimer};
use rand_core::OsRng;
use sha2::{Digest as _, Sha256};
use starstream_ledger::client::http::ClientBuilder;
use starstream_ledger::client::runtime::{
    Client, Ctx, call_coordination_script, compile_contract, resolve_utxo_results,
};
use starstream_ledger::client::wave::{
    encode_utxo_args, parse_digest, parse_script_args, read_utxo_results, utxo_method_type,
};
use starstream_ledger::{
    APPLICATION_WASM, CardanoCtx, Transaction, TransactionInput, TransactionOutput, encode_digest,
};
use tokio::fs;
use tokio::io::{AsyncWriteExt as _, stdout};
use tokio::try_join;
use tracing::{info, warn};
use wasmtime::Store;
use wasmtime::component::Val;
use zeroize::Zeroizing;

#[derive(Debug, Parser)]
#[command(version, about)]
/// Execute a Starstream mock ledger action against a node.
pub struct Args {
    /// Base URL of the ledger HTTP API.
    #[arg(
        long,
        global = true,
        value_name = "URL",
        default_value = "http://[::1]:9000/"
    )]
    url: Uri,

    /// Time an idle connection to the node is kept open for reuse, in seconds.
    #[arg(long, global = true, value_name = "SECONDS", default_value_t = 15)]
    pool_idle_timeout: u64,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Manage accounts.
    #[command(subcommand)]
    Account(AccountCommand),

    /// Query the ledger admin.
    #[command(subcommand)]
    Admin(AdminCommand),

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

    /// Print the genesis outputs.
    Genesis,

    /// Manage signing keys.
    #[command(subcommand)]
    Key(KeyCommand),

    /// Query the ledger network.
    #[command(subcommand)]
    Network(NetworkCommand),

    /// Inspect transactions.
    #[command(subcommand)]
    Transaction(TransactionCommand),

    /// Interact with UTXOs.
    #[command(subcommand)]
    Utxo(UtxoCommand),
}

#[derive(Debug, Subcommand)]
enum AccountCommand {
    /// Get the balance of an account.
    Balance {
        /// Hex-encoded Ed25519 public key of the account.
        #[arg(value_parser = parse_verifying_key)]
        account: VerifyingKey,
    },
    /// Fund an account with a credit signed by the admin key.
    Fund {
        /// Path to a file containing the hex-encoded Ed25519 admin signing key.
        #[arg(long, value_name = "PATH")]
        key: PathBuf,

        /// Network identifier the envelope is bound to, defaults to the one reported by the ledger.
        #[arg(long)]
        network: Option<String>,

        /// Admin nonce, defaults to the one following the last nonce used by the admin key.
        #[arg(long)]
        nonce: Option<u64>,

        /// Hex-encoded Ed25519 public key of the account to fund.
        #[arg(value_parser = parse_verifying_key)]
        account: VerifyingKey,

        /// Amount to credit the account with.
        amount: u64,
    },
    /// Get the last nonce used by an account to publish contracts.
    Nonce {
        /// Hex-encoded Ed25519 public key of the account.
        #[arg(value_parser = parse_verifying_key)]
        account: VerifyingKey,
    },
}

#[derive(Debug, Subcommand)]
enum AdminCommand {
    /// Get the last nonce used by the admin key to fund accounts.
    Nonce,
}

#[derive(Debug, Subcommand)]
enum BlockCommand {
    /// Print the hex-encoded envelopes of a ledger block, one per line.
    Get {
        /// Height of the block.
        height: u64,
    },
    /// Get the height of the latest ledger block.
    Height,
}

#[derive(Debug, Subcommand)]
#[allow(clippy::large_enum_variant)]
enum ContractCommand {
    /// Sign and publish a contract.
    Publish {
        /// Path to a file containing the hex-encoded Ed25519 signing key.
        #[arg(long, value_name = "PATH")]
        key: PathBuf,

        /// Network identifier the envelope is bound to, defaults to the one reported by the ledger.
        #[arg(long)]
        network: Option<String>,

        /// Publish nonce, defaults to the one following the last nonce used by the key.
        #[arg(long)]
        nonce: Option<u64>,

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
        #[arg(
            long,
            value_name = "PATH",
            required_unless_present = "simulate",
            conflicts_with = "simulate"
        )]
        key: Option<PathBuf>,

        /// Network identifier the envelope is bound to, defaults to the one reported by the ledger.
        #[arg(long, conflicts_with = "simulate")]
        network: Option<String>,

        /// Simulate the call instead of submitting the transaction.
        #[arg(long)]
        simulate: bool,

        /// Write the encoded transaction to a file.
        #[arg(long, value_name = "PATH")]
        output_transaction: Option<PathBuf>,

        /// Paths to contracts to resolve imports from instead of the ledger.
        #[arg(long = "import", value_name = "PATH")]
        imports: Vec<PathBuf>,

        /// Cardano block height reported to the script (`cardano#block-height`).
        #[arg(long, default_value_t = 0)]
        cardano_block_height: i64,

        /// Cardano current slot reported to the script (`cardano#current-slot`).
        #[arg(long, default_value_t = 0)]
        cardano_current_slot: i64,

        /// Digest of the published contract, either as multibase multihash or `sha256:HEX`.
        #[arg(value_parser = parse_digest)]
        digest: [u8; 32],

        /// Script to call.
        script: Box<str>,

        /// Script arguments. UTXO parameters are given as `[TRANSACTION]:INDEX` input
        /// references, where `TRANSACTION` is a digest as multibase multihash or `sha256:HEX`
        /// and an empty `TRANSACTION` refers to the genesis block.
        /// All other parameters are WAVE-encoded.
        args: Vec<Box<str>>,
    },
}

struct ImportClient<'a, T> {
    imports: HashMap<[u8; 32], Bytes>,
    client: &'a T,
}

impl<T: Client> Client for ImportClient<'_, T> {
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        if let Some(wasm) = self.imports.get(&digest) {
            return Ok(wasm.clone());
        }
        self.client.get_contract(digest).await
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
enum NetworkCommand {
    /// Get the name of the network the ledger accepts envelopes for.
    Name,
}

#[derive(Debug, Subcommand)]
enum TransactionCommand {
    /// Get a transaction from the ledger.
    Get {
        /// Digest of the transaction, either as multibase multihash or `sha256:HEX`.
        #[arg(value_parser = parse_digest)]
        digest: [u8; 32],
    },
    /// Print a transaction written by `contract script call --output-transaction`.
    Show {
        /// Path to the encoded transaction.
        path: PathBuf,
    },
}

#[derive(Debug, clap::Args)]
struct UtxoArgs {
    /// Digest of the transaction that produced the UTXO, either as multibase multihash
    /// or `sha256:HEX`, defaults to the genesis block.
    #[arg(long, value_name = "DIGEST", value_parser = parse_digest)]
    transaction: Option<[u8; 32]>,

    /// Index of the UTXO in the transaction outputs.
    index: u32,
}

impl From<UtxoArgs> for TransactionInput {
    fn from(UtxoArgs { transaction, index }: UtxoArgs) -> Self {
        match transaction {
            Some(transaction) => Self::new(transaction, index),
            None => Self::genesis(index),
        }
    }
}

#[derive(Debug, Subcommand)]
enum UtxoCommand {
    /// Call a method of a UTXO on the ledger, discarding the resulting state.
    Call {
        #[command(flatten)]
        utxo: UtxoArgs,

        /// Method to call.
        method: Box<str>,

        /// WAVE-encoded method arguments.
        args: Vec<Box<str>>,
    },
    /// Print the WIT of a UTXO's methods, in the shape `utxo call` invokes them.
    Wit {
        #[command(flatten)]
        utxo: UtxoArgs,
    },
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

fn parse_verifying_key(s: &str) -> anyhow::Result<VerifyingKey> {
    let mut buf = [0u8; 32];
    hex::decode_to_slice(s, &mut buf).context("key hex is not valid")?;
    let key = VerifyingKey::from_bytes(&buf)?;
    Ok(key)
}

impl Args {
    pub async fn exec(self) -> anyhow::Result<()> {
        exec(self).await
    }
}

async fn resolve_network(
    client: &starstream_ledger::client::http::tcp::Client<HttpConnector>,
    network: Option<String>,
) -> anyhow::Result<String> {
    match network {
        Some(network) => Ok(network),
        None => client.get_network().await,
    }
}

async fn exec(args: Args) -> anyhow::Result<()> {
    let Args {
        url,
        pool_idle_timeout,
        command,
    } = args;

    let mut http = hyper_util::client::legacy::Client::builder(TokioExecutor::new());
    http.pool_timer(TokioTimer::new())
        .pool_idle_timeout(Duration::from_secs(pool_idle_timeout));
    let client = ClientBuilder::from_hyper(http, HttpConnector::new(), url).build();
    match command {
        Command::Account(AccountCommand::Balance { account }) => {
            let balance = client.get_account_balance(&account).await?;
            let balance = balance.with_context(|| {
                format!("account `{}` not found", hex::encode(account.as_bytes()))
            })?;
            stdout()
                .write_all(format!("{balance}\n").as_bytes())
                .await
                .context("failed to write balance to stdout")
        }
        Command::Account(AccountCommand::Fund {
            key,
            network,
            nonce,
            account,
            amount,
        }) => {
            let key = read_signing_key(&key).await?;
            let (network, nonce) = try_join!(resolve_network(&client, network), async {
                match nonce {
                    Some(nonce) => Ok(nonce),
                    None => {
                        let nonce = client.get_admin_nonce().await?;
                        nonce.checked_add(1).context("admin nonce exhausted")
                    }
                }
            })?;
            client.fund(key, &network, nonce, &account, amount).await
        }
        Command::Account(AccountCommand::Nonce { account }) => {
            let nonce = client.get_account_nonce(&account).await?;
            let nonce = nonce.with_context(|| {
                format!("account `{}` not found", hex::encode(account.as_bytes()))
            })?;
            stdout()
                .write_all(format!("{nonce}\n").as_bytes())
                .await
                .context("failed to write nonce to stdout")
        }
        Command::Admin(AdminCommand::Nonce) => {
            let nonce = client.get_admin_nonce().await?;
            stdout()
                .write_all(format!("{nonce}\n").as_bytes())
                .await
                .context("failed to write nonce to stdout")
        }
        Command::Block(BlockCommand::Get { height }) => {
            let envelopes = client.get_block(height).await?;
            let envelopes = envelopes.with_context(|| format!("block {height} not found"))?;
            let mut out = String::default();
            for envelope in envelopes {
                out.push_str(&hex::encode(envelope));
                out.push('\n');
            }
            stdout()
                .write_all(out.as_bytes())
                .await
                .context("failed to write envelopes to stdout")
        }
        Command::Block(BlockCommand::Height) => {
            let height = client.block_height().await?;
            stdout()
                .write_all(format!("{height}\n").as_bytes())
                .await
                .context("failed to write height to stdout")
        }
        Command::Digest { path } => {
            let buf = fs::read(&path)
                .await
                .with_context(|| format!("failed to read `{}`", path.display()))?;
            let digest = Sha256::digest(&buf);
            stdout()
                .write_all(format!("{}\n", encode_digest(&digest.into())).as_bytes())
                .await
                .context("failed to write digest to stdout")
        }
        Command::Genesis => {
            let outputs = client.get_genesis().await?;
            let outputs = toml::Value::try_from(outputs).context("failed to encode TOML")?;
            let genesis = toml::Table::from_iter([("outputs".to_string(), outputs)]);
            let genesis = toml::to_string_pretty(&genesis).context("failed to encode TOML")?;
            stdout()
                .write_all(genesis.as_bytes())
                .await
                .context("failed to write genesis to stdout")
        }
        Command::Contract(ContractCommand::Publish {
            key,
            network,
            nonce,
            wasm,
        }) => {
            let key = read_signing_key(&key).await?;
            let wasm = fs::read(&wasm)
                .await
                .with_context(|| format!("failed to read `{}`", wasm.display()))?;
            let (network, nonce) = try_join!(resolve_network(&client, network), async {
                match nonce {
                    Some(nonce) => Ok(nonce),
                    None => {
                        let account = key.verifying_key();
                        let nonce = client.get_account_nonce(&account).await?;
                        let nonce = nonce.with_context(|| {
                            format!("account `{}` not found", hex::encode(account.as_bytes()))
                        })?;
                        nonce.checked_add(1).context("account nonce exhausted")
                    }
                }
            })?;
            let digest = client.publish_contract(key, &network, nonce, wasm).await?;
            stdout()
                .write_all(format!("{}\n", encode_digest(&digest)).as_bytes())
                .await
                .context("failed to write digest to stdout")
        }
        Command::Contract(ContractCommand::Script(ScriptCommand::Call {
            key,
            network,
            simulate,
            output_transaction,
            imports,
            cardano_block_height,
            cardano_current_slot,
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
            let wasm = imports.get_contract(digest).await?;
            let contract = compile_contract(
                &imports,
                client.engine(),
                client.wizer(),
                &wasm,
                None,
                &mut contracts,
            )
            .await?;

            let script = contract.get_coordination_script(&script)?;

            let ty = script.ty();
            let params = parse_script_args(ty, args)?;
            let mut results = vec![Val::Bool(false); ty.results().len()];

            let mut utxos = Vec::default();
            let ctx = Ctx {
                cardano: CardanoCtx {
                    block_height: cardano_block_height,
                    current_slot: cardano_current_slot,
                },
                ..Ctx::default()
            };
            let mut store = Store::new(client.engine(), ctx);
            let (tx, execution) = call_coordination_script(
                &mut store,
                &imports,
                client.wizer(),
                &contract,
                &wasm,
                &script,
                &mut contracts,
                params,
                &mut results,
                &mut utxos,
            )
            .await?;
            if let Err(err) = execution {
                warn!(?err, "failed to check execution trace");
            }
            resolve_utxo_results(&mut store, &utxos, &mut results)?;
            let mut results = wasm_wave::to_string(&Val::Tuple(results))
                .context("failed to encode result tuple")?;
            if let Some(path) = output_transaction {
                let tx = minicbor::to_vec(&tx).context("failed to encode transaction")?;
                fs::write(&path, &tx)
                    .await
                    .with_context(|| format!("failed to write `{}`", path.display()))?;
            }
            if let Some(key) = key {
                let network = resolve_network(&client, network).await?;
                let digest = client.transact(key, &network, tx).await?;
                info!(digest = %encode_digest(&digest), "submitted transaction");
            }
            results.push('\n');
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
                .write_all(format!("{}\n", hex::encode(key.to_bytes())).as_bytes())
                .await
                .context("failed to write signing key to stdout")
        }
        Command::Network(NetworkCommand::Name) => {
            let name = client.get_network().await?;
            stdout()
                .write_all(format!("{name}\n").as_bytes())
                .await
                .context("failed to write network name to stdout")
        }
        Command::Transaction(TransactionCommand::Get { digest }) => {
            let tx = client.get_transaction(digest).await?;
            let tx = tx.context("transaction not found")?;
            let tx = toml::to_string_pretty(&tx).context("failed to encode TOML")?;
            stdout()
                .write_all(tx.as_bytes())
                .await
                .context("failed to write transaction to stdout")
        }
        Command::Transaction(TransactionCommand::Show { path }) => {
            let buf = fs::read(&path)
                .await
                .with_context(|| format!("failed to read `{}`", path.display()))?;
            let tx: Transaction = minicbor::decode(&buf).context("failed to decode transaction")?;
            let tx = toml::to_string_pretty(&tx).context("failed to encode TOML")?;
            stdout()
                .write_all(tx.as_bytes())
                .await
                .context("failed to write transaction to stdout")
        }
        Command::Utxo(UtxoCommand::Call { utxo, method, args }) => {
            let input = TransactionInput::from(utxo);
            let wit = client.get_utxo_wit(&input, Some(APPLICATION_WASM)).await?;
            let ty = utxo_method_type(&wit, &method)?;
            let args = encode_utxo_args(&ty, args)?;
            let rx = client.call_utxo_method(&input, &method, &args).await?;
            let mut rx = pin!(rx);
            let mut results = read_utxo_results(&ty, &mut rx).await?;
            results.push('\n');
            stdout()
                .write_all(results.as_bytes())
                .await
                .context("failed to write result tuple to stdout")
        }
        Command::Utxo(UtxoCommand::Wit { utxo }) => {
            let input = TransactionInput::from(utxo);
            let wit = client.get_utxo_wit(&input, None).await?;
            stdout()
                .write_all(&wit)
                .await
                .context("failed to write WIT to stdout")
        }
    }
}
