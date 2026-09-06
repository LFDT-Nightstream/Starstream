//! Starstream ledger client.

use std::path::{Path, PathBuf};

use anyhow::Context as _;
use clap::{Parser, Subcommand};
use ed25519_dalek::{SigningKey, VerifyingKey};
use http::Uri;
use hyper_util::rt::TokioExecutor;
use rand_core::OsRng;
use sha2::{Digest as _, Sha256};
use starstream_ledger::client::http::ClientBuilder;
use starstream_ledger::encode_digest;
use tokio::fs;
use tokio::io::{AsyncWriteExt as _, stdout};
use tracing::info;
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

    /// Manage published contracts.
    #[command(subcommand)]
    Contract(ContractCommand),

    /// Manage signing keys.
    #[command(subcommand)]
    Key(KeyCommand),
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
#[allow(clippy::large_enum_variant)]
enum ContractCommand {
    /// Compute contract digest.
    Digest {
        /// Path to the contract.
        wasm: PathBuf,
    },
    /// Sign and publish a contract.
    Publish {
        #[command(flatten)]
        signing: SigningArgs,

        /// Path to the contract.
        wasm: PathBuf,
    },
}

#[derive(Debug, Subcommand)]
enum KeyCommand {
    /// Generate a new Ed25519 key pair.
    Generate,
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

    let http = hyper_util::client::legacy::Client::builder(TokioExecutor::new()).build_http();
    let client = ClientBuilder::new(http, url).network(network).build();
    match command {
        Command::Account(AccountCommand::Fund {
            signing: SigningArgs { key, nonce },
            account,
            amount,
        }) => {
            let key = read_signing_key(&key).await?;
            client.fund(key, nonce, &account, amount).await
        }
        Command::Contract(ContractCommand::Digest { wasm }) => {
            let wasm = fs::read(&wasm)
                .await
                .with_context(|| format!("failed to read `{}`", wasm.display()))?;
            let digest = Sha256::digest(&wasm);
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
    }
}
