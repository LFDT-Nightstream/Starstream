use core::future::poll_fn;
use core::net::{IpAddr, Ipv6Addr, SocketAddr};
use core::pin::pin;
use core::task::{Poll, ready};

use std::io::stderr;
use std::sync::Arc;

use clap::Parser;
use ed25519_dalek::{SigningKey, VerifyingKey};
use sha2::{Digest as _, Sha256};
use starstream_ledger::{CardanoCtx, Ledger};
use tokio::signal;
use tokio::task::JoinSet;
use tracing::{debug, error, info, warn};

/// The seed the default admin key is derived from (as its SHA-256 digest)
/// when `--admin-key` is not specified.
const DEFAULT_ADMIN_KEY_SEED: &str = "admin";

#[derive(Debug, Parser)]
#[command(version, about)]
struct Args {
    /// Cardano block height reported to contracts (`cardano#block-height`).
    #[arg(long, default_value_t = 0)]
    cardano_block_height: i64,

    /// Cardano current slot reported to contracts (`cardano#current-slot`).
    #[arg(long, default_value_t = 0)]
    cardano_current_slot: i64,

    /// Network identifier publish transactions must be bound to.
    #[arg(long, default_value = "dev")]
    network: String,

    /// Hex-encoded Ed25519 public key of the pre-funded admin account
    /// (genesis allocation). Defaults to the well-known pre-seeded key
    /// derived from the SHA-256 digest of `admin`.
    #[arg(long, value_name = "PUBKEY", value_parser = parse_admin_key, default_value_t = default_admin_key())]
    admin_key: Box<str>,

    /// Initial balance of the pre-funded admin account.
    #[arg(long, value_name = "BALANCE", default_value_t = u64::MAX)]
    admin_balance: u64,

    /// Address to serve API on
    #[arg(long, global = true, value_name = "ADDR", default_value_t = SocketAddr::new(IpAddr::V6(Ipv6Addr::UNSPECIFIED), 9000))]
    addr: SocketAddr,

    /// Maximum amount of concurrent requests
    #[arg(long, global = true, value_name = "MAX_REQUESTS", default_value_t = u32::from(u16::MAX))]
    max_requests: u32,
}

fn default_admin_key() -> Box<str> {
    let key = SigningKey::from_bytes(&Sha256::digest(DEFAULT_ADMIN_KEY_SEED).into());
    hex::encode(key.verifying_key().as_bytes()).into()
}

fn parse_admin_key(key: &str) -> Result<Box<str>, String> {
    let mut buf = [0u8; 32];
    hex::decode_to_slice(key, &mut buf)
        .map_err(|err| format!("public key is not a valid hex-encoded 32 bytes: {err}"))?;
    VerifyingKey::from_bytes(&buf)
        .map_err(|err| format!("public key is not a valid Ed25519 public key: {err}"))?;
    Ok(key.to_ascii_lowercase().into())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Args {
        cardano_block_height,
        cardano_current_slot,
        network,
        admin_key,
        admin_balance,
        addr,
        max_requests,
    } = Args::parse();

    tracing_subscriber::fmt()
        .with_writer(stderr)
        .with_env_filter(
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing::level_filters::LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    debug!("creating Wasmtime engine");
    let engine = wasmtime::Engine::default();

    let mut tasks = JoinSet::new();

    let cardano = CardanoCtx {
        block_height: cardano_block_height,
        current_slot: cardano_current_slot,
    };
    let ledger = Ledger::new(
        engine,
        max_requests,
        cardano,
        network,
        admin_key,
        admin_balance,
    );
    let ledger = Arc::new(ledger);
    let http_task = ledger.handle_http(addr).await?;
    tasks.spawn(http_task);

    let ctrl_c = signal::ctrl_c();
    let mut ctrl_c = pin!(ctrl_c);

    poll_fn(|cx| {
        loop {
            match tasks.poll_join_next(cx) {
                Poll::Ready(Some(Ok(()))) => debug!("successfully joined task"),
                Poll::Ready(Some(Err(err))) => error!(?err, "failed to join task"),
                Poll::Ready(None) => {
                    info!("no tasks left, shutting down");
                    return Poll::Ready(Ok(()));
                }
                Poll::Pending => break,
            }
        }
        match ready!(ctrl_c.as_mut().poll(cx)) {
            Ok(()) => {
                info!("^C received, shutting down");
                Poll::Ready(Ok(()))
            }
            Err(err) => {
                warn!(?err, "failed to listen for ^C, shutting down");
                Poll::Ready(Err(err))
            }
        }
    })
    .await?;
    tasks.abort_all();

    Ok(())
}
