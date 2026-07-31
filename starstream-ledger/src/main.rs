use core::future::poll_fn;
use core::net::{IpAddr, Ipv6Addr, SocketAddr};
use core::pin::pin;
use core::task::{Poll, ready};

use std::collections::HashMap;
use std::io::stderr;
use std::sync::Arc;

use clap::Parser;
use ed25519_dalek::VerifyingKey;
use starstream_ledger::{Account, CardanoCtx, Ledger};
use tokio::signal;
use tokio::task::JoinSet;
use tracing::{debug, error, info, warn};

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

    /// Pre-funded account (genesis allocation), repeatable.
    #[arg(long = "account", value_name = "PUBKEY=BALANCE", value_parser = parse_account)]
    accounts: Vec<(Box<str>, Account)>,

    /// Address to serve API on
    #[arg(long, global = true, value_name = "ADDR", default_value_t = SocketAddr::new(IpAddr::V6(Ipv6Addr::UNSPECIFIED), 9000))]
    addr: SocketAddr,

    /// Maximum amount of concurrent requests
    #[arg(long, global = true, value_name = "MAX_REQUESTS", default_value_t = u32::from(u16::MAX))]
    max_requests: u32,
}

fn parse_account(s: &str) -> Result<(Box<str>, Account), String> {
    let (key, balance) = s
        .split_once('=')
        .ok_or("expected `<hex-ed25519-pubkey>=<balance>`")?;
    let mut buf = [0u8; 32];
    hex::decode_to_slice(key, &mut buf)
        .map_err(|err| format!("public key is not a valid hex-encoded 32 bytes: {err}"))?;
    VerifyingKey::from_bytes(&buf)
        .map_err(|err| format!("public key is not a valid Ed25519 public key: {err}"))?;
    let balance = balance
        .parse()
        .map_err(|err| format!("balance is not a valid u64: {err}"))?;
    Ok((
        key.to_ascii_lowercase().into(),
        Account {
            balance,
            last_nonce: 0,
        },
    ))
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Args {
        cardano_block_height,
        cardano_current_slot,
        network,
        accounts,
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

    let accounts: HashMap<Box<str>, Account> = accounts.into_iter().collect();

    debug!("creating Wasmtime engine");
    let engine = wasmtime::Engine::default();

    let mut tasks = JoinSet::new();

    let cardano = CardanoCtx {
        block_height: cardano_block_height,
        current_slot: cardano_current_slot,
    };
    let ledger = Ledger::new(engine, max_requests, cardano, network, accounts);
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
