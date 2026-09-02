use core::future::poll_fn;
use core::net::{IpAddr, Ipv6Addr, SocketAddr};
use core::pin::pin;
use core::task::Poll;
use core::time::Duration;

use std::sync::Arc;

use anyhow::{Context as _, anyhow};
use clap::Parser;
use ed25519_dalek::VerifyingKey;
use sha2::{Digest as _, Sha256};
use starstream_ledger::server::Ledger;
use tokio::signal;
use tokio::time::timeout;
use tracing::{error, info, warn};

#[derive(Debug, Parser)]
#[command(version, about)]
struct Args {
    /// Network identifier to use.
    #[arg(long, default_value = "dev")]
    network: Box<str>,

    /// Hex-encoded Ed25519 public key of the admin account.
    #[arg(long, value_name = "ADMIN", value_parser = parse_admin_key, default_value = default_admin_key())]
    admin: VerifyingKey,

    /// Address to serve API on.
    #[arg(long, global = true, value_name = "ADDR", default_value_t = SocketAddr::new(IpAddr::V6(Ipv6Addr::UNSPECIFIED), 9000))]
    addr: SocketAddr,

    /// Maximum amount of concurrent requests.
    #[arg(long, global = true, value_name = "MAX_REQUESTS", default_value_t = u32::from(u16::MAX))]
    max_requests: u32,

    /// Time to wait for graceful shutdown before exiting, in seconds.
    #[arg(long, value_name = "SECONDS", default_value_t = 10)]
    shutdown_timeout: u64,
}

fn default_admin_key() -> String {
    let key = ed25519_dalek::SigningKey::from_bytes(&Sha256::digest("admin").into());
    hex::encode(key.verifying_key())
}

fn parse_admin_key(key: &str) -> Result<VerifyingKey, String> {
    let mut buf = [0u8; 32];
    hex::decode_to_slice(key, &mut buf)
        .map_err(|err| format!("public key is not a valid hex-encoded 32 bytes: {err}"))?;
    VerifyingKey::from_bytes(&buf)
        .map_err(|err| format!("public key is not a valid Ed25519 public key: {err}"))
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Args {
        network,
        admin,
        addr,
        max_requests,
        shutdown_timeout,
    } = Args::parse();
    let shutdown_timeout = Duration::from_secs(shutdown_timeout);

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

    let engine = wasmtime::Engine::default();

    let ledger = Ledger::new(engine, max_requests, network, admin);
    let ledger = Arc::new(ledger);

    let (http, http_shutdown) = ledger.handle_http(addr).await?;
    let http = tokio::spawn(http);
    let mut http = pin!(http);

    let ctrl_c = signal::ctrl_c();
    let mut ctrl_c = pin!(ctrl_c);

    #[cfg(unix)]
    let mut sigterm = signal::unix::signal(signal::unix::SignalKind::terminate())
        .context("failed to listen for SIGTERM")?;

    info!(%addr, "ledger running");

    let mut http_ready = false;
    let res = poll_fn(|cx| {
        debug_assert!(!http_ready);
        if let Poll::Ready(res) = http.as_mut().poll(cx) {
            http_ready = true;
            if let Err(err) = res {
                return Poll::Ready(Err(anyhow!(err).context("HTTP API task panicked")));
            } else {
                info!("HTTP API task finished");
                return Poll::Ready(Ok(()));
            }
        }
        if let Poll::Ready(res) = ctrl_c.as_mut().poll(cx) {
            return match res {
                Ok(()) => {
                    info!("^C received");
                    Poll::Ready(Ok(()))
                }
                Err(err) => {
                    warn!(?err, "failed to listen for ^C");
                    Poll::Ready(Err(err.into()))
                }
            };
        }
        #[cfg(unix)]
        if sigterm.poll_recv(cx).is_ready() {
            info!("SIGTERM received");
            return Poll::Ready(Ok(()));
        }
        Poll::Pending
    })
    .await;
    info!("shutting down");
    http_shutdown.notify_waiters();
    if !http_ready {
        match timeout(shutdown_timeout, http).await {
            Ok(Ok(())) => {}
            Ok(Err(err)) => error!(?err, "HTTP API task panicked"),
            Err(..) => error!("HTTP API failed to shut down within {shutdown_timeout:?}"),
        }
    }
    res
}
