use core::future::poll_fn;
use core::net::{IpAddr, Ipv6Addr, SocketAddr};
use core::pin::pin;
use core::task::Poll;
use core::time::Duration;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context as _, anyhow};
use bytes::Bytes;
use clap::Parser;
use ed25519_dalek::VerifyingKey;
use sha2::{Digest as _, Sha256};
use starstream_ledger::genesis::{Genesis, build_genesis};
use starstream_ledger::server::Ledger;
use starstream_ledger::{CardanoCtx, TransactionOutput};
use tokio::fs;
use tokio::net::TcpSocket;
use tokio::signal;
use tokio::time::timeout;
use tracing::{debug, error, info, warn};

#[derive(Debug, Parser)]
#[command(version, about)]
/// Run a Starstream mock ledger node.
pub struct Args {
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

    /// Fuel available to each UTXO method call over `/rpc`.
    #[arg(long, value_name = "FUEL", default_value_t = 1_000_000_000)]
    fuel: u64,

    /// Time to wait for graceful shutdown before exiting, in seconds.
    #[arg(long, value_name = "SECONDS", default_value_t = 10)]
    shutdown_timeout: u64,

    /// Time a client connection may stall before it is closed, in seconds.
    #[arg(long, global = true, value_name = "SECONDS", default_value_t = 30)]
    io_timeout: u64,

    /// Path to the genesis file.
    #[arg(long, value_name = "PATH")]
    genesis: Option<PathBuf>,

    /// Paths to contracts used to resolve genesis coordination script calls.
    #[arg(long = "import", value_name = "PATH", requires = "genesis")]
    imports: Vec<PathBuf>,

    /// Cardano block height reported to contracts (`cardano#block-height`).
    #[arg(long, default_value_t = 0)]
    cardano_block_height: i64,

    /// Cardano current slot reported to contracts (`cardano#current-slot`).
    #[arg(long, default_value_t = 0)]
    cardano_current_slot: i64,
}

fn bind_tcp(address: SocketAddr) -> anyhow::Result<TcpSocket> {
    debug!("binding TCP socket");
    let sock = match address {
        SocketAddr::V4(..) => TcpSocket::new_v4(),
        SocketAddr::V6(..) => TcpSocket::new_v6(),
    }
    .context("failed to create HTTP TCP socket")?;
    // Conditionally enable `SO_REUSEADDR` depending on the current
    // platform. On Unix we want this to be able to rebind an address in
    // the `TIME_WAIT` state which can happen then a server is killed with
    // active TCP connections and then restarted. On Windows though if
    // `SO_REUSEADDR` is specified then it enables multiple applications to
    // bind the port at the same time which is not something we want. Hence
    // this is conditionally set based on the platform (and deviates from
    // Tokio's default from always-on).
    sock.set_reuseaddr(!cfg!(windows))?;
    sock.bind(address)
        .with_context(|| format!("failed to bind on `{address}`"))?;
    Ok(sock)
}

async fn read_genesis(
    genesis: impl AsRef<Path>,
    imports: Vec<PathBuf>,
    cardano: CardanoCtx,
) -> anyhow::Result<Vec<TransactionOutput>> {
    let genesis = genesis.as_ref();
    let genesis = fs::read_to_string(genesis)
        .await
        .with_context(|| format!("failed to read `{}`", genesis.display()))?;
    let genesis: Genesis = toml::from_str(&genesis).context("failed to parse genesis")?;

    let mut imported = HashMap::with_capacity(imports.len());
    for path in imports {
        let wasm = fs::read(&path)
            .await
            .with_context(|| format!("failed to read `{}`", path.display()))?;
        imported.insert(Sha256::digest(&wasm).into(), Bytes::from(wasm));
    }
    let mut engine_config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut engine_config);
    let engine = wasmtime::Engine::new(&engine_config)?;
    build_genesis(&engine, genesis, &imported, cardano).await
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

impl Args {
    pub async fn exec(self) -> anyhow::Result<()> {
        exec(self).await
    }
}

async fn exec(args: Args) -> anyhow::Result<()> {
    let Args {
        network,
        admin,
        addr,
        max_requests,
        fuel,
        shutdown_timeout,
        io_timeout,
        genesis,
        imports,
        cardano_block_height,
        cardano_current_slot,
    } = args;
    let shutdown_timeout = Duration::from_secs(shutdown_timeout);
    let io_timeout = Duration::from_secs(io_timeout);
    let cardano = CardanoCtx {
        block_height: cardano_block_height,
        current_slot: cardano_current_slot,
    };

    let mut engine_config = wasmtime::Config::default();
    starstream_ledger::server::configure_wasmtime(&mut engine_config);
    let engine = wasmtime::Engine::new(&engine_config)?;

    let genesis = if let Some(genesis) = genesis {
        read_genesis(genesis, imports, cardano).await?
    } else {
        Vec::default()
    };

    let ledger = Ledger::new(engine, max_requests, fuel, network, admin, genesis, cardano);
    let ledger = Arc::new(ledger);

    let listener = bind_tcp(addr)?;
    let listener = listener
        .listen(max_requests)
        .context("failed to listen on TCP socket")?;
    let addr = listener
        .local_addr()
        .context("failed to get TCP listener local address")?;
    let (http, http_shutdown) = ledger.handle_http(listener, io_timeout);
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
    http_shutdown.notify_one();
    if !http_ready {
        match timeout(shutdown_timeout, http).await {
            Ok(Ok(())) => {}
            Ok(Err(err)) => error!(?err, "HTTP API task panicked"),
            Err(..) => error!("HTTP API failed to shut down within {shutdown_timeout:?}"),
        }
    }
    res
}
