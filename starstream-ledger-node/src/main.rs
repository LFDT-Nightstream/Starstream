use core::future::poll_fn;
use core::net::{IpAddr, Ipv6Addr, SocketAddr};
use core::pin::pin;
use core::task::Poll;
use core::time::Duration;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context as _, anyhow, bail, ensure};
use bytes::Bytes;
use clap::Parser;
use ed25519_dalek::VerifyingKey;
use serde::Deserialize;
use sha2::{Digest as _, Sha256};
use starstream_ledger::client::runtime::{
    Client, Contract, call_coordination_script, compile_component, new_contract,
};
use starstream_ledger::server::Ledger;
use starstream_ledger::{TransactionInput, TransactionOutput, encode_digest, parse_digest};
use tokio::fs;
use tokio::signal;
use tokio::time::timeout;
use tracing::{error, info, warn};
use wasmtime::component::Val;
use wasmtime_wizer::Wizer;

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

    /// Path to the genesis file.
    #[arg(long, value_name = "PATH")]
    genesis: Option<PathBuf>,

    /// Paths to contracts used to resolve genesis coordination script calls.
    #[arg(long = "import", value_name = "PATH", requires = "genesis")]
    imports: Vec<PathBuf>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct GenesisScript {
    /// Digest of the contract exporting the coordination script.
    contract: Box<str>,
    /// Coordination script to call.
    script: Box<str>,
    /// WAVE-encoded coordination script arguments.
    #[serde(default)]
    args: Vec<Box<str>>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Genesis {
    #[serde(default)]
    scripts: Vec<GenesisScript>,
}

struct GenesisClient<'a>(&'a HashMap<[u8; 32], Bytes>);

fn get_contract_wasm<'a>(
    contracts: &'a HashMap<[u8; 32], Bytes>,
    digest: &[u8; 32],
) -> anyhow::Result<&'a Bytes> {
    contracts.get(digest).with_context(|| {
        format!(
            "contract `{}` not found, pass it via `--import`",
            encode_digest(digest)
        )
    })
}

impl Client for GenesisClient<'_> {
    async fn get_contract_wasm(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        get_contract_wasm(self.0, &digest).cloned()
    }

    async fn get_input_utxo(&self, _input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        bail!("genesis scripts cannot have UTXO inputs")
    }
}

async fn build_genesis(
    engine: &wasmtime::Engine,
    genesis: impl AsRef<Path>,
    imports: Vec<PathBuf>,
) -> anyhow::Result<Vec<TransactionOutput>> {
    let genesis = genesis.as_ref();
    let genesis = fs::read_to_string(genesis)
        .await
        .with_context(|| format!("failed to read `{}`", genesis.display()))?;
    let Genesis { scripts } = toml::from_str(&genesis).context("failed to parse genesis")?;

    let mut imported = HashMap::with_capacity(imports.len());
    for path in imports {
        let wasm = fs::read(&path)
            .await
            .with_context(|| format!("failed to read `{}`", path.display()))?;
        imported.insert(Sha256::digest(&wasm).into(), Bytes::from(wasm));
    }
    let wizer = Wizer::new();
    let mut contracts: HashMap<_, Contract> = HashMap::with_capacity(imported.len());
    let mut outputs = Vec::default();
    for GenesisScript {
        contract,
        script,
        args,
    } in scripts
    {
        let digest = parse_digest(&contract)
            .with_context(|| format!("failed to parse `{contract}` as multibase multihash"))?;
        let Contract { contract, wasm } = if let Some(contract) = contracts.get(&digest) {
            contract.clone()
        } else {
            let wasm = get_contract_wasm(&imported, &digest)?;
            let component = compile_component(engine, &wizer, wasm)?;
            let contract = new_contract(
                &GenesisClient(&imported),
                &wizer,
                &component,
                None,
                &mut contracts,
            )
            .await?;
            let contract = Contract {
                contract: Some(contract),
                wasm: wasm.clone(),
            };
            contracts.insert(digest, contract.clone());
            contract
        };
        let contract = contract.context("contract was not compiled")?;
        let script = contract.get_coordination_script(&script)?;
        let ty = script.ty();
        let mut params = Vec::with_capacity(ty.params().len());
        let mut args = args.into_iter();
        for (name, ty) in ty.params() {
            let v = args
                .next()
                .with_context(|| format!("missing value for parameter `{name}`"))?;
            let v = wasm_wave::from_str::<Val>(&ty, v.as_ref())
                .with_context(|| format!("failed to parse value for parameter `{name}`"))?;
            params.push(v.into());
        }
        ensure!(args.next().is_none(), "trailing arguments");
        let mut results = vec![Val::Bool(false); ty.results().len()];
        let tx = call_coordination_script(
            &GenesisClient(&imported),
            &wizer,
            &contract,
            &wasm,
            &script,
            &mut contracts,
            params,
            &mut results,
        )
        .await?;
        outputs.extend(tx.outputs);
    }
    Ok(outputs)
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
        genesis,
        imports,
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

    let mut config = wasmtime::Config::default();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config)?;

    let genesis = if let Some(genesis) = genesis {
        build_genesis(&engine, genesis, imports).await?
    } else {
        Vec::default()
    };

    let ledger = Ledger::new(engine, max_requests, network, admin, genesis);
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
