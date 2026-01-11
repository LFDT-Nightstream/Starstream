mod api;

use clap::Parser;
use crate::{api::handler::Handler, api::tcp::server::run_server};
use starstream_ledger::{Chain, WasmComponent};
use std::sync::Arc;
use std::{path::PathBuf};
use anyhow::{Context};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Address to serve the dynamic handler on
    #[arg(default_value = "[::1]:7762")]
    addr: String,
}

fn load_file(path: &str) -> anyhow::Result<WasmComponent> {
    let component_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("genesis")
            .join(path);
        
    let component_bytes = std::fs::read(&component_path)
        .with_context(|| format!("failed to read component from {:?}", component_path))?;
    return Ok(component_bytes);
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // tracing_subscriber::fmt().init();

    // Enable maximum verbosity for wRPC crates to debug stream issues
    // Default to TRACE for wRPC, DEBUG for our code, INFO for others
    // Can override with RUST_LOG environment variable
    let filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| {
            tracing_subscriber::EnvFilter::new(
                "wrpc_transport=trace,\
                 wrpc_runtime_wasmtime=trace,\
                 wrpc_pack=trace,\
                 wrpc_multiplexer=trace,\
                 wit_bindgen_wrpc=trace,\
                 wrpc=trace,\
                 starstream=debug,\
                 info"
            )
        });
    
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(true)
        .with_thread_ids(true)
        .with_file(true)
        .with_line_number(true)
        .init();

    let Args { addr } = Args::parse();

    let mut genesis_block: Vec<WasmComponent> = vec![];
    genesis_block.push(load_file("no-args.wasm")?);
    genesis_block.push(load_file("args-simple.wasm")?);
    genesis_block.push(load_file("args-record.wasm")?);

    let chain = Arc::new(Chain::new(&genesis_block)?);
    let handler = Handler::new(Arc::clone(&chain));
    run_server(addr, handler).await
}

