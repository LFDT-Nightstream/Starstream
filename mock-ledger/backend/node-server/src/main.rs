mod api;

use clap::Parser;
use crate::{api::handler::Handler, api::tcp::server::run_server};
use starstream_ledger::Chain;
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

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

    let Args { addr } = Args::parse();

    let component_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("genesis")
            .join("no-args.wasm");
        
    let component_bytes = std::fs::read(&component_path)
        .with_context(|| format!("failed to read component from {:?}", component_path))?;

    let chain = Arc::new(Chain::new(&vec![component_bytes])?);
    let handler = Handler::new(Arc::clone(&chain));
    run_server(addr, handler).await
}

