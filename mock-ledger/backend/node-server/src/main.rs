mod api;
mod utils;
mod ledger;

use clap::Parser;
use crate::{api::handler::Handler, ledger::Chain, api::tcp::server::run_server};
use std::sync::Arc;

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

    let chain = Arc::new(Chain::new()?);
    let handler = Handler::new(Arc::clone(&chain));
    run_server(addr, handler).await
}

