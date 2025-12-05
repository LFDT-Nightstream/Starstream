mod handler;
mod hash;
mod server;

use clap::Parser;
use crate::handler::Handler;
use crate::server::run_server;

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

    let handler = Handler::new()?;
    run_server(addr, handler).await
}

