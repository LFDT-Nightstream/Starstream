//! The unified command-line interface to the Starstream language compiler and
//! test environment.

use std::process::ExitCode;

use clap::Parser;
use starstream_cli::Cli;

#[tokio::main]
async fn main() -> ExitCode {
    miette::set_panic_hook();

    tracing_subscriber::fmt()
        .without_time()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_env("STARSTREAM_LOG")
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .with_writer(std::io::stderr)
        .init();

    match Cli::parse().exec().await {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
