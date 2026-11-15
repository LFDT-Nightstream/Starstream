//! The unified command-line interface to the Starstream language compiler and
//! test environment.

use std::process::ExitCode;

use clap::Parser;
use starstream_cli::Cli;

#[cfg(not(target_family = "wasm"))]
#[tokio::main]
async fn main() -> ExitCode {
    init();
    report(Cli::parse().exec().await)
}

#[cfg(target_family = "wasm")]
fn main() -> ExitCode {
    init();
    report(starstream_runtime_web::block_on(Cli::parse().exec()))
}

fn init() {
    miette::set_panic_hook();

    tracing_subscriber::fmt()
        .without_time()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_env("STARSTREAM_LOG")
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .with_writer(std::io::stderr)
        .init();
}

fn report(res: miette::Result<()>) -> ExitCode {
    match res {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
