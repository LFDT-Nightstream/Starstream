use clap::Parser;
use starstream_ledger_cli::Args;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
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

    Args::parse().exec().await
}
