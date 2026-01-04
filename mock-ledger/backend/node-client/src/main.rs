use clap::Parser;
mod rpc;
mod no_args;
mod args_simple;
mod args_record;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Address of the dynamic handler server
    #[arg(long, default_value = "[::1]:7762")]
    addr: String,
    /// Contract hash to invoke
    #[arg(long)]
    contract_hash: String,
    /// Function name to invoke (e.g., "hello")
    #[arg(long, default_value = "get-value")]
    function: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

    let Args {
        addr,
        contract_hash,
        function,
    } = Args::parse();

    // TODO
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Once;

    static INIT: Once = Once::new();

    fn init_tracing() {
        INIT.call_once(|| {
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
        });
    }

    #[tokio::test]
    async fn test_no_args() -> anyhow::Result<()> {
        init_tracing();
        let addr = "[::1]:7762".to_string();
        
        let function = "get-value".to_string();
        let contract_hash = "0x1170FAD15BECBB08C00B29067171110B34E8B4CEBC648BA662147BA0F2F1224F".to_string();
        
        let result = no_args::explicit_call(addr.clone(), contract_hash.clone(), function.clone()).await?;
        assert_eq!(result, 5);
        let result = no_args::implicit_dynamic_call(addr.clone(), contract_hash.clone(), function.clone()).await?;
        assert_eq!(result, 5);
        let result = no_args::implicit_static_call(addr.clone(), contract_hash.clone()).await?;
        assert_eq!(result, 5);

        Ok(())
    }
    #[tokio::test]
    async fn test_args_simple() -> anyhow::Result<()> {
        init_tracing();
        let addr = "[::1]:7762".to_string();

        let function = "get-value".to_string();
        let contract_hash = "0x54AD8533C20E995DB809D203131C45F3D308322631B514044DF5B7B9E4EDABBC".to_string();
        
        let result = args_simple::explicit_call(addr.clone(), contract_hash.clone(), function.clone()).await?;
        assert_eq!(result, 5);
        
        Ok(())
    }

    #[tokio::test]
    async fn test_args_record() -> anyhow::Result<()> {
        init_tracing();
        let addr = "[::1]:7762".to_string();

        let function = "total-value".to_string();
        let contract_hash = "0x58A71921DA8E8558755C37B8EEAB79E416DDDE1B2B516155767EEC868BE26C5E".to_string();
        
        let result = args_record::explicit_call(addr.clone(), contract_hash.clone(), function.clone()).await?;
        assert_eq!(result, 50);
        
        Ok(())
    }
}
