use anyhow::Context as _;
use clap::Parser;
use starstream_ledger::encode::gen_ctx;
use wasmtime::{Engine, Store, component::Linker};
use wrpc_multiplexer::{MultiplexClient, InvocationContext};

mod bindings {
    wit_bindgen_wrpc::generate!({
        path: "./wit/rpc/wit",
        with: {
            "starstream:node-rpc/handler": generate,
            "starstream:wrpc-multiplexer/handler": generate,
        }
    });
}


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

    explicit_call(addr.clone(), contract_hash.clone(), function.clone()).await?;
    let wrpc = MultiplexClient::new(wrpc_transport::tcp::Client::from(addr.clone()));

    let mut config = wasmtime::Config::default();
    config.async_support(true);
    let engine = Engine::new(&config)?;
    let mut store = Store::new(&engine, gen_ctx(wrpc, InvocationContext::new(())));
    // let linker = Linker::new(&engine);
    // TODO: use this wrpc to make a call
    Ok(())
}

async fn explicit_call(
    addr: String,
    contract_hash: String,
    function: String,
) -> anyhow::Result<()> {
    let wrpc = wrpc_transport::tcp::Client::from(addr);
    let params = wit_bindgen_wrpc::bytes::Bytes::new(); // Empty params

    let result = bindings::starstream::wrpc_multiplexer::handler::call(
        &wrpc,
        (),
        &contract_hash.clone(),
        &function.clone(),
        &params,
    )
    .await
    .with_context(|| format!("failed to call `{contract_hash}.{function}`"))?;

    // print a hex string of the bytes for now
    // In a real implementation, you'd decode the result based on the function's return type
    println!("Result: {:?}", result);
    Ok(())
}