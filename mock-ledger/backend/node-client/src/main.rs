use anyhow::Context as _;
use clap::Parser;

mod bindings {
    wit_bindgen_wrpc::generate!({
        with: {
            "starstream:node-rpc/handler": generate,
        }
    });
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Address of the dynamic handler server
    #[arg(default_value = "[::1]:7762")]
    addr: String,
    /// Component instance to invoke (e.g., "wrpc-examples:hello/handler")
    #[arg(default_value = "root:component")]
    instance: String,
    /// Function name to invoke (e.g., "hello")
    #[arg(default_value = "get-value")]
    function: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

    let Args {
        addr,
        instance,
        function,
    } = Args::parse();

    let wrpc = wrpc_transport::tcp::Client::from(&addr);

    // Call through the dynamic handler
    // In a real implementation, you'd need to properly encode the parameters
    // using Component Model value encoding based on the function's signature
    let params = wit_bindgen_wrpc::bytes::Bytes::new(); // Empty params for hello() function

    let result = bindings::starstream::node_rpc::handler::call(
        &wrpc,
        (),
        "0xB9213ECB33239E80",
        &function.clone(),
        &params,
    )
    .await
    .with_context(|| format!("failed to call `{instance}.{function}`"))?;

    // In a real implementation, you'd decode the result based on the function's return type
    // For this POC, we'll assume it's a string (which hello() returns)
    let result_string = String::from_utf8(result.to_vec())
        .context("failed to decode result as UTF-8 string")?;

    eprintln!("Result: {result_string}");
    Ok(())
}

