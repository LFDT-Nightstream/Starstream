use anyhow::Context as _;
use bytes::BytesMut;
use clap::Parser;
use wrpc_wave::{WaveEncoder, read_value_sync};
mod args_record;
mod args_simple;
mod no_args;
mod rpc;
use wasm_wave::{
    untyped::UntypedFuncCall,
    value::{FuncType, Value, resolve_wit_func_type},
    wasm::{WasmFunc, WasmValue},
};
use wit_bindgen_wrpc::tokio_util::codec::Encoder;
use wit_parser::{PackageId, Resolve};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Address of the dynamic handler server
    #[arg(long, default_value = "[::1]:7762")]
    addr: String,
    /// Contract hash to invoke
    #[arg(long)]
    contract_hash: String,
    /// Function call to invoke using wasm-wave encoding (e.g., "get-value()")
    #[arg(long)]
    call: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt().init();

    let Args {
        addr,
        contract_hash,
        call,
    } = Args::parse();

    let wrpc = wrpc_transport::tcp::Client::from(addr);
    let interface = rpc::bindings::starstream::node_rpc::registry::get_wit(
        &wrpc,
        (),
        &contract_hash.clone(),
        true,
    )
    .await
    .with_context(|| format!("failed to get wit `{contract_hash}`"))?;
    println!("result: {interface:?}");

    let mut resolve = Resolve::new();
    let main = resolve.push_str(interface.entrypoint.clone(), &interface.wit)?;
    let untyped_call = UntypedFuncCall::parse(&call)?;
    let func_name = untyped_call.name().to_string();
    let func_type = get_func_type(&resolve, &main, &func_name)?;
    let param_types = func_type.params().collect::<Vec<_>>();

    println!("Calling {func_name} ({func_type})");

    let mut params = BytesMut::new();
    let values: Vec<Value> = untyped_call.to_wasm_params::<Value>(&param_types)?;
    if let Some(tuple_ty) = wasm_wave::value::Type::tuple(param_types) {
        let tuple_val = Value::make_tuple(&tuple_ty, values)?;
        let mut encoder = WaveEncoder::new(&tuple_ty);
        encoder.encode(&tuple_val, &mut params)?;
    }

    // TODO: properly error if wrong params (this bug is hidden by param parsing above)
    let result = rpc::bindings::starstream::wrpc_multiplexer::handler::call(
        &wrpc,
        (),
        &contract_hash.clone(),
        &func_name.clone(),
        &params.into(),
    )
    .await
    .with_context(|| format!("failed to call `{contract_hash}.{func_name}`"))?;

    let result_types = func_type.results().collect::<Vec<_>>();
    let mut values = Vec::new();
    for result_type in result_types.iter() {
        let val = read_value_sync(result_type, &result)?;
        values.push(val);
    }

    let output = match values.len() {
        0 => String::new(),
        1 => wasm_wave::to_string(&values.into_iter().next().unwrap())?,
        _ => {
            let tuple_ty = wasm_wave::value::Type::tuple(result_types).unwrap();
            let tuple_val = wasm_wave::value::Value::make_tuple(&tuple_ty, values)?;
            wasm_wave::to_string(&tuple_val)?
        }
    };
    println!("{output}");

    Ok(())
}

fn get_func_type(
    resolve: &Resolve,
    pkg_id: &PackageId,
    func_name: &str,
) -> anyhow::Result<FuncType> {
    let world_id = resolve.select_world(&[*pkg_id], None)?;

    let key = wit_parser::WorldKey::Name(func_name.to_string());
    let world_item = resolve.worlds[world_id]
        .exports
        .get(&key)
        .ok_or_else(|| anyhow::anyhow!("function '{func_name}' not found in world exports"))?;
    let func = match world_item {
        wit_parser::WorldItem::Function(func) => func,
        _ => return Err(anyhow::anyhow!("'{func_name}' is not a function")),
    };
    resolve_wit_func_type(&resolve, func)
        .map_err(|e| anyhow::anyhow!("failed to resolve function type for '{func_name}': {e}"))
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
            let filter =
                tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| {
                    tracing_subscriber::EnvFilter::new(
                        "wrpc_transport=trace,\
                         wrpc_runtime_wasmtime=trace,\
                         wrpc_pack=trace,\
                         wrpc_multiplexer=trace,\
                         wit_bindgen_wrpc=trace,\
                         wrpc=trace,\
                         starstream=debug,\
                         info",
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
        let contract_hash =
            "0x1170FAD15BECBB08C00B29067171110B34E8B4CEBC648BA662147BA0F2F1224F".to_string();

        let result =
            no_args::explicit_call(addr.clone(), contract_hash.clone(), function.clone()).await?;
        assert_eq!(result, 5);
        let result =
            no_args::implicit_dynamic_call(addr.clone(), contract_hash.clone(), function.clone())
                .await?;
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
        let contract_hash =
            "0x54AD8533C20E995DB809D203131C45F3D308322631B514044DF5B7B9E4EDABBC".to_string();

        let result =
            args_simple::explicit_call(addr.clone(), contract_hash.clone(), function.clone())
                .await?;
        assert_eq!(result, 5);

        Ok(())
    }

    #[tokio::test]
    async fn test_args_record() -> anyhow::Result<()> {
        init_tracing();
        let addr = "[::1]:7762".to_string();

        let function = "total-value".to_string();
        let contract_hash =
            "0x58A71921DA8E8558755C37B8EEAB79E416DDDE1B2B516155767EEC868BE26C5E".to_string();

        let result =
            args_record::explicit_call(addr.clone(), contract_hash.clone(), function.clone())
                .await?;
        assert_eq!(result, 50);

        Ok(())
    }
}
