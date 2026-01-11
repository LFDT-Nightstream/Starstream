use std::path::PathBuf;

use anyhow::Context as _;
use starstream_ledger::encode::gen_ctx;
use wasmtime::{Engine, Store, component::{Component, Linker, Val}};
use wrpc_multiplexer::{MultiplexClient, InvocationContext};
use wrpc_pack::{unpack};
use crate::rpc;

mod bindings {
    wasmtime::component::bindgen!({
        path: "../node-server/genesis/wit/no-args/wit"
    });
}

/**
 * Call the wRPC implicitly through Wasm component function calls
 * 
 * Benefits:
 * - automatic type handling
 * - better composability with libraries that expect a Wasm component as input
 * Downsides:
 * - ownership of resources is less clear (especially because the call indirection doesn't support host resource passing)
 * - lifetimes are less clear (a UTXO dying on the ledger side means the Wasm component is not referencing anything anymore)
 * - Possibly misleading for any composability / chained use-cases, as state is in the remote ledger and not in local components
 */
pub async fn implicit_dynamic_call(
    addr: String,
    contract_hash: String,
    function: String,
) -> anyhow::Result<i64> {
    let wrpc = MultiplexClient::new(wrpc_transport::tcp::Client::from(addr.clone()));

    // Note: each component gets its own Engine, since you can't compose these components anyways (as state is owned by the remote ledger)
    let mut config = wasmtime::Config::default();
    config.async_support(true);
    let engine = Engine::new(&config)?;

    let component_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../node-server")
        .join("genesis")
        .join("no-args.wasm");
    let component_bytes = std::fs::read(&component_path)
        .with_context(|| format!("failed to read component from {:?}", component_path))?;
    let component = Component::new(&engine, component_bytes)
              .context("failed to parse component")?;

    let mut store = Store::new(&engine, gen_ctx(wrpc, InvocationContext::new((), contract_hash.clone())));
    let linker = Linker::new(&engine);
    let instance = linker.instantiate_async(&mut store, &component).await?;

    let func = instance
        .get_func(&mut store, &"get-value")
        .ok_or_else(|| anyhow::anyhow!("function `{function}` not found in component"))?;

    let mut results = vec![Val::S64(0)];
    let params = vec![];
    func.call_async(store, &params, &mut results).await?;
    let result = match results[0] {
        Val::S64(i) => Some(i),
        _ => panic!("expected i64, got {:?}", results[0]),
    };
    Ok(result.unwrap())
}

/**
 * Same as `implicit_dynamic_call`, but with compile-time generated type bindings
 */
pub async fn implicit_static_call(
    addr: String,
    contract_hash: String,
) -> anyhow::Result<i64> {
    let wrpc = MultiplexClient::new(wrpc_transport::tcp::Client::from(addr.clone()));

    // Note: each component gets its own Engine, since you can't compose these components anyways (as state is owned by the remote ledger)
    let config = wasmtime::Config::default();
    let engine = Engine::new(&config)?;

    let component_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../node-server")
        .join("genesis")
        .join("no-args.wasm");
    let component_bytes = std::fs::read(&component_path)
        .with_context(|| format!("failed to read component from {:?}", component_path))?;
    let component = Component::new(&engine, component_bytes)
              .context("failed to parse component")?;

    let mut store = Store::new(&engine, gen_ctx(wrpc, InvocationContext::new((), contract_hash.clone())));
    let linker = Linker::new(&engine);

    let bindings = bindings::Root::instantiate_async(&mut store, &component, &linker).await?;
    let result = bindings.call_get_value(&mut store)?;
    Ok(result)
}

/**
 * Call the wRPC explicitly through the wRPC Multiplexer WIT interface
 * 
 * Benefits:
 * - Less Wasm component setup boilerplate
 * - Truer to what is actually happening (less chance of accidentally being confused by the abstraction of implicit calls)
 * Downsides:
 * - Manually ensure `pack` and `unpack` types have the right type annotation for your WIT
 */
pub async fn explicit_call(
    addr: String,
    contract_hash: String,
    function: String,
) -> anyhow::Result<i64> {
    let wrpc = wrpc_transport::tcp::Client::from(addr);
    let params = wit_bindgen_wrpc::bytes::Bytes::new(); // Empty params

    let result = rpc::bindings::starstream::wrpc_multiplexer::handler::call(
        &wrpc,
        (),
        &contract_hash.clone(),
        &function.clone(),
        &params.into(),
    )
    .await
    .with_context(|| format!("failed to call `{contract_hash}.{function}`"))?;

    // note: you have to "know" that the result of the call here is an i64. It will be a runtime error if you get the type wrong
    // you can do better if you have access to the WIT
    let result: i64 = unpack(&mut result.into())?;
    Ok(result)
}