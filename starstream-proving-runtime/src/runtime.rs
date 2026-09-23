use neo_wasm::host_event_bindings::HostEventBindings;
use wasmtime::error::Context as _;
use wasmtime::{Engine, Store};

#[must_use]
pub fn new_wasmtime_config() -> wasmtime::Config {
    let mut config = wasmtime::Config::new();
    config.guest_debug(true);
    config
}

/// Register the contract's core module before execution and enable tracing.
/// Neo-Wasm discovers each instance and captures its entry inputs automatically.
/// Componentize compiler output first, then use the same component bytes for
/// the contract, templates, and this registration: module selection is exact.
pub fn new_tracing_wasmtime_store<T: neo_wasm::WasmTraceSink + Send + 'static>(
    engine: &Engine,
    mut data: T,
    wasm: &[u8],
    bindings: &HostEventBindings,
) -> wasmtime::Result<Store<T>> {
    let module = crate::templates::first_core_module(wasm)?;
    data.wasm_trace_registry_mut()
        .register_module(module, bindings.clone())
        .map_err(|error| wasmtime::format_err!("failed to register trace module: {error}"))?;

    let mut store = Store::new(engine, data);
    store.set_debug_handler(neo_wasm::WasmtimeTraceHandler::<T>::new());
    store
        .edit_breakpoints()
        .context("guest debug is not enabled on the Wasmtime engine")?
        .single_step(true)
        .context("failed to enable single-step debugging")?;

    Ok(store)
}
