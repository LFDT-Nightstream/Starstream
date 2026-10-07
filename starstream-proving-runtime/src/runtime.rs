use neo_wasm::host_event_bindings::HostEventBindings;
use wasmtime::Store;
use wasmtime::error::Context as _;

#[must_use]
pub fn new_wasmtime_config() -> wasmtime::Config {
    let mut config = wasmtime::Config::new();
    config.guest_debug(true);
    config
}

/// Register a component's core module before it first executes.
///
/// Componentize compiler output first, then use the same component bytes for
/// the contract, templates, and this registration: module selection is exact.
pub fn register_tracing_component<T: neo_wasm::WasmTraceSink>(
    data: &mut T,
    wasm: &[u8],
    bindings: &HostEventBindings,
) -> wasmtime::Result<()> {
    let module = crate::templates::first_core_module(wasm)?;
    data.wasm_trace_registry_mut()
        .register_module(module, bindings.clone())
        .map_err(|error| wasmtime::format_err!("failed to register trace module: {error}"))
}

/// Register the contract's core module and create a single-step tracing store.
/// Neo-Wasm discovers each instance and captures its entry inputs automatically.
pub fn enable_tracing<T: neo_wasm::WasmTraceSink + Send + 'static>(
    store: &mut Store<T>,
) -> wasmtime::Result<()> {
    // NOTE: we need this guard because set_debug_handler panics otherwise
    wasmtime::ensure!(
        store.engine().get_guest_debug(),
        "tracing requires guest_debug to be enabled"
    );
    store.set_debug_handler(neo_wasm::WasmtimeTraceHandler::<T>::new());
    store
        .edit_breakpoints()
        .context("guest debug is not enabled on the Wasmtime engine")?
        .single_step(true)
        .context("failed to enable single-step debugging")?;

    Ok(())
}
