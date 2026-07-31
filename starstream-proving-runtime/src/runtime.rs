use std::ops::Deref;
use std::sync::Arc;

use starstream_runtime_next::{Contract, Host};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut, Engine, Store, StoreContextMut, bail};

/// Store data used by the Wasm tracing adapter.
///
/// The Starstream runtime only exposes a neutral post-instantiation hook.
/// This trait and all proving instrumentation state remain owned by
/// `starstream-proving-runtime`.
pub trait WasmTraceHost: Host + neo_wasm::WasmTraceSink {
    fn register_wasm_trace(
        &mut self,
        instance_index: u32,
        trace: neo_wasm::WasmtimeTraceState,
    ) -> wasmtime::Result<()>;
}

/// A Wasmtime configuration suitable for Wasm instruction tracing.
#[must_use]
pub fn new_wasmtime_config() -> wasmtime::Config {
    let mut config = wasmtime::Config::new();
    config.guest_debug(true);
    config
}

/// Construct a store that forwards guest instruction breakpoints to the trace
/// handler.
pub fn new_tracing_wasmtime_store<T: WasmTraceHost + Send>(
    engine: &Engine,
    data: T,
) -> wasmtime::Result<Store<T>> {
    let mut store = Store::new(engine, data);
    store.set_debug_handler(neo_wasm::WasmtimeTraceHandler::<T>::new());
    store
        .edit_breakpoints()
        .context("guest debug is not enabled on the Wasmtime engine")?
        .single_step(true)
        .context("failed to enable single-step debugging")?;
    Ok(store)
}

/// A runtime contract paired with the artifacts needed to trace each core
/// instance it creates.
pub struct TracedContract<T: 'static> {
    contract: Contract<T>,
}

impl<T: 'static> Clone for TracedContract<T> {
    fn clone(&self) -> Self {
        Self {
            contract: self.contract.clone(),
        }
    }
}

impl<T: 'static> Deref for TracedContract<T> {
    type Target = Contract<T>;

    fn deref(&self) -> &Self::Target {
        &self.contract
    }
}

impl<T: WasmTraceHost> TracedContract<T> {
    /// Compile a Starstream contract and its matching trace artifacts.
    pub fn new(engine: &Engine, wasm: impl AsRef<[u8]>) -> wasmtime::Result<Self> {
        let wasm = wasm.as_ref();
        let trace_artifacts = Arc::new(
            if wasmparser::Parser::is_core_wasm(wasm) {
                neo_wasm::extract_wasm_program_artifacts(wasm)
            } else {
                neo_wasm::extract_first_component_core_program_artifacts(wasm)
            }
            .map_err(|error| wasmtime::format_err!("failed to extract trace artifacts: {error}"))?,
        );
        let contract =
            Contract::new(engine, wasm)?.with_post_instantiation_hook(move |store, _instance| {
                register_unregistered_core_instance(store, &trace_artifacts)
            });
        Ok(Self { contract })
    }

    /// Access the backend-neutral runtime contract.
    #[must_use]
    pub fn contract(&self) -> &Contract<T> {
        &self.contract
    }
}

fn register_unregistered_core_instance<T: WasmTraceHost>(
    mut store: StoreContextMut<'_, T>,
    trace_artifacts: &neo_wasm::WasmProgramArtifacts,
) -> wasmtime::Result<()> {
    // Core Wasm start functions execute before the post-instantiation hook.
    // The trace handler reports their instructions through
    // `WasmTraceSink::record_untraced_instance`, since no trace state has
    // been registered yet.
    //
    // TODO: The hook receives the newly created component `Instance`, but
    // Wasmtime currently has no API for obtaining the core debug instances
    // instantiated underneath it. Until it does, scan the entire store and
    // require exactly one core instance without registered trace state. This
    // detects uninstrumented contracts, but the repeated full-store scan still
    // makes n component instantiations cumulatively O(n²). Ideally Wasmtime
    // would expose the component-to-core relationship (for example,
    // `Instance::debug_core_instances`), allowing direct registration here.
    let core_instances = store.as_context_mut().debug_all_instances();
    let unregistered_core_instances = core_instances
        .into_iter()
        .filter(|instance| {
            store
                .data()
                .wasm_trace_state(instance.debug_index_in_store())
                .is_none()
        })
        .collect::<Vec<_>>();
    let [core_instance] = unregistered_core_instances.as_slice() else {
        bail!(
            "instruction tracing requires exactly one unregistered core instance after component \
             instantiation, but found {}",
            unregistered_core_instances.len()
        )
    };

    let instance_index = core_instance.debug_index_in_store();
    let function_ids = neo_wasm::build_debug_function_id_map(core_instance, store.as_context_mut())
        .map_err(|error| {
            wasmtime::format_err!("failed to build the trace function-reference map: {error}")
        })?;
    let mut trace = neo_wasm::WasmtimeTraceState::from_program_artifacts(trace_artifacts);
    trace.set_func_ref_ids(function_ids);
    store.data_mut().register_wasm_trace(instance_index, trace)
}
