use std::collections::BTreeSet;
use std::ops::Deref;

use starstream_runtime_next::{
    ConstructorExport, Contract, CoordinationScriptExport, Host, Utxo, UtxoStorageExport,
};
use wasmtime::component::Val;
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut, Engine, Store, StoreContextMut, bail};

/// Store data used by the Neo-Wasm tracing adapter.
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

/// A Wasmtime configuration suitable for Neo-Wasm instruction tracing.
#[must_use]
pub fn new_wasmtime_config() -> wasmtime::Config {
    let mut config = wasmtime::Config::new();
    config.wasm_component_model(true);
    config.guest_debug(true);
    config
}

/// Construct a store that forwards guest instruction breakpoints to Neo-Wasm.
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

/// A runtime contract paired with the Neo-Wasm artifacts needed to trace each
/// core instance it creates.
pub struct TracedContract<T: 'static> {
    contract: Contract<T>,
    trace_artifacts: neo_wasm::WasmProgramArtifacts,
}

impl<T: 'static> Clone for TracedContract<T> {
    fn clone(&self) -> Self {
        Self {
            contract: self.contract.clone(),
            trace_artifacts: self.trace_artifacts.clone(),
        }
    }
}

impl<T: 'static> Deref for TracedContract<T> {
    type Target = Contract<T>;

    fn deref(&self) -> &Self::Target {
        &self.contract
    }
}

impl<T: Host> TracedContract<T> {
    /// Compile a Starstream contract and its matching Neo-Wasm trace artifacts.
    pub fn new(engine: &Engine, wasm: impl AsRef<[u8]>) -> wasmtime::Result<Self> {
        let wasm = wasm.as_ref();
        let trace_artifacts = if wasmparser::Parser::is_core_wasm(wasm) {
            neo_wasm::extract_wasm_program_artifacts(wasm)
        } else {
            neo_wasm::extract_first_component_core_program_artifacts(wasm)
        }
        .map_err(|error| wasmtime::format_err!("failed to extract trace artifacts: {error}"))?;
        let contract = Contract::new(engine, wasm)?;
        Ok(Self {
            contract,
            trace_artifacts,
        })
    }

    /// Access the backend-neutral runtime contract.
    #[must_use]
    pub fn contract(&self) -> &Contract<T> {
        &self.contract
    }

    /// Create a UTXO and register its core instance before the constructor's
    /// first guest instruction executes.
    pub async fn create_utxo(
        &self,
        mut store: impl AsContextMut<Data = T>,
        export: &ConstructorExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo>
    where
        T: WasmTraceHost + Send,
    {
        let before = debug_instance_indices(store.as_context_mut());
        let trace_artifacts = &self.trace_artifacts;
        self.contract
            .create_utxo_with_hook(
                store.as_context_mut(),
                export,
                params,
                move |store, _instance| register_new_core_instance(store, &before, trace_artifacts),
            )
            .await
    }

    /// Load a UTXO and register its core instance before its storage setter's
    /// first guest instruction executes.
    pub async fn load_utxo(
        &self,
        mut store: impl AsContextMut<Data = T>,
        export: &UtxoStorageExport,
        fields: impl Into<Vec<(String, Val)>>,
    ) -> wasmtime::Result<Utxo>
    where
        T: WasmTraceHost + Send,
    {
        let before = debug_instance_indices(store.as_context_mut());
        let trace_artifacts = &self.trace_artifacts;
        self.contract
            .load_utxo_with_hook(
                store.as_context_mut(),
                export,
                fields,
                move |store, _instance| register_new_core_instance(store, &before, trace_artifacts),
            )
            .await
    }

    /// Call a coordination script after registering its newly instantiated
    /// core module with Neo-Wasm.
    pub async fn call_coordination_script(
        &self,
        mut store: impl AsContextMut<Data = T>,
        export: &CoordinationScriptExport,
        params: impl AsRef<[Val]>,
        results: impl AsMut<[Val]>,
    ) -> wasmtime::Result<()>
    where
        T: WasmTraceHost + Send,
    {
        let before = debug_instance_indices(store.as_context_mut());
        let trace_artifacts = &self.trace_artifacts;
        self.contract
            .call_coordination_script_with_hook(
                store.as_context_mut(),
                export,
                params,
                results,
                move |store, _instance| register_new_core_instance(store, &before, trace_artifacts),
            )
            .await
    }
}

fn debug_instance_indices<T>(store: StoreContextMut<'_, T>) -> BTreeSet<u32> {
    store
        .debug_all_instances()
        .into_iter()
        .map(|instance| instance.debug_index_in_store())
        .collect()
}

fn register_new_core_instance<T: WasmTraceHost>(
    mut store: StoreContextMut<'_, T>,
    before: &BTreeSet<u32>,
    trace_artifacts: &neo_wasm::WasmProgramArtifacts,
) -> wasmtime::Result<()> {
    // TODO: The runtime hook also provides the newly created component
    // `Instance`, but Wasmtime currently has no API for obtaining the core
    // debug instances instantiated underneath it. Until it does, we have to
    // list every store-local core instance before and after instantiation and
    // compute the difference. Repeating that full-store scan makes a sequence
    // of component instantiations cumulatively quadratic. Ideally Wasmtime
    // would expose the component-to-core relationship (for example,
    // `Instance::debug_core_instances`); once available, pass the component
    // instance here and register its core instance directly.
    let new_core_instances = store
        .as_context_mut()
        .debug_all_instances()
        .into_iter()
        .filter(|instance| !before.contains(&instance.debug_index_in_store()))
        .collect::<Vec<_>>();
    let [core_instance] = new_core_instances.as_slice() else {
        bail!(
            "Neo-Wasm tracing currently requires exactly one new core instance, but component \
             instantiation created {}",
            new_core_instances.len()
        )
    };

    let instance_index = core_instance.debug_index_in_store();
    let function_ids = neo_wasm::build_debug_function_id_map(core_instance, store.as_context_mut())
        .map_err(|error| {
            wasmtime::format_err!("failed to build the Neo-Wasm function-reference map: {error}")
        })?;
    let mut trace = neo_wasm::WasmtimeTraceState::from_program_artifacts(trace_artifacts);
    trace.set_func_ref_ids(function_ids);
    store.data_mut().register_wasm_trace(instance_index, trace)
}
