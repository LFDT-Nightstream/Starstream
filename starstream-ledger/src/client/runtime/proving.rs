use std::collections::{BTreeSet, HashMap, HashSet};

use starstream_proving_runtime::{
    CapturedExecution, ComponentTemplates, MethodHash, ResourceHandle, WasmTraceSink,
    WasmtimeTraceRegistry, build_component_templates, check_captured_transaction, enable_tracing,
    register_tracing_component,
};
use starstream_runtime::CoordinationScriptExport;
use wasmtime::component::Type;
use wasmtime::error::Context as _;
use wasmtime::{Store, ensure};

use crate::client::CoordinationScriptArg;
use crate::client::runtime::{CompiledContract, Contract, Ctx};

impl WasmTraceSink for Ctx {
    fn wasm_trace_registry(&self) -> &WasmtimeTraceRegistry {
        &self.traces
    }

    fn wasm_trace_registry_mut(&mut self) -> &mut WasmtimeTraceRegistry {
        &mut self.traces
    }
}

fn build_templates(contract: &CompiledContract) -> wasmtime::Result<ComponentTemplates> {
    let scripts = contract
        .contract
        .coordination_script_names()
        .collect::<Vec<_>>();
    build_component_templates(&contract.instrumented, &scripts)
        .context("failed to build trace templates")
}

fn register_tracing(cx: &mut Ctx, contract: &CompiledContract) -> wasmtime::Result<()> {
    let templates = build_templates(contract)?;
    register_tracing_component(cx, &contract.instrumented, &templates.bindings)
}

/// Register compiled imports that have not been registered yet
fn register_imports(
    cx: &mut Ctx,
    imports: &HashMap<[u8; 32], Contract>,
    registered: &mut HashSet<[u8; 32]>,
) -> wasmtime::Result<()> {
    for (digest, Contract { contract, .. }) in imports {
        let Some(contract) = contract else {
            continue;
        };
        if registered.insert(*digest) {
            register_tracing(cx, contract)?;
        }
    }
    Ok(())
}

/// Execution trace capture of a single coordination script call.
///
/// The first error is kept and returned by [`Tracing::check`], it does not
/// affect the call being traced.
pub struct Tracing {
    templates: wasmtime::Result<ComponentTemplates>,
    registered: HashSet<[u8; 32]>,
    input_locals: Vec<usize>,
    input_methods: Vec<Vec<MethodHash>>,
}

impl Tracing {
    /// Enable tracing in `store` for `contract`, identified by `digest`, and its `imports`
    pub fn new(
        store: &mut Store<Ctx>,
        contract: &CompiledContract,
        digest: [u8; 32],
        export: &CoordinationScriptExport,
        imports: &HashMap<[u8; 32], Contract>,
        args: &[CoordinationScriptArg],
    ) -> Self {
        let mut registered = HashSet::from([digest]);
        let templates = (|| {
            if args
                .iter()
                .any(|arg| matches!(arg, CoordinationScriptArg::Utxo(_)))
            {
                // Direct scalar/resource parameters each lower to _one_ core local.
                // TODO: Support composite and indirectly lowered coordinator arguments.
                ensure!(
                    export.ty().params().len() <= 16
                        && export.ty().params().all(|(_, ty)| matches!(
                            ty,
                            Type::Bool
                                | Type::S8
                                | Type::U8
                                | Type::S16
                                | Type::U16
                                | Type::S32
                                | Type::U32
                                | Type::S64
                                | Type::U64
                                | Type::Float32
                                | Type::Float64
                                | Type::Char
                                | Type::Own(_)
                                | Type::Borrow(_)
                        )),
                    "traced input loading requires direct scalar/resource arguments"
                );
            }
            let templates = build_templates(contract)?;
            enable_tracing(store)?;
            let cx = store.data_mut();
            register_tracing_component(cx, &contract.instrumented, &templates.bindings)?;
            register_imports(cx, imports, &mut registered)?;
            Ok(templates)
        })();
        Self {
            templates,
            registered,
            input_locals: Vec::default(),
            input_methods: Vec::default(),
        }
    }

    /// Register `contract` of an input UTXO passed as the coordinator's core local `local`
    pub fn register_input(
        &mut self,
        store: &mut Store<Ctx>,
        contract: &CompiledContract,
        imports: &HashMap<[u8; 32], Contract>,
        local: usize,
        methods: &BTreeSet<(u64, u64, u64, u64)>,
    ) {
        let cx = store.data_mut();
        if self.templates.is_ok()
            && let Err(err) = register_tracing(cx, contract)
                .and_then(|()| register_imports(cx, imports, &mut self.registered))
        {
            self.templates = Err(err);
        }
        self.input_locals.push(local);
        self.input_methods.push(
            methods
                .iter()
                .map(|&(a, b, c, d)| MethodHash::from_u64_words([a, b, c, d]))
                .collect(),
        );
    }

    /// Check the captured trace of the call of `export`
    pub fn check(
        self,
        store: &Store<Ctx>,
        export: &CoordinationScriptExport,
    ) -> wasmtime::Result<CapturedExecution> {
        let templates = self.templates?;
        let mut handles = Vec::with_capacity(self.input_locals.len());
        if !self.input_locals.is_empty() {
            // The root coordinator is instantiated before input UTXOs;
            // the registry iterates in instance-index order.
            let mut instances = store
                .data()
                .traces
                .instances()
                .context("failed to capture trace")?;
            let (.., coordinator) = instances.next().context("missing coordinator trace")?;
            let fref = templates
                .export_fref(export.name())
                .context("missing coordinator export")?;
            let entry = coordinator
                .steps()
                .iter()
                .find(|step| step.current_function_ref == Some(fref))
                .context("missing coordinator entry arguments")?;
            for local in self.input_locals {
                let &(handle, ..) = entry
                    .locals_words
                    .get(local)
                    .context("missing input handle local")?;
                handles.push(ResourceHandle(handle));
            }
        }
        check_captured_transaction(&store.data().traces, handles, &self.input_methods)
    }
}
