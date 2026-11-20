use std::collections::{HashMap, HashSet};

use starstream_proving_runtime::{
    CapturedExecution, ComponentTemplates, MethodHash, ProofTimings, ResourceHandle,
    TransactionProof, TransactionProver, WasmSize, WasmTraceSink, WasmtimeTraceRegistry,
    build_component_templates, check_captured_transaction, enable_tracing,
    register_tracing_component,
};
use starstream_runtime::CoordinationScriptExport;
use wasmtime::component::Type;
use wasmtime::error::Context as _;
use wasmtime::{Store, ensure};

use crate::client::CoordinationScriptArg;
use crate::client::runtime::{CompiledContract, Contract, Ctx};

/// Loaded component bytes retained for independent verification preparation.
#[derive(Debug)]
pub struct VerificationSource {
    wasm: bytes::Bytes,
    scripts: Vec<String>,
    digest: [u8; 32],
    pub(super) input_utxos: Vec<(usize, String)>,
}

/// Checked execution and the component sources loaded for this call.
#[derive(Debug)]
pub struct TracedCall {
    pub execution: CapturedExecution,
    pub programs: Vec<VerificationSource>,
    pub proof_stats: Option<(ProofTimings, starstream_proving_runtime::ProofSizes)>,
}

pub(super) fn get_or_insert_program<'a>(
    programs: &'a mut Vec<VerificationSource>,
    contract: &CompiledContract,
    digest: &[u8; 32],
) -> &'a mut VerificationSource {
    let index = programs
        .iter()
        .position(|source| source.digest == *digest && source.wasm == contract.instrumented)
        .unwrap_or_else(|| {
            programs.push(VerificationSource {
                wasm: contract.instrumented.clone(),
                scripts: contract
                    .coordination_script_names()
                    .map(str::to_owned)
                    .collect(),
                digest: *digest,
                input_utxos: Vec::new(),
            });
            programs.len() - 1
        });
    &mut programs[index]
}

impl WasmTraceSink for Ctx {
    fn wasm_trace_registry(&self) -> &WasmtimeTraceRegistry {
        &self.traces
    }

    fn wasm_trace_registry_mut(&mut self) -> &mut WasmtimeTraceRegistry {
        &mut self.traces
    }
}

/// Templates of the contract identified by its raw SHA-256 digest.
fn build_templates(
    contract: &CompiledContract,
    digest: &[u8; 32],
) -> wasmtime::Result<ComponentTemplates> {
    let scripts = contract
        .contract
        .coordination_script_names()
        .collect::<Vec<_>>();
    build_component_templates(&contract.instrumented, &scripts, digest)
        .context("failed to build trace templates")
}

fn register_tracing<'a>(
    cx: &mut Ctx,
    contract: &CompiledContract,
    digest: &[u8; 32],
    programs: &'a mut Vec<VerificationSource>,
) -> wasmtime::Result<&'a mut VerificationSource> {
    let templates = build_templates(contract, digest)?;
    register_tracing_component(cx, &contract.instrumented, &templates.bindings)?;
    Ok(get_or_insert_program(programs, contract, digest))
}

/// Register compiled imports that have not been registered yet
fn register_imports(
    cx: &mut Ctx,
    imports: &HashMap<[u8; 32], Contract>,
    registered: &mut HashSet<[u8; 32]>,
    programs: &mut Vec<VerificationSource>,
) -> wasmtime::Result<()> {
    for (digest, Contract { contract, .. }) in imports {
        let Some(contract) = contract else {
            continue;
        };
        if registered.insert(*digest) {
            register_tracing(cx, contract, digest, programs)?;
        }
    }
    Ok(())
}

/// Execution trace capture of a single coordination script call.
///
/// The first error is kept and returned by [`Tracing::check`], it does not
/// affect the call being traced.
pub struct Tracing {
    programs: Vec<VerificationSource>,
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
        let mut programs = Vec::new();
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
            let templates = build_templates(contract, &digest)?;
            enable_tracing(store)?;
            let cx = store.data_mut();
            register_tracing_component(cx, &contract.instrumented, &templates.bindings)?;
            get_or_insert_program(&mut programs, contract, &digest);
            register_imports(cx, imports, &mut registered, &mut programs)?;
            Ok(templates)
        })();
        Self {
            programs,
            templates,
            registered,
            input_locals: Vec::default(),
            input_methods: Vec::default(),
        }
    }

    /// Register `contract`, identified by `digest`, of an input UTXO passed as
    /// the coordinator's core local `local`
    pub fn register_input(
        &mut self,
        store: &mut Store<Ctx>,
        contract: &CompiledContract,
        digest: [u8; 32],
        imports: &HashMap<[u8; 32], Contract>,
        local: usize,
        utxo: &crate::TransactionOutput,
    ) {
        let cx = store.data_mut();
        let mut register = || {
            let source = register_tracing(cx, contract, &digest, &mut self.programs)?;
            source
                .input_utxos
                .push((self.input_locals.len(), utxo.instance.to_string()));
            register_imports(cx, imports, &mut self.registered, &mut self.programs)
        };
        if self.templates.is_ok()
            && let Err(err) = register()
        {
            self.templates = Err(err);
        }
        self.input_locals.push(local);
        self.input_methods.push(
            utxo.methods
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
    ) -> wasmtime::Result<TracedCall> {
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
        let execution =
            check_captured_transaction(&store.data().traces, handles, &self.input_methods)?;
        Ok(TracedCall {
            execution,
            programs: self.programs,
            proof_stats: None,
        })
    }
}

/// Prove `execution` using the traces captured in `store`.
///
/// Use [`super::attach_proof`] to include the result in a ledger transaction.
pub fn prove(
    store: &Store<Ctx>,
    execution: &CapturedExecution,
    prover: &mut TransactionProver,
) -> wasmtime::Result<(TransactionProof, ProofTimings)> {
    prover.prove(&store.data().traces, execution)
}

/// Reprocess the loaded component bytes, independently of the proof's artifacts.
pub fn prepare_verification(
    sources: &[VerificationSource],
    sizes: impl IntoIterator<Item = WasmSize>,
) -> wasmtime::Result<starstream_proving_runtime::PreparedVerification> {
    let scripts = sources
        .iter()
        .map(|source| {
            source
                .scripts
                .iter()
                .map(String::as_str)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let programs = sources
        .iter()
        .zip(&scripts)
        .map(
            |(source, scripts)| starstream_proving_runtime::VerificationProgram {
                wasm: &source.wasm,
                coordination_exports: scripts,
                contract_digest: &source.digest,
                input_utxos: &source.input_utxos,
            },
        )
        .collect::<Vec<_>>();

    starstream_proving_runtime::prepare_verification(&programs, sizes)
}
