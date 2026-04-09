use crate::defaults::{COORD_WAT, UTXO_A_WAT, UTXO_B_WAT};
use crate::model::{
    CompileArtifact, CompileState, ContractCompileResult, ContractId, DemoContracts,
    DemoInitialStates, DemoSnapshot, EditorFile, ExecutionSnapshot, ProcessTraceView,
    StatusResponse, StepStatus, TraceEventView,
};
use anyhow::Context;
use ark_ff::PrimeField;
use starstream_component_runtime::{
    ProcessKind, ProgramDefinition, WasmtimeComponentStarstreamExecutor,
};
use starstream_interleaving_proof::abi::commit as commit_trace_effect;
use starstream_interleaving_proof::prove as prove_interleaving;
use starstream_interleaving_spec::{
    CoroutineId, CoroutineState, FunctionId, Hash, InterfaceId, Ledger, LedgerEffectsCommitment,
    ProcessId, Ref, TransactionBody, UtxoEntry, UtxoId, Value, WasmModule, WitEffectOutput,
    WitLedgerEffect, ZkTransactionProof, builder::TransactionBuilder,
};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::process::Command;

const RUST_COMPONENT_PREFIX: &str = ";; rust-component:";

pub struct DemoBackendState {
    pub contracts: DemoContracts,
    pub initial_states: DemoInitialStates,
    pub compile: CompileState,
    pub execution: ExecutionSnapshot,
    pub proof_output: Option<String>,
    pub proof_public_statement: Option<String>,
    pub proof_transaction_debug: Option<String>,
    pub ledger_before_debug: Option<String>,
    pub ledger_after_debug: Option<String>,
    recording: Option<RecordedExecution>,
    cached_artifacts: Option<DemoTransactionArtifacts>,
}

impl DemoBackendState {
    pub fn new() -> Self {
        Self {
            contracts: default_contracts(),
            initial_states: default_initial_states(),
            compile: CompileState {
                generation: 0,
                results: vec![],
            },
            execution: default_execution(),
            proof_output: None,
            proof_public_statement: None,
            proof_transaction_debug: None,
            ledger_before_debug: None,
            ledger_after_debug: None,
            recording: None,
            cached_artifacts: None,
        }
    }

    pub fn snapshot(&self) -> DemoSnapshot {
        DemoSnapshot {
            contracts: self.contracts.clone(),
            initial_states: self.initial_states.clone(),
            compile: self.compile.clone(),
            execution: self.execution.clone(),
            proof_output: self.proof_output.clone(),
            proof_public_statement: self.proof_public_statement.clone(),
            proof_transaction_debug: self.proof_transaction_debug.clone(),
            ledger_before_debug: self.ledger_before_debug.clone(),
            ledger_after_debug: self.ledger_after_debug.clone(),
        }
    }

    pub fn reset_scenario(&mut self) -> StatusResponse {
        self.contracts = default_contracts();
        self.initial_states = default_initial_states();
        self.compile = CompileState {
            generation: self.compile.generation + 1,
            results: vec![],
        };
        self.invalidate_execution_state();
        StatusResponse {
            ok: true,
            message: "loaded default demo scenario".to_owned(),
        }
    }

    pub fn update_contract(&mut self, id: ContractId, source: String) -> StatusResponse {
        match id {
            ContractId::Coord => self.contracts.coord_wat = source,
            ContractId::UtxoA => self.contracts.utxo_a_wat = source,
            ContractId::UtxoB => self.contracts.utxo_b_wat = source,
        }
        self.invalidate_execution_state();
        StatusResponse {
            ok: true,
            message: format!("updated {id:?} source"),
        }
    }

    pub fn update_initial_states(&mut self, utxo_a: u64, utxo_b: u64) -> StatusResponse {
        self.initial_states = DemoInitialStates { utxo_a, utxo_b };
        self.invalidate_execution_state();
        StatusResponse {
            ok: true,
            message: "updated initial ledger states".to_owned(),
        }
    }

    pub fn editor_files(&self, id: ContractId) -> anyhow::Result<Vec<EditorFile>> {
        let source = self.contract_source(id);
        load_editor_files_for_source(source)
    }

    pub fn update_editor_file(
        &mut self,
        id: ContractId,
        file_id: &str,
        source: String,
    ) -> anyhow::Result<StatusResponse> {
        let contract_source = self.contract_source(id).to_owned();
        if let Some(component_dir) = rust_component_dir(&contract_source) {
            let path = component_dir.join(file_id);
            std::fs::write(&path, source)
                .with_context(|| format!("failed to write {}", path.display()))?;
        } else if file_id == "component.wat" {
            self.set_contract_source(id, source);
        } else {
            anyhow::bail!("unknown editor file `{file_id}` for non-rust contract");
        }

        self.invalidate_execution_state();

        Ok(StatusResponse {
            ok: true,
            message: format!("updated {file_id}"),
        })
    }

    pub fn compile_contracts(&mut self) -> StatusResponse {
        let results = vec![
            compile_contract(ContractId::Coord, &self.contracts.coord_wat),
            compile_contract(ContractId::UtxoA, &self.contracts.utxo_a_wat),
            compile_contract(ContractId::UtxoB, &self.contracts.utxo_b_wat),
        ];
        let all_ok = results.iter().all(|result| result.ok);
        self.compile = CompileState {
            generation: self.compile.generation + 1,
            results,
        };
        self.invalidate_execution_state();
        self.execution.verification_status = if all_ok {
            StepStatus::Ready
        } else {
            StepStatus::Error("compile failed".to_owned())
        };
        StatusResponse {
            ok: all_ok,
            message: if all_ok {
                "compiled all demo contracts".to_owned()
            } else {
                "one or more contracts failed to compile".to_owned()
            },
        }
    }

    pub fn trace_execution(&mut self) -> StatusResponse {
        self.invalidate_execution_state();

        if !compile_ready(&self.compile) {
            self.execution.verification_status =
                StepStatus::Error("compile the demo contracts before tracing".to_owned());
            return StatusResponse {
                ok: false,
                message: "compile is required before tracing".to_owned(),
            };
        }

        match build_recording(&self.contracts, &self.initial_states) {
            Ok(recording) => {
                self.execution.total_steps = recording.frames.len();
                self.execution.per_process_traces = recording.full_process_traces.clone();
                self.execution.interleaved_trace.clear();
                self.execution.active_process = None;
                self.execution.highlighted_edge = None;
                self.execution.step_index = 0;
                self.execution.verification_status = StepStatus::Ready;
                self.recording = Some(recording);
                StatusResponse {
                    ok: true,
                    message: "runtime traces generated".to_owned(),
                }
            }
            Err(err) => {
                self.execution.verification_status = StepStatus::Error(format!("{err:#}"));
                StatusResponse {
                    ok: false,
                    message: "failed to generate runtime traces".to_owned(),
                }
            }
        }
    }

    pub fn reset_execution(&mut self) -> StatusResponse {
        self.execution.interleaved_trace.clear();
        self.execution.step_index = 0;
        self.execution.active_process = None;
        self.execution.highlighted_edge = None;
        self.execution.semantics_ok = None;
        self.execution.semantics_message = None;
        if self.recording.is_none() {
            self.execution.verification_status = if compile_ready(&self.compile) {
                StepStatus::Ready
            } else {
                StepStatus::Idle
            };
            return StatusResponse {
                ok: true,
                message: "interleaving cursor reset".to_owned(),
            };
        }
        self.execution.total_steps = self.recording.as_ref().map_or(0, |r| r.frames.len());
        self.execution.verification_status = StepStatus::Ready;
        StatusResponse {
            ok: true,
            message: "interleaving cursor reset".to_owned(),
        }
    }

    pub fn step_execution(&mut self) -> StatusResponse {
        if self.recording.is_none() {
            self.execution.verification_status =
                StepStatus::Error("generate traces before stepping interleaving".to_owned());
            return StatusResponse {
                ok: false,
                message: "trace is required before stepping".to_owned(),
            };
        }

        let recording = self.recording.as_ref().expect("recording initialized");
        if self.execution.step_index >= recording.frames.len() {
            self.execution.verification_status = StepStatus::Completed;
            return StatusResponse {
                ok: true,
                message: "execution is already complete".to_owned(),
            };
        }

        let frame = &recording.frames[self.execution.step_index];
        apply_frame(&mut self.execution, frame);
        self.execution.step_index += 1;
        self.execution.total_steps = recording.frames.len();
        self.execution.verification_status = if self.execution.step_index >= recording.frames.len()
        {
            StepStatus::Completed
        } else {
            StepStatus::Running
        };

        StatusResponse {
            ok: true,
            message: format!("advanced execution to step {}", self.execution.step_index),
        }
    }

    pub fn run_to_completion(&mut self) -> StatusResponse {
        if self.recording.is_none() {
            self.execution.verification_status =
                StepStatus::Error("generate traces before interleaving".to_owned());
            return StatusResponse {
                ok: false,
                message: "trace is required before running".to_owned(),
            };
        }

        let recording = self.recording.as_ref().expect("recording initialized");
        while self.execution.step_index < recording.frames.len() {
            let frame = &recording.frames[self.execution.step_index];
            apply_frame(&mut self.execution, frame);
            self.execution.step_index += 1;
        }

        self.execution.total_steps = recording.frames.len();
        self.execution.verification_status = StepStatus::Completed;
        StatusResponse {
            ok: true,
            message: "execution completed".to_owned(),
        }
    }

    pub fn verify_semantics(&mut self) -> StatusResponse {
        match self
            .artifacts()
            .and_then(|artifacts| verify_demo_transaction_semantics(&artifacts))
        {
            Ok(message) => {
                self.execution.semantics_ok = Some(true);
                self.execution.semantics_message = Some(message.clone());
                StatusResponse { ok: true, message }
            }
            Err(err) => {
                let message = format!("{err:#}");
                self.execution.semantics_ok = Some(false);
                self.execution.semantics_message = Some(message.clone());
                StatusResponse { ok: false, message }
            }
        }
    }

    pub fn fold_proof(&mut self) -> StatusResponse {
        if self.recording.is_none() {
            self.execution.verification_status =
                StepStatus::Error("generate traces before folding".to_owned());
            return StatusResponse {
                ok: false,
                message: "trace is required before folding".to_owned(),
            };
        }
        if self.execution.total_steps == 0 || self.execution.step_index < self.execution.total_steps
        {
            self.execution.verification_status =
                StepStatus::Error("finish stepping the interleaving before folding".to_owned());
            return StatusResponse {
                ok: false,
                message: "finish the interleaving before folding".to_owned(),
            };
        }

        match self.artifacts().and_then(summarize_and_prove_transaction) {
            Ok(proof_view) => {
                self.proof_output = Some(proof_view.summary);
                self.proof_public_statement = Some(proof_view.public_statement);
                self.proof_transaction_debug = Some(proof_view.transaction_debug);
                self.ledger_before_debug = None;
                self.ledger_after_debug = None;
                StatusResponse {
                    ok: true,
                    message: "folded proof successfully".to_owned(),
                }
            }
            Err(err) => {
                self.proof_output = Some(format!("proof generation failed\n\n{err:#}"));
                self.proof_public_statement = None;
                self.proof_transaction_debug = None;
                self.ledger_before_debug = None;
                self.ledger_after_debug = None;
                self.execution.verification_status = StepStatus::Error(format!("{err:#}"));
                StatusResponse {
                    ok: false,
                    message: "proof generation failed".to_owned(),
                }
            }
        }
    }

    pub fn apply_transaction_to_ledger(&mut self) -> StatusResponse {
        if self.proof_public_statement.is_none() {
            return StatusResponse {
                ok: false,
                message: "fold the proof before applying the transaction".to_owned(),
            };
        }

        match self.artifacts().and_then(apply_demo_transaction) {
            Ok((ledger_before, ledger_after)) => {
                self.ledger_before_debug = Some(ledger_before);
                self.ledger_after_debug = Some(ledger_after);
                self.execution.semantics_ok = Some(true);
                self.execution.semantics_message = Some("Transaction applied to ledger".to_owned());
                StatusResponse {
                    ok: true,
                    message: "applied transaction to ledger".to_owned(),
                }
            }
            Err(err) => {
                self.ledger_before_debug = None;
                self.ledger_after_debug = None;
                self.execution.semantics_ok = Some(false);
                self.execution.semantics_message = Some(format!("{err:#}"));
                StatusResponse {
                    ok: false,
                    message: "failed to apply transaction to ledger".to_owned(),
                }
            }
        }
    }

    fn contract_source(&self, id: ContractId) -> &str {
        match id {
            ContractId::Coord => &self.contracts.coord_wat,
            ContractId::UtxoA => &self.contracts.utxo_a_wat,
            ContractId::UtxoB => &self.contracts.utxo_b_wat,
        }
    }

    fn set_contract_source(&mut self, id: ContractId, source: String) {
        match id {
            ContractId::Coord => self.contracts.coord_wat = source,
            ContractId::UtxoA => self.contracts.utxo_a_wat = source,
            ContractId::UtxoB => self.contracts.utxo_b_wat = source,
        }
    }

    fn clear_proof_outputs(&mut self) {
        self.proof_output = None;
        self.proof_public_statement = None;
        self.proof_transaction_debug = None;
        self.ledger_before_debug = None;
        self.ledger_after_debug = None;
    }

    fn invalidate_execution_state(&mut self) {
        self.execution = default_execution();
        self.clear_proof_outputs();
        self.recording = None;
        self.cached_artifacts = None;
    }

    fn artifacts(&mut self) -> anyhow::Result<&DemoTransactionArtifacts> {
        if self.cached_artifacts.is_none() {
            let artifacts =
                build_demo_transaction_artifacts(&self.contracts, &self.initial_states)?;
            self.cached_artifacts = Some(artifacts);
        }
        Ok(self.cached_artifacts.as_ref().expect("artifacts cached"))
    }
}

#[derive(Clone, Debug)]
struct RecordedExecution {
    frames: Vec<RecordedFrame>,
    full_process_traces: Vec<ProcessTraceView>,
}

#[derive(Clone, Debug)]
struct RecordedFrame {
    process: String,
    highlighted_edge: Option<String>,
    batch: Vec<TraceEventView>,
}

struct DemoTransactionArtifacts {
    ledger: Ledger,
    tx: starstream_interleaving_spec::ProvenTransaction,
    utxo_a_hash: Hash<WasmModule>,
    utxo_b_hash: Hash<WasmModule>,
}

#[derive(Clone, Debug)]
struct ProofView {
    summary: String,
    public_statement: String,
    transaction_debug: String,
}

fn default_contracts() -> DemoContracts {
    DemoContracts {
        coord_wat: COORD_WAT.to_owned(),
        utxo_a_wat: UTXO_A_WAT.to_owned(),
        utxo_b_wat: UTXO_B_WAT.to_owned(),
    }
}

fn default_initial_states() -> DemoInitialStates {
    DemoInitialStates {
        utxo_a: 2,
        utxo_b: 0,
    }
}

fn default_execution() -> ExecutionSnapshot {
    ExecutionSnapshot {
        step_index: 0,
        total_steps: 0,
        active_process: None,
        highlighted_edge: None,
        per_process_traces: vec![
            ProcessTraceView {
                process: "coord".to_owned(),
                effects: vec![],
                processed: 0,
                commitment: None,
            },
            ProcessTraceView {
                process: "utxo-a".to_owned(),
                effects: vec![],
                processed: 0,
                commitment: None,
            },
            ProcessTraceView {
                process: "utxo-b".to_owned(),
                effects: vec![],
                processed: 0,
                commitment: None,
            },
        ],
        interleaved_trace: Vec::<TraceEventView>::new(),
        verification_status: StepStatus::Idle,
        semantics_ok: None,
        semantics_message: None,
    }
}

fn compile_ready(compile: &CompileState) -> bool {
    !compile.results.is_empty() && compile.results.iter().all(|result| result.ok)
}

fn compile_contract(contract: ContractId, source: &str) -> ContractCompileResult {
    match compile_contract_inner(source) {
        Ok(artifact) => ContractCompileResult {
            contract,
            ok: true,
            artifact: Some(artifact),
            error: None,
        },
        Err(err) => ContractCompileResult {
            contract,
            ok: false,
            artifact: None,
            error: Some(format!("{err:#}")),
        },
    }
}

fn compile_contract_inner(source: &str) -> anyhow::Result<CompileArtifact> {
    let wasm = load_contract_bytes(source)?;
    let wat_pretty =
        wasmprinter::print_bytes(&wasm).context("failed to pretty-print compiled bytes as WAT")?;
    let wit = decode_wit(&wasm);
    Ok(CompileArtifact {
        wat_pretty: Some(wat_pretty),
        wit,
    })
}

fn decode_wit(wasm: &[u8]) -> Option<String> {
    let decoded = std::panic::catch_unwind(|| wit_component::decode(wasm)).ok()?;
    let decoded = decoded.ok()?;
    let printed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut printer = wit_component::WitPrinter::default();
        printer.print(decoded.resolve(), decoded.package(), &[])?;
        Ok::<_, anyhow::Error>(std::mem::take(&mut printer.output).into())
    }))
    .ok()?;
    printed.ok()
}

fn load_editor_files_for_source(source: &str) -> anyhow::Result<Vec<EditorFile>> {
    if let Some(component_dir) = rust_component_dir(source) {
        let files = [
            ("src/lib.rs", "lib.rs", "rust"),
            ("wit/world.wit", "world.wit", "wit"),
        ];
        let mut out = Vec::new();
        for (id, label, language) in files {
            let path = component_dir.join(id);
            let source = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read {}", path.display()))?;
            out.push(EditorFile {
                id: id.to_owned(),
                label: label.to_owned(),
                language: language.to_owned(),
                source,
            });
        }
        return Ok(out);
    }

    Ok(vec![EditorFile {
        id: "component.wat".to_owned(),
        label: "component.wat".to_owned(),
        language: "wat".to_owned(),
        source: source.to_owned(),
    }])
}

fn load_contract_bytes(source: &str) -> anyhow::Result<Vec<u8>> {
    if let Some(component_dir) = rust_component_dir(source) {
        return build_rust_component(component_dir);
    }

    wat::parse_str(source).context("failed to parse WAT/component WAT")
}

fn rust_component_dir(source: &str) -> Option<PathBuf> {
    let first_line = source.lines().next()?.trim();
    let relative = first_line.strip_prefix(RUST_COMPONENT_PREFIX)?.trim();
    if relative.is_empty() {
        return None;
    }
    Some(workspace_root().join(relative))
}

fn build_rust_component(component_dir: PathBuf) -> anyhow::Result<Vec<u8>> {
    let output = Command::new("cargo")
        .args(["component", "build", "--release"])
        .current_dir(&component_dir)
        .output()
        .with_context(|| {
            format!(
                "failed to spawn cargo-component in {}",
                component_dir.display()
            )
        })?;

    anyhow::ensure!(
        output.status.success(),
        "cargo component build failed in {}\n\n{}",
        component_dir.display(),
        String::from_utf8_lossy(&output.stderr)
    );

    let crate_name = rust_package_name(&component_dir)?.replace('-', "_");
    let workspace_target = workspace_root()
        .join("target/wasm32-wasip1/release")
        .join(format!("{crate_name}.wasm"));
    let local_target = component_dir
        .join("target/wasm32-wasip1/release")
        .join(format!("{crate_name}.wasm"));
    let wasm_path = if workspace_target.exists() {
        workspace_target
    } else {
        local_target
    };

    std::fs::read(&wasm_path)
        .with_context(|| format!("failed to read built component {}", wasm_path.display()))
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .to_path_buf()
}

fn rust_package_name(component_dir: &Path) -> anyhow::Result<String> {
    let manifest_path = component_dir.join("Cargo.toml");
    let manifest = std::fs::read_to_string(&manifest_path)
        .with_context(|| format!("failed to read {}", manifest_path.display()))?;

    let mut in_package = false;
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            in_package = trimmed == "[package]";
            continue;
        }

        if !in_package || !trimmed.starts_with("name") {
            continue;
        }

        let Some((_, value)) = trimmed.split_once('=') else {
            continue;
        };
        let name = value.trim().trim_matches('"');
        if !name.is_empty() {
            return Ok(name.to_owned());
        }
    }

    anyhow::bail!(
        "failed to infer Rust guest crate name from {}",
        manifest_path.display()
    )
}

fn build_recording(
    contracts: &DemoContracts,
    initial_states: &DemoInitialStates,
) -> anyhow::Result<RecordedExecution> {
    let (runtime, utxo_a_pid, utxo_b_pid, coord_pid) =
        execute_demo_runtime(contracts, initial_states)?;

    Ok(record_execution(
        runtime.effect_log(),
        &[
            (coord_pid, "coord"),
            (utxo_a_pid, "utxo-a"),
            (utxo_b_pid, "utxo-b"),
        ],
    ))
}

fn execute_demo_runtime(
    contracts: &DemoContracts,
    initial_states: &DemoInitialStates,
) -> anyhow::Result<(
    WasmtimeComponentStarstreamExecutor,
    ProcessId,
    ProcessId,
    ProcessId,
)> {
    let mut runtime = WasmtimeComponentStarstreamExecutor::new()?;

    let utxo_a_bytes =
        load_contract_bytes(&contracts.utxo_a_wat).context("failed to load utxo-a")?;
    let utxo_b_bytes =
        load_contract_bytes(&contracts.utxo_b_wat).context("failed to load utxo-b")?;
    let coord_bytes = load_contract_bytes(&contracts.coord_wat).context("failed to load coord")?;
    let utxo_a_hash = hash_component_bytes(&utxo_a_bytes);
    let utxo_b_hash = hash_component_bytes(&utxo_b_bytes);
    let coord_hash = hash_component_bytes(&coord_bytes);

    let (utxo_a_pid, utxo_a_component) = runtime.compile_component(&ProgramDefinition {
        kind: ProcessKind::Utxo,
        program_hash: utxo_a_hash,
        module_bytes: utxo_a_bytes,
    })?;
    let _utxo_a_instance = runtime.instantiate_process(utxo_a_pid, &utxo_a_component)?;
    let _ = runtime.restore_process_state(
        utxo_a_pid,
        &CoroutineState::Utxo {
            storage: vec![Value(initial_states.utxo_a)],
        },
    )?;
    runtime.executor_mut().bind_resource(1, utxo_a_pid);

    let (utxo_b_pid, utxo_b_component) = runtime.compile_component(&ProgramDefinition {
        kind: ProcessKind::Utxo,
        program_hash: utxo_b_hash,
        module_bytes: utxo_b_bytes,
    })?;
    let _utxo_b_instance = runtime.instantiate_process(utxo_b_pid, &utxo_b_component)?;
    let _ = runtime.restore_process_state(
        utxo_b_pid,
        &CoroutineState::Utxo {
            storage: vec![Value(initial_states.utxo_b)],
        },
    )?;
    runtime.executor_mut().bind_resource(2, utxo_b_pid);

    let (coord_pid, coord_component) = runtime.compile_component(&ProgramDefinition {
        kind: ProcessKind::Coord,
        program_hash: coord_hash,
        module_bytes: coord_bytes,
    })?;
    let coord_instance = runtime.instantiate_component(&coord_component)?;
    runtime.run_main_with_resources(coord_pid, &coord_instance, &[1, 2])?;

    Ok((runtime, utxo_a_pid, utxo_b_pid, coord_pid))
}

fn build_demo_transaction_artifacts(
    contracts: &DemoContracts,
    initial_states: &DemoInitialStates,
) -> anyhow::Result<DemoTransactionArtifacts> {
    let (mut runtime, utxo_a_pid, utxo_b_pid, coord_pid) =
        execute_demo_runtime(contracts, initial_states)?;
    let traces = runtime.executor().traces().clone();
    let utxo_a_hash = runtime
        .executor()
        .state()
        .processes
        .get(utxo_a_pid.0)
        .map(|slot| slot.definition.program_hash)
        .context("missing utxo-a process hash")?;
    let utxo_b_hash = runtime
        .executor()
        .state()
        .processes
        .get(utxo_b_pid.0)
        .map(|slot| slot.definition.program_hash)
        .context("missing utxo-b process hash")?;
    let coord_hash = runtime
        .executor()
        .state()
        .processes
        .get(coord_pid.0)
        .map(|slot| slot.definition.program_hash)
        .context("missing coord process hash")?;
    let mut ledger = Ledger::new();

    let utxo_a_id = UtxoId {
        contract_hash: utxo_a_hash,
        nonce: 0,
    };
    let utxo_b_id = UtxoId {
        contract_hash: utxo_b_hash,
        nonce: 0,
    };
    let utxo_a_state = CoroutineState::Utxo {
        storage: vec![Value(initial_states.utxo_a)],
    };
    let utxo_b_state = CoroutineState::Utxo {
        storage: vec![Value(initial_states.utxo_b)],
    };

    ledger.utxos.insert(
        utxo_a_id.clone(),
        UtxoEntry {
            state: utxo_a_state.clone(),
            contract_hash: utxo_a_hash,
        },
    );
    ledger.utxos.insert(
        utxo_b_id.clone(),
        UtxoEntry {
            state: utxo_b_state.clone(),
            contract_hash: utxo_b_hash,
        },
    );
    ledger.utxo_to_coroutine.insert(
        utxo_a_id.clone(),
        CoroutineId {
            creation_tx_hash: Hash([1, 0, 0, 0], PhantomData::<TransactionBody>),
            creation_output_index: 0,
        },
    );
    ledger.utxo_to_coroutine.insert(
        utxo_b_id.clone(),
        CoroutineId {
            creation_tx_hash: Hash([2, 0, 0, 0], PhantomData::<TransactionBody>),
            creation_output_index: 0,
        },
    );
    ledger.contract_counters.insert(utxo_a_hash, 1);
    ledger.contract_counters.insert(utxo_b_hash, 1);

    let utxo_a_trace = traces
        .get(&utxo_a_pid)
        .cloned()
        .context("missing utxo-a trace for proof build")?;
    let utxo_b_trace = traces
        .get(&utxo_b_pid)
        .cloned()
        .context("missing utxo-b trace for proof build")?;
    let coord_trace = traces
        .get(&coord_pid)
        .cloned()
        .context("missing coord trace for proof build")?;
    let utxo_a_continuation =
        derive_demo_input_continuation(&mut runtime, utxo_a_pid, &utxo_a_trace)?;
    let utxo_b_continuation =
        derive_demo_input_continuation(&mut runtime, utxo_b_pid, &utxo_b_trace)?;

    let tx = TransactionBuilder::new()
        .with_input_and_trace_commitment(
            utxo_a_id,
            utxo_a_continuation,
            utxo_a_trace.clone(),
            trace_commitment(&utxo_a_trace),
        )
        .with_input_and_trace_commitment(
            utxo_b_id,
            utxo_b_continuation,
            utxo_b_trace.clone(),
            trace_commitment(&utxo_b_trace),
        )
        .with_coord_script_and_trace_commitment(
            coord_hash,
            coord_trace.clone(),
            trace_commitment(&coord_trace),
        )
        .with_entrypoint(coord_pid.0)
        .build(ZkTransactionProof::Dummy);

    let _ = ledger
        .apply_transaction(&tx)
        .context("demo transaction does not validate against the mocked verifier")?;

    Ok(DemoTransactionArtifacts {
        ledger,
        tx,
        utxo_a_hash,
        utxo_b_hash,
    })
}

fn derive_demo_input_continuation(
    runtime: &mut WasmtimeComponentStarstreamExecutor,
    pid: ProcessId,
    trace: &[WitLedgerEffect],
) -> anyhow::Result<Option<CoroutineState>> {
    if trace
        .iter()
        .any(|effect| matches!(effect, WitLedgerEffect::Burn {}))
    {
        return Ok(None);
    }

    runtime
        .snapshot_process_state(pid)
        .map_err(anyhow::Error::from)
}

fn summarize_and_prove_transaction(
    artifacts: &DemoTransactionArtifacts,
) -> anyhow::Result<ProofView> {
    let (inst, wit) = artifacts
        .ledger
        .interleaving_artifacts(&artifacts.tx.body, &artifacts.tx.witness)
        .context("failed to derive interleaving artifacts for proof")?;

    let public_statement = format_interleaving_instance(&inst);
    let transaction_debug = format_proven_transaction(&artifacts.tx);

    let proof = prove_interleaving(inst, wit).context("interleaving prover failed")?;

    let summary = match proof {
        ZkTransactionProof::NeoProof {
            steps_public,
            mcss_public,
            ..
        } => {
            format!(
                "Proof generated\n\nkind: neo-fold\nmcss-public: {}\nstep-bundles: {}\ninputs: {}\nnew-outputs: {}\ncoord-scripts: {}",
                mcss_public.len(),
                steps_public.len(),
                artifacts.tx.body.inputs.len(),
                artifacts.tx.body.new_outputs.len(),
                artifacts.tx.body.coordination_scripts_keys.len(),
            )
        }
        ZkTransactionProof::Dummy => "Unexpected dummy proof".to_owned(),
    };

    Ok(ProofView {
        summary,
        public_statement,
        transaction_debug,
    })
}

fn verify_demo_transaction_semantics(
    artifacts: &DemoTransactionArtifacts,
) -> anyhow::Result<String> {
    let _ = artifacts
        .ledger
        .apply_transaction(&artifacts.tx)
        .context("mocked verifier rejected the transaction semantics")?;
    Ok("Mocked verifier accepted the transaction semantics".to_owned())
}

fn apply_demo_transaction(
    artifacts: &DemoTransactionArtifacts,
) -> anyhow::Result<(String, String)> {
    let aliases = demo_contract_aliases(artifacts.utxo_a_hash, artifacts.utxo_b_hash);
    let ledger_before = format_ledger(&artifacts.ledger, &aliases);
    let applied_ledger = artifacts
        .ledger
        .apply_transaction(&artifacts.tx)
        .context("failed to apply demo transaction to ledger")?;
    let ledger_after = format_ledger(&applied_ledger, &aliases);
    Ok((ledger_before, ledger_after))
}

fn format_interleaving_instance(
    inst: &starstream_interleaving_spec::InterleavingInstance,
) -> String {
    let roots = inst
        .host_calls_roots
        .iter()
        .enumerate()
        .map(|(idx, root)| format!("  [{idx}] {}", format_commitment(root)))
        .collect::<Vec<_>>()
        .join("\n");
    let process_table = inst
        .process_table
        .iter()
        .enumerate()
        .map(|(idx, hash)| {
            format!(
                "  [{idx}] hash={} utxo={} token={}",
                format_hash(*hash),
                inst.is_utxo.get(idx).copied().unwrap_or(false),
                inst.is_token.get(idx).copied().unwrap_or(false),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let ownership_in = inst
        .ownership_in
        .iter()
        .enumerate()
        .map(|(idx, owner)| format!("  [{idx}] {}", format_pid_option(*owner)))
        .collect::<Vec<_>>()
        .join("\n");
    let ownership_out = inst
        .ownership_out
        .iter()
        .enumerate()
        .map(|(idx, owner)| format!("  [{idx}] {}", format_pid_option(*owner)))
        .collect::<Vec<_>>()
        .join("\n");
    let input_states = inst
        .input_states
        .iter()
        .enumerate()
        .map(|(idx, state)| format!("  [{idx}] {}", format_coroutine_state(state)))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "InterleavingInstance\n\nentrypoint: p{}\nn_inputs: {}\nn_new: {}\nn_coords: {}\nmust_burn: {:?}\n\nhost_calls_roots:\n{}\n\nprocess_table:\n{}\n\nownership_in:\n{}\n\nownership_out:\n{}\n\ninput_states:\n{}",
        inst.entrypoint.0,
        inst.n_inputs,
        inst.n_new,
        inst.n_coords,
        inst.must_burn,
        roots,
        process_table,
        ownership_in,
        ownership_out,
        input_states,
    )
}

fn format_proven_transaction(tx: &starstream_interleaving_spec::ProvenTransaction) -> String {
    let body = &tx.body;
    let inputs = body
        .inputs
        .iter()
        .enumerate()
        .map(|(idx, input)| format!("  [{idx}] {}", format_utxo_id(input)))
        .collect::<Vec<_>>()
        .join("\n");
    let continuations = body
        .continuations
        .iter()
        .enumerate()
        .map(|(idx, continuation)| {
            let value = continuation
                .as_ref()
                .map(format_coroutine_state)
                .unwrap_or_else(|| "burn".to_owned());
            format!("  [{idx}] {value}")
        })
        .collect::<Vec<_>>()
        .join("\n");
    let new_outputs = body
        .new_outputs
        .iter()
        .enumerate()
        .map(|(idx, out)| {
            format!(
                "  [{idx}] state={} contract_hash={}",
                format_coroutine_state(&out.state),
                format_hash(out.contract_hash),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let ownership_out = body
        .ownership_out
        .iter()
        .enumerate()
        .map(|(idx, _)| format!("  [{idx}] opaque output-ref mapping"))
        .collect::<Vec<_>>()
        .join("\n");
    let coord_keys = body
        .coordination_scripts_keys
        .iter()
        .enumerate()
        .map(|(idx, hash)| format!("  [{idx}] {}", format_hash(*hash)))
        .collect::<Vec<_>>()
        .join("\n");

    let witness = &tx.witness;
    let witness_summary = format!(
        "spending_proofs: {}\nnew_output_proofs: {}\ncoordination_scripts: {}",
        witness.spending_proofs.len(),
        witness.new_output_proofs.len(),
        witness.coordination_scripts.len(),
    );

    format!(
        "ProvenTransaction\n\nbody.entrypoint: {}\n\ninputs:\n{}\n\ncontinuations:\n{}\n\nnew_outputs:\n{}\n\nownership_out:\n{}\n\ncoordination_scripts_keys:\n{}\n\nwitness:\n{}",
        body.entrypoint,
        if inputs.is_empty() {
            "  <none>".to_owned()
        } else {
            inputs
        },
        if continuations.is_empty() {
            "  <none>".to_owned()
        } else {
            continuations
        },
        if new_outputs.is_empty() {
            "  <none>".to_owned()
        } else {
            new_outputs
        },
        if ownership_out.is_empty() {
            "  <none>".to_owned()
        } else {
            ownership_out
        },
        if coord_keys.is_empty() {
            "  <none>".to_owned()
        } else {
            coord_keys
        },
        witness_summary,
    )
}

fn format_pid_option(pid: Option<ProcessId>) -> String {
    pid.map(|p| format!("p{}", p.0))
        .unwrap_or_else(|| "none".to_owned())
}

fn format_coroutine_state(state: &CoroutineState) -> String {
    match state {
        CoroutineState::Token { .. } => "token".to_owned(),
        CoroutineState::Utxo { storage } => {
            let values = storage
                .iter()
                .map(|value| value.0.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("utxo(storage=[{}])", values)
        }
    }
}

fn format_utxo_id(id: &UtxoId) -> String {
    format!(
        "contract_hash={} nonce={}",
        format_hash(id.contract_hash),
        id.nonce
    )
}

fn format_ledger(ledger: &Ledger, aliases: &HashMap<Hash<WasmModule>, String>) -> String {
    let mut utxos = ledger.utxos.iter().collect::<Vec<_>>();
    utxos.sort_by_key(|(utxo_id, _)| (utxo_id.contract_hash.0, utxo_id.nonce));
    let utxo_lines = utxos
        .into_iter()
        .map(|(utxo_id, entry)| {
            let alias = aliases
                .get(&utxo_id.contract_hash)
                .cloned()
                .unwrap_or_else(|| "contract".to_owned());
            let coroutine = ledger.utxo_to_coroutine.get(utxo_id);
            format!(
                "  {alias}#{nonce}\n    state: {state}\n    contract: {hash}\n    created-at: {created}",
                alias = alias,
                nonce = utxo_id.nonce,
                state = format_coroutine_state(&entry.state),
                hash = format_hash_short(entry.contract_hash),
                created = coroutine
                    .map(|cid| format!("tx {} / out {}", format_hash_words_short(cid.creation_tx_hash.0), cid.creation_output_index))
                    .unwrap_or_else(|| "unknown".to_owned()),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let mut counters = ledger.contract_counters.iter().collect::<Vec<_>>();
    counters.sort_by_key(|(hash, _)| hash.0);
    let counter_lines = counters
        .into_iter()
        .map(|(hash, next)| {
            let alias = aliases
                .get(hash)
                .cloned()
                .unwrap_or_else(|| "contract".to_owned());
            format!(
                "  {} => next nonce {} ({})",
                alias,
                next,
                format_hash_short(*hash)
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "Ledger\n\nutxos:\n{}\n\ncontract_counters:\n{}",
        if utxo_lines.is_empty() {
            "  <none>".to_owned()
        } else {
            utxo_lines
        },
        if counter_lines.is_empty() {
            "  <none>".to_owned()
        } else {
            counter_lines
        },
    )
}

fn format_hash_words_short(words: [u64; 4]) -> String {
    format!("#{:x}…{:x}", words[0], words[3])
}

fn format_hash_short(hash: Hash<WasmModule>) -> String {
    format!("#{:x}…{:x}", hash.0[0], hash.0[3])
}

fn demo_contract_aliases(
    utxo_a_hash: Hash<WasmModule>,
    utxo_b_hash: Hash<WasmModule>,
) -> HashMap<Hash<WasmModule>, String> {
    HashMap::from([
        (utxo_a_hash, "utxo-a".to_owned()),
        (utxo_b_hash, "utxo-b".to_owned()),
    ])
}

fn pack_bytes_to_safe_limbs(bytes: &[u8]) -> Vec<ark_poseidon2::F> {
    let mut out = Vec::with_capacity(bytes.len().div_ceil(7));

    for chunk in bytes.chunks(7) {
        let mut limb = [0u8; 8];
        limb[..chunk.len()].copy_from_slice(chunk);
        out.push(ark_poseidon2::F::from(u64::from_le_bytes(limb)));
    }

    out
}

fn hash_component_bytes(bytes: &[u8]) -> Hash<WasmModule> {
    let mut msg = pack_bytes_to_safe_limbs("starstream/program_hash/v1/poseidon2".as_bytes());
    msg.push(ark_poseidon2::F::from(bytes.len() as u64));
    msg.extend(pack_bytes_to_safe_limbs(bytes));

    let hash = ark_poseidon2::sponge_12_trace(&msg).expect("poseidon2 component hash");

    Hash(
        [
            hash[0].into_bigint().0[0],
            hash[1].into_bigint().0[0],
            hash[2].into_bigint().0[0],
            hash[3].into_bigint().0[0],
        ],
        PhantomData,
    )
}

fn record_execution(
    effect_log: &[(ProcessId, WitLedgerEffect)],
    labels: &[(ProcessId, &str)],
) -> RecordedExecution {
    let label_map = labels
        .iter()
        .map(|(pid, label)| (pid.0, (*label).to_owned()))
        .collect::<HashMap<_, _>>();
    let mut full_process_traces = vec![
        ProcessTraceView {
            process: "coord".to_owned(),
            effects: vec![],
            processed: 0,
            commitment: None,
        },
        ProcessTraceView {
            process: "utxo-a".to_owned(),
            effects: vec![],
            processed: 0,
            commitment: None,
        },
        ProcessTraceView {
            process: "utxo-b".to_owned(),
            effects: vec![],
            processed: 0,
            commitment: None,
        },
    ];
    let mut commitments = HashMap::<String, LedgerEffectsCommitment>::new();
    let mut pending_callers = HashMap::<usize, Vec<usize>>::new();
    let mut frames = Vec::new();
    let mut batch = Vec::<TraceEventView>::new();

    for (pid, effect) in effect_log {
        let process = label_map
            .get(&pid.0)
            .cloned()
            .unwrap_or_else(|| format!("pid-{}", pid.0));
        let highlighted_edge = match effect {
            WitLedgerEffect::Resume { target, .. } => {
                pending_callers.entry(target.0).or_default().push(pid.0);
                Some(format!(
                    "{} -> {}",
                    process,
                    label_map
                        .get(&target.0)
                        .cloned()
                        .unwrap_or_else(|| format!("pid-{}", target.0))
                ))
            }
            WitLedgerEffect::Yield { .. } | WitLedgerEffect::Burn {} => pending_callers
                .get_mut(&pid.0)
                .and_then(|callers| callers.pop())
                .map(|caller| {
                    format!(
                        "{} -> {}",
                        process,
                        label_map
                            .get(&caller)
                            .cloned()
                            .unwrap_or_else(|| format!("pid-{caller}"))
                    )
                }),
            _ => None,
        };

        let effect_text = concise_effect(effect);
        if let Some(trace) = full_process_traces
            .iter_mut()
            .find(|trace| trace.process == process)
        {
            trace.effects.push(effect_text.clone());
            let next = commit_trace_effect(
                commitments
                    .get(&trace.process)
                    .cloned()
                    .unwrap_or_else(LedgerEffectsCommitment::iv),
                effect.clone(),
            );
            trace.commitment = Some(format_commitment(&next));
            commitments.insert(trace.process.clone(), next);
        }
        batch.push(TraceEventView {
            process: process.clone(),
            effect: effect_text.clone(),
        });

        if is_semantic_step_boundary(effect) {
            let display = batch
                .last()
                .expect("batch contains at least the boundary effect");
            frames.push(RecordedFrame {
                process: display.process.clone(),
                highlighted_edge,
                batch: std::mem::take(&mut batch),
            });
        }
    }

    if !batch.is_empty() {
        let display = batch.last().expect("batch is not empty");
        frames.push(RecordedFrame {
            process: display.process.clone(),
            highlighted_edge: None,
            batch,
        });
    }

    RecordedExecution {
        frames,
        full_process_traces,
    }
}

fn apply_frame(execution: &mut ExecutionSnapshot, frame: &RecordedFrame) {
    execution.active_process = Some(frame.process.clone());
    execution.highlighted_edge = frame.highlighted_edge.clone();
    for event in &frame.batch {
        if let Some(trace) = execution
            .per_process_traces
            .iter_mut()
            .find(|trace| trace.process == event.process)
        {
            trace.processed = trace.processed.saturating_add(1).min(trace.effects.len());
        }
        execution.interleaved_trace.push(event.clone());
    }
}

fn is_semantic_step_boundary(effect: &WitLedgerEffect) -> bool {
    matches!(
        effect,
        WitLedgerEffect::Resume { .. }
            | WitLedgerEffect::Yield { .. }
            | WitLedgerEffect::Burn {}
            | WitLedgerEffect::Return { .. }
            | WitLedgerEffect::CallEffectHandler { .. }
    )
}

fn concise_effect(effect: &WitLedgerEffect) -> String {
    match effect {
        WitLedgerEffect::Resume {
            target,
            f_id,
            val,
            ret,
            caller,
        } => format!(
            "resume p{} f={} val={} ret={} caller={}",
            target.0,
            format_function_id(*f_id),
            format_ref(*val),
            format_ref_output(*ret),
            format_optional_pid_output(*caller),
        ),
        WitLedgerEffect::Yield { val } => format!("yield {}", format_ref(*val)),
        WitLedgerEffect::Return {} => "return".to_owned(),
        WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        } => format!(
            "program-hash p{} {}",
            target.0,
            format_hash_output(*program_hash)
        ),
        WitLedgerEffect::NewUtxo {
            program_hash,
            val,
            id,
        } => format!(
            "new-utxo {} init={} id={}",
            format_hash(*program_hash),
            format_ref(*val),
            format_pid_output(*id),
        ),
        WitLedgerEffect::NewToken {
            program_hash,
            val,
            id,
        } => format!(
            "new-token {} init={} id={}",
            format_hash(*program_hash),
            format_ref(*val),
            format_pid_output(*id),
        ),
        WitLedgerEffect::NewCoord {
            program_hash,
            val,
            id,
        } => format!(
            "new-coord {} init={} id={}",
            format_hash(*program_hash),
            format_ref(*val),
            format_pid_output(*id),
        ),
        WitLedgerEffect::InstallHandler { interface_id } => {
            format!("install-handler {}", format_interface_id(*interface_id))
        }
        WitLedgerEffect::UninstallHandler { interface_id } => {
            format!("uninstall-handler {}", format_interface_id(*interface_id))
        }
        WitLedgerEffect::GetHandlerFor {
            interface_id,
            handler_id,
        } => format!(
            "get-handler {} -> {}",
            format_interface_id(*interface_id),
            format_pid_output(*handler_id),
        ),
        WitLedgerEffect::CallEffectHandler {
            interface_id,
            f_id,
            val,
            ret,
        } => format!(
            "call-handler {} f={} val={} ret={}",
            format_interface_id(*interface_id),
            format_function_id(*f_id),
            format_ref(*val),
            format_ref_output(*ret),
        ),
        WitLedgerEffect::Enter { f_id } => format!("enter f={}", format_function_id(*f_id)),
        WitLedgerEffect::Burn {} => "burn".to_owned(),
        WitLedgerEffect::Activation { val, caller } => format!(
            "activation val={} caller={}",
            format_ref_output(*val),
            format_pid_output(*caller),
        ),
        WitLedgerEffect::Init { val, caller } => format!(
            "init val={} caller={}",
            format_ref_output(*val),
            format_pid_output(*caller),
        ),
        WitLedgerEffect::NewRef { size, ret } => {
            format!("new-ref words={} -> {}", size, format_ref_output(*ret))
        }
        WitLedgerEffect::RefPush { vals } => format!("ref-push {}", format_values(vals)),
        WitLedgerEffect::RefGet { reff, offset, ret } => format!(
            "ref-get {} @{} -> {}",
            format_ref(*reff),
            offset,
            format_values_output(*ret),
        ),
        WitLedgerEffect::RefWrite { reff, offset, vals } => format!(
            "ref-write {} @{} {}",
            format_ref(*reff),
            offset,
            format_values(vals),
        ),
        WitLedgerEffect::Bind { owner_id } => format!("bind owner=p{}", owner_id.0),
        WitLedgerEffect::Unbind { token_id } => format!("unbind token=p{}", token_id.0),
    }
}

fn format_ref(reff: Ref) -> String {
    format!("r{}", reff.0)
}

fn format_function_id(f_id: FunctionId) -> String {
    let full = f_id.to_string();
    if full.len() <= 10 {
        return full;
    }
    format!("{}…", &full[..10])
}

fn format_interface_id(interface_id: InterfaceId) -> String {
    format!(
        "#{:x}:{:x}:{:x}:{:x}",
        interface_id.0[0], interface_id.0[1], interface_id.0[2], interface_id.0[3]
    )
}

fn format_ref_output(output: WitEffectOutput<Ref>) -> String {
    format_ref(output.unwrap())
}

fn format_pid_output(output: WitEffectOutput<ProcessId>) -> String {
    let pid = output.unwrap();
    format!("p{}", pid.0)
}

fn format_optional_pid_output(output: WitEffectOutput<Option<ProcessId>>) -> String {
    match output.unwrap() {
        Some(pid) => format!("p{}", pid.0),
        None => "none".to_owned(),
    }
}

fn format_hash_output(output: WitEffectOutput<Hash<WasmModule>>) -> String {
    format_hash(output.unwrap())
}

fn format_hash(hash: Hash<WasmModule>) -> String {
    format!(
        "#{:x}:{:x}:{:x}:{:x}",
        hash.0[0], hash.0[1], hash.0[2], hash.0[3]
    )
}

fn trace_commitment(trace: &[WitLedgerEffect]) -> LedgerEffectsCommitment {
    trace
        .iter()
        .cloned()
        .fold(LedgerEffectsCommitment::iv(), commit_trace_effect)
}

fn format_commitment(commitment: &LedgerEffectsCommitment) -> String {
    format!(
        "#{:x}:{:x}:{:x}:{:x}",
        commitment.0[0].into_bigint().0[0],
        commitment.0[1].into_bigint().0[0],
        commitment.0[2].into_bigint().0[0],
        commitment.0[3].into_bigint().0[0]
    )
}

fn format_values(values: &[Value; 4]) -> String {
    format!(
        "[{}, {}, {}, {}]",
        values[0].0, values[1].0, values[2].0, values[3].0
    )
}

fn format_values_output(output: WitEffectOutput<[Value; 4]>) -> String {
    format_values(&output.unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_backed_stepper_records_real_effects() {
        let mut state = DemoBackendState::new();
        let compile = state.compile_contracts();
        assert!(compile.ok, "{compile:?}");

        let trace = state.trace_execution();
        assert!(
            trace.ok,
            "{trace:?} / status={:?}",
            state.execution.verification_status
        );
        assert!(state.execution.total_steps > 0);
        assert!(state.execution.interleaved_trace.is_empty());
        assert!(
            state
                .execution
                .per_process_traces
                .iter()
                .any(|trace| trace.process == "utxo-a" && !trace.effects.is_empty())
        );
        assert!(
            state
                .execution
                .per_process_traces
                .iter()
                .any(|trace| trace.process == "utxo-b" && !trace.effects.is_empty())
        );
        let utxo_a_trace = state
            .execution
            .per_process_traces
            .iter()
            .find(|trace| trace.process == "utxo-a")
            .expect("utxo-a trace should exist");
        let utxo_a_enter_count = utxo_a_trace
            .effects
            .iter()
            .filter(|effect| effect.starts_with("enter "))
            .count();
        assert_eq!(utxo_a_enter_count, 1, "utxo-a should be entered once");

        let first = state.step_execution();
        assert!(first.ok, "{first:?}");
        assert_eq!(state.execution.step_index, 1);
        assert!(!state.execution.interleaved_trace.is_empty());
        assert!(
            state
                .execution
                .interleaved_trace
                .iter()
                .any(|event| event.effect.contains("resume"))
        );

        let reset = state.reset_execution();
        assert!(reset.ok, "{reset:?}");
        assert_eq!(state.execution.step_index, 0);
        assert!(state.execution.interleaved_trace.is_empty());

        let run = state.run_to_completion();
        assert!(run.ok, "{run:?}");
        assert_eq!(state.execution.step_index, state.execution.total_steps);
        assert!(matches!(
            state.execution.verification_status,
            StepStatus::Completed
        ));
        assert!(
            state
                .execution
                .interleaved_trace
                .iter()
                .any(|event| event.process == "utxo-a")
        );
        assert!(
            state
                .execution
                .interleaved_trace
                .iter()
                .any(|event| event.process == "utxo-b")
        );
    }
}
