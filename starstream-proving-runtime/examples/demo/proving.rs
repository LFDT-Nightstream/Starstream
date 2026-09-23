use std::sync::{Arc, Mutex};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use neo_application::MemoryCheckError;
use neo_wasm::comm_chain::{CommChainState, absorbed_event_blocks, fold_event_blocks};
use serde_json::Value;
use serde_json::json;
use starstream_interleaving_prover::TraceCommitments;
use starstream_interleaving_prover::{Error as InterleavingError, Unsatisfied};
use starstream_interleaving_spec::TransactionStatement;
use starstream_interleaving_spec::interleaver::{InterleavedTransaction, interleave_transaction};
use wasmtime::ensure;

use crate::common::{TracedExecution, coroutine_id};
use crate::model::{State, root};
use crate::proof_config::{entry_pc, params, profile};

pub type Shared = Arc<Mutex<State>>;

pub fn phase(state: &Shared, active: Option<usize>, name: &str) -> Instant {
    let mut state = state.lock().unwrap();
    state.active = active;
    state.phase = name.into();
    state.phase_started_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64;
    if let Some(index) = active {
        state.proofs[index].status = name.into();
    }
    Instant::now()
}

fn exact_failure_step(error: &InterleavingError) -> Option<usize> {
    match error {
        InterleavingError::Unsatisfied(Unsatisfied::Constraint { step, .. })
        | InterleavingError::CcsCheck { step, .. } => Some(*step),
        InterleavingError::Unsatisfied(Unsatisfied::Memory(source)) => {
            Some(memory_error_row(source))
        }
        _ => None,
    }
}

fn memory_error_row<Id: std::fmt::Debug + std::fmt::Display>(
    error: &MemoryCheckError<Id>,
) -> usize {
    match error {
        MemoryCheckError::WitnessWidth { row, .. }
        | MemoryCheckError::ValueNotU32 { row, .. }
        | MemoryCheckError::NonBooleanGate { row, .. }
        | MemoryCheckError::RomMismatch { row, .. }
        | MemoryCheckError::RomReadBeforeInitialization { row, .. }
        | MemoryCheckError::ReadMismatch { row, .. }
        | MemoryCheckError::ReadBeforeInitialization { row, .. }
        | MemoryCheckError::ZeroReadMismatch { row, .. }
        | MemoryCheckError::ReadModifyWriteMismatch { row, .. }
        | MemoryCheckError::ReadModifyWriteBeforeInitialization { row, .. }
        | MemoryCheckError::ZeroReadModifyWriteMismatch { row, .. } => *row,
    }
}

#[derive(Default)]
pub struct Session {
    pub captures: Vec<Capture>,
    pub selected: Vec<usize>,
    pub latest: usize,
    pub locked: Vec<bool>,
    pub statement: Option<TransactionStatement>,
    pub statement_outputs: Value,
    pub statement_current: bool,
    pub roots: TraceCommitments,
    pub interleaved: Option<InterleavedTransaction>,
    pub interleaved_roots: Option<TraceCommitments>,
    #[cfg(target_vendor = "apple")]
    interleaving: Option<InterleavingProof>,
}

pub struct Capture {
    pub execution: TracedExecution,
    pub view: Value,
}

#[cfg(target_vendor = "apple")]
struct InterleavingProof {
    context: starstream_interleaving_prover::proving::TransactionProofContext,
    proof: starstream_interleaving_prover::proving::TransactionProof,
    adapter: neo_prover_metal::MetalNifsProver,
}

impl Session {
    pub fn new(execution: TracedExecution, view: Value) -> Self {
        let count = execution.instances.len();
        Self {
            statement: Some(execution.transaction.statement.clone()),
            statement_outputs: view["outputs"].clone(),
            statement_current: true,
            captures: vec![Capture { execution, view }],
            selected: vec![0; count],
            locked: vec![false; count],
            ..Self::default()
        }
    }

    pub fn replace_unlocked(&mut self, execution: TracedExecution, view: Value) -> Vec<usize> {
        self.captures.push(Capture { execution, view });
        self.latest = self.captures.len() - 1;
        self.statement_current = false;
        let replaced = (0..self.selected.len())
            .filter(|&index| !self.locked[index])
            .collect::<Vec<_>>();
        for &index in &replaced {
            self.selected[index] = self.latest;
        }
        self.retain_selected_captures();
        replaced
    }

    fn retain_selected_captures(&mut self) {
        let old = std::mem::take(&mut self.captures);
        let mut ids = vec![usize::MAX; old.len()];
        for (index, capture) in old.into_iter().enumerate() {
            if index != self.latest && !self.selected.contains(&index) {
                continue;
            }
            ids[index] = self.captures.len();
            self.captures.push(capture);
        }
        self.latest = ids[self.latest];
        for selected in &mut self.selected {
            *selected = ids[*selected];
        }
    }

    pub fn update_statement(&mut self) {
        let latest = &self.captures[self.latest];
        self.statement = Some(latest.execution.transaction.statement.clone());
        self.statement_outputs = latest.view["outputs"].clone();
        self.statement_current = true;
    }

    pub fn invalidate_interleaving_proof(&mut self) {
        #[cfg(target_vendor = "apple")]
        {
            self.interleaving = None;
        }
    }

    pub fn source_execution(&self, index: usize) -> wasmtime::Result<&TracedExecution> {
        self.captures
            .get(self.selected[index])
            .map(|capture| &capture.execution)
            .ok_or_else(|| wasmtime::format_err!("Run the script first"))
    }

    pub fn current_statement(&self) -> wasmtime::Result<&TransactionStatement> {
        self.statement
            .as_ref()
            .ok_or_else(|| wasmtime::format_err!("Run the script first"))
    }

    pub fn current_roots(&self) -> wasmtime::Result<TraceCommitments> {
        let mut roots = TraceCommitments::new();
        let count = self.selected.len();
        for index in 0..count {
            let trace = &self.source_execution(index)?.instances[index].trace;
            let root = trace
                .last()
                .ok_or_else(|| wasmtime::format_err!("Empty trace {index}"))?
                .state_after
                .comm_chain;
            roots.insert(coroutine_id(index), root);
        }
        Ok(roots)
    }

    pub fn current_transaction(&self) -> wasmtime::Result<InterleavedTransaction> {
        let count = self.selected.len();
        let traces = (0..count)
            .map(|index| {
                let execution = self.source_execution(index)?;
                let blocks = absorbed_event_blocks(&execution.instances[index].trace);
                starstream_proving_runtime::decode_absorbed_blocks(
                    &execution.templates.decoder,
                    &blocks,
                )
                .map_err(Into::into)
            })
            .collect::<wasmtime::Result<Vec<_>>>()?;
        interleave_transaction(&traces).map_err(Into::into)
    }

    pub fn build_interleaving(&mut self) -> wasmtime::Result<()> {
        self.interleaved = None;
        self.interleaved_roots = None;
        let transaction = self.current_transaction()?;
        self.interleaved_roots = Some(self.current_roots()?);
        self.interleaved = Some(transaction);
        Ok(())
    }

    pub fn view(&self) -> wasmtime::Result<Value> {
        let mut view = self
            .captures
            .get(self.latest)
            .ok_or_else(|| wasmtime::format_err!("Run the script first"))?
            .view
            .clone();
        for (index, capture) in self.selected.iter().copied().enumerate() {
            view["traces"][index] = self.captures[capture].view["traces"][index].clone();
        }
        view["locked"] = json!(self.locked);
        view["statement_current"] = json!(self.statement_current);
        view["statement_json"] = json!(serde_json::to_string_pretty(self.current_statement()?)?);
        let latest_statement = &self.captures[self.latest].execution.transaction.statement;
        view["statement_differs"] = json!(self.current_statement()? != latest_statement);
        view["latest_statement_json"] = json!(serde_json::to_string_pretty(latest_statement)?);
        view["outputs"] = self.statement_outputs.clone();
        view["latest_outputs"] = self.captures[self.latest].view["outputs"].clone();
        if let Ok(commitment) = starstream_interleaving_prover::transaction_commitment(
            self.current_statement()?,
            &self.current_roots()?,
        ) {
            view["commitment"] = json!(root(commitment));
        }
        view["interleaving"] = json!(
            self.interleaved
                .as_ref()
                .map(crate::model::interleaving_rows)
                .unwrap_or_default()
        );
        view["interleaving_ready"] = json!(self.interleaved.is_some());
        view["interleaving_stale"] = json!(
            self.interleaved_roots
                .as_ref()
                .is_some_and(|roots| self.current_roots().is_ok_and(|current| &current != roots))
        );
        Ok(view)
    }

    pub fn check_trace(&self, index: usize) -> wasmtime::Result<Value> {
        let execution = self.source_execution(index)?;
        let started = Instant::now();
        let result = crate::common::check_wasm_instance(execution, index);
        Ok(json!({"accepted": result.is_ok(),
            "reason": result.err().map(|error| format!("{error:#}")), "elapsed_ms": started.elapsed().as_secs_f64() * 1000.0}))
    }

    pub fn check_transaction(&self) -> wasmtime::Result<Value> {
        let roots = self.current_roots()?;
        let started = Instant::now();
        let Some(transaction) = self.interleaved.as_ref() else {
            return Ok(
                json!({"accepted": false, "reason": "Build the interleaving first",
                "failure_step": null,
                "elapsed_ms": started.elapsed().as_secs_f64() * 1000.0,
                "authenticated_roots": self.roots.len(), "total_roots": roots.len()}),
            );
        };
        let statement = self.current_statement()?;
        let result = starstream_interleaving_prover::verify_transaction_sat(
            &transaction.trace,
            3,
            statement,
            &roots,
        );
        let failure_step = result.as_ref().err().and_then(|error| {
            exact_failure_step(error).or_else(|| {
                matches!(
                    error,
                    InterleavingError::Unsatisfied(Unsatisfied::BatchedMemory { .. })
                        | InterleavingError::BatchedCcsCheck { .. }
                )
                .then(|| {
                    starstream_interleaving_prover::verify_transaction_sat(
                        &transaction.trace,
                        1,
                        statement,
                        &roots,
                    )
                    .err()
                    .as_ref()
                    .and_then(exact_failure_step)
                })
                .flatten()
            })
        });
        Ok(
            json!({"accepted": result.is_ok(), "reason": result.err().map(|error| error.to_string()),
            "failure_step": failure_step,
            "elapsed_ms": started.elapsed().as_secs_f64() * 1000.0,
            "authenticated_roots": self.roots.len(), "total_roots": roots.len()}),
        )
    }

    pub fn prove(&mut self, index: usize, state: &Shared) -> wasmtime::Result<()> {
        let count = self.selected.len();
        if index == count {
            return self.prove_interleaving(state);
        }
        let execution = self.source_execution(index)?;
        let instance = execution
            .instances
            .get(index)
            .ok_or_else(|| wasmtime::format_err!("Unknown trace"))?;
        if self.roots.contains_key(&coroutine_id(index)) {
            return Ok(());
        }
        let final_state = instance
            .trace
            .last()
            .ok_or_else(|| wasmtime::format_err!("Empty trace"))?
            .state_after;
        crate::common::check_wasm_instance(execution, index)?;
        let mut prover = metal_prover()?;
        let started = phase(state, Some(index), "Preprocessing");
        let prep = neo_wasm::nebula::preprocess_seeded_host_events_reduced_memory_test_only(
            params(14),
            profile(14, instance.trace.len()),
            &execution.artifacts,
            entry_pc(&execution.templates.program_tables, instance.entry_fref)?,
            &execution.templates.bindings,
            instance.entry_fref,
            0x5742_0002,
            CommChainState::default(),
        )?;
        state.lock().unwrap().proofs[index].preprocessing_ms =
            Some(started.elapsed().as_secs_f64() * 1000.0);
        let started = phase(state, Some(index), "Proving");
        let proof = prover.prove(&prep, &instance.trace)?;
        state.lock().unwrap().proofs[index].proving_ms =
            Some(started.elapsed().as_secs_f64() * 1000.0);
        ensure!(
            prover.backend().as_str() == "metal",
            "The prover did not use Metal"
        );
        let started = phase(state, Some(index), "Verifying");
        prover.verify(&prep, &proof, final_state)?;
        let blocks = absorbed_event_blocks(&instance.trace)
            .into_iter()
            .map(|block| block.words.map(neo_math::F::new))
            .collect::<Vec<_>>();
        ensure!(
            fold_event_blocks(CommChainState::default(), &blocks).canonical_u64()
                == final_state.comm_chain,
            "The verified root differs from the decoded host events"
        );
        self.roots
            .insert(coroutine_id(index), final_state.comm_chain);
        let mut view = state.lock().unwrap();
        view.proofs[index].verification_ms = Some(started.elapsed().as_secs_f64() * 1000.0);
        view.proofs[index].root = Some(root(final_state.comm_chain));
        view.proofs[index].status = "Verified".into();
        view.transaction_check = None;
        // Only authenticated roots survive here; release each large Wasm
        // context before starting the next program's proof.
        Ok(())
    }

    #[cfg(target_vendor = "apple")]
    fn prove_interleaving(&mut self, state: &Shared) -> wasmtime::Result<()> {
        use starstream_interleaving_prover::proving::TransactionProofContext;
        if self.interleaving.is_some() {
            return Ok(());
        }
        ensure!(
            self.roots == self.current_roots()?,
            "Prove every selected Wasm trace first"
        );
        let transaction = self
            .interleaved
            .as_ref()
            .ok_or_else(|| wasmtime::format_err!("Build the interleaving first"))?;
        starstream_interleaving_prover::verify_transaction_sat(
            &transaction.trace,
            3,
            self.current_statement()?,
            &self.roots,
        )?;
        let index = self.selected.len();
        let started = phase(state, Some(index), "Preprocessing");
        let context =
            TransactionProofContext::new(3, crate::common::interleaving_params(), [7; 32])?;
        let mut adapter = neo_prover_metal::MetalNifsProver::new()?;
        state.lock().unwrap().proofs[index].preprocessing_ms =
            Some(started.elapsed().as_secs_f64() * 1000.0);
        let started = phase(state, Some(index), "Proving");
        let proof = context.prove_with_nifs_adapter(
            &mut adapter,
            &transaction.trace,
            self.current_statement()?,
            &self.roots,
        )?;
        ensure!(
            adapter.session().activity().dispatches > 0,
            "No Metal dispatches"
        );
        state.lock().unwrap().proofs[index].proving_ms =
            Some(started.elapsed().as_secs_f64() * 1000.0);
        let started = phase(state, Some(index), "Verifying");
        context.verify_with_opening_backend(
            &proof,
            self.current_statement()?,
            &self.roots,
            adapter.final_witness_opening_backend(),
        )?;
        self.interleaving = Some(InterleavingProof {
            context,
            proof,
            adapter,
        });
        let mut view = state.lock().unwrap();
        view.proofs[index].verification_ms = Some(started.elapsed().as_secs_f64() * 1000.0);
        view.proofs[index].status = "Verified".into();
        Ok(())
    }

    #[cfg(not(target_vendor = "apple"))]
    fn prove_interleaving(&mut self, _: &Shared) -> wasmtime::Result<()> {
        wasmtime::bail!("This prototype requires Apple Metal for proving")
    }

    #[cfg(target_vendor = "apple")]
    pub fn verify(&mut self, state: &Shared) -> wasmtime::Result<Value> {
        let statement = self.current_statement()?.clone();
        let stored = self
            .interleaving
            .as_mut()
            .ok_or_else(|| wasmtime::format_err!("Prove the transaction first"))?;
        let roots = self.roots.clone();
        let commitment =
            starstream_interleaving_prover::transaction_commitment(&statement, &roots)?;
        let started = phase(state, None, "Verifying transaction");
        let result = stored.context.verify_with_opening_backend(
            &stored.proof,
            &statement,
            &roots,
            stored.adapter.final_witness_opening_backend(),
        );
        let elapsed = started.elapsed().as_secs_f64() * 1000.0;
        result?;
        Ok(
            json!({"accepted": true, "description": "Transaction and authenticated Wasm roots",
            "elapsed_ms": elapsed, "commitment": root(commitment)}),
        )
    }

    #[cfg(not(target_vendor = "apple"))]
    pub fn verify(&mut self, _: &Shared) -> wasmtime::Result<Value> {
        wasmtime::bail!("This prototype requires Apple Metal for proving")
    }
}

#[cfg(target_vendor = "apple")]
fn metal_prover() -> wasmtime::Result<neo_wasm::WasmProver> {
    Ok(neo_wasm::WasmProver::metal()?)
}

#[cfg(not(target_vendor = "apple"))]
fn metal_prover() -> wasmtime::Result<neo_wasm::WasmProver> {
    wasmtime::bail!("This prototype requires Apple Metal for proving")
}
