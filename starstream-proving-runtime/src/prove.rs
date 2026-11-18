//! Compressed Wasm instance proofs linked by an interleaving proof.
//!
//! Proofs have no byte encoding yet. For untrusted proofs, the verifier must
//! rebuild program commitments from contracts and the statement from the
//! ledger transaction instead of trusting the prover's values.

use std::collections::BTreeMap;
use std::time::{Duration, Instant};

use neo_wasm::nebula::{WasmApplication, WasmSize};
use neo_wasm::{CommChainState, WasmStepState};
use nightstream::nebula::{self, Circuit};
use starstream_interleaving_prover::proving::{
    CompressedInterleavingProof, InterleavingProofContext, Limits, ProofConfig, method_dictionary,
};
use starstream_interleaving_prover::{CoroutineId, TraceCommitments};
use starstream_interleaving_spec::{MethodHash, TransactionStatement};
use wasmtime::error::Context as _;
use wasmtime::{bail, ensure};

use crate::CapturedExecution;

/// Security target supported by the Wasm profiles and interleaving limits.
const SECURITY_BITS: u32 = 109;

/// Interleaving rows of one proof step.
const BATCH_SIZE: usize = 16;

/// Shared across programs with the same Wasm size.
struct WasmCircuit {
    prover: nebula::Prover<WasmApplication>,
    verifier: nebula::Verifier<WasmApplication>,
    setup: nightstream::CompressionSetup,
    key: nightstream::CompressionKey,
}

struct Interleaving {
    methods: Vec<MethodHash>,
    context: InterleavingProofContext,
    setup: nightstream::CompressionSetup,
    key: nightstream::CompressionKey,
}

/// A core Wasm instance proof and its verification inputs.
pub struct WasmInstanceProof {
    /// The packed coroutine id whose trace commitment this instance proves.
    pub coroutine: u32,
    pub size: WasmSize,
    program: nebula::Program,
    initial: WasmStepState,
    last: WasmStepState,
    proof: nebula::FinalProof,
}

pub struct TransactionProof {
    pub instances: Vec<WasmInstanceProof>,
    pub interleaving: CompressedInterleavingProof,
    methods: Vec<MethodHash>,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ProofTimings {
    /// Circuit and compression-key setup, including cache lookups.
    pub setup: Duration,
    pub proving: Duration,
    pub compression: Duration,
}

/// Reuse across transactions to amortize circuit compilation and key setup.
pub struct TransactionProver {
    engine: nightstream::Engine,
    wasm: BTreeMap<WasmSize, WasmCircuit>,
    interleaving: Option<Interleaving>,
}

impl TransactionProver {
    pub fn new(engine: nightstream::Engine) -> Self {
        Self {
            engine,
            wasm: BTreeMap::new(),
            interleaving: None,
        }
    }

    fn wasm_circuit(&mut self, size: WasmSize) -> wasmtime::Result<&WasmCircuit> {
        if !self.wasm.contains_key(&size) {
            let circuit = Circuit::compile(size.geometry(), size.application()?)?;
            let prover = circuit.prover(self.engine, SECURITY_BITS)?;
            let verifier = circuit.verifier(self.engine, SECURITY_BITS)?;
            let setup = prover.compression_setup()?;
            let key = verifier.compression_key()?;
            self.wasm.insert(
                size,
                WasmCircuit {
                    prover,
                    verifier,
                    setup,
                    key,
                },
            );
        }
        Ok(&self.wasm[&size])
    }

    fn interleaving(&mut self, methods: &[MethodHash]) -> wasmtime::Result<&Interleaving> {
        if self
            .interleaving
            .as_ref()
            .is_none_or(|cached| cached.methods != methods)
        {
            let mut config = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, BATCH_SIZE)?;
            config.engine = self.engine;
            let context = InterleavingProofContext::new(config, methods.to_vec())?;
            let setup = context.compression_setup()?;
            let key = context.compression_key()?;
            self.interleaving = Some(Interleaving {
                methods: methods.to_vec(),
                context,
                setup,
                key,
            });
        }
        Ok(self.interleaving.as_ref().expect("set above"))
    }

    /// `execution` must have been decoded from `registry`.
    pub fn prove(
        &mut self,
        registry: &neo_wasm::WasmtimeTraceRegistry,
        execution: &CapturedExecution,
    ) -> wasmtime::Result<(TransactionProof, ProofTimings)> {
        let mut timings = ProofTimings::default();
        let transaction = &execution.transaction;
        let coroutines = coroutine_ids(execution)?;

        let mut instances = Vec::with_capacity(coroutines.len());
        let captured = registry
            .instances()
            .map_err(|error| wasmtime::format_err!("trace capture failed: {error}"))?
            .collect::<Vec<_>>();
        // Checked before any proof: an instance captured after `execution`
        // was decoded must not be left out, and a missing one must not
        // shift the others onto the wrong coroutine.
        ensure!(
            captured.len() == coroutines.len(),
            "the registry holds {} instances but the execution has {} coroutines",
            captured.len(),
            coroutines.len()
        );
        for ((index, captured), &coroutine) in captured.into_iter().zip(&coroutines) {
            let artifacts = captured.artifacts();
            let trace = neo_wasm::traces_from_wasmtime_steps_with_host_events(
                captured.steps(),
                artifacts,
                CommChainState::default(),
            )
            .map_err(|error| {
                wasmtime::format_err!("failed to normalize instance {index}: {error}")
            })?;
            let last = trace
                .last()
                .with_context(|| format!("instance {index} ran nothing"))?
                .state_after;
            ensure!(
                last.is_terminal() && !last.trapped,
                "instance {index} did not run to completion"
            );

            let mut preload = neo_wasm::preload_from_program_artifacts(artifacts);
            neo_wasm::memory_semantics::preload_host_event_tables(
                &mut preload,
                &artifacts.host_event_bindings,
                artifacts,
            );
            let entry_fref = captured
                .steps()
                .first()
                .and_then(|row| row.current_function_ref)
                .with_context(|| format!("instance {index} has no entry function"))?;
            let entry_pc = artifacts
                .tables
                .function_entries
                .iter()
                .find(|&&(fref, _)| fref == u64::from(entry_fref))
                .map(|&(_, pc)| pc)
                .with_context(|| format!("instance {index} enters an unknown function"))?;
            let initial = neo_wasm::host_event_top_level_initial_state(
                &artifacts.tables,
                entry_pc,
                &artifacts.host_event_bindings,
                entry_fref,
                CommChainState::default(),
            )
            .map_err(|error| wasmtime::format_err!("instance {index}: {error}"))?;
            ensure!(
                trace[0].state_before == initial,
                "instance {index} does not start at its entry function"
            );

            let size = WasmSize::for_run(&preload, &trace)
                .map_err(|error| wasmtime::format_err!("instance {index}: {error}"))?;
            let program = size.application()?.program(&preload)?;
            let segments = size
                .segments(&trace)?
                .collect::<Result<Vec<_>, _>>()
                .map_err(|error| wasmtime::format_err!("instance {index}: {error}"))?;

            let started = Instant::now();
            let circuit = self.wasm_circuit(size)?;
            timings.setup += started.elapsed();

            let started = Instant::now();
            let mut run = circuit.prover.start(&program, initial)?;
            for segment in &segments {
                run.segment(segment)?;
            }
            let proof = run.finish()?;
            timings.proving += started.elapsed();

            let started = Instant::now();
            let proof = circuit.prover.compress(&proof, &circuit.setup)?;
            timings.compression += started.elapsed();

            instances.push(WasmInstanceProof {
                coroutine,
                size,
                program,
                initial,
                last,
                proof,
            });
        }
        let roots = trace_commitments(&instances);
        let methods = method_dictionary(&transaction.trace);
        let started = Instant::now();
        let interleaving = self.interleaving(&methods)?;
        timings.setup += started.elapsed();

        let started = Instant::now();
        let proof = interleaving
            .context
            .prove(&transaction.trace, &transaction.statement, &roots)
            .context("failed to prove the interleaving")?;
        timings.proving += started.elapsed();

        let started = Instant::now();
        let proof = interleaving.context.compress(&proof, &interleaving.setup)?;
        timings.compression += started.elapsed();

        Ok((
            TransactionProof {
                instances,
                interleaving: proof,
                methods,
            },
            timings,
        ))
    }

    /// Verify instance execution and interleaving against `statement`.
    pub fn verify(
        &mut self,
        proof: &TransactionProof,
        statement: &TransactionStatement,
    ) -> wasmtime::Result<()> {
        for (index, instance) in proof.instances.iter().enumerate() {
            ensure!(
                instance.last.is_terminal() && !instance.last.trapped,
                "instance {index} did not run to completion"
            );
            let circuit = self.wasm_circuit(instance.size)?;
            circuit
                .verifier
                .verify_final(
                    &instance.program,
                    &instance.initial,
                    &instance.last,
                    &circuit.key,
                    &instance.proof,
                )
                .with_context(|| format!("instance {index} has an invalid proof"))?;
        }
        let roots = trace_commitments(&proof.instances);
        if roots.len() != proof.instances.len() {
            bail!("two instances claim the same coroutine");
        }
        let interleaving = self.interleaving(&proof.methods)?;
        interleaving
            .context
            .verify_compressed(&proof.interleaving, &interleaving.key, statement, &roots)
            .context("the interleaving proof is invalid")?;
        Ok(())
    }
}

/// The packed coroutine id of each captured instance, in instance order.
fn coroutine_ids(execution: &CapturedExecution) -> wasmtime::Result<Vec<u32>> {
    let transaction = &execution.transaction;
    let mut ids = vec![None; execution.traces.len()];
    let utxos = (0..).map(CoroutineId::Utxo).zip(&transaction.utxos);
    // Coord(0) is the transaction context, so the entry script is Coord(1).
    let coordinators = (1..).map(CoroutineId::Coord).zip(&transaction.coordinators);
    for (id, process) in utxos.chain(coordinators) {
        let slot = ids
            .get_mut(process.0)
            .context("the interleaving names a trace that was not captured")?;
        ensure!(slot.is_none(), "one trace has two coroutine ids");
        *slot = Some(id.encoded());
    }
    ids.into_iter()
        .map(|id| id.context("a captured trace is not part of the transaction"))
        .collect()
}

fn trace_commitments(instances: &[WasmInstanceProof]) -> TraceCommitments {
    instances
        .iter()
        .map(|instance| (instance.coroutine, instance.last.comm_chain))
        .collect()
}
