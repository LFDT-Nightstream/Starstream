//! Compressed Wasm instance proofs linked by an interleaving proof.
//!
//! Verification uses independently prepared programs and entry states.
//! The caller still authenticates the component bytes and transaction statement.
//! The full transaction proof has no wire format yet.

use std::collections::BTreeMap;
use std::time::{Duration, Instant};

use neo_wasm::nebula::{WasmApplication, WasmSize, WasmStep};
use neo_wasm::{CommChainState, WasmStepState, WasmtimeTraceState};
use nightstream::nebula::{self, Circuit};
use starstream_interleaving_prover::proving::{
    CompressedInterleavingProof, InterleavingProofContext, Limits, ProofConfig, method_dictionary,
};
use starstream_interleaving_prover::{CoroutineId, TraceCommitments};
use starstream_interleaving_spec::interleaver::InterleavedTransaction;
use starstream_interleaving_spec::{MethodHash, TransactionStatement};
use wasmtime::error::Context as _;
use wasmtime::{bail, ensure};

use crate::{CapturedExecution, PreparedVerification};

/// Security target supported by the Wasm profiles and interleaving limits.
const SECURITY_BITS: u32 = 109;

/// Interleaving rows of one proof step.
const BATCH_SIZE: usize = 16;

const MAX_CONCURRENT_WASM_PROOFS: usize = 2;

/// Shared across programs with the same Wasm size.
struct WasmCircuit {
    prover: nebula::Prover<WasmApplication>,
    verifier: nebula::Verifier<WasmApplication>,
    setup: nightstream::CompressionSetup,
    key: nightstream::CompressionKey,
}

struct Interleaving {
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

/// Compressed proof sizes in bytes, excluding copied verification inputs.
#[derive(Clone, Debug)]
pub struct ProofSizes {
    /// One entry per Wasm instance, in instance order.
    pub wasm: Vec<usize>,
    pub interleaving: usize,
}

impl ProofSizes {
    pub fn total(&self) -> usize {
        self.wasm.iter().sum::<usize>() + self.interleaving
    }
}

impl TransactionProof {
    /// Round-trip compressed proofs through bytes and report their sizes.
    ///
    /// Programs, Wasm states, coroutine metadata, and the method dictionary
    /// are copied, not serialized; this is not a transaction wire format.
    pub fn through_bytes(&self) -> wasmtime::Result<(Self, ProofSizes)> {
        let mut sizes = ProofSizes {
            wasm: Vec::with_capacity(self.instances.len()),
            interleaving: 0,
        };
        let mut instances = Vec::with_capacity(self.instances.len());
        for instance in &self.instances {
            let bytes = instance.proof.to_bytes()?;
            sizes.wasm.push(bytes.len());
            instances.push(WasmInstanceProof {
                coroutine: instance.coroutine,
                size: instance.size,
                program: instance.program.clone(),
                initial: instance.initial,
                last: instance.last,
                proof: nebula::FinalProof::from_bytes(&bytes)?,
            });
        }
        let bytes = self.interleaving.to_bytes()?;
        sizes.interleaving = bytes.len();
        Ok((
            Self {
                instances,
                interleaving: CompressedInterleavingProof::from_bytes(&bytes)?,
                methods: self.methods.clone(),
            },
            sizes,
        ))
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ProofPhaseTimings {
    /// Circuit and compression-key setup, including cache lookups.
    pub setup: Duration,
    pub proving: Duration,
    pub compression: Duration,
}

#[derive(Clone, Debug)]
pub struct WasmProofTimings {
    pub coroutine: CoroutineId,
    pub size: WasmSize,
    pub circuit_reused: bool,
    pub phases: ProofPhaseTimings,
}

#[derive(Clone, Debug, Default)]
pub struct ProofTimings {
    pub wasm: Vec<WasmProofTimings>,
    pub interleaving: ProofPhaseTimings,
    pub interleaving_circuit_reused: bool,
    /// Sum of the component phases, including phases that ran at the same time.
    pub total: ProofPhaseTimings,
    /// Elapsed time for proving and compression, including worker scheduling.
    pub prove_compress_wall: Duration,
}

impl ProofPhaseTimings {
    fn accumulate(&mut self, other: Self) {
        self.setup += other.setup;
        self.proving += other.proving;
        self.compression += other.compression;
    }
}

impl ProofTimings {
    fn record_wasm(
        &mut self,
        coroutine: CoroutineId,
        size: WasmSize,
        reused: bool,
        phases: ProofPhaseTimings,
    ) {
        self.total.accumulate(phases);
        self.wasm.push(WasmProofTimings {
            coroutine,
            size,
            circuit_reused: reused,
            phases,
        });
    }
}

struct PreparedWasm {
    coroutine: CoroutineId,
    size: WasmSize,
    program: nebula::Program,
    initial: WasmStepState,
    last: WasmStepState,
    segments: Vec<Vec<WasmStep>>,
}

impl PreparedWasm {
    fn new(
        index: u32,
        captured: &WasmtimeTraceState,
        coroutine: CoroutineId,
    ) -> wasmtime::Result<Self> {
        let artifacts = captured.artifacts();
        let trace = neo_wasm::traces_from_wasmtime_steps_with_host_events(
            captured.steps(),
            artifacts,
            CommChainState::default(),
        )
        .map_err(|error| wasmtime::format_err!("failed to normalize instance {index}: {error}"))?;
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

        Ok(Self {
            coroutine,
            size,
            program,
            initial,
            last,
            segments,
        })
    }

    fn prove(
        self,
        circuit: &WasmCircuit,
    ) -> wasmtime::Result<(WasmInstanceProof, ProofPhaseTimings)> {
        let start = Instant::now();
        let mut run = circuit.prover.start(&self.program, self.initial)?;
        for segment in &self.segments {
            run.segment(segment)?;
        }
        let proof = run.finish()?;
        let compression = Instant::now();
        let proof = circuit.prover.compress(&proof, &circuit.setup)?;
        let finish = Instant::now();
        Ok((
            WasmInstanceProof {
                coroutine: self.coroutine.encoded(),
                size: self.size,
                program: self.program,
                initial: self.initial,
                last: self.last,
                proof,
            },
            ProofPhaseTimings {
                setup: Duration::ZERO,
                proving: compression - start,
                compression: finish - compression,
            },
        ))
    }
}

impl Interleaving {
    fn prove(
        &self,
        transaction: &InterleavedTransaction,
        roots: &TraceCommitments,
    ) -> wasmtime::Result<(CompressedInterleavingProof, ProofPhaseTimings)> {
        let start = Instant::now();
        let proof = self
            .context
            .prove(&transaction.trace, &transaction.statement, roots)
            .context("failed to prove the interleaving")?;
        let compression = Instant::now();
        let proof = self.context.compress(&proof, &self.setup)?;
        let finish = Instant::now();
        Ok((
            proof,
            ProofPhaseTimings {
                setup: Duration::ZERO,
                proving: compression - start,
                compression: finish - compression,
            },
        ))
    }
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

    fn interleaving(&mut self) -> wasmtime::Result<&Interleaving> {
        if self.interleaving.is_none() {
            let mut config = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, BATCH_SIZE)?;
            config.engine = self.engine;
            let context = InterleavingProofContext::new(config)?;
            let setup = context.compression_setup()?;
            let key = context.compression_key()?;
            self.interleaving = Some(Interleaving {
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
        let mut prepared = captured
            .into_iter()
            .zip(coroutines)
            .map(|((index, captured), coroutine)| PreparedWasm::new(index, captured, coroutine));
        let warm_tail = if self.engine == nightstream::Engine::Optimized
            && self.interleaving.is_some()
            && rayon::current_thread_index().is_none()
        {
            prepared.len().min(MAX_CONCURRENT_WASM_PROOFS)
        } else {
            0
        };
        let leading = prepared.len() - warm_tail;
        for instance in prepared.by_ref().take(leading) {
            self.prove_wasm_instance(instance?, &mut instances, &mut timings)?;
        }
        let tail: Vec<_> = prepared.collect();
        let ready = warm_tail > 0
            && tail.iter().all(|instance| {
                instance
                    .as_ref()
                    .is_ok_and(|instance| self.wasm.contains_key(&instance.size))
            });
        timings.interleaving_circuit_reused = self.interleaving.is_some();
        let (proof, phases) =
            if let Some(interleaving) = self.interleaving.as_ref().filter(|_| ready) {
                self.prove_parallel(
                    tail.into_iter().collect::<Result<_, _>>()?,
                    transaction,
                    interleaving,
                    &mut instances,
                    &mut timings,
                )?
            } else {
                for instance in tail {
                    self.prove_wasm_instance(instance?, &mut instances, &mut timings)?;
                }
                let roots = trace_commitments(&instances);
                let started = Instant::now();
                let interleaving = self.interleaving()?;
                let setup = started.elapsed();
                let (proof, mut phases) = interleaving.prove(transaction, &roots)?;
                phases.setup = setup;
                timings.prove_compress_wall += phases.proving + phases.compression;
                (proof, phases)
            };
        timings.interleaving = phases;
        timings.total.accumulate(phases);

        Ok((
            TransactionProof {
                instances,
                interleaving: proof,
                methods: method_dictionary(&transaction.trace),
            },
            timings,
        ))
    }

    fn prove_wasm_instance(
        &mut self,
        instance: PreparedWasm,
        instances: &mut Vec<WasmInstanceProof>,
        timings: &mut ProofTimings,
    ) -> wasmtime::Result<()> {
        let size = instance.size;
        let coroutine = instance.coroutine;
        let circuit_reused = self.wasm.contains_key(&size);
        let started = Instant::now();
        let circuit = self.wasm_circuit(size)?;
        let setup = started.elapsed();

        let (proof, mut phases) = instance.prove(circuit)?;
        phases.setup = setup;
        timings.prove_compress_wall += phases.proving + phases.compression;
        timings.record_wasm(coroutine, size, circuit_reused, phases);
        instances.push(proof);
        Ok(())
    }

    fn prove_parallel(
        &self,
        prepared: Vec<PreparedWasm>,
        transaction: &InterleavedTransaction,
        interleaving: &Interleaving,
        instances: &mut Vec<WasmInstanceProof>,
        timings: &mut ProofTimings,
    ) -> wasmtime::Result<(CompressedInterleavingProof, ProofPhaseTimings)> {
        let mut roots = trace_commitments(instances);
        roots.extend(
            prepared
                .iter()
                .map(|instance| (instance.coroutine.encoded(), instance.last.comm_chain)),
        );
        let started = Instant::now();
        let count = rayon::current_num_threads();
        let pool = |part: usize| {
            let threads = (count.saturating_mul(part)).div_ceil(30);
            nightstream::CpuProverPool::new(std::num::NonZeroUsize::new(threads)?).ok()
        };
        let workers = pool(18).zip(pool(18));
        let workers = workers.as_ref();
        let (wasm, interleaving) = std::thread::scope(|scope| {
            let interleaving = scope.spawn(|| match workers {
                Some((_, pool)) => pool.install(|| interleaving.prove(transaction, &roots)),
                None => interleaving.prove(transaction, &roots),
            });
            let workers = prepared
                .into_iter()
                .map(|instance| {
                    let coroutine = instance.coroutine;
                    let size = instance.size;
                    let circuit = &self.wasm[&size];
                    (
                        coroutine,
                        size,
                        scope.spawn(move || match workers {
                            Some((pool, _)) => pool.install(|| instance.prove(circuit)),
                            None => instance.prove(circuit),
                        }),
                    )
                })
                .collect::<Vec<_>>();
            let wasm = workers
                .into_iter()
                .map(|(coroutine, size, worker)| (coroutine, size, worker.join()))
                .collect::<Vec<_>>();
            (wasm, interleaving.join())
        });
        timings.prove_compress_wall += started.elapsed();
        for (coroutine, size, result) in wasm {
            let (proof, phases) =
                result.unwrap_or_else(|panic| std::panic::resume_unwind(panic))?;
            instances.push(proof);
            timings.record_wasm(coroutine, size, true, phases);
        }
        interleaving.unwrap_or_else(|panic| std::panic::resume_unwind(panic))
    }

    /// Verify against independently prepared programs and the expected statement.
    pub fn verify(
        &mut self,
        proof: &TransactionProof,
        expected: &PreparedVerification,
        statement: &TransactionStatement,
    ) -> wasmtime::Result<()> {
        for (index, instance) in proof.instances.iter().enumerate() {
            let (program, initial) =
                expected.instance(instance.size, &instance.program, &instance.initial)?;
            ensure!(
                instance.last.is_terminal() && !instance.last.trapped,
                "instance {index} did not run to completion"
            );
            let circuit = self.wasm_circuit(instance.size)?;
            circuit
                .verifier
                .verify_final(
                    program,
                    initial,
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
        let interleaving = self.interleaving()?;
        interleaving
            .context
            .verify_compressed(
                &proof.methods,
                &proof.interleaving,
                &interleaving.key,
                statement,
                &roots,
            )
            .context("the interleaving proof is invalid")?;
        Ok(())
    }
}

/// Coroutine IDs in captured instance order.
fn coroutine_ids(execution: &CapturedExecution) -> wasmtime::Result<Vec<CoroutineId>> {
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
        *slot = Some(id);
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
