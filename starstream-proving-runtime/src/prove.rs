//! Compressed Wasm instance proofs linked by an interleaving proof.
//!
//! Verification uses independently prepared programs and entry states.
//! The caller still authenticates the component bytes and transaction statement.

mod wire;
pub use wire::EncodedTransactionProof;

use std::collections::{BTreeMap, VecDeque};
use std::sync::Arc;
use std::thread::{ScopedJoinHandle, scope};
use std::time::{Duration, Instant};

use neo_wasm::nebula::{WasmApplication, WasmSize, WasmStep};
use neo_wasm::{CommChainState, WasmStepState, WasmVmStep, WasmtimeTraceState};
use nightstream::nebula::{self, Circuit};
use starstream_interleaving_prover::proving::{
    CompressedInterleavingProof, InterleavingProof, InterleavingProofContext, Limits, ProofConfig,
    ProvingError, method_dictionary,
};
use starstream_interleaving_prover::{CoroutineId, TraceCommitments, Unsatisfied};
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

struct InterleavingResult {
    proof: Result<CompressedInterleavingProof, ProvingError>,
    proving: Duration,
    compression: Duration,
}

impl Interleaving {
    fn prove_with_finalized_roots(
        &self,
        transaction: &InterleavedTransaction,
    ) -> wasmtime::Result<(TraceCommitments, InterleavingResult)> {
        let started = Instant::now();
        let (proof, roots) = self
            .context
            .prove_with_finalized_roots(&transaction.trace, &transaction.statement)
            .context("failed to prove the interleaving")?;
        Ok((roots, self.compress(&proof, started.elapsed())))
    }

    fn compress(&self, proof: &InterleavingProof, proving: Duration) -> InterleavingResult {
        let started = Instant::now();
        let proof = self.context.compress(proof, &self.setup);
        InterleavingResult {
            proof,
            proving,
            compression: started.elapsed(),
        }
    }
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

struct WasmInput {
    index: u32,
    size: WasmSize,
    program: nebula::Program,
    initial: WasmStepState,
    last: WasmStepState,
    trace: Vec<WasmVmStep>,
}

impl WasmInput {
    fn from_capture(
        index: u32,
        captured: &WasmtimeTraceState,
        circuits: &BTreeMap<WasmSize, Arc<WasmCircuit>>,
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
        let program = match circuits.get(&size) {
            Some(circuit) => circuit.prover.application().program(&preload)?,
            None => size.application()?.program(&preload)?,
        };
        Ok(Self {
            index,
            size,
            program,
            initial,
            last,
            trace,
        })
    }

    fn segments(
        &self,
    ) -> wasmtime::Result<impl Iterator<Item = wasmtime::Result<Vec<WasmStep>>> + '_> {
        let index = self.index;
        Ok(self.size.segments(&self.trace)?.map(move |segment| {
            segment.map_err(|error| wasmtime::format_err!("instance {index}: {error}"))
        }))
    }

    fn run(
        &self,
        circuit: &WasmCircuit,
        segments: impl IntoIterator<Item = wasmtime::Result<Vec<WasmStep>>>,
    ) -> wasmtime::Result<(nebula::Proof, Duration)> {
        let started = Instant::now();
        let mut run = circuit.prover.start(&self.program, self.initial)?;
        let mut proving = started.elapsed();
        for segment in segments {
            let segment = segment?;
            let started = Instant::now();
            run.segment(&segment)?;
            proving += started.elapsed();
        }
        let started = Instant::now();
        ensure!(
            *run.state() == self.last,
            "instance {} ended in another state",
            self.index
        );
        let proof = run.finish()?;
        Ok((proof, proving + started.elapsed()))
    }

    fn prove(
        self,
        circuit: &WasmCircuit,
        mut timings: WasmProofTimings,
        prebuilt: Option<Vec<Vec<WasmStep>>>,
    ) -> wasmtime::Result<(WasmInstanceProof, WasmProofTimings)> {
        let (proof, proving) = match prebuilt {
            Some(segments) => self.run(circuit, segments.into_iter().map(Ok)),
            None => {
                let result = self.run(circuit, self.segments()?);
                if result.is_err() {
                    self.segments()?.try_for_each(|segment| segment.map(drop))?;
                }
                result
            }
        }?;
        drop(self.trace);
        timings.phases.proving = proving;
        let started = Instant::now();
        let proof = circuit.prover.compress(&proof, &circuit.setup)?;
        timings.phases.compression = started.elapsed();
        Ok((
            WasmInstanceProof {
                coroutine: timings.coroutine.encoded(),
                size: self.size,
                program: self.program,
                initial: self.initial,
                last: self.last,
                proof,
            },
            timings,
        ))
    }
}

fn join<T>(worker: ScopedJoinHandle<'_, T>) -> T {
    worker
        .join()
        .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
}

/// Reuse across transactions to amortize circuit compilation and key setup.
pub struct TransactionProver {
    engine: nightstream::Engine,
    wasm: BTreeMap<WasmSize, Arc<WasmCircuit>>,
    interleaving: Option<Arc<Interleaving>>,
}

impl TransactionProver {
    pub fn new(engine: nightstream::Engine) -> Self {
        Self {
            engine,
            wasm: BTreeMap::new(),
            interleaving: None,
        }
    }

    fn wasm_circuit(&mut self, size: WasmSize) -> wasmtime::Result<&Arc<WasmCircuit>> {
        if !self.wasm.contains_key(&size) {
            let circuit = Circuit::compile(size.geometry(), size.application()?)?;
            let prover = circuit.prover(self.engine, SECURITY_BITS)?;
            let verifier = circuit.verifier(self.engine, SECURITY_BITS)?;
            let setup = prover.compression_setup()?;
            let key = verifier.compression_key()?;
            self.wasm.insert(
                size,
                Arc::new(WasmCircuit {
                    prover,
                    verifier,
                    setup,
                    key,
                }),
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
            self.interleaving = Some(Arc::new(Interleaving {
                context,
                setup,
                key,
            }));
        }
        Ok(self.interleaving.as_deref().expect("set above"))
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
        let cached = self
            .interleaving
            .as_ref()
            .filter(|_| {
                self.engine == nightstream::Engine::Optimized
                    && !coroutines.is_empty()
                    && rayon::current_thread_index().is_none()
            })
            .cloned();
        let mut early_interleaving = None;
        let mut proof_wall = Duration::ZERO;
        let mut inputs = captured.into_iter().zip(coroutines);
        let count = inputs.len();
        let leading = if cached.is_some() {
            count.saturating_sub(MAX_CONCURRENT_WASM_PROOFS)
        } else {
            0
        };
        let mut all_reused = true;
        for (count, tail) in [(leading, false), (count - leading, true)] {
            if count == 0 {
                continue;
            }
            let context = cached.as_ref().filter(|_| tail && all_reused);
            let started = Instant::now();
            let pools = context.and_then(|_| {
                let share = if leading == 0 { 16 } else { 18 };
                let threads = std::num::NonZeroUsize::new(
                    rayon::current_num_threads()
                        .saturating_mul(share)
                        .div_ceil(30),
                )?;
                let pool = || nightstream::CpuProverPool::new(threads).ok();
                pool().zip(pool())
            });
            let pools = pools.as_ref();
            scope(|workers| -> wasmtime::Result<()> {
                let mut early = context.map(|context| {
                    workers.spawn(move || {
                        let job = || context.prove_with_finalized_roots(transaction);
                        match pools {
                            Some((_, pool)) => pool.install(job),
                            None => job(),
                        }
                    })
                });
                let mut overlap = early.is_some();
                let mut pending: VecDeque<ScopedJoinHandle<'_, _>> = VecDeque::new();
                let mut record = |(instance, timing): (WasmInstanceProof, WasmProofTimings)| {
                    timings.total.accumulate(timing.phases);
                    timings.wasm.push(timing);
                    instances.push(instance);
                };
                let mut wasm_result =
                    inputs
                        .by_ref()
                        .take(count)
                        .try_for_each(|((index, captured), coroutine)| {
                            let input = WasmInput::from_capture(index, captured, &self.wasm);
                            let circuit_reused = input
                                .as_ref()
                                .is_ok_and(|input| self.wasm.contains_key(&input.size));
                            if !circuit_reused {
                                all_reused = false;
                                while let Some(worker) = pending.pop_front() {
                                    record(join(worker)?);
                                }
                                if let Some(worker) = early.take() {
                                    early_interleaving = Some(worker.join());
                                    proof_wall += started.elapsed();
                                }
                                overlap = false;
                            }
                            let input = input?;
                            let prebuilt = if overlap || (!tail && all_reused) {
                                None
                            } else {
                                Some(input.segments()?.collect::<wasmtime::Result<Vec<_>>>()?)
                            };
                            let started = Instant::now();
                            let circuit = Arc::clone(self.wasm_circuit(input.size)?);
                            let timing = WasmProofTimings {
                                coroutine,
                                size: input.size,
                                circuit_reused,
                                phases: ProofPhaseTimings {
                                    setup: started.elapsed(),
                                    ..Default::default()
                                },
                            };
                            let job = move || input.prove(&circuit, timing, prebuilt);
                            if overlap {
                                pending.push_back(workers.spawn(move || match pools {
                                    Some((pool, _)) => pool.install(job),
                                    None => job(),
                                }));
                            } else {
                                let started = Instant::now();
                                let result = job()?;
                                proof_wall += started.elapsed();
                                record(result);
                            }
                            Ok(())
                        });
                for worker in pending {
                    let joined = worker.join();
                    if wasm_result.is_ok() {
                        wasm_result = joined
                            .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
                            .map(&mut record);
                    }
                }
                if let Some(worker) = early {
                    early_interleaving = Some(worker.join());
                    proof_wall += started.elapsed();
                }
                wasm_result
            })?;
        }
        let methods = method_dictionary(&transaction.trace);
        timings.interleaving_circuit_reused = self.interleaving.is_some();
        let started = Instant::now();
        let interleaving = self.interleaving()?;
        timings.interleaving.setup = started.elapsed();

        let result = match early_interleaving {
            Some(result) => {
                let (finalized, result) =
                    result.unwrap_or_else(|panic| std::panic::resume_unwind(panic))?;
                if finalized.len() != instances.len()
                    || instances.iter().any(|instance| {
                        finalized.get(&instance.coroutine) != Some(&instance.last.comm_chain)
                    })
                {
                    return Err(ProvingError::Circuit(
                        Unsatisfied::TransactionCommitment.into(),
                    ))
                    .context("failed to prove the interleaving");
                }
                result
            }
            None => {
                let roots = trace_commitments(&instances);
                let started = Instant::now();
                let proof = interleaving
                    .context
                    .prove(&transaction.trace, &transaction.statement, &roots)
                    .context("failed to prove the interleaving")?;
                let result = interleaving.compress(&proof, started.elapsed());
                proof_wall += started.elapsed();
                result
            }
        };
        let proof = result.proof?;
        timings.prove_compress_wall = proof_wall;
        timings.interleaving.proving = result.proving;
        timings.interleaving.compression = result.compression;
        timings.total.accumulate(timings.interleaving);

        Ok((
            TransactionProof {
                instances,
                interleaving: proof,
                methods,
            },
            timings,
        ))
    }

    /// Verify against independently prepared programs and the expected statement.
    pub fn verify(
        &mut self,
        proof: &TransactionProof,
        expected: &PreparedVerification,
        statement: &TransactionStatement,
    ) -> wasmtime::Result<()> {
        ensure!(
            expected.input_count() == statement.inputs.len(),
            "missing verification program for a transaction input"
        );
        let token_count = expected.input_token_count();
        ensure!(
            token_count == statement.input_tokens.len(),
            "token program bindings must cover exactly the transaction input tokens"
        );
        ensure!(
            proof
                .instances
                .iter()
                .filter(|instance| matches!(
                    CoroutineId::from_packed(instance.coroutine),
                    Some(CoroutineId::Token(_))
                ))
                .count()
                == token_count,
            "missing or unexpected token instance"
        );
        for (index, instance) in proof.instances.iter().enumerate() {
            let (program, initial) = expected.instance(
                instance.coroutine,
                instance.size,
                &instance.program,
                &instance.initial,
            )?;
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
