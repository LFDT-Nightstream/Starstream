//! Transaction-bound Nebula proofs. Host checks remain the fast diagnostic path.

use super::*;
pub use crate::memory::MemoryId;
use crate::terminal::TerminalClaim;
use neo_application::MemoryPortActivation;
use nightstream::Engine;
use nightstream::nebula::{self, R1csApplication, RowRelation, Size};
use p3_field::PrimeField64;
use starstream_interleaving_spec::{MethodHash, TransactionStatement};
use std::num::NonZeroUsize;

#[derive(Debug, thiserror::Error)]
pub enum ProvingError {
    #[error(transparent)]
    Circuit(#[from] crate::Error),
    #[error(transparent)]
    Backend(#[from] nightstream::Error),
    #[error(transparent)]
    Application(#[from] nebula::R1csApplicationError),
    #[error(transparent)]
    Binding(#[from] nebula::CatalogBindingError),
    #[error(transparent)]
    Prove(#[from] nebula::ProveError),
    #[error(transparent)]
    Verification(#[from] nebula::VerifyError),
    #[error(transparent)]
    FinalClaim(#[from] FinalClaimError),
    #[error("cannot prove an empty trace")]
    EmptyTrace,
    #[error("trace requires {segments} segments, but the profile allows {max}")]
    TraceTooLong { segments: usize, max: usize },
    #[error("invalid proof configuration: {0}")]
    Configuration(&'static str),
    #[error("method dictionary must be sorted, unique, and match the normalized trace")]
    MethodDictionary,
}

/// Statistical security that the local profiles ask of the backend.
const MINIMUM_SECURITY_BITS: u32 = 109;

/// The geometry with the most segments, halving from `limit`, that the
/// backend accepts at [`MINIMUM_SECURITY_BITS`].
///
/// The backend charges the memory fingerprint term `2 * m_mem / q^2` once per
/// segment, so the memory sizes and the slot count bound the segment count.
fn secure_geometry(
    r: usize,
    mu: usize,
    b_ops: usize,
    n: usize,
    limit: usize,
) -> Option<nebula::Geometry> {
    std::iter::successors(Some(limit), |s_max| Some(s_max / 2))
        .take_while(|s_max| *s_max > 0)
        .filter_map(|s_max| nebula::Geometry::new(r, mu, b_ops, n, s_max))
        .find(|geometry| geometry.segments_security_bits() >= MINIMUM_SECURITY_BITS)
}

/// Verifier-owned circuit configuration.
#[derive(Clone, Debug)]
pub struct ProofConfig {
    pub batch_size: usize,
    pub geometry: nebula::Geometry,
    pub regions: Vec<(MemoryId, nebula::Region)>,
    pub slots: Vec<Vec<usize>>,
    pub engine: Engine,
    pub minimum_security_bits: u32,
}

impl ProofConfig {
    /// Size a local benchmark from its trace, including ROM preloads. This is
    /// not a ledger sizing policy: a verifier must independently accept the
    /// resulting configuration, never take arbitrary prover-supplied sizes.
    pub fn for_trace_benchmark(
        trace: &Trace,
        batch_size: usize,
        segment_batches: usize,
    ) -> Result<Self, ProvingError> {
        if !segment_batches.is_power_of_two() {
            return Err(ProvingError::Configuration(
                "benchmark segment length must be a power of two",
            ));
        }
        let batch = Batch::new(batch_size)?;
        let normalized = normalize(trace);
        let packed = batch.pack(&normalized.steps);
        let preload = crate::memory::preload_tables(&normalized.method_table).entries();
        let mut memories = Vec::new();
        for memory in batch.memory.entries() {
            let mut widths = vec![1usize; memory.ports[0].address_columns.len()];
            let mut include = |address: Vec<u64>| {
                for (width, value) in widths.iter_mut().zip(address) {
                    *width = (*width).max((u64::BITS - value.leading_zeros()) as usize);
                }
            };
            for (_, address, _) in preload.iter().filter(|(id, _, _)| *id == memory.id) {
                include(address.iter().map(|x| u64::from(*x)).collect());
            }
            for row in &packed.rows {
                for port in &memory.ports {
                    let active = match port.activation {
                        MemoryPortActivation::Always => true,
                        MemoryPortActivation::When(column) => row[column] == F::ONE,
                        MemoryPortActivation::Unless(column) => row[column] == F::ZERO,
                    };
                    if active {
                        include(
                            port.address_columns
                                .iter()
                                .map(|column| row[*column].as_canonical_u64())
                                .collect(),
                        );
                    }
                }
            }
            if widths.iter().sum::<usize>() >= 32 {
                return Err(ProvingError::Configuration(
                    "benchmark memory region is too large",
                ));
            }
            memories.push((memory.id, memory.kind, widths));
        }
        let (regions, [rom_cells, ram_cells]) = nebula::pack_regions(memories)?;
        let ports = batch_size
            * batch
                .memory
                .entries()
                .iter()
                .map(|m| m.ports.len())
                .sum::<usize>();
        let r = rom_cells
            .max(segment_batches as u64)
            .max(2)
            .next_power_of_two()
            .ilog2() as usize;
        let mu = ram_cells
            .max(rom_cells)
            .max(segment_batches as u64)
            .max(2)
            .next_power_of_two()
            .ilog2() as usize;
        let geometry = secure_geometry(r, mu, ports, segment_batches, 4096 / segment_batches)
            .ok_or(ProvingError::Configuration(
                "invalid benchmark memory geometry",
            ))?;
        Ok(Self {
            batch_size,
            geometry,
            regions,
            slots: (0..ports).map(|rank| vec![rank]).collect(),
            engine: Engine::Optimized,
            minimum_security_bits: MINIMUM_SECURITY_BITS,
        })
    }

    /// Share operation slots between ports that the relation never activates
    /// on the same step, and resize the geometry to the smaller slot count.
    pub fn with_planned_slots(mut self) -> Result<Self, ProvingError> {
        let batch_size = NonZeroUsize::new(self.batch_size)
            .ok_or(ProvingError::Circuit(crate::Error::InvalidBatchSize))?;
        self.slots = crate::memory::planned_slots(batch_size)?;
        let g = self.geometry;
        self.geometry = secure_geometry(g.r, g.mu, self.slots.len(), g.n, g.s_max).ok_or(
            ProvingError::Configuration("memory geometry exceeds backend limits"),
        )?;
        Ok(self)
    }

    /// Use segments of `segment_batches` batches, keeping the memory sizes.
    pub fn with_segment_batches(mut self, segment_batches: usize) -> Result<Self, ProvingError> {
        let g = self.geometry;
        self.geometry = 4096usize
            .checked_div(segment_batches)
            .and_then(|limit| secure_geometry(g.r, g.mu, g.b_ops, segment_batches, limit))
            .ok_or(ProvingError::Configuration("invalid segment length"))?;
        Ok(self)
    }

    /// Report circuit dimensions without assembling or preparing a prover.
    pub fn size_parts(&self) -> Result<Vec<nebula::SizePart>, ProvingError> {
        let batch = Batch::new(self.batch_size)?;
        let app = application(&batch, self)?;
        Ok(nebula::size_parts(&self.geometry, &app)?)
    }

    /// Experimental bounded profile for small integration tests, not a ledger
    /// capacity policy. Single-part addresses have eight bits; multipart ones
    /// have four per part. Each segment contains eight packed batches.
    pub fn small(batch_size: usize) -> Result<Self, ProvingError> {
        let batch = Batch::new(batch_size)?;
        let (regions, [rom_cells, ram_cells]) =
            nebula::pack_regions(batch.memory.entries().iter().map(|m| {
                let parts = m.ports[0].address_columns.len();
                (m.id, m.kind, vec![if parts == 1 { 8 } else { 4 }; parts])
            }))?;
        // One slot per port preserves the catalog's access order, including
        // writes followed by reads in the same execution row.
        let ports = batch_size
            * batch
                .memory
                .entries()
                .iter()
                .map(|m| m.ports.len())
                .sum::<usize>();
        let slots = (0..ports).map(|rank| vec![rank]).collect();
        let r = rom_cells.max(2).next_power_of_two().ilog2() as usize;
        let mu = ram_cells.max(rom_cells).max(2).next_power_of_two().ilog2() as usize;
        let geometry = secure_geometry(r, mu, ports, 8, 512).ok_or(ProvingError::Configuration(
            "memory geometry exceeds backend limits",
        ))?;
        Ok(Self {
            batch_size,
            geometry,
            regions,
            slots,
            engine: Engine::Optimized,
            minimum_security_bits: MINIMUM_SECURITY_BITS,
        })
    }
}

pub struct TransactionProof {
    pub proof: nebula::Proof,
    // Authenticated by the memory verifier; terminal projection is checked below.
    pub final_state: Vec<F>,
}

pub struct TransactionProofContext {
    batch: Batch,
    config: ProofConfig,
    methods: Vec<MethodHash>,
    program: nebula::Program,
    prover: nebula::Prover<R1csApplication<MemoryId>>,
    verifier: nebula::Verifier<R1csApplication<MemoryId>>,
}

fn application(
    batch: &Batch,
    config: &ProofConfig,
) -> Result<R1csApplication<MemoryId>, ProvingError> {
    Ok(R1csApplication::new(
        RowRelation::of(batch.relation.r1cs())?,
        &batch.continuity,
        &batch.memory,
        Size {
            geometry: config.geometry,
            rows: NonZeroUsize::new(batch.size)
                .ok_or(ProvingError::Circuit(crate::Error::InvalidBatchSize))?,
            regions: config.regions.clone(),
            slots: config.slots.clone(),
        },
    )?)
}

/// Dictionary used by normalization. A verifier must agree on this table
/// independently; deriving it from the trace is suitable for local tests.
pub fn method_dictionary(trace: &Trace) -> Vec<MethodHash> {
    crate::step::method_table(trace)
}

impl TransactionProofContext {
    /// Compile a verifier-owned relation and bind its method/effect ROM.
    /// Currently normalization requires the exact trace-local dictionary.
    pub fn new(config: ProofConfig, methods: Vec<MethodHash>) -> Result<Self, ProvingError> {
        if methods.windows(2).any(|pair| pair[0].0 >= pair[1].0) {
            return Err(ProvingError::MethodDictionary);
        }
        let batch = Batch::new(config.batch_size)?;
        let app = application(&batch, &config)?;
        let program = app.program(&crate::memory::preload_tables(&methods))?;
        let circuit = nebula::Circuit::compile(config.geometry, app)?;
        let prover = circuit.prover(config.engine, config.minimum_security_bits)?;
        let verifier = circuit.verifier(config.engine, config.minimum_security_bits)?;
        Ok(Self {
            batch,
            config,
            methods,
            program,
            prover,
            verifier,
        })
    }

    pub fn prove(
        &self,
        trace: &Trace,
        statement: &TransactionStatement,
        roots: &crate::TraceCommitments,
    ) -> Result<TransactionProof, ProvingError> {
        let normalized = normalize(trace);
        let padding = normalized
            .steps
            .last()
            .ok_or(ProvingError::EmptyTrace)?
            .padding_after();
        if normalized.method_table != self.methods {
            return Err(ProvingError::MethodDictionary);
        }
        let mut packed =
            super::check_normalized(&self.batch, normalized, Some(roots), Some(statement))?;
        let final_state = final_state(&self.batch, &packed);
        pad_to_segments(&self.batch, &mut packed, padding, &self.config.geometry)?;
        let mut run = self
            .prover
            .start(&self.program, state_words(&initial_state()))?;
        let segment_rows = self.config.geometry.n * self.batch.size;
        for segment in packed.rows.chunks(segment_rows) {
            run.segment(&step_inputs(&self.batch, segment)?)?;
        }
        Ok(TransactionProof {
            proof: run.finish()?,
            final_state,
        })
    }

    pub fn verify(
        &self,
        proof: &TransactionProof,
        statement: &TransactionStatement,
        roots: &crate::TraceCommitments,
    ) -> Result<(), ProvingError> {
        let expected = crate::transaction_commitment(statement, roots)?.map(F::new);
        check_final_state(
            &proof.final_state,
            TerminalClaim::Transaction {
                commitment: expected,
            },
        )?;
        self.verifier.verify(
            &self.program,
            &state_words(&initial_state()),
            &state_words(&proof.final_state),
            &proof.proof,
        )?;
        Ok(())
    }
}

/// The backend's input of each step: its rows in the step layout.
fn step_inputs(batch: &Batch, rows: &[Vec<F>]) -> Result<Vec<Vec<F>>, ProvingError> {
    let width = batch.relation.columns().column_count();
    rows.chunks(batch.size)
        .map(|step| {
            let step: Vec<&[F]> = step.iter().map(Vec::as_slice).collect();
            Ok(nebula::SharedColumns::NONE.step_witness(width, COL_ONE, &step, |_| 0)?)
        })
        .collect()
}

/// Fill the final segment with padding rows, preserving the terminal state.
fn pad_to_segments(
    batch: &Batch,
    packed: &mut PackedWitness,
    padding: Wit,
    geometry: &nebula::Geometry,
) -> Result<(), ProvingError> {
    if packed.rows.is_empty() {
        return Err(ProvingError::EmptyTrace);
    }
    let segment_rows = geometry.n * batch.size;
    let segments = packed.rows.len().div_ceil(segment_rows);
    if segments > geometry.s_max {
        return Err(ProvingError::TraceTooLong {
            segments,
            max: geometry.s_max,
        });
    }
    if !packed.rows.len().is_multiple_of(segment_rows) {
        let pad = crate::witness::build_witness_vector(&padding);
        packed.rows.resize(segments * segment_rows, pad);
        packed.origins.resize(packed.rows.len(), None);
    }
    Ok(())
}

fn state_words(state: &[F]) -> Vec<u64> {
    state.iter().map(PrimeField64::as_canonical_u64).collect()
}

// Verifier-owned initial state, independent of the witness being proved.
// An added carried column must acquire an explicit initialization here.
fn initial_state() -> Vec<F> {
    build_ivc_state_continuity_links()
        .iter()
        .flat_map(|group| &group.links)
        .map(|link| match link.next_step_column {
            // The transaction context has the turn, with an empty call stack.
            COL_CURR_BEFORE => crate::ivc_state::CoroutineId::Coord(0).field(),
            COL_CURR_PHASE_BEFORE => F::from_u8(crate::ivc_state::CurrPhase::Executing.value()),
            COL_LAST_INPUT_HAS_ABI_BEFORE => F::ONE,
            // Coord(0) is the transaction context: scripts start at Coord(1).
            COL_NEXT_COORD_ID_BEFORE | COL_COORD_CURSOR_BEFORE => F::ONE,
            COL_CALL_SP_BEFORE
            | COL_INSTANCE_DEPTH_BEFORE
            | COL_EFFECT_HEAD_BEFORE
            | COL_EFFECT_DEPTH_BEFORE
            | COL_EFFECT_ARENA_LEN_BEFORE
            | COL_NEXT_UTXO_ID_BEFORE
            | COL_TX_PHASE_BEFORE
            | COL_ABI_READ_REMAINING_BEFORE
            | COL_ABI_READ_ORDINAL_BEFORE
            | COL_OUTPUT_CURSOR_BEFORE
            | COL_ENABLED_METHOD_LOG_LEN_BEFORE => F::ZERO,
            column if COL_IO_BEFORE.contains(&column) => F::ZERO,
            column
                if COL_PENDING_ROOT_BEFORE.contains(&column)
                    || COL_PENDING_METHOD_BEFORE.contains(&column) =>
            {
                F::ZERO
            }
            column => panic!("missing canonical initial value for carried column {column}"),
        })
        .collect()
}

fn final_state(batch: &Batch, packed: &PackedWitness) -> Vec<F> {
    let last = packed.rows.last().expect("nonempty execution");
    batch
        .continuity
        .links()
        .map(|link| last[link.previous_step_column])
        .collect()
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum FinalClaimError {
    #[error("final-state length is {actual}, expected {expected}")]
    LengthMismatch { expected: usize, actual: usize },
    #[error(transparent)]
    Terminal(#[from] Unsatisfied),
}

fn check_final_state(
    final_state: &[F],
    expected_terminal: TerminalClaim,
) -> Result<(), FinalClaimError> {
    let groups = build_ivc_state_continuity_links();
    let links = groups
        .iter()
        .flat_map(|group| &group.links)
        .collect::<Vec<_>>();
    if final_state.len() != links.len() {
        return Err(FinalClaimError::LengthMismatch {
            expected: links.len(),
            actual: final_state.len(),
        });
    }
    expected_terminal.check(|column| {
        let index = links
            .iter()
            .position(|link| link.previous_step_column == column)
            .expect("terminal columns are carried");
        final_state[index]
    })?;
    Ok(())
}

#[cfg(test)]
mod tests;
