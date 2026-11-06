//! Transaction-bound Nebula proofs. Host checks remain the fast diagnostic path.

use super::*;
pub use crate::memory::MemoryId;
use crate::terminal::TerminalClaim;
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
        let (regions, [rom_cells, ram_cells]) = nebula::pack_regions(memories);
        let ports = batch
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
        let segments = 4096usize
            .checked_div(segment_batches)
            .filter(|n| *n > 0)
            .ok_or(ProvingError::Configuration(
                "invalid benchmark segment length",
            ))?;
        let geometry = nebula::Geometry::new(r, mu, ports, segment_batches, segments).ok_or(
            ProvingError::Configuration("invalid benchmark memory geometry"),
        )?;
        Ok(Self {
            batch_size,
            geometry,
            regions,
            slots: (0..ports).map(|rank| vec![rank]).collect(),
            engine: Engine::Optimized,
            minimum_security_bits: 109,
        })
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
            }));
        // One slot per port preserves the catalog's access order, including
        // writes followed by reads in the same execution row.
        let ports: usize = batch.memory.entries().iter().map(|m| m.ports.len()).sum();
        let slots = (0..ports).map(|rank| vec![rank]).collect();
        let r = rom_cells.max(2).next_power_of_two().ilog2() as usize;
        let mu = ram_cells.max(rom_cells).max(2).next_power_of_two().ilog2() as usize;
        let geometry = nebula::Geometry::new(r, mu, ports, 8, 512).ok_or(
            ProvingError::Configuration("memory geometry exceeds backend limits"),
        )?;
        Ok(Self {
            batch_size,
            geometry,
            regions,
            slots,
            engine: Engine::Optimized,
            minimum_security_bits: 109,
        })
    }
}

#[derive(Debug)]
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
            // Batch already packs the execution rows into each assignment.
            rows: NonZeroUsize::MIN,
            regions: config.regions.clone(),
            slots: config.slots.clone(),
        },
    )?)
}

/// Dictionary used by normalization. A verifier must agree on this table
/// independently; deriving it from the trace is suitable for local tests.
pub fn method_dictionary(trace: &Trace) -> Vec<MethodHash> {
    normalize(trace).method_table
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
        let n = self.config.geometry.n;
        pad_to_segments(&self.batch, &mut packed, padding, &self.config.geometry)?;
        let mut run = self
            .prover
            .start(&self.program, state_words(&initial_state()))?;
        for segment in packed.rows.chunks(n) {
            run.segment(segment)?;
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

/// Fill the final segment with inactive batches, preserving the terminal state.
fn pad_to_segments(
    batch: &Batch,
    packed: &mut PackedWitness,
    padding: Wit,
    geometry: &nebula::Geometry,
) -> Result<(), ProvingError> {
    if packed.rows.is_empty() {
        return Err(ProvingError::EmptyTrace);
    }
    let segments = packed.rows.len().div_ceil(geometry.n);
    if segments > geometry.s_max {
        return Err(ProvingError::TraceTooLong {
            segments,
            max: geometry.s_max,
        });
    }
    if !packed.rows.len().is_multiple_of(geometry.n) {
        let pad = batch
            .pack(&[padding])
            .rows
            .pop()
            .expect("one padding batch");
        while !packed.rows.len().is_multiple_of(geometry.n) {
            packed.rows.push(pad.clone());
        }
        packed.origins.resize(packed.rows.len() * batch.size, None);
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
            COL_CURR_BEFORE => crate::ivc_state::CoroutineId::Coord(1).field(),
            COL_CURR_PHASE_BEFORE => F::from_u8(crate::ivc_state::CurrPhase::Executing.value()),
            COL_CALL_SP_BEFORE | COL_LAST_INPUT_HAS_ABI_BEFORE => F::ONE,
            COL_INSTANCE_DEPTH_BEFORE
            | COL_EFFECT_HEAD_BEFORE
            | COL_EFFECT_DEPTH_BEFORE
            | COL_EFFECT_ARENA_LEN_BEFORE
            | COL_NEXT_UTXO_ID_BEFORE
            | COL_TX_PHASE_BEFORE
            | COL_COORD_FINALIZED_BEFORE
            | COL_ABI_READ_REMAINING_BEFORE
            | COL_ABI_READ_ORDINAL_BEFORE
            | COL_OUTPUT_CURSOR_BEFORE
            | COL_ENABLED_METHOD_LOG_LEN_BEFORE => F::ZERO,
            column if COL_IO_BEFORE.contains(&column) => F::ZERO,
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
