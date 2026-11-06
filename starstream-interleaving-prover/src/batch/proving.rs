//! Transaction-bound Nebula proofs of the interleaving relation and its memory.
//!
//! One Nebula step proves `batch_size` consecutive rows of the one-row relation,
//! and every memory of the catalog is checked in the circuit. The verifier reads
//! the method table from the proof, so it only binds the table to `D_init`.

use super::*;
use crate::{
    memory::{MemoryId, preload_tables},
    terminal::TerminalClaim,
};
use core::num::NonZeroUsize;
use neo_application::MemorySpec;
pub use nightstream::Error as NightstreamError;
pub use nightstream::nebula::{
    CatalogBindingError, Proof, ProveError, R1csApplicationError, SegmentError, VerifyError,
};
use nightstream::{
    Engine,
    nebula::{
        Circuit, Geometry, Program, Prover, R1csApplication, RowRelation, Run, Size, Verifier,
        batched_slots, pack_regions,
    },
};
use p3_field::PrimeField64;
use starstream_interleaving_spec::{MethodHash, TransactionStatement};

const ROM_BITS: usize = 9;
const RAM_BITS: usize = 13;
const SEGMENT_STEPS: usize = 32;
const MAX_SEGMENTS: usize = 64;

const CALL_DEPTH_BITS: usize = 5;
const COROUTINE_BITS: usize = 6;
const EFFECT_SCOPE_BITS: usize = 5;
const ENABLED_METHOD_LOG_BITS: usize = 6;
const HANDLE_BITS: usize = 4;
const INSTANCE_DEPTH_BITS: usize = 5;
const METHOD_BITS: usize = 6;
const WORD_STRIDE_BITS: usize = 3;

#[derive(Debug, thiserror::Error)]
pub enum ProvingError {
    #[error(transparent)]
    Circuit(#[from] crate::Error),
    #[error("the batch size does not fit the proof geometry")]
    Geometry,
    #[error(transparent)]
    Application(#[from] R1csApplicationError),
    #[error(transparent)]
    Program(#[from] CatalogBindingError),
    #[error(transparent)]
    Nightstream(#[from] NightstreamError),
    #[error(transparent)]
    Segment(#[from] SegmentError),
    #[error(transparent)]
    Prove(#[from] ProveError),
    #[error(transparent)]
    Verify(#[from] VerifyError),
    #[error(transparent)]
    FinalClaim(#[from] FinalClaimError),
}

/// A memory proof and the claim that it is verified against.
#[derive(Debug)]
pub struct TransactionProof {
    pub proof: Proof,
    pub method_table: Vec<MethodHash>,
    // TODO(privacy): Publish only the transaction digest/terminal projection,
    // not the whole carried state (which exposes activity counters).
    pub final_state: Vec<u64>,
}

/// The circuit of a fixed batch size, compiled once by each party.
pub struct TransactionProofContext {
    batch: Batch,
    application: R1csApplication<MemoryId>,
    prover: Prover<R1csApplication<MemoryId>>,
    verifier: Verifier<R1csApplication<MemoryId>>,
}

impl TransactionProofContext {
    /// Both parties must agree on the batch size. `minimum_security_bits`
    /// bounds the folding profile and the memory fingerprint term.
    pub fn new(batch_size: usize, minimum_security_bits: u32) -> Result<Self, ProvingError> {
        let batch = Batch::new(batch_size)?;
        let (geometry, application) = application(batch_size)?;
        let circuit = Circuit::compile(geometry, application.clone())?;
        let prover = circuit.prover(Engine::Optimized, minimum_security_bits)?;
        let verifier = circuit.verifier(Engine::Optimized, minimum_security_bits)?;
        Ok(Self {
            batch,
            application,
            prover,
            verifier,
        })
    }

    /// Checks the statement on the host, then proves the relation and its
    /// memory accesses.
    pub fn prove(
        &self,
        trace: &Trace,
        statement: &TransactionStatement,
        roots: &crate::TraceCommitments,
    ) -> Result<TransactionProof, ProvingError> {
        let normalized = normalize(trace);
        let method_table = normalized.method_table.clone();
        let inputs = step_inputs(self.batch.size, &normalized.steps);
        super::check_normalized(&self.batch, normalized, Some(roots), Some(statement))?;
        let program = self.program(&method_table)?;
        let mut run = self.prover.start(&program, initial_state())?;
        for segment in inputs.chunks(SEGMENT_STEPS) {
            run.segment(segment)?;
        }
        let final_state = run.state().clone();
        let proof = run.finish()?;
        Ok(TransactionProof {
            proof,
            method_table,
            final_state,
        })
    }

    /// Authenticates the statement/instance-root digest and terminal state.
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
        let program = self.program(&proof.method_table)?;
        self.verifier
            .verify(&program, &initial_state(), &proof.final_state, &proof.proof)?;
        Ok(())
    }

    fn program(&self, method_table: &[MethodHash]) -> Result<Program, ProvingError> {
        let preload = preload_tables(method_table);
        let program = self.application.program(&preload)?;
        Ok(program)
    }
}

/// Runs `trace` through the memory application without proving it, and returns
/// its final state. It fails on a trace that does not fit the proof's memories.
pub fn check_run(trace: &Trace, batch_size: usize) -> Result<Vec<u64>, ProvingError> {
    let normalized = normalize(trace);
    let (.., application) = application(batch_size)?;
    let program = application.program(&preload_tables(&normalized.method_table))?;
    let mut run = Run::new(application, &program, initial_state());
    for segment in step_inputs(batch_size, &normalized.steps).chunks(SEGMENT_STEPS) {
        run.segment(segment)?;
    }
    Ok(run.state().clone())
}

fn application(batch_size: usize) -> Result<(Geometry, R1csApplication<MemoryId>), ProvingError> {
    let rows = NonZeroUsize::new(batch_size).ok_or(crate::Error::InvalidBatchSize)?;
    let relation = crate::ccs::build_relation()?;
    let continuity = ContinuityCatalog::new(build_ivc_state_continuity_links(), relation.columns())
        .map_err(crate::Error::from)?;
    let catalog = build_memory_layout();
    let slots = batched_slots(&catalog, &port_slots(catalog.entries()), rows);
    let geometry = Geometry::new(ROM_BITS, RAM_BITS, slots.len(), SEGMENT_STEPS, MAX_SEGMENTS)
        .ok_or(ProvingError::Geometry)?;
    let (regions, ..) = pack_regions(
        catalog
            .entries()
            .iter()
            .map(|memory| (memory.id, memory.kind, region_widths(memory.id))),
    );
    let application = R1csApplication::new(
        RowRelation::of(relation.r1cs())?,
        &continuity,
        &catalog,
        Size {
            geometry,
            rows,
            regions,
            slots,
        },
    )?;
    Ok((geometry, application))
}

/// One slot for each port, in catalog order, so dependent ports stay in order.
fn port_slots(memories: &[MemorySpec<MemoryId>]) -> Vec<Vec<usize>> {
    let ports = memories.iter().map(|memory| memory.ports.len()).sum();
    (0..ports).map(|port| vec![port]).collect()
}

/// The bit width of each address part of `id`, least significant part first.
fn region_widths(id: MemoryId) -> Vec<usize> {
    use MemoryId::*;
    match id {
        CallStackCaller
        | CallStackExpectedArgument
        | CallStackExpectedResult
        | CallStackExpectedMethod
        | CallStackIsEffect
        | CallStackConstructor
        | CallStackSavedPhase
        | CallStackSavedEffectHead
        | CallStackSavedEffectDepth => vec![CALL_DEPTH_BITS + WORD_STRIDE_BITS],
        InstanceStack => vec![INSTANCE_DEPTH_BITS],
        EffectScopeParent | EffectScopeOwner | EffectScopeFrameId => vec![EFFECT_SCOPE_BITS],
        EffectRegistered => vec![EFFECT_SCOPE_BITS, METHOD_BITS],
        AbiMethodCount | AbiGeneration => vec![COROUTINE_BITS],
        EnabledMethodLogUtxo
        | EnabledMethodLogMethod
        | EnabledMethodLogGeneration
        | EnabledMethodLogOrdinal => vec![ENABLED_METHOD_LOG_BITS],
        MethodTable => vec![METHOD_BITS + WORD_STRIDE_BITS],
        ResourceResolver => vec![COROUTINE_BITS, HANDLE_BITS],
        TraceCommitments => vec![COROUTINE_BITS + WORD_STRIDE_BITS],
    }
}

/// The step inputs of whole segments: `rows` rows each, in row order, then
/// state-preserving padding. Every row uses the constant of the first row.
fn step_inputs(rows: usize, steps: &[Wit]) -> Vec<Vec<F>> {
    let padding = steps.last().expect("nonempty execution").padding_after();
    let padded = steps.len().next_multiple_of(rows * SEGMENT_STEPS);
    let witnesses = steps
        .iter()
        .chain(core::iter::repeat_n(&padding, padded - steps.len()))
        .map(build_witness_vector)
        .collect::<Vec<_>>();
    witnesses
        .chunks(rows)
        .map(|chunk| {
            chunk
                .iter()
                .enumerate()
                .flat_map(|(row, witness)| {
                    witness.iter().enumerate().map(move |(column, value)| {
                        if column == COL_ONE && row != 0 {
                            F::ZERO
                        } else {
                            *value
                        }
                    })
                })
                .collect()
        })
        .collect()
}

// Verifier-owned initial state, independent of the witness being proved.
// An added carried column must acquire an explicit initialization here.
fn initial_state() -> Vec<u64> {
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
        .map(|value| value.as_canonical_u64())
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
    final_state: &[u64],
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
        F::new(final_state[index])
    })?;
    Ok(())
}

#[cfg(test)]
mod tests;
