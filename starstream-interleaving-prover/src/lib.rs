mod batch;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

pub use batch::proving;
mod ccs;
mod commitment;
mod transaction_commitment;
pub use transaction_commitment::transaction_commitment;
mod ivc_state;
pub use ivc_state::CoroutineId;
mod memory;
mod opcode;
mod step;
mod terminal;
mod transaction;
mod witness;

pub use transaction::verify_transaction_sat;

use neo_application::{ContinuityCheckError, MemoryCheckError};
use neo_math::F;
use starstream_interleaving_spec::Trace;

use crate::memory::MemoryId;

pub use batch::verify_sat_batched;
pub use batch::verify_sat_with_commitments;

/// Final outer event roots, keyed by packed coroutine INSTANCE identity:
/// coordinator i = 2*i, UTXO i = 2*i+1. Different instances of the same program
/// have separate chains. Contains exactly the instances that emitted events.
pub type TraceCommitments = BTreeMap<u32, [u64; 4]>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("batch size must be positive and its dimensions must fit usize")]
    InvalidBatchSize,
    #[error(transparent)]
    MemoryCatalogError(#[from] neo_application::MemoryCatalogError),
    #[error(transparent)]
    Unsatisfied(#[from] Unsatisfied),
    #[error(transparent)]
    R1csBuildError(#[from] neo_application::R1csBuildError),
    #[error(transparent)]
    ApplicationRelationError(#[from] neo_application::ApplicationRelationError),
    #[error(transparent)]
    ColumnRegistryError(#[from] neo_application::ColumnRegistryError),
    #[error(transparent)]
    ContinuityCatalogError(#[from] neo_application::ContinuityCatalogError),
    #[error(transparent)]
    ContinuityCheckError(ContinuityCheckError),
    /// `slot` counts witness rows, padding included.
    #[error("failed to check the CCS assignment of slot {slot}: {source}")]
    CcsCheck {
        slot: usize,
        #[source]
        source: neo_ccs::CcsError,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Unsatisfied {
    #[error("transaction inputs, outputs or boundary state do not match the statement")]
    TransactionStatement,
    #[error("transaction commitment does not match the statement")]
    TransactionCommitment,
    #[error("per-instance trace commitments do not match the statement")]
    TraceCommitments,
    #[error("constraint {constraint:?} failed at relation row {row} for step {step}")]
    Constraint {
        /// Index in the original trace, never an index of a padding slot.
        step: usize,
        /// Constraint row in the relation used by this invocation.
        row: usize,
        constraint: String,
    },
    #[error(
        "constraint {constraint:?} failed in padding at batch {batch}, slot {slot} (size {batch_size}), relation row {row}"
    )]
    PaddingConstraint {
        batch: usize,
        slot: usize,
        batch_size: usize,
        row: usize,
        constraint: String,
    },
    /// The source boundary counts witness rows, padding included.
    #[error(transparent)]
    Continuity(ContinuityCheckError),
    /// The source row counts witness rows, padding included. It is the trace
    /// step unless padding precedes it.
    #[error(transparent)]
    Memory(#[from] MemoryCheckError<MemoryId>),
    #[error("terminal call-stack pointer must be zero, got {actual:?}")]
    TerminalCallStackNotEmpty { actual: F },
    #[error("terminal invocation depth must be zero, got {actual:?}")]
    TerminalInstanceStackNotEmpty { actual: F },
    #[error("terminal coroutine must be a coordinator, got packed id {actual:?}")]
    TerminalCoroutineNotCoordinator { actual: F },
}

/// Build the circuit witness for `trace` and check that it satisfies every
/// relation currently implemented by the prover. Signer requirements are checked
/// against the supplied transaction signer IDs.
///
/// Host-side validation for diagnostics and tests; does not construct a proof.
pub fn verify_sat(trace: &Trace, signers: &BTreeSet<u64>) -> Result<(), Error> {
    verify_sat_batched(trace, 1, signers)
}

#[cfg(test)]
fn build_witness_rows(trace: &Trace) -> (Vec<Vec<F>>, neo_application::MemoryPreload<MemoryId>) {
    let normalized = step::normalize(trace);
    let preload = memory::preload_tables(&normalized.method_table);
    let rows = normalized
        .steps
        .iter()
        .map(witness::build_witness_vector)
        .collect();

    (rows, preload)
}

#[cfg(test)]
fn verify_witness_rows(
    rows: &[Vec<F>],
    preload: &neo_application::MemoryPreload<MemoryId>,
) -> Result<(), Error> {
    batch::check_single_rows(rows, preload)
}

#[cfg(test)]
mod tests;
