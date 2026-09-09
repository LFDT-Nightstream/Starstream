mod batch;
mod ccs;
mod ivc_state;
mod memory;
mod opcode;
mod step;
mod witness;

use neo_application::{ContinuityCheckError, MemoryCheckError};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::Trace;

use crate::memory::MemoryId;

pub use batch::verify_sat_batched;

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
    #[error("failed to check the CCS assignment for step {step}: {source}")]
    CcsCheck {
        step: usize,
        #[source]
        source: neo_ccs::CcsError,
    },
    #[error("failed to check CCS batch {batch} (size {batch_size}): {source}")]
    BatchedCcsCheck {
        batch: usize,
        batch_size: usize,
        #[source]
        source: neo_ccs::CcsError,
    },
    #[error("continuity checker failed in batch coordinates (size {batch_size}): {source}")]
    BatchedContinuityCheck {
        batch_size: usize,
        #[source]
        source: ContinuityCheckError,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Unsatisfied {
    #[error("constraint {constraint:?} failed at relation row {row} for step {step}")]
    Constraint {
        /// Index in the original trace, never an index of a padding slot.
        step: usize,
        /// Constraint row in the relation used by this invocation.
        row: usize,
        constraint: &'static str,
    },
    #[error(
        "constraint {constraint:?} failed in padding at batch {batch}, slot {slot} (size {batch_size}), relation row {row}"
    )]
    PaddingConstraint {
        batch: usize,
        slot: usize,
        batch_size: usize,
        row: usize,
        constraint: &'static str,
    },
    /// Single-step coordinates: source boundary and columns refer to the
    /// unbatched witness.
    #[error(transparent)]
    Continuity(ContinuityCheckError),
    /// Single-step coordinates: source row is a trace step.
    #[error(transparent)]
    Memory(#[from] MemoryCheckError<MemoryId>),
    /// Source row is a batch index; source columns use the batched layout.
    #[error("memory failure in batch coordinates (size {batch_size}): {source}")]
    BatchedMemory {
        batch_size: usize,
        #[source]
        source: MemoryCheckError<MemoryId>,
    },
    /// Source boundary is between batches, not between individual steps.
    #[error("continuity failure in batch coordinates (size {batch_size}): {source}")]
    BatchedContinuity {
        batch_size: usize,
        #[source]
        source: ContinuityCheckError,
    },
    #[error("terminal call-stack pointer must be zero, got {actual:?}")]
    TerminalCallStackNotEmpty { actual: F },
    #[error("terminal coroutine must be a coordinator, got packed id {actual:?}")]
    TerminalCoroutineNotCoordinator { actual: F },
}

/// Build the circuit witness for `trace` and check that it satisfies every
/// relation currently implemented by the prover.
///
/// This is the pre-proof-system validation surface. Once proof construction is
/// wired in, it should remain useful for diagnostics and tests.
pub fn verify_sat(trace: &Trace) -> Result<(), Error> {
    verify_sat_batched(trace, 1)
}

fn verify_execution_statement(rows: &[Vec<F>]) -> Result<(), Error> {
    // An empty trace leaves Quint's initial coordinator frame on the stack.
    let terminal_call_sp = rows
        .last()
        .map_or(F::ONE, |row| row[crate::ccs::layout::COL_CALL_SP_AFTER]);

    if terminal_call_sp != F::ZERO {
        return Err(Unsatisfied::TerminalCallStackNotEmpty {
            actual: terminal_call_sp,
        }
        .into());
    }

    let terminal_curr = rows
        .last()
        .map_or(F::new(2), |row| row[crate::ccs::layout::COL_CURR_AFTER]);
    let terminal_curr_tag = rows.last().map_or(F::ZERO, |row| {
        let tag_column = crate::ccs::layout::range_check_layout()
            .bit_columns_for(crate::ccs::layout::COL_CURR_AFTER)
            .expect("the packed coroutine ID has decomposition bits")
            .start;
        row[tag_column]
    });

    if terminal_curr_tag != F::ZERO {
        return Err(Unsatisfied::TerminalCoroutineNotCoordinator {
            actual: terminal_curr,
        }
        .into());
    }

    // TODO: Bind the canonical initial carried state and this terminal
    // condition into the proof statement once proof construction is wired.

    Ok(())
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
