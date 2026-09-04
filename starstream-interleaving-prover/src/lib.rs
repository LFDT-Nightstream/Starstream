mod ccs;
mod ivc_state;
mod memory;
mod opcode;
mod step;
mod witness;

use neo_application::{
    ContinuityCatalog, ContinuityCheckError, MemoryCheckError, check_continuity_rows,
    check_memory_rows,
};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::Trace;

use crate::{
    ccs::{PUBLIC_INPUTS, build_relation},
    ivc_state::build_ivc_state_continuity_links,
    memory::MemoryId,
    witness::build_witness_vector,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
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
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Unsatisfied {
    #[error("constraint {constraint:?} failed at relation row {row} for step {step}")]
    Constraint {
        step: usize,
        row: usize,
        constraint: &'static str,
    },
    #[error(transparent)]
    Continuity(ContinuityCheckError),
    #[error(transparent)]
    Memory(#[from] MemoryCheckError<MemoryId>),
    #[error("terminal call-stack pointer must be zero, got {actual:?}")]
    TerminalCallStackNotEmpty { actual: F },
}

/// Build the circuit witness for `trace` and check that it satisfies every
/// relation currently implemented by the prover.
///
/// This is the pre-proof-system validation surface. Once proof construction is
/// wired in, it should remain useful for diagnostics and tests.
pub fn verify_sat(trace: &Trace) -> Result<(), Error> {
    let rows = build_witness_rows(trace);
    verify_witness_rows(&rows)?;
    verify_execution_statement(&rows)
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

    // TODO: Bind the canonical initial carried state and this terminal
    // condition into the proof statement once proof construction is wired.
    // Quint's full `execution_complete` predicate also requires the terminal
    // coroutine to be a coordinator; add that check when `curr` is populated.

    Ok(())
}

fn build_witness_rows(trace: &Trace) -> Vec<Vec<F>> {
    step::normalize(trace)
        .iter()
        .map(build_witness_vector)
        .collect()
}

fn verify_witness_rows(rows: &[Vec<F>]) -> Result<(), Error> {
    let relation = build_relation()?;
    let memory = crate::memory::build_memory_layout();
    let continuity =
        ContinuityCatalog::new(build_ivc_state_continuity_links(), relation.columns())?;

    for (step, row_assignment) in rows.iter().enumerate() {
        match neo_ccs::check_ccs_rowwise_zero(
            relation.r1cs().structure(),
            &row_assignment[0..PUBLIC_INPUTS],
            &row_assignment[PUBLIC_INPUTS..],
        ) {
            Ok(()) => {}
            Err(neo_ccs::CcsError::RowFail { row }) => {
                return Err(Unsatisfied::Constraint {
                    step,
                    row,
                    constraint: relation.r1cs().catalog().rows()[row].tag().label(),
                }
                .into());
            }
            Err(source) => return Err(Error::CcsCheck { step, source }),
        }
    }

    let policy = crate::memory::sanity_checking_policy(&memory);
    let preload = crate::memory::preload_tables();

    check_memory_rows(&memory, relation.columns(), rows, &preload, &policy)
        .map_err(Unsatisfied::Memory)?;

    match check_continuity_rows(&continuity, rows) {
        Ok(()) => {}
        Err(source @ ContinuityCheckError::Mismatch { .. }) => {
            return Err(Unsatisfied::Continuity(source).into());
        }
        Err(source) => return Err(Error::ContinuityCheckError(source)),
    }

    // TODO: The eventual proof/batching path must enforce the continuity links
    // as constraints rather than relying on this diagnostic check.

    Ok(())
}

#[cfg(test)]
mod tests;
