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
}

/// Build the circuit witness for `trace` and check that it satisfies every
/// relation currently implemented by the prover.
///
/// This is the pre-proof-system validation surface. Once proof construction is
/// wired in, it should remain useful for diagnostics and tests.
pub fn verify_sat(trace: &Trace) -> Result<(), Error> {
    let rows = build_witness_rows(trace);
    verify_witness_rows(&rows)
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

    // TODO: Constrain and check the initial and terminal carried state once the
    // state witness is populated. Quint's replay already checks
    // `execution_complete`, including that the call stack is empty.

    Ok(())
}

#[cfg(test)]
mod tests;
