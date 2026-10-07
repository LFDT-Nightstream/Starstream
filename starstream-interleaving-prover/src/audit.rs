//! Full-history audit compatibility adapter; not recursive NIFS verification.
//!
//! TODO(upstream): Replace this with the new proving frontend when it lands.

use neo_fold_clean::{
    frontends::{
        f_prime::{
            image::{NifsCeClaimShape, NifsPayloadShape},
            recursive_plan::{AccumulatorPlanOptions, RecursiveStepImagePlan},
        },
        r1cs_f_prime::{
            self, R1csChainBuilder, R1csFPrimeDerivedStructure, R1csFPrimePreprocessing, SparseR1cs,
        },
    },
    lifecycle::UncompressedAudit,
    paper::params::Params,
};
use neo_math::F;

#[derive(Debug, thiserror::Error)]
pub enum AuditError {
    #[error(transparent)]
    Frontend(#[from] Box<r1cs_f_prime::Error>),
    #[error("audit image shape did not converge")]
    ShapeDidNotConverge,
}

impl From<r1cs_f_prime::Error> for AuditError {
    fn from(error: r1cs_f_prime::Error) -> Self {
        Self::Frontend(Box::new(error))
    }
}

/// Size the source-image claim like neo-wasm's canonical audit preprocessing.
pub(crate) fn derive(
    relation: &SparseR1cs,
    mut plan: RecursiveStepImagePlan,
    params: &Params,
) -> Result<R1csFPrimeDerivedStructure, AuditError> {
    let c_data_entries = params.kappa() as usize * neo_math::D;
    let mut r_len = 8;
    let mut s_col_len = 8;
    for _ in 0..8 {
        plan.nifs_payload_shapes = vec![NifsPayloadShape::CeClaim(NifsCeClaimShape {
            c_data_entries,
            x_rows: neo_math::D,
            // Fixed Goldilocks F' claim layout, shared with neo-wasm.
            x_active_cols: 5,
            r_len,
            y_ring_inner_lens: vec![64; 8],
            y_zcol_len: 64,
            s_col_len,
        })];
        plan.accumulator = Some(AccumulatorPlanOptions {
            ce_claim_payload_index: 0,
            c_data_entries,
            child_count: u64::from(params.k_rho()),
            unified: true,
        });
        let derived = r1cs_f_prime::derive_sparse_preprocessing_structure(relation, &plan)?;
        let shape = &derived.structure().ccs;
        let required_r = shape.n.max(2).next_power_of_two().ilog2() as usize;
        let required_s = shape.m.max(2).next_power_of_two().ilog2() as usize;
        if (required_r, required_s) == (r_len, s_col_len) {
            return Ok(derived);
        }
        (r_len, s_col_len) = (required_r, required_s);
    }
    Err(AuditError::ShapeDidNotConverge)
}

pub fn prove(
    prep: &R1csFPrimePreprocessing,
    assignments: impl IntoIterator<Item = Vec<F>>,
) -> Result<UncompressedAudit, AuditError> {
    let mut chain = R1csChainBuilder::new(prep)?;
    for assignment in assignments {
        chain.append_assignment(assignment)?;
    }
    Ok(chain.finish_with_audit()?)
}
