//! Experimental transaction-bound, relation-only R1CS-F' proofs.
//!
//! TODO: Prove RAM/ROM accesses and bind their initialization. For now memory
//! consistency is only checked on the host; the proof authenticates the local
//! relation and carried-state continuity, NOT full interleaving semantics.

use super::*;
use crate::terminal::TerminalClaim;
pub use neo_ajtai::AjtaiError;
use neo_application::range_checked_variable_widths;
use neo_fold_clean::{
    engine::ccs_native::poseidon2::POSEIDON2_GOLDILOCKS_BITS,
    frontends::{
        f_prime::{
            image::FPrimeImageLayout,
            recursive_plan::{
                RecursiveStepImagePlan, StateXOutPlanOptions, build_recursive_step_image_config,
                build_semantic_state_preimage_fields,
            },
        },
        r1cs_f_prime::{
            SparseR1cs,
            ivc::{R1csIvc, R1csIvcPreprocessing, R1csIvcRelation},
        },
    },
    lifecycle::verify_uncompressed,
    paper::{
        digest::digest_fields_as_digest32,
        f_prime::{
            poseidon_trace::encode_poseidon_trace,
            ring_action_trace::{LowNormEncoding, RingActionTraceLayout},
        },
    },
};
pub use neo_fold_clean::{
    frontends::r1cs_f_prime::ivc::R1csIvcError,
    lifecycle::{Error as LifecycleError, Uncompressed},
    paper::params::Params,
};
use starstream_interleaving_spec::TransactionStatement;

#[derive(Debug, thiserror::Error)]
pub enum ProvingError {
    #[error(transparent)]
    Circuit(#[from] crate::Error),
    #[error(transparent)]
    Ivc(#[from] Box<R1csIvcError>),
    #[error(transparent)]
    Verification(#[from] LifecycleError),
    #[error(transparent)]
    Setup(#[from] AjtaiError),
    #[error(transparent)]
    FinalClaim(#[from] FinalClaimError),
}

impl From<R1csIvcError> for ProvingError {
    fn from(error: R1csIvcError) -> Self {
        Self::Ivc(Box::new(error))
    }
}

/// Uncompressed relation proof and its authenticated final carried state.
/// This does not prove memory consistency; see the module-level TODO.
#[derive(Clone, Debug)]
pub struct TransactionProof {
    pub proof: Uncompressed,
    // TODO(privacy): Publish only the transaction digest/terminal projection,
    // not the whole carried state (which exposes activity counters).
    pub final_state: Vec<F>,
}

/// Reusable preprocessing for a fixed batch size and parameter set.
/// The verifier must use its own trusted context, not prover-supplied setup.
pub struct TransactionProofContext {
    batch: Batch,
    preprocessing: R1csIvcPreprocessing,
}

impl TransactionProofContext {
    /// Both parties must agree on the parameters, batch size and setup seed.
    /// The seed is verifier-owned configuration, never taken from the proof.
    /// Weak test parameters are supported for demos; they are not secure.
    pub fn new(
        batch_size: usize,
        params: Params,
        setup_seed: [u8; 32],
    ) -> Result<Self, ProvingError> {
        let batch = Batch::new(batch_size)?;
        let relation = sparse_relation(&batch);
        let plan = recursive_plan(&batch, &relation);
        // The recursive relation (not the application) determines the PP width.
        // TODO(upstream): Expose preprocessing from a compiled relation so we
        // don't compile it twice just to install the verifier-owned setup.
        let shape = relation.clone().into();
        let compiled = R1csIvcRelation::compile_fixed_point(&params, &shape, &plan)?;
        register_setup(
            &params,
            compiled.structure().m.div_ceil(neo_math::D),
            setup_seed,
        )?;
        drop(compiled);
        let preprocessing = R1csIvcPreprocessing::new(params, relation, plan)?;
        Ok(Self {
            batch,
            preprocessing,
        })
    }

    /// Checks RAM/ROM and statement consistency on the host, then proves the
    /// local relation and carried continuity. No memory argument is included.
    pub fn prove(
        &self,
        trace: &Trace,
        statement: &TransactionStatement,
        roots: &crate::TraceCommitments,
    ) -> Result<TransactionProof, ProvingError> {
        let normalized = normalize(trace);
        let packed =
            super::check_normalized(&self.batch, normalized, Some(roots), Some(statement))?;
        let final_state = final_state(&self.batch, &packed);
        let mut chain = R1csIvc::new(&self.preprocessing);
        for row in packed.rows {
            chain.extend(row)?;
        }
        Ok(TransactionProof {
            proof: chain.finish()?,
            final_state,
        })
    }

    /// Authenticates the statement/instance-root digest and terminal state.
    /// Does not replay the trace or establish the host-only memory checks.
    pub fn verify(
        &self,
        proof: &TransactionProof,
        statement: &TransactionStatement,
        roots: &crate::TraceCommitments,
    ) -> Result<(), ProvingError> {
        let expected = crate::transaction_commitment(statement, roots)?.map(F::new);
        check_final_state(
            proof.proof.state.semantic_state_digest,
            &proof.final_state,
            TerminalClaim::Transaction {
                commitment: expected,
            },
        )?;
        verify_uncompressed(&self.preprocessing.prep, &proof.proof)?;
        Ok(())
    }
}

fn register_setup(params: &Params, cols: usize, seed: [u8; 32]) -> Result<(), AjtaiError> {
    // Rejects an existing setup for these dimensions with a different seed or kappa.
    neo_ajtai::set_global_pp_seeded(neo_math::D, params.kappa() as usize, cols, seed)
}

fn sparse_relation(batch: &Batch) -> SparseR1cs {
    let core = batch.relation.r1cs();
    let ccs = core.structure();
    SparseR1cs::new(
        ccs.matrices[0].clone(),
        ccs.matrices[1].clone(),
        ccs.matrices[2].clone(),
        ccs.n,
        ccs.m,
        core.public_input_count(),
    )
    .unwrap()
}

fn state_digest(fields: &[F]) -> [u8; 32] {
    digest_fields_as_digest32(
        encode_poseidon_trace(&build_semantic_state_preimage_fields(fields)).digest_native,
    )
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
            COL_NEXT_UTXO_ID_BEFORE
            | COL_TX_PHASE_BEFORE
            | COL_COORD_FINALIZED_BEFORE
            | COL_ABI_READ_REMAINING_BEFORE
            | COL_ABI_READ_ORDINAL_BEFORE
            | COL_OUTPUT_CURSOR_BEFORE
            | COL_ENABLED_METHOD_LOG_LEN_BEFORE
            | COL_PENDING_CTOR_PRESENT_BEFORE
            | COL_PENDING_CTOR_HOLDER_BEFORE
            | COL_PENDING_CTOR_HANDLE_BEFORE => F::ZERO,
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
    #[error("final-state digest mismatch")]
    DigestMismatch,
}

fn check_final_state(
    authenticated_state_digest: [u8; 32],
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
    if state_digest(final_state) != authenticated_state_digest {
        return Err(FinalClaimError::DigestMismatch);
    }
    Ok(())
}

fn recursive_plan(batch: &Batch, r1cs: &SparseR1cs) -> RecursiveStepImagePlan {
    let widths = range_checked_variable_widths(batch.relation.columns());
    // R1csIvc compiles the recursive verifier and solves its own fixed point.
    // Only the application widths and semantic-state binding are supplied here;
    // there is no legacy image NIFS payload or accumulator to configure.
    let mut plan = RecursiveStepImagePlan {
        limbs: widths.iter().sum::<usize>() + 1,
        app_private_var_widths: widths,
        boundary_bits: 4 * POSEIDON2_GOLDILOCKS_BITS,
        kmul_count: 0,
        ring_action_pair_count: 0,
        projection_batches: vec![],
        ring_action_pair_layout: RingActionTraceLayout::new(
            LowNormEncoding::U64,
            LowNormEncoding::U64,
            LowNormEncoding::U64,
            LowNormEncoding::U64,
        ),
        sponge_transcript_permutes: 0,
        nifs_payload_shapes: vec![],
        accumulator: None,
        state_x_out: None,
    };
    let layout = FPrimeImageLayout::new(build_recursive_step_image_config(&plan));
    plan.state_x_out = Some(StateXOutPlanOptions {
        pc: 1,
        public_x_out_lane_bit_starts: std::array::from_fn(|i| {
            layout.boundary.offset + i * POSEIDON2_GOLDILOCKS_BITS
        }),
        app_public_input_var_indices: (0..r1cs.m_in).collect(),
        app_public_input_bit_var_indices: vec![],
        // Batch already eliminated the middle links and remapped the endpoints
        // into its column-major assignment layout.
        semantic_state_in_var_indices: batch
            .continuity
            .links()
            .map(|link| link.next_step_column)
            .collect(),
        semantic_state_out_var_indices: batch
            .continuity
            .links()
            .map(|link| link.previous_step_column)
            .collect(),
        initial_semantic_state_digest_anchor: Some(state_digest(&initial_state())),
    });
    plan
}

#[cfg(test)]
mod tests;
