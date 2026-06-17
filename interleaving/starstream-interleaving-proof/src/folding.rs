//! Bridge from the ark-based interleaving step circuit to neo-fold-clean's
//! Plonky3-based pure-R1CS folding backend (Nightstream main).
//!
//! Step 3b is staged. This module currently establishes the mechanical core:
//!   * the ark `FpGoldilocks` <-> p3/neo `Goldilocks` field bridge,
//!   * extraction of the (uniform) per-step R1CS shape (A, B, C, m_in) from the
//!     interleaving step circuit, and
//!   * the per-step assignment vector `z = [instance | witness]`,
//! all validated against neo's own [`R1cs::is_satisfied_by`].
//!
//! The folding lifecycle is wired on top of this:
//!   * a `RecursiveStepImagePlan` whose `semantic_state_{in,out}_var_indices`
//!     are the program-state IVC wires ([`IvcWireLayout`]) so the fold enforces
//!     step-to-step continuity (cross-linking),
//!   * `R1csChainBuilder::append_assignment` per step, then `finish` +
//!     `verify_uncompressed`.
//!
//! TODO(nebula-cross-link): the Nebula running commitments (ic_rs_ws / ic_is_fs,
//! fingerprints, scan cursor) are also threaded across steps but are NOT yet in
//! the cross-linked state set — only the program-state IvcWires are. Add their
//! witness columns to the semantic-state indices in a follow-up so the fold also
//! binds memory-commitment continuity.

// Staged WIP: the foundation below is exercised by tests; the folding lifecycle
// that consumes it (prove via R1csChainBuilder) lands next.
#![allow(dead_code)]

use crate::F as ArkF;
use crate::circuit::{InterRoundWires, IvcWireLayout, StepCircuitBuilder};
use crate::memory::IVCMemory as _;
use crate::nebula::tracer::{NebulaMemory, NebulaMemoryParams};
use ark_ff::PrimeField as _;
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use neo_ccs::{CcsMatrix, CscMat};
use neo_fold_clean::frontends::r1cs_f_prime::SparseR1cs;
use neo_math::F as NeoF;
use p3_field::PrimeCharacteristicRing as _;
use starstream_interleaving_spec::{InterleavingInstance, InterleavingWitness};

/// ark Goldilocks -> p3/neo Goldilocks. Both are the same field
/// (p = 2^64 - 2^32 + 1) in canonical form, so this is a value-preserving
/// re-encoding through the canonical `u64`.
pub(crate) fn to_neo(v: ArkF) -> NeoF {
    NeoF::from_u64(v.into_bigint().0[0])
}

/// The uniform per-step R1CS shape plus the program-state IVC wire layout used
/// for cross-linking.
pub(crate) struct StepShape {
    pub r1cs: SparseR1cs,
    pub ivc_layout: IvcWireLayout,
}

/// Drive the interleaving step circuit over the whole (Nebula-backed) trace,
/// returning the uniform per-step R1CS shape and one `z = [instance | witness]`
/// assignment per step.
pub(crate) fn extract_r1cs_and_assignments(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
) -> Result<(StepShape, Vec<Vec<NeoF>>), SynthesisError> {
    let ops = crate::make_interleaved_trace(&inst, &wit);

    let mut builder = StepCircuitBuilder::<NebulaMemory<1>>::new(inst, ops);
    // TODO(perf/soundness): with the real Poseidon IC commitment the per-step
    // circuit is ~140k constraints (in-circuit Poseidon per memory op), which
    // neo-fold-clean's dense matrices can't hold. For the first proof we disable
    // the Poseidon commitment (already unsound while the FS challenges are
    // stubbed), which shrinks the circuit. Re-enable once the commitment is
    // proved out-of-the-dense-matrix (or the matrices go sparse).
    let mb = builder.trace_memory_ops(NebulaMemoryParams {
        unsound_disable_poseidon_commitment: true,
    });
    let mut mem = mb.constraints();

    let num_steps = builder.ops.len();
    let mut irw = InterRoundWires::new(builder.instance.entrypoint.0 as u64);

    let mut assignments = Vec::with_capacity(num_steps);
    let mut shape: Option<StepShape> = None;

    for i in 0..num_steps {
        let cs = ConstraintSystem::<ArkF>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        // Capture the IVC wire layout (and the matrix shape) on the first step;
        // the circuit shape is uniform across steps.
        let capture = i == 0;
        let (next_irw, (), layout) =
            builder.make_step_circuit(i, &mut mem, cs.clone(), irw, capture)?;
        irw = next_irw;

        if let Some(unsat) = cs.which_is_unsatisfied()? {
            tracing::error!(step = i, location = unsat, "step CCS is unsat");
        }
        assert!(
            cs.is_satisfied()?,
            "interleaving step {i} should be satisfied"
        );

        // z = [instance | witness], in the same column order as the matrices.
        let z: Vec<NeoF> = cs
            .instance_assignment()?
            .into_iter()
            .chain(cs.witness_assignment()?)
            .map(to_neo)
            .collect();

        if capture {
            let ivc_layout = layout.expect("ivc layout requested on first step");

            cs.finalize();
            let m_in = cs.num_instance_variables();
            let m = m_in + cs.num_witness_variables();
            // Borrow the matrices (don't consume the CS: the threaded `irw`
            // holds FpVars that clone the CS ref, so it isn't uniquely owned).
            let mut all = cs.to_matrices().expect("R1CS matrices");
            let mut matrices = all.remove("R1CS").unwrap_or_else(|| {
                panic!(
                    "no R1CS predicate; available: {:?}",
                    all.keys().collect::<Vec<_>>()
                )
            });
            // matrices = [A, B, C], each a Vec of sparse rows `Vec<(coeff, col)>`.
            let c = matrices.pop().unwrap();
            let b = matrices.pop().unwrap();
            let a = matrices.pop().unwrap();
            let n = a.len();
            tracing::info!(n, m, m_in, num_steps, "extracted per-step R1CS shape");

            let r1cs = SparseR1cs::new(
                csc_from_rows(&a, n, m),
                csc_from_rows(&b, n, m),
                csc_from_rows(&c, n, m),
                n,
                m,
                m_in,
            )
            .expect("SparseR1cs shape");

            shape = Some(StepShape { r1cs, ivc_layout });
        }

        assignments.push(z);
    }

    Ok((shape.expect("at least one step"), assignments))
}

/// Build a sparse `CcsMatrix` (CSC) from ark's sparse row form `Vec<(coeff, col)>`.
fn csc_from_rows(rows: &[Vec<(ArkF, usize)>], n: usize, m: usize) -> CcsMatrix<NeoF> {
    let mut triplets = Vec::new();
    for (r, row) in rows.iter().enumerate() {
        for (coeff, col) in row {
            triplets.push((r, *col, to_neo(*coeff)));
        }
    }
    CcsMatrix::Csc(CscMat::from_triplets(triplets, n, m))
}

// ── neo-fold-clean r1cs_f_prime folding ──────────────────────────────────────

use neo_fold_clean::engine::ccs_native::poseidon2::POSEIDON2_GOLDILOCKS_BITS;
use neo_fold_clean::frontends::f_prime::image::{
    FPrimeImageLayout, NifsCeClaimShape, NifsPayloadShape,
};
use neo_fold_clean::frontends::f_prime::recursive_plan::{
    AccumulatorPlanOptions, RecursiveStepImagePlan, StateXOutPlanOptions,
    build_recursive_step_image_config,
};
use neo_fold_clean::frontends::r1cs_f_prime::{self, R1csChainBuilder};
use neo_fold_clean::paper::f_prime::ring_action_trace::{LowNormEncoding, RingActionTraceLayout};
use neo_fold_clean::verify_uncompressed_audit;

/// Build the F' recursive-step plan for our per-step R1CS.
///
/// Modeled on neo-fold-clean's `make_small_plan` test fixture. Currently
/// STATELESS (no `semantic_state_*` indices) — it binds the public input but
/// does not yet cross-link the per-step IVC state. Cross-linking is the next
/// patch: set `semantic_state_{in,out}_var_indices` to the [`IvcWireLayout`]
/// indices (+ an initial-state anchor).
///
/// The NIFS CE-claim shape below is a stub; the real shape is a params/structure
/// dependent fixed point that the recursive compile surfaces via
/// `PostParentShapeMismatch` — tune these constants from that error.
fn build_stateless_plan(m: usize, m_in: usize) -> RecursiveStepImagePlan {
    let limbs = m * POSEIDON2_GOLDILOCKS_BITS + 1;

    // CE-claim fixed point for our structure under the seeded params, read off
    // the `PostParentShapeMismatch` error (the documented convergence workflow).
    const C_DATA_ENTRIES: usize = 972;
    let ce_shape = NifsCeClaimShape {
        c_data_entries: C_DATA_ENTRIES,
        x_rows: 54,
        x_active_cols: 5,
        r_len: 13,
        y_ring_inner_lens: vec![64; 8],
        y_zcol_len: 64,
        s_col_len: 19,
    };

    let probe_plan = RecursiveStepImagePlan {
        limbs,
        app_private_var_widths: Vec::new(),
        boundary_bits: 4 * POSEIDON2_GOLDILOCKS_BITS,
        kmul_count: 0,
        ring_action_pair_count: 0,
        ring_action_pair_layout: RingActionTraceLayout::new(
            LowNormEncoding::U64,
            LowNormEncoding::U64,
            LowNormEncoding::U64,
            LowNormEncoding::U64,
        ),
        sponge_transcript_permutes: 0,
        nifs_payload_shapes: vec![NifsPayloadShape::CeClaim(ce_shape)],
        accumulator: Some(AccumulatorPlanOptions {
            ce_claim_payload_index: 0,
            c_data_entries: C_DATA_ENTRIES,
            child_count: 14,
            unified: true,
        }),
        state_x_out: None,
    };

    let probe_layout = FPrimeImageLayout::new(build_recursive_step_image_config(&probe_plan));
    let boundary_start = probe_layout.boundary.offset;
    let public_x_out_lane_bit_starts: [usize; 4] =
        std::array::from_fn(|i| boundary_start + i * POSEIDON2_GOLDILOCKS_BITS);

    let mut plan = probe_plan;
    plan.state_x_out = Some(StateXOutPlanOptions {
        pc: 1,
        public_x_out_lane_bit_starts,
        app_public_input_var_indices: (0..m_in).collect(),
        app_public_input_bit_var_indices: Vec::new(),
        semantic_state_in_var_indices: Vec::new(),
        semantic_state_out_var_indices: Vec::new(),
        initial_semantic_state_digest_anchor: None,
    });
    plan
}

/// Fold every step's R1CS assignment with neo-fold-clean's `r1cs_f_prime`
/// frontend (bit-decomposed → low-norm witness) and verify the IVC proof.
pub(crate) fn fold_and_verify(shape: &StepShape, assignments: &[Vec<NeoF>]) {
    let plan = build_stateless_plan(shape.r1cs.m, shape.r1cs.m_in);
    let prep = r1cs_f_prime::preprocess_sparse_seeded(&shape.r1cs, &plan, 0x5742_0001)
        .expect("preprocess_sparse_seeded");

    let mut chain = R1csChainBuilder::new(&prep).expect("chain builder");
    for z in assignments {
        chain
            .append_assignment(z.clone())
            .expect("append_assignment");
    }
    // Multi-chunk chains use the audit (chain-replay) verifier; the terminal-only
    // `verify_uncompressed` is for single-chunk chains.
    let audit = chain.finish_with_audit().expect("finish_with_audit");
    verify_uncompressed_audit(&prep.prep, &audit).expect("verify_uncompressed_audit");
}
