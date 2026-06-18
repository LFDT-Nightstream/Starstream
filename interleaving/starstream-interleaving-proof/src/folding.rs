//! Adapts the ark-based interleaving step circuit to neo-fold-clean's pure-R1CS
//! frontend.
//!
//! TODO(nebula-cross-link): only program-state IVC wires are cross-linked today;
//! add the Nebula commitment state to the semantic-state indices.
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

pub(crate) fn to_neo(v: ArkF) -> NeoF {
    NeoF::from_u64(v.into_bigint().0[0])
}

pub(crate) struct StepShape {
    pub r1cs: SparseR1cs,
    pub ivc_layout: IvcWireLayout,
}

pub(crate) fn extract_r1cs_and_assignments(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
) -> Result<(StepShape, Vec<Vec<NeoF>>), SynthesisError> {
    let ops = crate::make_interleaved_trace(&inst, &wit);

    let mut builder = StepCircuitBuilder::<NebulaMemory<1>>::new(inst, ops);
    // TODO(soundness): re-enable the Poseidon IC commitment.
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
            // Borrow the matrices; `irw` still holds variables from this CS.
            let mut all = cs.to_matrices().expect("R1CS matrices");
            let mut matrices = all.remove("R1CS").unwrap_or_else(|| {
                panic!(
                    "no R1CS predicate; available: {:?}",
                    all.keys().collect::<Vec<_>>()
                )
            });
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

use neo_fold_clean::UncompressedAudit;
use neo_fold_clean::engine::ccs_native::poseidon2::POSEIDON2_GOLDILOCKS_BITS;
use neo_fold_clean::frontends::f_prime::image::{
    FPrimeImageLayout, NifsCeClaimShape, NifsPayloadShape,
};
use neo_fold_clean::frontends::f_prime::recursive_plan::build_semantic_state_preimage_fields;
use neo_fold_clean::frontends::f_prime::recursive_plan::{
    AccumulatorPlanOptions, RecursiveStepImagePlan, StateXOutPlanOptions,
    build_recursive_step_image_config,
};
use neo_fold_clean::frontends::r1cs_f_prime::{self, R1csChainBuilder};
use neo_fold_clean::paper::digest::digest_fields_as_digest32;
use neo_fold_clean::paper::f_prime::poseidon_trace::encode_poseidon_trace;
use neo_fold_clean::paper::f_prime::ring_action_trace::{LowNormEncoding, RingActionTraceLayout};

fn build_plan(
    m: usize,
    m_in: usize,
    semantic_in: Vec<usize>,
    semantic_out: Vec<usize>,
    anchor: [u8; 32],
) -> RecursiveStepImagePlan {
    let limbs = m * POSEIDON2_GOLDILOCKS_BITS + 1;

    // Re-tune this fixed point if the recursive shape changes.
    const C_DATA_ENTRIES: usize = 972;
    let ce_shape = NifsCeClaimShape {
        c_data_entries: C_DATA_ENTRIES,
        x_rows: 54,
        x_active_cols: 5,
        r_len: 14,
        y_ring_inner_lens: vec![64; 8],
        y_zcol_len: 64,
        s_col_len: 20,
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
        semantic_state_in_var_indices: semantic_in,
        semantic_state_out_var_indices: semantic_out,
        initial_semantic_state_digest_anchor: Some(anchor),
    });
    plan
}

fn semantic_state_digest(app_state: &[NeoF]) -> [u8; 32] {
    digest_fields_as_digest32(
        encode_poseidon_trace(&build_semantic_state_preimage_fields(app_state)).digest_native,
    )
}

/// Fold every step's R1CS assignment and return the public shape, plan, and audit.
pub(crate) fn fold(
    shape: &StepShape,
    assignments: &[Vec<NeoF>],
    anchor: [u8; 32],
) -> (SparseR1cs, RecursiveStepImagePlan, UncompressedAudit) {
    // `IvcWireLayout` indices are witness-local; the plan's semantic-state
    // indices address the full assignment `z = [instance | witness]`, so shift
    // by `m_in` (the instance count).
    let to_z = |w: usize| shape.r1cs.m_in + w;
    let in_idx: Vec<usize> = shape
        .ivc_layout
        .input_indices()
        .into_iter()
        .map(to_z)
        .collect();
    let out_idx: Vec<usize> = shape
        .ivc_layout
        .output_indices()
        .into_iter()
        .map(to_z)
        .collect();

    // The instance-derived anchor assumes the first IVC field is `id_curr`; keep
    // that layout in sync with `IVC_SEMANTIC_FIELD_COUNT`.
    debug_assert_eq!(
        in_idx.len(),
        starstream_interleaving_spec::IVC_SEMANTIC_FIELD_COUNT,
        "IvcWireLayout::FIELD_COUNT diverged from IVC_SEMANTIC_FIELD_COUNT"
    );
    let z0 = assignments.first().expect("at least one step");
    debug_assert_eq!(
        semantic_state_digest(&in_idx.iter().map(|&i| z0[i]).collect::<Vec<_>>()),
        anchor,
        "prover's step-0 input state doesn't match the instance-derived anchor"
    );

    let plan = build_plan(shape.r1cs.m, shape.r1cs.m_in, in_idx, out_idx, anchor);
    let prep = r1cs_f_prime::preprocess_sparse_seeded(
        &shape.r1cs,
        &plan,
        starstream_interleaving_spec::FOLD_SEED,
    )
    .expect("preprocess_sparse_seeded");

    let mut chain = R1csChainBuilder::new(&prep).expect("chain builder");
    for z in assignments {
        chain
            .append_assignment(z.clone())
            .expect("append_assignment");
    }
    let audit = chain.finish_with_audit().expect("finish_with_audit");

    (shape.r1cs.clone(), plan, audit)
}
