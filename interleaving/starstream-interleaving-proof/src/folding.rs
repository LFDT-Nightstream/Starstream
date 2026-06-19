//! Adapts the ark-based interleaving step circuit to neo-fold-clean's pure-R1CS
//! frontend.
//!
//! TODO(nebula-cross-link): only program-state IVC wires are cross-linked today;
//! add the Nebula commitment state to the semantic-state indices.

use crate::F as ArkF;
use crate::circuit::{InterRoundWires, IvcWireLayout, IvcWiresVars, StepCircuitBuilder};
use crate::memory::IVCMemory as _;
use crate::nebula::tracer::{NebulaMemory, NebulaMemoryParams};
use ark_ff::PrimeField as _;
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use neo_ccs::{CcsMatrix, CscMat};
use neo_fold_clean::frontends::r1cs_f_prime::SparseR1cs;
use neo_math::F as NeoF;
use p3_field::PrimeCharacteristicRing as _;
use starstream_interleaving_spec::{InterleavingInstance, InterleavingWitness};
use std::num::NonZeroUsize;

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
    batch_size: NonZeroUsize,
) -> Result<(StepShape, Vec<Vec<NeoF>>), SynthesisError> {
    let ops = crate::make_interleaved_trace(&inst, &wit);

    let mut builder = StepCircuitBuilder::<NebulaMemory<1>>::new(inst, ops);
    // TODO(soundness): re-enable the Poseidon IC commitment.
    let mb = builder.trace_memory_ops(
        NebulaMemoryParams {
            unsound_disable_poseidon_commitment: true,
        },
        batch_size.get(),
    );
    let mut mem = mb.constraints();

    // `trace_memory_ops` padded the step list to a whole number of batches.
    let num_steps = builder.ops.len();
    debug_assert_eq!(num_steps % batch_size, 0);
    let num_batches = num_steps / batch_size;
    let mut irw = InterRoundWires::new(builder.instance.entrypoint.0 as u64);

    let mut assignments = Vec::with_capacity(num_batches);
    let mut shape: Option<StepShape> = None;

    for batch in 0..num_batches {
        let cs = ConstraintSystem::<ArkF>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        // Synthesize batch_size sub-steps into one constraint system, threading
        // the program state in-circuit so the whole batch folds as a single R1CS
        // instance (amortizing the per-fold recursion overhead). Only the batch
        // boundary (first input / last output) is cross-linked by the fold.
        let mut first_input: Option<IvcWiresVars> = None;
        let mut prev_output: Option<IvcWiresVars> = None;

        for k in 0..batch_size.get() {
            let i = batch * batch_size.get() + k;
            let (next_irw, (), input_vars, output_vars) =
                builder.make_step_circuit(i, &mut mem, cs.clone(), irw)?;
            irw = next_irw;

            // Intra-batch continuity: this sub-step's input state == previous
            // sub-step's output state.
            if let Some(prev) = prev_output.as_ref() {
                prev.enforce_equal(&input_vars)?;
            }
            if first_input.is_none() {
                first_input = Some(input_vars);
            }
            prev_output = Some(output_vars);
        }

        let first_input = first_input.expect("batch has at least one sub-step");
        let last_output = prev_output.expect("batch has at least one sub-step");

        if let Some(unsat) = cs.which_is_unsatisfied()? {
            tracing::error!(batch, location = unsat, "batch CCS is unsat");
        }
        assert!(
            cs.is_satisfied()?,
            "interleaving batch {batch} should be satisfied"
        );

        // Ark matrices use instance-then-witness column order.
        let z: Vec<NeoF> = cs
            .instance_assignment()?
            .into_iter()
            .chain(cs.witness_assignment()?)
            .map(to_neo)
            .collect();

        // The per-batch R1CS shape and IVC-wire layout are uniform across
        // batches, so capture them once from the first batch.
        if batch == 0 {
            cs.finalize();
            let m_in = cs.num_instance_variables();
            let m = m_in + cs.num_witness_variables();

            // The batch's cross-linked program state spans the first sub-step's
            // input wires to the last sub-step's output wires.
            let ivc_layout = IvcWireLayout {
                input: first_input.indices(&cs)?,
                output: last_output.indices(&cs)?,
            };

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
            tracing::info!(
                n,
                m,
                m_in,
                num_batches,
                batch_size,
                "extracted per-batch R1CS shape"
            );

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

    Ok((shape.expect("at least one batch"), assignments))
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
    batch_size: usize,
    ce_override: Option<NifsCeClaimShape>,
) -> RecursiveStepImagePlan {
    let limbs = m * POSEIDON2_GOLDILOCKS_BITS + 1;

    // The CE-claim shape is a deterministic fixed point of the R1CS shape; it
    // shifts slightly as m grows with BATCH_SIZE (e.g. `s_col_len` ticks up).
    // Start from a best-known guess; `fold` self-tunes via a
    // PostParentShapeMismatch retry when the actual shape differs, so a stale
    // guess only costs one extra preprocess on the first attempt.
    let ce_shape = ce_override.unwrap_or_else(|| NifsCeClaimShape {
        c_data_entries: 972,
        x_rows: 54,
        x_active_cols: 5,
        r_len: 14,
        y_ring_inner_lens: vec![64; 8],
        y_zcol_len: 64,
        s_col_len: if batch_size <= 2 { 20 } else { 21 },
    });
    let c_data_entries = ce_shape.c_data_entries;

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
            c_data_entries,
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
    batch_size: usize,
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

    // Fold the chain. The recursive CE-claim shape is deterministic for this
    // R1CS but shifts with BATCH_SIZE; if the compile reports the actual shape
    // via PostParentShapeMismatch, rebuild the plan with it and retry once.
    let mut ce_override: Option<NifsCeClaimShape> = None;
    loop {
        let plan = build_plan(
            shape.r1cs.m,
            shape.r1cs.m_in,
            in_idx.clone(),
            out_idx.clone(),
            anchor,
            batch_size,
            ce_override.clone(),
        );
        let prep = r1cs_f_prime::preprocess_sparse_seeded(
            &shape.r1cs,
            &plan,
            starstream_interleaving_spec::FOLD_SEED,
        )
        .expect("preprocess_sparse_seeded");

        let mut chain = R1csChainBuilder::new(&prep).expect("chain builder");
        let mut fold_err = None;
        for z in assignments {
            if let Err(e) = chain.append_assignment(z.clone()) {
                fold_err = Some(e);
                break;
            }
        }

        match fold_err {
            None => {
                let audit = chain.finish_with_audit().expect("finish_with_audit");
                return (shape.r1cs.clone(), plan, audit);
            }
            Some(e) => match post_parent_actual_shape(&e) {
                Some(actual) if ce_override.is_none() => {
                    tracing::info!(
                        ?actual,
                        "retuning CE-claim shape from PostParentShapeMismatch"
                    );
                    ce_override = Some(actual);
                }
                _ => panic!("append_assignment: {e:?}"),
            },
        }
    }
}

/// Pull the compiler's computed CE-claim shape out of a `PostParentShapeMismatch`
/// so the plan can be rebuilt to match (see `fold`'s retry loop).
fn post_parent_actual_shape(
    err: &neo_fold_clean::frontends::r1cs_f_prime::Error,
) -> Option<NifsCeClaimShape> {
    use neo_fold_clean::frontends::f_prime::compiler::FPrimeShellCompilerError;
    use neo_fold_clean::frontends::r1cs_f_prime::Error;
    use neo_fold_clean::frontends::r1cs_f_prime::compiler::R1csCompilerError;

    match err {
        Error::Compiler(R1csCompilerError::Shell(
            FPrimeShellCompilerError::PostParentShapeMismatch { actual, .. },
        )) => Some(actual.clone()),
        _ => None,
    }
}
