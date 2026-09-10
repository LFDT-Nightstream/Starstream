//! Test-only R1CS-F' integration; deliberately not an execution-proof API.
//!
//! TODO: Prove RAM/ROM accesses and bind their initialization. For now memory
//! consistency is only checked on the host; the proof authenticates the local
//! relation and carried-state continuity, NOT full interleaving semantics.

use super::*;
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
            ivc::{R1csIvc, R1csIvcPreprocessing},
        },
    },
    lifecycle::{Uncompressed, verify_uncompressed},
    paper::{
        digest::digest_fields_as_digest32,
        f_prime::{
            poseidon_trace::encode_poseidon_trace,
            ring_action_trace::{LowNormEncoding, RingActionTraceLayout},
        },
        params::Params,
    },
};
use neo_params::{NeoParams, goldilocks_paper_b2};
use p3_field::PrimeField64;

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
            COL_CALL_SP_BEFORE => F::ONE,
            COL_NEXT_UTXO_ID_BEFORE
            | COL_ENABLED_METHOD_LOG_LEN_BEFORE
            | COL_PENDING_CTOR_PRESENT_BEFORE
            | COL_PENDING_CTOR_HOLDER_BEFORE
            | COL_PENDING_CTOR_HANDLE_BEFORE => F::ZERO,
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
enum FinalClaimError {
    #[error("final-state length is {actual}, expected {expected}")]
    LengthMismatch { expected: usize, actual: usize },
    #[error("terminal call-stack pointer must be zero, got {actual:?}")]
    NonterminalStack { actual: F },
    #[error("terminal coroutine must be a coordinator, got packed id {actual:?}")]
    TerminalCoroutineNotCoordinator { actual: F },
    #[error("final-state digest mismatch")]
    DigestMismatch,
}

fn check_final_claim(digest: [u8; 32], claim: &[F]) -> Result<(), FinalClaimError> {
    let groups = build_ivc_state_continuity_links();
    let links = groups
        .iter()
        .flat_map(|group| &group.links)
        .collect::<Vec<_>>();
    if claim.len() != links.len() {
        return Err(FinalClaimError::LengthMismatch {
            expected: links.len(),
            actual: claim.len(),
        });
    }
    for (link, value) in links.iter().zip(claim) {
        match link.previous_step_column {
            COL_CALL_SP_AFTER if *value != F::ZERO => {
                return Err(FinalClaimError::NonterminalStack { actual: *value });
            }
            COL_CURR_AFTER if value.as_canonical_u64() & 1 != 0 => {
                return Err(FinalClaimError::TerminalCoroutineNotCoordinator { actual: *value });
            }
            _ => {}
        }
    }
    if state_digest(claim) != digest {
        return Err(FinalClaimError::DigestMismatch);
    }
    Ok(())
}

fn verify_relation_proof(
    prep: &R1csIvcPreprocessing,
    proof: &Uncompressed,
    claim: &[F],
) -> Result<(), Box<dyn std::error::Error>> {
    // TODO: Replace publication of the whole carried state with a private-state
    // commitment and a public terminal/lifetime projection. Log length already
    // leaks activity; generation counters must stay private when RAM is wired.
    check_final_claim(proof.state.semantic_state_digest, claim)?;
    verify_uncompressed(&prep.prep, proof)?;
    Ok(())
}

// Matches neo-wasm's non-Nebula R1CS-F' test profile. These are test-only
// parameters, not a production security claim.
fn test_params() -> Params {
    Params::test_only_from_neo_params(
        NeoParams::new(
            goldilocks_paper_b2::Q,
            goldilocks_paper_b2::ETA as u32,
            goldilocks_paper_b2::D as u32,
            2,
            1 << 15,
            goldilocks_paper_b2::B_BASE,
            goldilocks_paper_b2::K_RHO,
            goldilocks_paper_b2::T,
            goldilocks_paper_b2::EXTENSION_DEGREE,
            40,
        )
        .unwrap(),
    )
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

#[test]
fn relation_adapter_preserves_assignments_widths_and_state_endpoints() {
    let normalized = normalize(&crate::tests::constructor_trace([1, 2, 3, 4]));
    let mut expected_final = None;
    for size in [1, 3, 8] {
        let batch = Batch::new(size).unwrap();
        let packed = batch.pack(&normalized.steps);
        let sparse = sparse_relation(&batch);
        let plan = recursive_plan(&batch, &sparse);
        let binding = plan.state_x_out.as_ref().unwrap();
        let widths = range_checked_variable_widths(batch.relation.columns());
        assert_eq!(widths.len(), sparse.m);
        assert_eq!(plan.app_private_var_widths, widths);
        assert_eq!(
            binding.initial_semantic_state_digest_anchor,
            Some(state_digest(&initial_state()))
        );
        let single_links = build_ivc_state_continuity_links();
        let single_links = single_links.iter().flat_map(|group| &group.links);
        for ((&input, &output), link) in binding
            .semantic_state_in_var_indices
            .iter()
            .zip(&binding.semantic_state_out_var_indices)
            .zip(single_links)
        {
            assert_eq!(input, link.next_step_column * size);
            assert_eq!(output, link.previous_step_column * size + size - 1);
        }
        for row in &packed.rows {
            sparse.is_satisfied_by(row).unwrap();
            for (&value, &width) in row.iter().zip(&widths) {
                assert!(width == 64 || value.as_canonical_u64() < (1u64 << width));
            }
        }
        let actual_initial = batch
            .continuity
            .links()
            .map(|link| packed.rows[0][link.next_step_column])
            .collect::<Vec<_>>();
        assert_eq!(actual_initial, initial_state());
        let claim = final_state(&batch, &packed);
        assert_eq!(expected_final.get_or_insert_with(|| claim.clone()), &claim);
        check_final_claim(state_digest(&claim), &claim).unwrap();

        // Check the converted backend relation, not just our CCS checker.
        let mut tampered = packed.rows[0].clone();
        tampered[COL_SEL_NEW_UTXO * size] = F::new(2);
        assert!(sparse.is_satisfied_by(&tampered).is_err());
    }
}

#[test]
fn final_claim_requires_termination_and_authentication() {
    let batch = Batch::new(3).unwrap();
    let normalized = normalize(&crate::tests::constructor_trace([1, 2, 3, 4]));
    let claim = final_state(&batch, &batch.pack(&normalized.steps));
    let digest = state_digest(&claim);
    for index in 0..claim.len() {
        let mut altered = claim.clone();
        altered[index] += F::ONE;
        assert!(check_final_claim(digest, &altered).is_err());
    }
    assert_eq!(
        check_final_claim(digest, &claim[..claim.len() - 1]),
        Err(FinalClaimError::LengthMismatch {
            expected: claim.len(),
            actual: claim.len() - 1,
        })
    );
    let nonterminal = initial_state();
    assert_eq!(
        check_final_claim(state_digest(&nonterminal), &nonterminal),
        Err(FinalClaimError::NonterminalStack { actual: F::ONE })
    );
}

#[test]
#[ignore = "expensive relation-only proving; run explicitly with --release --ignored"]
fn relation_proving_smoke() -> Result<(), Box<dyn std::error::Error>> {
    let total = std::time::Instant::now();
    let trace = crate::tests::constructor_trace([1, 2, 3, 4]);
    let batch = Batch::new(2)?;
    let normalized = normalize(&trace);
    let packed = batch.pack(&normalized.steps);
    // Host diagnostics include RAM/ROM checking; these checks are NOT proved.
    let preload = crate::memory::preload_tables(&normalized.method_table);
    batch.check(&packed, &preload)?;
    let claim = final_state(&batch, &packed);
    check_final_claim(state_digest(&claim), &claim)?;
    // Five real instructions in three batches, with one trailing padding slot:
    // base -> bootstrap-recursive -> steady-state recursive (nonempty running
    // accumulator). Two batches would only exercise the bootstrap fold.
    assert_eq!(packed.rows.len(), 3);
    assert_eq!(packed.origins.last(), Some(&None));
    eprintln!("deriving relation-only recursive plan");
    let sparse = sparse_relation(&batch);
    eprintln!(
        "application relation: {} rows, {} columns, batch size {}",
        sparse.n, sparse.m, batch.size
    );
    let plan = recursive_plan(&batch, &sparse);
    eprintln!("preprocessing relation-only proof");
    let started = std::time::Instant::now();
    let prep = R1csIvcPreprocessing::new_seeded(test_params(), sparse, plan, 0x57a2)?;
    eprintln!(
        "preprocessing: {:.2?}; recursive CCS: {} rows, {} columns",
        started.elapsed(),
        prep.relation().structure().n,
        prep.relation().structure().m,
    );
    assert!(prep.prep.enforces_terminal_induction());
    let mut chain = R1csIvc::new(&prep);
    for (index, row) in packed.rows.into_iter().enumerate() {
        if index == 2 {
            assert!(matches!(
                &chain.audit().expect("two batches have been proved").proof.state.proof,
                neo_fold_clean::paper::construction2::ProofState::Active { running, .. }
                    if !running.claims.is_empty()
            ));
        }
        let branch = ["base", "bootstrap-recursive", "steady-state recursive"][index];
        eprintln!("proving relation batch {index} ({branch})");
        let started = std::time::Instant::now();
        chain.extend(row)?;
        eprintln!("batch {index} ({branch}): {:.2?}", started.elapsed());
    }
    eprintln!("finalizing relation-only proof");
    let started = std::time::Instant::now();
    let proof = chain.finish()?;
    eprintln!("finalization: {:.2?}", started.elapsed());
    let started = std::time::Instant::now();
    verify_relation_proof(&prep, &proof, &claim)?;
    eprintln!("verification: {:.2?}", started.elapsed());
    let mut altered = claim.clone();
    *altered.last_mut().unwrap() += F::ONE;
    assert!(verify_relation_proof(&prep, &proof, &altered).is_err());
    eprintln!("total relation-only smoke test: {:.2?}", total.elapsed());
    Ok(())
}
