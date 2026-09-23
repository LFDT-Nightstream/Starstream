use super::*;
use neo_params::{NeoParams, goldilocks_paper_b2};
use p3_field::PrimeField64;

fn check_execution_final_claim(digest: [u8; 32], claim: &[F]) -> Result<(), FinalClaimError> {
    check_final_state(digest, claim, TerminalClaim::Execution)
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

fn transaction_fixture() -> (Trace, TransactionStatement, crate::TraceCommitments) {
    use crate::ivc_state::CoroutineId::{Coord, Utxo};
    let (trace, statement) = crate::transaction::tests::fixture();
    // Only these three steps emit program events.
    let events = Trace::new([trace.0[0].clone(), trace.0[2].clone(), trace.0[3].clone()]);
    let roots = crate::commitment::tests::expected(&events, &[Utxo(0), Coord(1), Utxo(0)]);
    (trace, statement, roots)
}

#[test]
fn transaction_claim_binds_statement_roots_and_finished_phase() {
    let (trace, statement, roots) = transaction_fixture();
    let expected = crate::transaction_commitment(&statement, &roots)
        .unwrap()
        .map(F::new);
    for size in [1, 3, 8] {
        super::super::verify_transaction(&trace, size, &statement, &roots).unwrap();
        let batch = Batch::new(size).unwrap();
        let packed = batch.pack(&normalize(&trace).steps);
        let claim = final_state(&batch, &packed);
        let digest = state_digest(&claim);
        let mut wrong_log_length = claim.clone();
        wrong_log_length[carried_index(COL_ENABLED_METHOD_LOG_LEN_AFTER)] += F::ONE;
        assert_eq!(
            check_final_state(
                digest,
                &wrong_log_length,
                TerminalClaim::Transaction {
                    commitment: expected
                }
            ),
            Err(FinalClaimError::DigestMismatch),
        );
        check_final_state(
            digest,
            &claim,
            TerminalClaim::Transaction {
                commitment: expected,
            },
        )
        .unwrap();
        let actual_initial: Vec<_> = std::iter::once(F::ONE)
            .chain(
                batch
                    .continuity
                    .links()
                    .map(|link| packed.rows[0][link.next_step_column]),
            )
            .collect();
        assert_eq!(actual_initial, initial_state());

        let mut changed = statement.clone();
        changed.inputs[0].storage.0[0] += 1;
        let wrong = crate::transaction_commitment(&changed, &roots)
            .unwrap()
            .map(F::new);
        assert_eq!(
            check_final_state(
                digest,
                &claim,
                TerminalClaim::Transaction { commitment: wrong }
            ),
            Err(FinalClaimError::Terminal(
                Unsatisfied::TransactionCommitment
            ))
        );
        let mut changed_roots = roots.clone();
        changed_roots.values_mut().next().unwrap()[0] += 1;
        let wrong = crate::transaction_commitment(&statement, &changed_roots)
            .unwrap()
            .map(F::new);
        assert_eq!(
            check_final_state(
                digest,
                &claim,
                TerminalClaim::Transaction { commitment: wrong }
            ),
            Err(FinalClaimError::Terminal(
                Unsatisfied::TransactionCommitment
            ))
        );

        let phase_index = carried_index(COL_TX_PHASE_AFTER);
        let mut running = claim.clone();
        running[phase_index] = F::new(crate::ivc_state::TxPhase::Running as u64);
        assert!(matches!(
            check_final_state(
                state_digest(&running),
                &running,
                TerminalClaim::Transaction {
                    commitment: expected
                }
            ),
            Err(FinalClaimError::Terminal(Unsatisfied::TransactionStatement))
        ));
        for index in 0..claim.len() {
            let mut changed = claim.clone();
            changed[index] += F::ONE;
            assert!(
                check_final_state(
                    digest,
                    &changed,
                    TerminalClaim::Transaction {
                        commitment: expected
                    }
                )
                .is_err()
            );
        }
    }
}

#[test]
#[ignore = "expensive relation-only proving; run explicitly with --release --ignored"]
fn transaction_proof_round_trip() -> Result<(), ProvingError> {
    let (trace, statement, roots) = transaction_fixture();
    let context = TransactionProofContext::new(3, test_params(), [0x57; 32])?;
    assert!(context.preprocessing.prep.enforces_terminal_induction());
    let proof = context.prove(&trace, &statement, &roots)?;
    // Seven instructions in three batches (including trailing padding) exercise
    // base, bootstrap-recursive, and steady-state recursive proving.
    assert_eq!(proof.proof.state.chunk_count, 3);
    assert!(matches!(
        &proof.proof.state.proof,
        neo_fold_clean::paper::construction2::ProofState::Active { running, .. }
            if running.as_materialized().is_some_and(|running| !running.claims.is_empty())
    ));
    context.verify(&proof, &statement, &roots)?;
    let mut changed = statement.clone();
    changed.outputs[0].storage.0[0] += 1;
    assert!(context.verify(&proof, &changed, &roots).is_err());
    let mut changed_roots = roots.clone();
    changed_roots.values_mut().next().unwrap()[0] += 1;
    assert!(context.verify(&proof, &statement, &changed_roots).is_err());
    let mut changed_proof = proof.clone();
    let log_index = carried_index(COL_ENABLED_METHOD_LOG_LEN_AFTER);
    changed_proof.final_state[log_index] += F::ONE;
    assert!(matches!(
        context.verify(&changed_proof, &statement, &roots),
        Err(ProvingError::FinalClaim(FinalClaimError::DigestMismatch))
    ));
    Ok(())
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
            .skip(1)
            .zip(binding.semantic_state_out_var_indices.iter().skip(1))
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
        let actual_initial = std::iter::once(F::ONE)
            .chain(
                batch
                    .continuity
                    .links()
                    .map(|link| packed.rows[0][link.next_step_column]),
            )
            .collect::<Vec<_>>();
        assert_eq!(actual_initial, initial_state());
        let claim = final_state(&batch, &packed);
        assert_eq!(expected_final.get_or_insert_with(|| claim.clone()), &claim);
        check_execution_final_claim(state_digest(&claim), &claim).unwrap();

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
        assert!(check_execution_final_claim(digest, &altered).is_err());
    }
    assert_eq!(
        check_execution_final_claim(digest, &claim[..claim.len() - 1]),
        Err(FinalClaimError::LengthMismatch {
            expected: claim.len(),
            actual: claim.len() - 1,
        })
    );
    let mut nonterminal = claim.clone();
    let stack_index = build_ivc_state_continuity_links()
        .into_iter()
        .flat_map(|group| group.links)
        .position(|link| link.previous_step_column == COL_CALL_SP_AFTER)
        .unwrap()
        + 1;
    nonterminal[stack_index] = F::ONE;
    assert_eq!(
        check_execution_final_claim(state_digest(&nonterminal), &nonterminal),
        Err(FinalClaimError::Terminal(
            Unsatisfied::TerminalCallStackNotEmpty { actual: F::ONE }
        ))
    );
}

fn carried_index(column: usize) -> usize {
    build_ivc_state_continuity_links()
        .into_iter()
        .flat_map(|g| g.links)
        .position(|link| link.previous_step_column == column)
        .unwrap()
        + 1
}

#[test]
fn setup_registration_pins_seed_and_parameters() {
    // Unusual width isolates this registry entry from all circuit fixtures.
    let cols = 100_003;
    register_setup(&test_params(), cols, [7; 32]).unwrap();
    register_setup(&test_params(), cols, [7; 32]).unwrap();
    assert!(register_setup(&test_params(), cols, [8; 32]).is_err());
    assert!(register_setup(&Params::production(), cols, [7; 32]).is_err());
}
