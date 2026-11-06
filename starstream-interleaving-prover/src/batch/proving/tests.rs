use super::*;

fn check_execution_final_claim(claim: &[u64]) -> Result<(), FinalClaimError> {
    check_final_state(claim, TerminalClaim::Execution)
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
        let claim = check_run(&trace, size).unwrap();
        check_final_state(
            &claim,
            TerminalClaim::Transaction {
                commitment: expected,
            },
        )
        .unwrap();

        let mut changed = statement.clone();
        changed.inputs[0].storage.0[0] += 1;
        let wrong = crate::transaction_commitment(&changed, &roots)
            .unwrap()
            .map(F::new);
        assert_eq!(
            check_final_state(&claim, TerminalClaim::Transaction { commitment: wrong }),
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
            check_final_state(&claim, TerminalClaim::Transaction { commitment: wrong }),
            Err(FinalClaimError::Terminal(
                Unsatisfied::TransactionCommitment
            ))
        );

        let mut running = claim.clone();
        running[carried_index(COL_TX_PHASE_AFTER)] = crate::ivc_state::TxPhase::Running as u64;
        assert!(matches!(
            check_final_state(
                &running,
                TerminalClaim::Transaction {
                    commitment: expected
                }
            ),
            Err(FinalClaimError::Terminal(Unsatisfied::TransactionStatement))
        ));
    }
}

#[test]
#[ignore = "expensive Nebula proving; run explicitly with --release --ignored"]
fn transaction_proof_round_trip() -> Result<(), ProvingError> {
    let (trace, statement, roots) = transaction_fixture();
    let context = TransactionProofContext::new(3, 109)?;
    let mut proof = context.prove(&trace, &statement, &roots)?;
    context.verify(&proof, &statement, &roots)?;
    let mut changed = statement.clone();
    changed.outputs[0].storage.0[0] += 1;
    assert!(context.verify(&proof, &changed, &roots).is_err());
    let mut changed_roots = roots.clone();
    changed_roots.values_mut().next().unwrap()[0] += 1;
    assert!(context.verify(&proof, &statement, &changed_roots).is_err());
    let method_table = core::mem::take(&mut proof.method_table);
    assert!(matches!(
        context.verify(&proof, &statement, &roots),
        Err(ProvingError::Verify(_))
    ));
    proof.method_table = method_table;
    proof.final_state[carried_index(COL_ENABLED_METHOD_LOG_LEN_AFTER)] += 1;
    assert!(matches!(
        context.verify(&proof, &statement, &roots),
        Err(ProvingError::Verify(_))
    ));
    Ok(())
}

#[test]
fn step_inputs_keep_the_rows_and_the_state_endpoints() {
    let trace = crate::tests::constructor_trace([1, 2, 3, 4]);
    let normalized = normalize(&trace);
    let relation = crate::ccs::build_relation().unwrap();
    let mut expected_final = None;
    for size in [1, 3, 8] {
        let (geometry, application) = application(size).unwrap();
        assert_eq!(geometry.n, SEGMENT_STEPS);
        let inputs = step_inputs(size, &normalized.steps);
        assert_eq!(inputs.len() % SEGMENT_STEPS, 0);
        let width = inputs[0].len() / size;
        assert_eq!(
            inputs[0][..width],
            build_witness_vector(&normalized.steps[0])
        );
        let actual_initial = build_ivc_state_continuity_links()
            .iter()
            .flat_map(|group| &group.links)
            .map(|link| inputs[0][link.next_step_column].as_canonical_u64())
            .collect::<Vec<_>>();
        assert_eq!(actual_initial, initial_state());
        for input in &inputs {
            for (row, witness) in input.chunks(width).enumerate() {
                let (public, private) = witness.split_at(PUBLIC_INPUTS);
                if row == 0 {
                    assert_eq!(public, [F::ONE]);
                } else {
                    assert_eq!(public, [F::ZERO]);
                }
                let mut relation_witness = witness.to_vec();
                relation_witness[COL_ONE] = F::ONE;
                relation.r1cs().check_rows(&relation_witness).unwrap();
                assert_eq!(private.len(), width - PUBLIC_INPUTS);
            }
        }
        let claim = application.output_words(inputs.last().unwrap());
        assert_eq!(expected_final.get_or_insert_with(|| claim.clone()), &claim);
        assert_eq!(check_run(&trace, size).unwrap(), claim);
        check_execution_final_claim(&claim).unwrap();
    }
}

#[test]
fn a_changed_memory_value_has_no_run() {
    let normalized = normalize(&crate::tests::constructor_trace([1, 2, 3, 4]));
    let (.., application) = application(1).unwrap();
    let program = application
        .program(&preload_tables(&normalized.method_table))
        .unwrap();
    let mut inputs = step_inputs(1, &normalized.steps);
    let (row, port) = build_memory_layout()
        .entries()
        .iter()
        .flat_map(|memory| &memory.ports)
        .filter(|port| matches!(port.kind, MemoryPortKind::Read))
        .find_map(|port| {
            inputs
                .iter()
                .position(|input| match port.activation {
                    MemoryPortActivation::Always => true,
                    MemoryPortActivation::When(column) => input[column] == F::ONE,
                    MemoryPortActivation::Unless(column) => input[column] == F::ZERO,
                })
                .map(|row| (row, port.clone()))
        })
        .expect("the trace reads a memory");
    inputs[row][port.value_column] += F::ONE;
    let mut run = Run::new(application, &program, initial_state());
    let failed = inputs
        .chunks(SEGMENT_STEPS)
        .any(|segment| run.segment(segment).is_err());
    assert!(failed);
}

#[test]
fn final_claim_requires_termination() {
    let claim = check_run(&crate::tests::constructor_trace([1, 2, 3, 4]), 3).unwrap();
    check_execution_final_claim(&claim).unwrap();
    assert_eq!(
        check_execution_final_claim(&claim[..claim.len() - 1]),
        Err(FinalClaimError::LengthMismatch {
            expected: claim.len(),
            actual: claim.len() - 1,
        })
    );
    let mut nonterminal = claim.clone();
    nonterminal[carried_index(COL_CALL_SP_AFTER)] = 1;
    assert_eq!(
        check_execution_final_claim(&nonterminal),
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
}
