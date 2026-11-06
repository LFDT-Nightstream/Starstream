use super::*;

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
        check_final_state(
            &claim,
            TerminalClaim::Transaction {
                commitment: expected,
            },
        )
        .unwrap();
        let actual_initial: Vec<_> = batch
            .continuity
            .links()
            .map(|link| packed.rows[0][link.next_step_column])
            .collect();
        assert_eq!(actual_initial, initial_state());
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
        running[carried_index(COL_TX_PHASE_AFTER)] =
            F::new(crate::ivc_state::TxPhase::Running as u64);
        assert_eq!(
            check_final_state(
                &running,
                TerminalClaim::Transaction {
                    commitment: expected
                }
            ),
            Err(FinalClaimError::Terminal(Unsatisfied::TransactionStatement))
        );
    }
}

#[test]
#[ignore = "expensive Nebula proving; run explicitly with --release --ignored"]
fn transaction_proof_round_trip() -> Result<(), ProvingError> {
    let (trace, statement, roots) = transaction_fixture();
    let context = TransactionProofContext::new(ProofConfig::small(3)?, method_dictionary(&trace))?;
    let mut proof = context.prove(&trace, &statement, &roots)?;
    context.verify(&proof, &statement, &roots)?;
    let bytes = context.prover.encode_proof(&proof.proof)?;
    let decoded = context.verifier.decode_proof(&bytes)?;
    context.verify(
        &TransactionProof {
            proof: decoded,
            final_state: proof.final_state.clone(),
        },
        &statement,
        &roots,
    )?;
    assert!(
        context
            .verifier
            .decode_proof(&bytes[..bytes.len() - 1])
            .is_err()
    );
    let mut changed = statement.clone();
    changed.outputs[0].storage.0[0] += 1;
    assert!(context.verify(&proof, &changed, &roots).is_err());
    let mut changed_roots = roots.clone();
    changed_roots.values_mut().next().unwrap()[0] += 1;
    assert!(context.verify(&proof, &statement, &changed_roots).is_err());
    // This carried field is not fixed by the terminal projection. The backend
    // must authenticate it, rather than accepting the supplied final state.
    proof.final_state[carried_index(COL_ENABLED_METHOD_LOG_LEN_AFTER)] += F::ONE;
    assert!(matches!(
        context.verify(&proof, &statement, &roots),
        Err(ProvingError::Verification(_))
    ));
    Ok(())
}

#[test]
fn relation_adapter_preserves_assignments_and_state_endpoints() {
    let normalized = normalize(&crate::tests::constructor_trace([1, 2, 3, 4]));
    let mut expected_final = None;
    for size in [1, 3, 8] {
        let batch = Batch::new(size).unwrap();
        let packed = batch.pack(&normalized.steps);
        let config = ProofConfig::small(size).unwrap();
        let app = application(&batch, &config).unwrap();
        let relation = RowRelation::of(batch.relation.r1cs()).unwrap();
        let check = |z: &[F]| {
            relation.rows.iter().all(|sides| {
                let [a, b, c] = sides
                    .each_ref()
                    .map(|terms| terms.iter().map(|(i, v)| z[*i] * *v).sum::<F>());
                a * b == c
            })
        };
        for row in &packed.rows {
            assert!(check(row));
        }
        let claim = final_state(&batch, &packed);
        assert_eq!(
            app.output_words(packed.rows.last().unwrap()),
            state_words(&claim)
        );
        assert_eq!(expected_final.get_or_insert_with(|| claim.clone()), &claim);
        check_final_state(&claim, TerminalClaim::Execution).unwrap();
        let mut tampered = packed.rows[0].clone();
        tampered[COL_SEL_NEW_UTXO * size] = F::new(2);
        assert!(!check(&tampered));
        // Every memory, including the method ROM, must remain in the proof.
        let mut missing_memory = config.clone();
        missing_memory.regions.pop();
        assert!(application(&batch, &missing_memory).is_err());
    }
}

#[test]
fn final_claim_requires_termination_and_correct_width() {
    let batch = Batch::new(3).unwrap();
    let normalized = normalize(&crate::tests::constructor_trace([1, 2, 3, 4]));
    let claim = final_state(&batch, &batch.pack(&normalized.steps));
    assert_eq!(
        check_final_state(&claim[..claim.len() - 1], TerminalClaim::Execution),
        Err(FinalClaimError::LengthMismatch {
            expected: claim.len(),
            actual: claim.len() - 1
        })
    );
    let mut nonterminal = claim;
    nonterminal[carried_index(COL_CALL_SP_AFTER)] = F::ONE;
    assert_eq!(
        check_final_state(&nonterminal, TerminalClaim::Execution),
        Err(FinalClaimError::Terminal(
            Unsatisfied::TerminalCallStackNotEmpty { actual: F::ONE }
        ))
    );
}

#[test]
fn dictionary_must_be_unique_and_canonical() {
    let config = ProofConfig::small(1).unwrap();
    let method = MethodHash([1; 8]);
    assert!(matches!(
        TransactionProofContext::new(config, vec![method, method]),
        Err(ProvingError::MethodDictionary)
    ));
}

fn carried_index(column: usize) -> usize {
    build_ivc_state_continuity_links()
        .into_iter()
        .flat_map(|g| g.links)
        .position(|link| link.previous_step_column == column)
        .unwrap()
}

#[test]
fn nebula_native_run_preserves_state_and_binds_method_rom() {
    let (trace, _, _) = transaction_fixture();
    let normalized = normalize(&trace);
    for config in [
        ProofConfig::small(3).unwrap(),
        ProofConfig::for_trace_benchmark(&trace, 3, 8).unwrap(),
    ] {
        let batch = Batch::new(config.batch_size).unwrap();
        let mut packed = batch.pack(&normalized.steps);
        let expected_final = final_state(&batch, &packed);
        let segment_size = config.geometry.n;
        let original_batches = packed.rows.len();
        assert!(!original_batches.is_multiple_of(segment_size));
        pad_to_segments(
            &batch,
            &mut packed,
            normalized.steps.last().unwrap().padding_after(),
            &config.geometry,
        )
        .unwrap();
        assert_eq!(
            packed.rows.len(),
            original_batches.div_ceil(segment_size) * segment_size
        );
        assert_eq!(packed.origins.len(), packed.rows.len() * batch.size);
        assert!(
            packed.origins[original_batches * batch.size..]
                .iter()
                .all(Option::is_none)
        );
        batch
            .check(
                &packed,
                &crate::memory::preload_tables(&normalized.method_table),
            )
            .unwrap();
        assert_eq!(final_state(&batch, &packed), expected_final);
        let app = application(&batch, &config).unwrap();
        let program = app
            .program(&crate::memory::preload_tables(&normalized.method_table))
            .unwrap();
        let mut run = nebula::Run::new(app.clone(), &program, state_words(&initial_state()));
        for segment in packed.rows.chunks(segment_size) {
            run.segment(segment).unwrap();
        }
        assert_eq!(*run.state(), state_words(&final_state(&batch, &packed)));

        let mut other_methods = normalized.method_table.clone();
        other_methods[0].0[0] ^= 1;
        let other_program = app
            .program(&crate::memory::preload_tables(&other_methods))
            .unwrap();
        let mut run = nebula::Run::new(app, &other_program, state_words(&initial_state()));
        assert!(
            packed
                .rows
                .chunks(segment_size)
                .any(|segment| run.segment(segment).is_err())
        );
    }
}

#[test]
fn segment_padding_reports_empty_and_oversized_inputs() {
    let (trace, _, _) = transaction_fixture();
    let normalized = normalize(&trace);
    let config = ProofConfig::small(3).unwrap();
    let batch = Batch::new(config.batch_size).unwrap();
    let padding = || normalized.steps.last().unwrap().padding_after();
    let mut empty = batch.pack(&[]);
    assert!(matches!(
        pad_to_segments(&batch, &mut empty, padding(), &config.geometry),
        Err(ProvingError::EmptyTrace)
    ));

    let mut packed = batch.pack(&normalized.steps);
    let before = packed.rows.clone();
    let segments = packed.rows.len();
    assert!(segments > 1);
    let g = config.geometry;
    let geometry = nebula::Geometry::new(g.r, g.mu, g.b_ops, 1, segments - 1).unwrap();
    assert!(matches!(
        pad_to_segments(&batch, &mut packed, padding(), &geometry),
        Err(ProvingError::TraceTooLong { segments: actual, max })
            if actual == segments && max == segments - 1
    ));
    assert_eq!(packed.rows, before, "reject before adding padding");

    // An execution exactly at capacity needs no padding and is accepted.
    let geometry = nebula::Geometry::new(g.r, g.mu, g.b_ops, 1, segments).unwrap();
    pad_to_segments(&batch, &mut packed, padding(), &geometry).unwrap();
    assert_eq!(packed.rows, before);
}
