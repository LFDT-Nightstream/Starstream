use super::*;

fn transaction_fixture() -> (Trace, TransactionStatement, crate::TraceCommitments) {
    use crate::ivc_state::CoroutineId::{Coord, Utxo};
    let (trace, statement) = crate::transaction::tests::fixture();
    // Loading, the entry coordinator's entry and return, and the storage read
    // are the only program events; `expected` skips the host steps.
    let roots = crate::commitment::tests::expected(
        &Trace::new(trace.0[..6].to_vec()),
        &[Utxo(0), Coord(1), Coord(1), Utxo(0)],
    );
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
    let context = TransactionProofContext::new(
        ProofConfig::for_limits(Limits::SMALL_DEFAULTS, 3)?,
        method_dictionary(&trace),
    )?;
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
fn step_relation_preserves_assignments_and_state_endpoints() {
    let normalized = normalize(&crate::tests::constructor_trace([1, 2, 3, 4]));
    let mut expected_final = None;
    for size in [1, 3, 8] {
        let batch = Batch::new(size).unwrap();
        let packed = batch.pack(&normalized.steps);
        let config = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, size).unwrap();
        let app = application(&batch, &config).unwrap();
        let inputs = step_inputs(&batch, &packed.rows).unwrap();
        for input in &inputs {
            app.check(input).unwrap();
        }
        let claim = final_state(&batch, &packed);
        assert_eq!(
            app.output_words(inputs.last().unwrap()),
            state_words(&claim)
        );
        assert_eq!(expected_final.get_or_insert_with(|| claim.clone()), &claim);
        check_final_state(&claim, TerminalClaim::Execution).unwrap();
        // Each slot of a step is checked, and so is each link between slots.
        for slot in 0..size {
            let mut tampered = inputs[0].clone();
            tampered[app.witness_column(slot, COL_SEL_NEW_UTXO).unwrap()] = F::new(2);
            assert!(app.check(&tampered).is_err());
        }
        if size > 1 {
            let mut tampered = inputs[0].clone();
            tampered[app.witness_column(1, COL_NEXT_UTXO_ID_BEFORE).unwrap()] += F::ONE;
            assert!(app.check(&tampered).is_err());
        }
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
    let config = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, 1).unwrap();
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
        ProofConfig::for_limits(Limits::SMALL_DEFAULTS, 3).unwrap(),
        ProofConfig::for_trace_benchmark(&trace, 3, 8).unwrap(),
    ] {
        let batch = Batch::new(config.batch_size).unwrap();
        let mut packed = batch.pack(&normalized.steps);
        let expected_final = final_state(&batch, &packed);
        let segment_size = config.geometry.n * batch.size;
        let original_rows = packed.rows.len();
        assert!(!original_rows.is_multiple_of(segment_size));
        pad_to_segments(
            &batch,
            &mut packed,
            normalized.steps.last().unwrap().padding_after(),
            &config.geometry,
        )
        .unwrap();
        assert_eq!(
            packed.rows.len(),
            original_rows.next_multiple_of(segment_size)
        );
        assert_eq!(packed.origins.len(), packed.rows.len());
        assert!(packed.origins[original_rows..].iter().all(Option::is_none));
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
        let segments: Vec<_> = packed
            .rows
            .chunks(segment_size)
            .map(|segment| step_inputs(&batch, segment).unwrap())
            .collect();
        for segment in &segments {
            run.segment(segment).unwrap();
        }
        assert_eq!(*run.state(), state_words(&final_state(&batch, &packed)));

        let mut other_methods = normalized.method_table.clone();
        other_methods[0].0[0] ^= 1;
        let other_program = app
            .program(&crate::memory::preload_tables(&other_methods))
            .unwrap();
        let mut run = nebula::Run::new(app, &other_program, state_words(&initial_state()));
        assert!(segments.iter().any(|segment| run.segment(segment).is_err()));
    }
}

#[test]
fn segment_padding_reports_empty_and_oversized_inputs() {
    let (trace, _, _) = transaction_fixture();
    let normalized = normalize(&trace);
    let config = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, 3).unwrap();
    let batch = Batch::new(config.batch_size).unwrap();
    let padding = || normalized.steps.last().unwrap().padding_after();
    let mut empty = batch.pack(&[]);
    assert!(matches!(
        pad_to_segments(&batch, &mut empty, padding(), &config.geometry),
        Err(ProvingError::EmptyTrace)
    ));

    let mut packed = batch.pack(&normalized.steps);
    let before = packed.rows.clone();
    // With one batch per segment, each batch is a segment.
    let segments = packed.rows.len() / batch.size;
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

#[test]
fn planned_slots_bind_and_run_like_one_slot_per_port() {
    let (trace, _, _) = transaction_fixture();
    let normalized = normalize(&trace);
    let preload = crate::memory::preload_tables(&normalized.method_table);
    for size in [1, 3, 8] {
        let per_port = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, size).unwrap();
        let planned = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, size)
            .unwrap()
            .with_planned_slots()
            .unwrap();
        eprintln!(
            "batch {size}: {} ports in {} slots; segments {} -> {}",
            per_port.slots.len(),
            planned.slots.len(),
            per_port.geometry.s_max,
            planned.geometry.s_max,
        );
        assert!(planned.slots.len() < per_port.slots.len());
        let batch = Batch::new(size).unwrap();
        let mut finals = Vec::new();
        for config in [per_port, planned] {
            let mut packed = batch.pack(&normalized.steps);
            pad_to_segments(
                &batch,
                &mut packed,
                normalized.steps.last().unwrap().padding_after(),
                &config.geometry,
            )
            .unwrap();
            let app = application(&batch, &config).unwrap();
            let program = app.program(&preload).unwrap();
            let mut run = nebula::Run::new(app, &program, state_words(&initial_state()));
            for segment in packed.rows.chunks(config.geometry.n * size) {
                run.segment(&step_inputs(&batch, segment).unwrap()).unwrap();
            }
            finals.push(run.state().clone());
        }
        assert_eq!(finals[0], finals[1]);
    }
}

/// The limits profile must hold every address the fixture uses.
#[test]
fn initial_limits_cover_the_fixture() -> Result<(), ProvingError> {
    let (trace, _, _) = transaction_fixture();
    let used = ProofConfig::for_trace_benchmark(&trace, 3, 8)?;
    let limits = ProofConfig::for_limits(Limits::SMALL_DEFAULTS, 3)?;
    for (id, region) in &used.regions {
        let (_, limit) = limits
            .regions
            .iter()
            .find(|(limit, _)| limit == id)
            .expect("both profiles size every memory");
        assert!(
            region.widths.iter().zip(&limit.widths).all(|(a, b)| a <= b),
            "{}: uses {:?}, limit {:?}",
            id.name(),
            region.widths,
            limit.widths
        );
    }
    Ok(())
}

/// Local timing probe. `STARSTREAM_PROOF_BATCH_SIZE` sets the batch size,
/// `STARSTREAM_PROOF_PLANNED_SLOTS` shares operation slots between ports, and
/// `STARSTREAM_PROOF_LIMITS` replaces [`Limits::SMALL_DEFAULTS`] with seven
/// comma-separated counts in its field order.
#[test]
#[ignore = "expensive Nebula proving; run explicitly with --release --ignored --nocapture"]
fn transaction_proof_timings() -> Result<(), ProvingError> {
    let (trace, statement, roots) = transaction_fixture();
    let batch_size = std::env::var("STARSTREAM_PROOF_BATCH_SIZE")
        .ok()
        .and_then(|size| size.parse().ok())
        .unwrap_or(3);
    // Seven comma-separated counts, in field order, replace the defaults.
    let counts: Vec<usize> = std::env::var("STARSTREAM_PROOF_LIMITS")
        .iter()
        .flat_map(|limits| limits.split(','))
        .flat_map(str::parse)
        .collect();
    let limits = match counts[..] {
        [
            utxos,
            coordinators,
            call_depth,
            handles_per_holder,
            effect_scopes,
            methods,
            registrations,
        ] => Limits {
            utxos,
            coordinators,
            call_depth,
            handles_per_holder,
            effect_scopes,
            methods,
            registrations,
        },
        _ => Limits::SMALL_DEFAULTS,
    };
    eprintln!("{limits:?}");
    let mut config = ProofConfig::for_limits(limits, batch_size)?;
    if std::env::var_os("STARSTREAM_PROOF_PLANNED_SLOTS").is_some() {
        config = config.with_planned_slots()?;
    }
    eprintln!(
        "{} steps, batch {batch_size}, {:?}",
        trace.0.len(),
        config.geometry
    );
    for part in config.size_parts()? {
        eprintln!("  {part:?}");
    }
    let start = std::time::Instant::now();
    let context = TransactionProofContext::new(config, method_dictionary(&trace))?;
    eprintln!("preprocessing: {:?}", start.elapsed());
    let start = std::time::Instant::now();
    let proof = context.prove(&trace, &statement, &roots)?;
    eprintln!("proving: {:?}", start.elapsed());
    let start = std::time::Instant::now();
    context.verify(&proof, &statement, &roots)?;
    eprintln!("verification: {:?}", start.elapsed());
    eprintln!(
        "proof: {} bytes",
        context.prover.encode_proof(&proof.proof)?.len()
    );
    Ok(())
}

/// Where the rows of one step go, and the memory sizes of each local profile.
#[test]
#[ignore = "prints a census; run with -- --ignored --nocapture"]
fn relation_census() {
    let relation = crate::ccs::build_relation().unwrap();
    let mut by_tag = std::collections::BTreeMap::<String, usize>::new();
    for row in relation.r1cs().catalog().rows() {
        *by_tag.entry(row.tag().label().to_owned()).or_default() += 1;
    }
    let total: usize = by_tag.values().sum();
    let mut tags: Vec<_> = by_tag.into_iter().collect();
    tags.sort_by_key(|(_, rows)| std::cmp::Reverse(*rows));
    println!(
        "{total} rows, {} columns",
        relation.columns().column_count()
    );
    let (named, rest) = tags.split_at(12.min(tags.len()));
    for (tag, rows) in named {
        println!("{rows:>6} {tag}");
    }
    println!(
        "{:>6} ({} other groups)",
        rest.iter().map(|(_, rows)| rows).sum::<usize>(),
        rest.len()
    );
    let (trace, _, _) = transaction_fixture();
    for (name, config) in [
        (
            "initial limits",
            ProofConfig::for_limits(Limits::SMALL_DEFAULTS, 16).unwrap(),
        ),
        (
            "trace-sized",
            ProofConfig::for_trace_benchmark(&trace, 16, 8).unwrap(),
        ),
    ] {
        println!("{name}: {:?}", config.geometry);
        for (id, region) in &config.regions {
            println!("  {:<32} {:?}", id.name(), region.widths);
        }
    }
}
