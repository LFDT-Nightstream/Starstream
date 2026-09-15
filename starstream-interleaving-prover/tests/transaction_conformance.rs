use neo_math::F;
use p3_field::PrimeField64;
use starstream_interleaving_prover::{
    Error, TraceCommitments, Unsatisfied, verify_transaction_sat,
};
use starstream_interleaving_spec::{
    InputUtxo, MethodHash, OutputUtxo, QuintError, QuintVerifier, ResourceHandle, StarstreamValue,
    Step, Trace, TransactionStatement, events,
};

const METHOD: MethodHash = MethodHash([1, 0, 2, 0, 3, 0, 4, 0]);
const OTHER: MethodHash = MethodHash([5, 0, 6, 0, 7, 0, 8, 0]);
const UNIT: StarstreamValue = StarstreamValue::UNIT_VALUE;

#[test]
#[ignore = "requires Quint; run through npm test"]
fn registration_count_bound_matches_quint() {
    let verifier = QuintVerifier::new().unwrap();
    for count in [255, 256] {
        let (mut trace, mut statement, commitments) = fixture(false);
        // Preloads have no program event: the original commitments still apply.
        trace.0.splice(
            2..2,
            std::iter::repeat_n(Step::PreloadMethod { method: METHOD }, count - 1),
        );
        statement.inputs[0].methods = vec![METHOD; count];
        statement.outputs[0].methods = vec![METHOD; count];
        trace.0.splice(
            trace.0.len() - 1..trace.0.len() - 1,
            std::iter::repeat_n(Step::ReadAbi { method: METHOD }, count - 1),
        );
        let spec = verifier.verify_transaction(&trace, &statement);
        let circuit = verify_transaction_sat(&trace, 3, &statement, &commitments);
        if count == 255 {
            spec.unwrap();
            circuit.unwrap();
        } else {
            assert!(
                matches!(spec, Err(QuintError::RejectedStep { .. })),
                "{spec:?}"
            );
            assert!(
                matches!(
                    circuit,
                    Err(Error::Unsatisfied(Unsatisfied::Constraint { .. }))
                ),
                "{circuit:?}"
            );
        }
    }
}

fn fixture(consumed: bool) -> (Trace, TransactionStatement, TraceCommitments) {
    fixture_with_handle(consumed, ResourceHandle(7))
}

fn fixture_with_handle(
    consumed: bool,
    handle: ResourceHandle,
) -> (Trace, TransactionStatement, TraceCommitments) {
    let input = StarstreamValue([0xffff_ffff_0000_0000, 1 << 50, (1 << 32) + 1, 0]);
    let output = StarstreamValue([17, 18, 19, 20]);
    let mut trace = Trace::new([
        Step::SetStorage {
            storage: input.clone(),
            resource: handle.into(),
        },
        Step::PreloadMethod { method: METHOD },
        Step::CallMethod {
            resource: handle,
            method: METHOD,
            arguments: UNIT,
            result: UNIT.into(),
        },
        Step::EnterMethod {
            method: METHOD,
            arguments: UNIT,
        },
    ]);
    // Ownership is explicit and independent of normalization: Coord(1)=2, Utxo(0)=1.
    let mut owners = vec![Some(1), None, Some(2), Some(1)];
    if consumed {
        trace.0.push(Step::YieldBegin);
        owners.push(Some(1));
    }
    trace.0.extend([
        Step::Return {
            result: UNIT.into(),
        },
        Step::Return {
            result: UNIT.into(),
        },
        if consumed {
            Step::SkipConsumed
        } else {
            Step::GetStorage {
                storage: output.clone().into(),
            }
        },
        Step::FinishTransaction,
    ]);
    owners.extend([
        Some(1),
        Some(2),
        if consumed { None } else { Some(1) },
        None,
    ]);
    if !consumed {
        trace
            .0
            .insert(trace.0.len() - 1, Step::ReadAbi { method: METHOD });
        owners.insert(owners.len() - 1, None);
    }
    let commitments = expected(&trace, &owners);
    let statement = TransactionStatement {
        inputs: vec![InputUtxo {
            storage: input,
            methods: vec![METHOD],
        }],
        outputs: if consumed {
            vec![]
        } else {
            vec![OutputUtxo {
                utxo: 0,
                storage: output,
                methods: vec![METHOD],
            }]
        },
    };
    (trace, statement, commitments)
}

fn expected(trace: &Trace, owners: &[Option<u32>]) -> TraceCommitments {
    assert_eq!(trace.0.len(), owners.len());
    let mut result = TraceCommitments::new();
    for (step, owner) in trace.0.iter().zip(owners) {
        let blocks = events::encode(step);
        let Some(owner) = owner else {
            assert!(blocks.is_empty());
            continue;
        };
        assert!(!blocks.is_empty());
        let mut chain = result.get(owner).copied().unwrap_or([0; 4]).map(F::new);
        for block in blocks {
            chain = neo_application::event_commitment::commit_block(chain, block.map(F::new));
        }
        result.insert(*owner, chain.map(|x| x.as_canonical_u64()));
    }
    result
}

fn new_utxo_transaction(
    new_methods: &[MethodHash],
) -> (Trace, TransactionStatement, TraceCommitments) {
    // Input UTXO 0 survives; UTXO 1 survives iff its constructor registers an ABI.
    let mut trace = Trace::new([
        Step::SetStorage {
            storage: UNIT,
            resource: ResourceHandle(0).into(),
        },
        Step::PreloadMethod { method: METHOD },
        Step::PreloadMethod { method: METHOD }, // idempotent membership, recorded in the input sequence
        Step::NewUtxo {
            arguments: UNIT,
            resource: ResourceHandle(1).into(),
        },
        Step::EnterConstructor { arguments: UNIT },
    ]);
    let mut owners = vec![Some(1), None, None, Some(2), Some(3)];
    for &method in new_methods {
        trace.0.push(Step::RegisterMethod { method });
        owners.push(Some(3));
    }
    trace.0.extend([
        Step::Return {
            result: UNIT.into(),
        },
        Step::Return {
            result: UNIT.into(),
        },
        Step::GetStorage {
            storage: UNIT.into(),
        },
        Step::ReadAbi { method: METHOD },
        Step::ReadAbi { method: METHOD },
    ]);
    owners.extend([Some(3), Some(2), Some(1), None, None]);
    let mut statement = TransactionStatement {
        inputs: vec![InputUtxo {
            storage: UNIT,
            methods: vec![METHOD, METHOD],
        }],
        outputs: vec![OutputUtxo {
            utxo: 0,
            storage: UNIT,
            methods: vec![METHOD, METHOD],
        }],
    };
    if new_methods.is_empty() {
        trace.0.push(Step::SkipConsumed);
        owners.push(None);
    } else {
        let storage = StarstreamValue([21, 22, 23, 24]);
        trace.0.push(Step::GetStorage {
            storage: storage.clone().into(),
        });
        owners.push(Some(3));
        for &method in new_methods {
            trace.0.push(Step::ReadAbi { method });
            owners.push(None);
        }
        statement.outputs.push(OutputUtxo {
            utxo: 1,
            storage,
            methods: new_methods.to_vec(),
        });
    }
    trace.0.push(Step::FinishTransaction);
    owners.push(None);
    let commitments = expected(&trace, &owners);
    (trace, statement, commitments)
}

fn no_input_transaction() -> (Trace, TransactionStatement, TraceCommitments) {
    let trace = Trace::new([
        Step::Return {
            result: UNIT.into(),
        },
        Step::FinishTransaction,
    ]);
    let commitments = expected(&trace, &[Some(2), None]);
    (trace, TransactionStatement::default(), commitments)
}

fn positive_cases() -> Vec<(Trace, TransactionStatement, TraceCommitments)> {
    vec![
        fixture(false),
        fixture_with_handle(false, ResourceHandle(19)),
        fixture(true),
        new_utxo_transaction(&[]),
        new_utxo_transaction(&[OTHER, METHOD, OTHER]),
        no_input_transaction(),
        replaced_abi_transaction(),
    ]
}

fn replaced_abi_transaction() -> (Trace, TransactionStatement, TraceCommitments) {
    let (mut trace, mut statement, _) = fixture(false);
    trace.0.splice(
        4..4,
        [
            Step::YieldBegin,
            Step::RegisterMethod { method: OTHER },
            Step::RegisterMethod { method: METHOD },
            Step::RegisterMethod { method: OTHER },
        ],
    );
    let read = trace
        .0
        .iter()
        .position(|s| matches!(s, Step::ReadAbi { .. }))
        .unwrap();
    trace.0.splice(
        read..read + 1,
        [
            Step::ReadAbi { method: OTHER },
            Step::ReadAbi { method: METHOD },
            Step::ReadAbi { method: OTHER },
        ],
    );
    statement.outputs[0].methods = vec![OTHER, METHOD, OTHER];
    let commitments = expected(
        &trace,
        &[
            Some(1),
            None,
            Some(2),
            Some(1), // load, preload, call, enter
            Some(1),
            Some(1),
            Some(1),
            Some(1), // yield and registrations
            Some(1),
            Some(2),
            Some(1), // returns and storage
            None,
            None,
            None,
            None, // ABI reads and finish
        ],
    );
    (trace, statement, commitments)
}

#[test]
fn input_handle_choice_is_not_part_of_transaction_io() {
    let (first, first_statement, first_commitments) = fixture(false);
    let (second, second_statement, second_commitments) =
        fixture_with_handle(false, ResourceHandle(19));
    assert_eq!(first_statement, second_statement);
    // The coordinator's call event still commits to the handle it actually uses.
    assert_ne!(first_commitments, second_commitments);
    verify_transaction_sat(&first, 3, &first_statement, &first_commitments).unwrap();
    verify_transaction_sat(&second, 3, &first_statement, &second_commitments).unwrap();
}

#[test]
fn storage_transaction_accepts_surviving_and_consumed_outputs() {
    for (trace, statement, commitments) in positive_cases() {
        for size in [1, 3, 8] {
            verify_transaction_sat(&trace, size, &statement, &commitments).unwrap();
        }
    }
}

fn rejects(
    name: &str,
    trace: &Trace,
    statement: &TransactionStatement,
    commitments: &TraceCommitments,
) {
    for size in [1, 3, 8] {
        assert!(
            matches!(
                verify_transaction_sat(trace, size, statement, commitments),
                Err(Error::Unsatisfied(reason)) if !matches!(reason, Unsatisfied::TraceCommitments)
            ),
            "{name}: batch size {size}"
        );
    }
}

fn negative_cases() -> Vec<(&'static str, Trace, TransactionStatement, TraceCommitments)> {
    let (trace, statement, commitments) = fixture(false);
    let mut cases = vec![];
    let read = trace
        .0
        .iter()
        .position(|s| matches!(s, Step::ReadAbi { .. }))
        .unwrap();
    let mut missing_read = trace.clone();
    missing_read.0.remove(read);
    cases.push((
        "missing ABI read",
        missing_read,
        statement.clone(),
        commitments.clone(),
    ));
    let mut extra_read = trace.clone();
    extra_read.0.insert(read, Step::ReadAbi { method: METHOD });
    cases.push((
        "extra ABI read",
        extra_read,
        statement.clone(),
        commitments.clone(),
    ));
    let mut early_read = trace.clone();
    early_read.0.swap(read, read - 1);
    cases.push((
        "ABI read before storage",
        early_read,
        statement.clone(),
        commitments.clone(),
    ));
    let mut wrong_output = statement.clone();
    wrong_output.outputs[0].methods = vec![OTHER];
    cases.push((
        "wrong output ABI",
        trace.clone(),
        wrong_output,
        commitments.clone(),
    ));
    let mut empty_output = statement.clone();
    empty_output.outputs[0].methods.clear();
    cases.push((
        "empty output ABI",
        trace.clone(),
        empty_output,
        commitments.clone(),
    ));
    let (replaced, replaced_statement, replaced_commitments) = replaced_abi_transaction();
    let read = replaced
        .0
        .iter()
        .position(|s| matches!(s, Step::ReadAbi { .. }))
        .unwrap();
    let mut reordered = replaced.clone();
    reordered.0.swap(read, read + 1);
    let mut reordered_statement = replaced_statement.clone();
    reordered_statement.outputs[0].methods.swap(0, 1);
    cases.push((
        "reordered ABI with matching statement",
        reordered,
        reordered_statement,
        replaced_commitments.clone(),
    ));
    let mut stale = replaced;
    stale.0[read] = Step::ReadAbi { method: METHOD };
    let mut stale_statement = replaced_statement;
    stale_statement.outputs[0].methods[0] = METHOD;
    cases.push((
        "old generation output ABI",
        stale,
        stale_statement,
        replaced_commitments,
    ));
    for (name, index, replacement) in [
        (
            "execute before inputs loaded",
            0,
            Step::Return {
                result: UNIT.into(),
            },
        ),
        (
            "load during execution",
            3,
            Step::SetStorage {
                storage: UNIT,
                resource: ResourceHandle(8).into(),
            },
        ),
        (
            "preload during execution",
            3,
            Step::PreloadMethod { method: OTHER },
        ),
        (
            "export during execution",
            3,
            Step::GetStorage {
                storage: UNIT.into(),
            },
        ),
        ("skip live output", 6, Step::SkipConsumed),
        ("omit live output", 6, Step::FinishTransaction),
        ("skip during execution", 3, Step::SkipConsumed),
        ("finish during execution", 3, Step::FinishTransaction),
        (
            "execute after finalization",
            7,
            Step::Return {
                result: UNIT.into(),
            },
        ),
    ] {
        let mut changed = trace.clone();
        changed.0[index] = replacement;
        cases.push((name, changed, statement.clone(), commitments.clone()));
    }
    let mut missing = trace.clone();
    missing.0.remove(1);
    cases.push((
        "missing input ABI",
        missing,
        statement.clone(),
        commitments.clone(),
    ));
    let mut early_preload = trace.clone();
    early_preload.0.swap(0, 1);
    cases.push((
        "preload before loading",
        early_preload,
        statement.clone(),
        commitments.clone(),
    ));
    let mut stale = trace.clone();
    stale.0.splice(
        4..4,
        [Step::YieldBegin, Step::RegisterMethod { method: OTHER }],
    );
    stale.0.splice(
        7..7,
        [
            Step::CallMethod {
                resource: ResourceHandle(7),
                method: METHOD,
                arguments: UNIT,
                result: UNIT.into(),
            },
            Step::EnterMethod {
                method: METHOD,
                arguments: UNIT,
            },
            Step::Return {
                result: UNIT.into(),
            },
        ],
    );
    cases.push((
        "preloaded ABI invalidated by resume",
        stale,
        statement.clone(),
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.inputs[0].storage.0[0] = 0;
    cases.push((
        "wrong input storage",
        trace.clone(),
        changed,
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.inputs.push(changed.inputs[0].clone());
    cases.push(("omitted input", trace.clone(), changed, commitments.clone()));
    let mut changed = statement.clone();
    changed.inputs.clear();
    cases.push(("excess input", trace.clone(), changed, commitments.clone()));
    let mut changed = trace.clone();
    if let Step::SetStorage { resource, .. } = &mut changed.0[0] {
        *resource = ResourceHandle(9).into();
    }
    cases.push((
        "call through an unbound input handle",
        changed,
        statement.clone(),
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.inputs[0].methods = vec![OTHER];
    cases.push((
        "undeclared preload",
        trace.clone(),
        changed,
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.inputs[0].methods.push(OTHER);
    cases.push((
        "missing declared method",
        trace.clone(),
        changed,
        commitments.clone(),
    ));
    let mut reordered = trace.clone();
    reordered.0.insert(2, Step::PreloadMethod { method: OTHER });
    let mut reordered_statement = statement.clone();
    reordered_statement.inputs[0].methods = vec![OTHER, METHOD];
    reordered_statement.outputs[0].methods = vec![METHOD, OTHER];
    reordered
        .0
        .insert(reordered.0.len() - 1, Step::ReadAbi { method: OTHER });
    cases.push((
        "preload sequence order",
        reordered,
        reordered_statement,
        commitments.clone(),
    ));
    let mut duplicate_preload = trace.clone();
    duplicate_preload
        .0
        .insert(2, Step::PreloadMethod { method: METHOD });
    duplicate_preload.0.insert(
        duplicate_preload.0.len() - 1,
        Step::ReadAbi { method: METHOD },
    );
    let mut duplicate_statement = statement.clone();
    duplicate_statement.outputs[0].methods.push(METHOD);
    cases.push((
        "extra duplicate preload",
        duplicate_preload,
        duplicate_statement,
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.outputs[0].storage.0[0] += 1;
    cases.push((
        "wrong output storage",
        trace.clone(),
        changed,
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.outputs[0].utxo = 1;
    cases.push((
        "wrong output ID",
        trace.clone(),
        changed,
        commitments.clone(),
    ));
    let mut changed = statement.clone();
    changed.outputs.clear();
    cases.push((
        "undeclared output",
        trace.clone(),
        changed,
        commitments.clone(),
    ));
    let mut duplicate = trace.clone();
    duplicate.0.insert(7, trace.0[6].clone());
    cases.push((
        "duplicate export",
        duplicate,
        statement.clone(),
        commitments.clone(),
    ));
    let mut changed = trace.clone();
    changed.0.push(Step::GetStorage {
        storage: UNIT.into(),
    });
    cases.push((
        "export after finish",
        changed,
        statement.clone(),
        commitments.clone(),
    ));
    let mut changed = trace.clone();
    changed.0.pop();
    cases.push((
        "missing finish",
        changed,
        statement.clone(),
        commitments.clone(),
    ));
    let (mut consumed, consumed_statement, consumed_commitments) = fixture(true);
    consumed.0[7] = Step::GetStorage {
        storage: UNIT.into(),
    };
    cases.push((
        "export consumed output",
        consumed,
        consumed_statement,
        consumed_commitments,
    ));
    cases
}

#[test]
fn storage_transaction_rejects_invalid_boundaries_and_statements() {
    for (name, trace, statement, commitments) in negative_cases() {
        rejects(name, &trace, &statement, &commitments);
    }
}

#[test]
#[ignore = "requires Quint; run npm test in starstream-interleaving-spec"]
fn transaction_circuit_and_quint_agree() {
    let verifier = QuintVerifier::new().unwrap();
    for (trace, statement, commitments) in positive_cases() {
        verifier.verify_transaction(&trace, &statement).unwrap();
        verify_transaction_sat(&trace, 3, &statement, &commitments).unwrap();
    }
    for (name, trace, statement, commitments) in negative_cases() {
        match verifier.verify_transaction(&trace, &statement) {
            Err(QuintError::RejectedStep { index, .. })
                if matches!(
                    name,
                    "undeclared preload"
                        | "missing declared method"
                        | "preload sequence order"
                        | "extra duplicate preload"
                ) =>
            {
                assert_eq!(
                    index,
                    trace.0.len() - 1,
                    "{name}: ABI binding must be deferred until finish_transaction"
                );
            }
            Err(QuintError::RejectedStep { .. } | QuintError::IncompleteExecution(_)) => {}
            result => panic!("{name}: expected a semantic rejection, got {result:?}"),
        }
        rejects(name, &trace, &statement, &commitments);
    }
}
