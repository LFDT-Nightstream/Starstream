use neo_math::F;
use p3_field::PrimeField64;
use starstream_interleaving_prover::CoroutineId::{Coord, Utxo};
use starstream_interleaving_prover::{
    CoroutineId, Error, TraceCommitments, Unsatisfied, verify_transaction_sat,
};
use starstream_interleaving_spec::{
    EntryCall, InputUtxo, MethodHash, OutputUtxo, QuintError, QuintVerifier, ResourceHandle,
    StarstreamValue, Step, Trace, TransactionStatement, events,
};

/// The entry script call that every fixture runs, with a non-unit argument root.
const ENTRY: EntryCall = EntryCall {
    script: MethodHash([9, 0, 10, 0, 11, 0, 12, 0]),
    arguments: StarstreamValue([31, 32, 33, 34]),
};

/// The host's entry call and the entry coordinator's matching entry event.
fn entry_steps() -> [Step; 2] {
    [
        Step::BeginExecution {
            script: ENTRY.script,
            arguments: ENTRY.arguments,
        },
        Step::EnterCall {
            method: ENTRY.script,
            arguments: ENTRY.arguments,
        },
    ]
}

const METHOD: MethodHash = MethodHash([1, 0, 2, 0, 3, 0, 4, 0]);
const CONSTRUCTOR: MethodHash = MethodHash([0xc; 8]);
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
            trace.0.len() - 2..trace.0.len() - 2,
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
            entry_utxo_handle: handle,
        },
        Step::PreloadMethod { method: METHOD },
    ]);
    trace.0.extend(entry_steps());
    trace.0.extend([
        Step::CallMethod {
            resource: handle,
            method: METHOD,
            arguments: UNIT,
        },
        Step::EnterCall {
            method: METHOD,
            arguments: UNIT,
        },
    ]);
    // Ownership is explicit and independent of normalization.
    // The entry call is a host step; the entry event belongs to Coord(1).
    let mut owners = vec![
        Some(Utxo(0)),
        None,
        None,
        Some(Coord(1)),
        Some(Coord(1)),
        Some(Utxo(0)),
    ];
    if consumed {
        trace.0.push(Step::YieldBegin);
        owners.push(Some(Utxo(0)));
    }
    trace.0.extend([
        Step::Return {
            result: UNIT.into(),
        },
        Step::ReceiveCallResult {
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
        Step::FinalizeCoordinator,
        Step::FinishTransaction,
    ]);
    owners.extend([
        Some(Utxo(0)),
        Some(Coord(1)),
        Some(Coord(1)),
        if consumed { None } else { Some(Utxo(0)) },
        None,
        None,
    ]);
    if !consumed {
        trace
            .0
            .insert(trace.0.len() - 2, Step::ReadAbi { method: METHOD });
        owners.insert(owners.len() - 2, None);
    }
    let commitments = expected(&trace, &owners);
    let statement = TransactionStatement {
        input_tokens: Vec::new(),
        output_tokens: Vec::new(),
        signers: Default::default(),
        entry: ENTRY,
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

fn expected(trace: &Trace, owners: &[Option<CoroutineId>]) -> TraceCommitments {
    assert_eq!(trace.0.len(), owners.len());
    let mut result = TraceCommitments::new();
    for (step, owner) in trace.0.iter().zip(owners) {
        let blocks = events::encode(step);
        let Some(owner) = owner else {
            assert!(blocks.is_empty());
            continue;
        };
        assert!(!blocks.is_empty());
        let owner = owner.encoded();
        let mut chain = result.get(&owner).copied().unwrap_or([0; 4]).map(F::new);
        for block in blocks {
            chain = neo_application::event_commitment::commit_block(chain, block.map(F::new));
        }
        result.insert(owner, chain.map(|x| x.as_canonical_u64()));
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
            entry_utxo_handle: ResourceHandle(0),
        },
        Step::PreloadMethod { method: METHOD },
        Step::PreloadMethod { method: METHOD }, // idempotent membership, recorded in the input sequence
    ]);
    trace.0.extend(entry_steps());
    trace.0.extend([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: UNIT,
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: UNIT,
        },
    ]);
    let mut owners = vec![
        Some(Utxo(0)),
        None,
        None,
        None,
        Some(Coord(1)),
        Some(Coord(1)),
        Some(Utxo(1)),
    ];
    for &method in new_methods {
        trace.0.push(Step::RegisterMethod { method });
        owners.push(Some(Utxo(1)));
    }
    trace.0.extend([
        Step::Return {
            result: UNIT.into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(1).into(),
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
    owners.extend([
        Some(Utxo(1)),
        Some(Coord(1)),
        Some(Coord(1)),
        Some(Utxo(0)),
        None,
        None,
    ]);
    let mut statement = TransactionStatement {
        input_tokens: Vec::new(),
        output_tokens: Vec::new(),
        signers: Default::default(),
        entry: ENTRY,
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
        owners.push(Some(Utxo(1)));
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
    trace.0.push(Step::FinalizeCoordinator);
    owners.push(None);
    trace.0.push(Step::FinishTransaction);
    owners.push(None);
    let commitments = expected(&trace, &owners);
    (trace, statement, commitments)
}

fn no_input_transaction() -> (Trace, TransactionStatement, TraceCommitments) {
    let mut trace = Trace::new(entry_steps());
    trace.0.extend([
        Step::Return {
            result: UNIT.into(),
        },
        Step::FinalizeCoordinator,
        Step::FinishTransaction,
    ]);
    let commitments = expected(&trace, &[None, Some(Coord(1)), Some(Coord(1)), None, None]);
    let statement = TransactionStatement {
        input_tokens: Vec::new(),
        output_tokens: Vec::new(),
        signers: Default::default(),
        entry: ENTRY,
        inputs: vec![],
        outputs: vec![],
    };
    (trace, statement, commitments)
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
        6..6,
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
            Some(Utxo(0)),
            None, // load, preload
            None,
            Some(Coord(1)), // entry call and event
            Some(Coord(1)),
            Some(Utxo(0)), // call, enter
            Some(Utxo(0)),
            Some(Utxo(0)),
            Some(Utxo(0)),
            Some(Utxo(0)), // yield and registrations
            Some(Utxo(0)),
            Some(Coord(1)),
            Some(Coord(1)),
            Some(Utxo(0)), // returns and storage
            None,
            None,
            None,
            None,
            None, // ABI reads, coordinator and finish
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

fn is_semantic_rejection(result: &Result<(), Error>) -> bool {
    matches!(result, Err(Error::Unsatisfied(reason)) if !matches!(reason,
        Unsatisfied::TraceCommitments | Unsatisfied::TransactionCommitment))
}

fn rejects(
    name: &str,
    trace: &Trace,
    statement: &TransactionStatement,
    commitments: &TraceCommitments,
) {
    for size in [1, 3, 8] {
        assert!(
            is_semantic_rejection(&verify_transaction_sat(trace, size, statement, commitments)),
            "{name}: batch size {size}"
        );
    }
}

#[test]
fn transaction_digest_rejects_wrong_instance_roots() {
    for consumed in [false, true] {
        let (trace, statement, roots) = fixture(consumed);
        for id in roots.keys() {
            let mut changed = roots.clone();
            changed.get_mut(id).unwrap()[0] ^= 1;
            // Direct storage/ABI checks still agree. Transaction verification
            // must reject through the aggregate digest, without a direct root comparison.
            let result = verify_transaction_sat(&trace, 3, &statement, &changed);
            assert!(
                !is_semantic_rejection(&result),
                "stale roots must not count as semantic rejection"
            );
            assert!(matches!(
                result,
                Err(Error::Unsatisfied(Unsatisfied::TransactionCommitment))
            ));
        }
    }
}

fn negative_cases() -> Vec<(&'static str, Trace, TransactionStatement, TraceCommitments)> {
    let (trace, statement, commitments) = fixture(false);
    let mut cases = vec![];
    let mut without_coordinator = trace.clone();
    without_coordinator
        .0
        .retain(|step| !matches!(step, Step::FinalizeCoordinator));
    cases.push((
        "missing coordinator finalization",
        without_coordinator.clone(),
        statement.clone(),
        commitments.clone(),
    ));
    let mut duplicate = trace.clone();
    // Add another finalization immediately before FinishTransaction.
    duplicate
        .0
        .insert(duplicate.0.len() - 1, Step::FinalizeCoordinator);
    cases.push((
        "duplicate coordinator finalization",
        duplicate,
        statement.clone(),
        commitments.clone(),
    ));
    for (name, index) in [
        ("coordinator finalized during loading", 2),
        ("coordinator finalized before the entry event", 3),
        ("coordinator finalized during execution", 5),
        ("coordinator finalized before outputs", 8),
        ("coordinator finalized before ABI completion", 9),
    ] {
        let mut early = without_coordinator.clone();
        early.0.insert(index, Step::FinalizeCoordinator);
        cases.push((name, early, statement.clone(), commitments.clone()));
    }
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
            5,
            Step::SetStorage {
                storage: UNIT,
                entry_utxo_handle: ResourceHandle(8),
            },
        ),
        (
            "preload during execution",
            5,
            Step::PreloadMethod { method: OTHER },
        ),
        (
            "export during execution",
            5,
            Step::GetStorage {
                storage: UNIT.into(),
            },
        ),
        ("skip live output", 8, Step::SkipConsumed),
        ("omit live output", 8, Step::FinishTransaction),
        ("skip during execution", 5, Step::SkipConsumed),
        ("finish during execution", 5, Step::FinishTransaction),
        (
            "execute after finalization",
            9,
            Step::Return {
                result: UNIT.into(),
            },
        ),
    ] {
        let mut changed = trace.clone();
        changed.0[index] = replacement;
        if matches!(changed.0[index], Step::FinishTransaction) {
            changed.0.insert(index, Step::FinalizeCoordinator);
        }
        cases.push((name, changed, statement.clone(), commitments.clone()));
    }
    // The entry call (step 2) and its entry event (step 3).
    let mut without_entry = trace.clone();
    without_entry.0.remove(2);
    cases.push((
        "execution without the entry call",
        without_entry,
        statement.clone(),
        commitments.clone(),
    ));
    let mut without_entry_event = trace.clone();
    without_entry_event.0.remove(3);
    cases.push((
        "entry body without its entry event",
        without_entry_event,
        statement.clone(),
        commitments.clone(),
    ));
    let mut repeated_entry = trace.clone();
    repeated_entry.0.insert(4, trace.0[2].clone());
    cases.push((
        "repeated entry call",
        repeated_entry,
        statement.clone(),
        commitments.clone(),
    ));
    let mut early_entry = trace.clone();
    early_entry.0.swap(1, 2);
    cases.push((
        "entry call before the input ABI is loaded",
        early_entry,
        statement.clone(),
        commitments.clone(),
    ));
    for (name, entry) in [
        (
            "statement names another entry script",
            EntryCall {
                script: OTHER,
                ..ENTRY
            },
        ),
        (
            "statement carries other entry arguments",
            EntryCall {
                arguments: UNIT,
                ..ENTRY
            },
        ),
    ] {
        cases.push((
            name,
            trace.clone(),
            TransactionStatement {
                entry,
                ..statement.clone()
            },
            commitments.clone(),
        ));
    }
    let mut mismatched_event = trace.clone();
    mismatched_event.0[3] = Step::EnterCall {
        method: OTHER,
        arguments: ENTRY.arguments,
    };
    cases.push((
        "entry event names another script",
        mismatched_event,
        statement.clone(),
        commitments.clone(),
    ));
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
        6..6,
        [Step::YieldBegin, Step::RegisterMethod { method: OTHER }],
    );
    stale.0.splice(
        9..9,
        [
            Step::CallMethod {
                resource: ResourceHandle(7),
                method: METHOD,
                arguments: UNIT,
            },
            Step::EnterCall {
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
    if let Step::SetStorage {
        entry_utxo_handle, ..
    } = &mut changed.0[0]
    {
        *entry_utxo_handle = ResourceHandle(9);
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
        .insert(reordered.0.len() - 2, Step::ReadAbi { method: OTHER });
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
        duplicate_preload.0.len() - 2,
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
    duplicate.0.insert(9, trace.0[8].clone());
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
    consumed.0[9] = Step::GetStorage {
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

#[test]
#[ignore = "requires Quint; run through npm test"]
fn signer_and_token_statement_matches_quint() {
    use starstream_interleaving_spec::{InputToken, OutputToken, StarstreamValue};
    let verifier = QuintVerifier::new().unwrap();
    let (mut trace, mut statement, _) = fixture(false);
    trace.0.insert(4, Step::AssertSignedBy { key: 17 });
    statement.signers.insert(17);
    let storage = StarstreamValue([1, 2, 3, 4]);
    statement.input_tokens.push(InputToken {
        owner: 0,
        storage: storage.clone(),
    });
    statement.output_tokens.push(OutputToken {
        token: 0,
        owner: 0,
        storage,
    });
    verifier.verify_transaction(&trace, &statement).unwrap();
    statement.signers.clear();
    assert!(matches!(
        verifier.verify_transaction(&trace, &statement),
        Err(QuintError::RejectedStep { .. })
    ));
    statement.signers.insert(17);
    statement.output_tokens[0].token = 1;
    assert!(matches!(
        verifier.verify_transaction(&trace, &statement),
        Err(QuintError::IncompleteExecution(_))
    ));
}
