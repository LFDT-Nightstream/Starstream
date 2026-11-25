use crate::ccs::layout::*;
use crate::tests::CONSTRUCTOR;
use crate::{Error, Unsatisfied, build_witness_rows, verify_witness_rows};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, StarstreamValue, Step, Trace};

fn bindings(first_input: bool, second_input: bool, duplicate: bool) -> Trace {
    // Inputs load before the entry call; constructors run in its body.
    let (mut loading, mut steps) = (Vec::new(), Vec::new());
    for (index, input) in [first_input, second_input].into_iter().enumerate() {
        let handle = ResourceHandle(if duplicate { 0 } else { index as u32 });
        if input {
            loading.push(Step::SetStorage {
                kind: starstream_interleaving_spec::StorageKind::Utxo,
                storage: StarstreamValue::UNIT_VALUE,
                entry_handle: handle,
            });
            loading.push(Step::PreloadMethod {
                method: MethodHash([1; 8]),
            });
        } else {
            steps.extend([
                Step::NewUtxo {
                    constructor: CONSTRUCTOR,
                    arguments: StarstreamValue::UNIT_VALUE,
                },
                Step::EnterConstructor {
                    constructor: CONSTRUCTOR,
                    arguments: StarstreamValue::UNIT_VALUE,
                },
                Step::Return {
                    result: StarstreamValue::UNIT_VALUE.into(),
                },
                Step::ReceiveNewUtxoHandle {
                    resource: handle.into(),
                },
            ]);
        }
    }
    steps.push(Step::Return {
        result: StarstreamValue::UNIT_VALUE.into(),
    });
    crate::tests::loaded(loading, steps)
}

#[test]
fn bindings_require_empty_keys_at_both_write_sites() {
    for (first_input, second_input) in [(false, false), (true, false), (true, true)] {
        let (rows, preload) = build_witness_rows(&bindings(first_input, second_input, false));
        verify_witness_rows(&rows, &preload).unwrap();
        let (mut rows, preload) = build_witness_rows(&bindings(first_input, second_input, true));
        assert!(matches!(
            verify_witness_rows(&rows, &preload),
            Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
        ));

        // Supplying the actual old target must not bypass the freshness rule.
        let second_write = rows
            .iter_mut()
            .filter(|row| row[COL_RESOURCE_RESOLVER_WRITE] == F::ONE)
            .nth(1)
            .unwrap();
        second_write[COL_UNBOUND_VALUE] = F::ONE; // Utxo(0)
        assert!(matches!(
            verify_witness_rows(&rows, &preload),
            Err(Error::Unsatisfied(Unsatisfied::Constraint {
                constraint,
                ..
            })) if constraint == "unbound memory value is zero"));
    }
}

#[test]
fn duplicate_constructor_bindings_fail_across_batch_sizes() {
    for size in [1, 2, 8] {
        crate::verify_sat_batched(&bindings(false, false, false), size, &Default::default())
            .unwrap();
        assert!(
            crate::verify_sat_batched(&bindings(false, false, true), size, &Default::default())
                .is_err()
        );
    }
}

#[test]
fn storage_loads_bind_each_kind_in_its_own_index_space() {
    use crate::CoroutineId::{Coord, Token, Utxo};
    use starstream_interleaving_spec::StorageKind;

    let load = |kind, handle| Step::SetStorage {
        kind,
        storage: StarstreamValue([1, 2, 3, 4]),
        entry_handle: ResourceHandle(handle),
    };
    let trace = crate::tests::loaded(
        [
            load(StorageKind::Utxo, 8),
            Step::PreloadMethod {
                method: MethodHash([1; 8]),
            },
            load(StorageKind::Token { interface: 2 }, 7),
            load(StorageKind::Token { interface: 2 }, 9),
        ],
        [Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        }],
    );
    let roots = crate::commitment::tests::expected(
        &trace,
        &[Utxo(0), Token(0), Token(1), Coord(1), Coord(1)],
    );
    crate::verify_sat_with_commitments(&trace, 3, &roots, &Default::default()).unwrap();
    let (rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();
    for (row, (target, handle)) in rows
        .iter()
        .filter(|row| row[COL_SEL_SET_STORAGE] == F::ONE)
        .zip([(Utxo(0), 8), (Token(0), 7), (Token(1), 9)])
    {
        assert_eq!(row[COL_RESOURCE_RESOLVER_ADDR_CID], Coord(1).field());
        assert_eq!(row[COL_RESOURCE_RESOLVER_ADDR_HANDLE], F::new(handle));
        assert_eq!(row[COL_RESOURCE_RESOLVER_VALUE], target.field());
        assert_eq!(row[COL_EVENT_OWNER], target.field());
    }

    let mut duplicate = trace.clone();
    duplicate.0[0] = load(StorageKind::Utxo, 7);
    assert!(crate::verify_sat(&duplicate, &Default::default()).is_err());

    let mut late_utxo = trace.clone();
    late_utxo.0[..3].rotate_right(1);
    assert!(crate::verify_sat(&late_utxo, &Default::default()).is_err());

    let mut late_method = trace.clone();
    late_method.0.insert(3, trace.0[1].clone());
    assert!(crate::verify_sat(&late_method, &Default::default()).is_err());

    let mut missing_abi = trace;
    missing_abi.0.remove(1);
    assert!(crate::verify_sat(&missing_abi, &Default::default()).is_err());
}

#[test]
fn attach_resolves_both_handles_and_requires_a_detached_token() {
    use crate::CoroutineId::{Token, Utxo};
    use starstream_interleaving_spec::{Out, StorageKind};
    let unit = StarstreamValue::UNIT_VALUE;
    let load = |kind, handle| Step::SetStorage {
        kind,
        entry_handle: ResourceHandle(handle),
        storage: unit.clone(),
    };
    let done = Step::Return {
        result: Out(unit.clone()),
    };
    let attach = Step::Attach {
        token: ResourceHandle(8),
        destination: ResourceHandle(9),
    };
    let enter = Step::EnterAttach {
        interface: 3,
        destination: ResourceHandle(42),
    };
    let receive = Step::ReceiveCallResult {
        result: Out(unit.clone()),
    };
    let upcast = Step::Upcast {
        source: ResourceHandle(7),
        result: Out(ResourceHandle(9)),
    };
    let trace = crate::tests::loaded(
        [
            load(StorageKind::Utxo, 7),
            Step::PreloadMethod {
                method: MethodHash([1; 8]),
            },
            load(StorageKind::Token { interface: 3 }, 8),
        ],
        [
            upcast.clone(),
            attach.clone(),
            enter.clone(),
            done.clone(),
            receive.clone(),
            done.clone(),
        ],
    );
    crate::verify_sat(&trace, &Default::default()).unwrap();
    let (rows, preload) = build_witness_rows(&trace);
    let index = |step: &Step| trace.0.iter().position(|s| s == step).unwrap();
    let call = index(&attach);
    // The execution fixture starts detached. With the same trace and a ledger
    // input, the attach write must see the packed owner rather than zero.
    let (_, mut statement) = crate::transaction::tests::fixture();
    statement.inputs[0].storage = unit.clone();
    statement.outputs[0].storage = unit.clone();
    statement
        .input_tokens
        .push(starstream_interleaving_spec::InputToken {
            owner: 0,
            storage: unit.clone(),
        });
    statement
        .output_tokens
        .push(starstream_interleaving_spec::OutputToken {
            token: 0,
            owner: 0,
            storage: unit.clone(),
        });
    assert!(matches!(
        crate::verify_transaction_sat(&trace, 1, &statement, &Default::default()),
        Err(Error::Unsatisfied(Unsatisfied::Memory(neo_application::MemoryCheckError::ReadModifyWriteMismatch {
            memory: crate::memory::MemoryId::TokenOwner, address, row, expected, actual: 0,
        }))) if row == call && address == [Token(0).encoded()] && expected == Utxo(0).encoded()
    ));
    let entry = index(&enter);
    assert_eq!(rows[call][COL_CALL_TARGET], Token(0).field());
    assert_eq!(rows[call][COL_RESOLVER_SECOND_VALUE], Utxo(0).field());
    assert_eq!(
        rows[entry][COL_RESOURCE_RESOLVER_ADDR_CID],
        Token(0).field()
    );
    assert_eq!(rows[entry][COL_RESOURCE_RESOLVER_ADDR_HANDLE], F::new(42));
    assert_eq!(rows[entry][COL_RESOURCE_RESOLVER_VALUE], Utxo(0).field());

    for (row, column, value) in [
        (call, COL_RESOLVER_SECOND_VALUE, Utxo(1).field()),
        (entry, COL_RESOURCE_RESOLVER_VALUE, Utxo(1).field()),
    ] {
        let mut changed = rows.clone();
        changed[row][column] = value;
        assert!(verify_witness_rows(&changed, &preload).is_err());
    }
    for (position, replacement) in [
        (
            index(&upcast),
            Step::Upcast {
                source: ResourceHandle(99),
                result: Out(ResourceHandle(9)),
            },
        ),
        (
            call,
            Step::Attach {
                token: ResourceHandle(7),
                destination: ResourceHandle(9),
            },
        ),
        (
            call,
            Step::Attach {
                token: ResourceHandle(8),
                destination: ResourceHandle(8),
            },
        ),
        (
            entry,
            Step::EnterAttach {
                interface: 4,
                destination: ResourceHandle(42),
            },
        ),
    ] {
        let mut changed = trace.clone();
        changed.0[position] = replacement;
        assert!(crate::verify_sat(&changed, &Default::default()).is_err());
    }

    let mut non_unit = trace.clone();
    let result = Out(StarstreamValue([1, 0, 0, 0]));
    non_unit.0[entry + 1] = Step::Return {
        result: result.clone(),
    };
    non_unit.0[entry + 2] = Step::ReceiveCallResult { result };
    assert!(crate::verify_sat(&non_unit, &Default::default()).is_err());

    // Use a fresh callback handle so only the token's existing ownership conflicts.
    let mut twice = trace;
    let last = twice.0.len() - 1;
    twice.0.splice(
        last..last,
        [
            attach,
            Step::EnterAttach {
                interface: 3,
                destination: ResourceHandle(43),
            },
            done,
            receive,
        ],
    );
    assert!(crate::verify_sat(&twice, &Default::default()).is_err());
}

#[test]
fn token_finalization_reads_the_owner_written_by_attach() {
    use crate::CoroutineId::{Token, Utxo};
    use starstream_interleaving_spec::{OutputToken, StorageKind};
    let unit = StarstreamValue::UNIT_VALUE;
    let method = MethodHash([1; 8]);
    let done = Step::Return {
        result: unit.clone().into(),
    };
    let trace = crate::tests::loaded(
        [
            Step::SetStorage {
                kind: StorageKind::Utxo,
                entry_handle: ResourceHandle(7),
                storage: unit.clone(),
            },
            Step::PreloadMethod { method },
            Step::SetStorage {
                kind: StorageKind::Token { interface: 3 },
                entry_handle: ResourceHandle(8),
                storage: unit.clone(),
            },
        ],
        [
            Step::Attach {
                token: ResourceHandle(8),
                destination: ResourceHandle(7),
            },
            Step::EnterAttach {
                interface: 3,
                destination: ResourceHandle(42),
            },
            done.clone(),
            Step::ReceiveCallResult {
                result: unit.clone().into(),
            },
            done,
            Step::GetStorage {
                storage: unit.clone().into(),
            },
            Step::ReadAbi { method },
            Step::GetTokenStorage {
                interface: 3,
                storage: unit.clone().into(),
            },
            Step::FinalizeCoordinator,
            Step::FinishTransaction,
        ],
    );
    let getter = trace
        .0
        .iter()
        .position(|step| matches!(step, Step::GetTokenStorage { .. }))
        .unwrap();
    // Execution-only preload leaves the token detached. Only Attach can supply the owner.
    let check = |owner| {
        let output = OutputToken {
            token: 0,
            owner,
            storage: unit.clone(),
        };
        let normalized = crate::step::normalize_with_inputs(&trace, &Default::default(), &[output]);
        let preload = crate::memory::preload_tables(&normalized.method_table);
        let rows = normalized
            .steps
            .iter()
            .map(crate::witness::build_witness_vector)
            .collect::<Vec<_>>();
        verify_witness_rows(&rows, &preload)
    };
    check(0).unwrap();
    assert!(matches!(check(1),
        Err(Error::Unsatisfied(Unsatisfied::Memory(neo_application::MemoryCheckError::ReadMismatch {
            memory: crate::memory::MemoryId::TokenOwner, address, row, expected, actual,
        }))) if row == getter && address == [Token(0).encoded()] && expected == Utxo(0).encoded() && actual == Utxo(1).encoded()
    ));
}
