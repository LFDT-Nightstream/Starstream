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
        second_write[COL_RESOURCE_RESOLVER_BEFORE] = F::ONE; // Utxo(0)
        assert!(matches!(
            verify_witness_rows(&rows, &preload),
            Err(Error::Unsatisfied(Unsatisfied::Constraint {
                constraint,
                ..
            })) if constraint == "resource bindings are fresh"));
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
            load(StorageKind::Token, 7),
            load(StorageKind::Token, 9),
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
