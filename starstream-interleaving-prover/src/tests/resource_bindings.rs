use crate::ccs::layout::*;
use crate::{Error, Unsatisfied, build_witness_rows, verify_witness_rows};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, StarstreamValue, Step, Trace};

fn bindings(first_input: bool, second_input: bool, duplicate: bool) -> Trace {
    let mut steps = Vec::new();
    for (index, input) in [first_input, second_input].into_iter().enumerate() {
        let handle = ResourceHandle(if duplicate { 0 } else { index as u32 });
        if input {
            steps.push(Step::SetStorage {
                storage: StarstreamValue::UNIT_VALUE,
                coordinator_handle: handle,
            });
            steps.push(Step::PreloadMethod {
                method: MethodHash([1; 8]),
            });
        } else {
            steps.extend([
                Step::NewUtxo {
                    arguments: StarstreamValue::UNIT_VALUE,
                    resource: handle.into(),
                },
                Step::EnterConstructor {
                    arguments: StarstreamValue::UNIT_VALUE,
                },
                Step::Return {
                    result: StarstreamValue::UNIT_VALUE.into(),
                },
            ]);
        }
    }
    steps.push(Step::Return {
        result: StarstreamValue::UNIT_VALUE.into(),
    });
    Trace::new(steps)
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
                constraint: "resource bindings are fresh",
                ..
            }))
        ));
    }
}

#[test]
fn duplicate_constructor_bindings_fail_across_batch_sizes() {
    for size in [1, 2, 8] {
        crate::verify_sat_batched(&bindings(false, false, false), size).unwrap();
        assert!(crate::verify_sat_batched(&bindings(false, false, true), size).is_err());
    }
}
