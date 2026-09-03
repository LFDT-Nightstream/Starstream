use neo_application::MemoryCheckError;
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, StarstreamValue, Step, Trace};

use super::{Error, Unsatisfied, build_witness_rows, verify_sat, verify_witness_rows};
use crate::{
    ccs::layout::{COL_CALL_STACK_EXPECTED_ARG_VALUE, COL_SEL_ENTER_CONSTRUCTOR},
    memory::MemoryId,
};

fn constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::YieldBegin,
        Step::RegisterMethod {
            method: MethodHash([1, 1, 1, 1]),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ])
}

#[test]
fn accepts_utxo_constructor() {
    verify_sat(&constructor_trace([0, 1, 2, 3])).unwrap();
}

#[test]
fn accepts_repeated_constructor_arguments() {
    verify_sat(&constructor_trace([7, 7, 7, 7])).unwrap();
}

#[test]
fn rejects_constructor_with_mismatched_arguments() {
    let mut trace = constructor_trace([1, 2, 3, 4]);
    trace.0[1] = Step::EnterConstructor {
        arguments: vec![0, 1, 2, 3].into(),
    };

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Memory(
            MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedArgument,
                row: 1,
                ..
            }
        )))
    ));
}

#[test]
fn rejects_tampered_opcode_selector() {
    let trace = constructor_trace([0, 1, 2, 3]);
    let mut rows = build_witness_rows(&trace);
    verify_witness_rows(&rows).unwrap();

    rows[0][COL_SEL_ENTER_CONSTRUCTOR] = F::ONE;

    assert!(matches!(
        verify_witness_rows(&rows),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 0,
            constraint: "opcode selectors are one-hot",
            ..
        }))
    ));
}

#[test]
fn rejects_tampered_memory_value() {
    let trace = constructor_trace([0, 1, 2, 3]);
    let mut rows = build_witness_rows(&trace);
    verify_witness_rows(&rows).unwrap();

    rows[1][COL_CALL_STACK_EXPECTED_ARG_VALUE[0]] += F::ONE;

    let error = verify_witness_rows(&rows).unwrap_err();
    assert!(
        matches!(
            &error,
            Error::Unsatisfied(Unsatisfied::Memory(MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedArgument,
                row: 1,
                ..
            }))
        ),
        "{error:?}"
    );
}
