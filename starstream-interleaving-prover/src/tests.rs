use neo_application::{ContinuityCheckError, MemoryCheckError};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, StarstreamValue, Step, Trace};

use super::{Error, Unsatisfied, build_witness_rows, verify_sat, verify_witness_rows};
use crate::{
    ccs::layout::{
        COL_CALL_STACK_EXPECTED_ARG_VALUE, COL_CURR_AFTER, COL_CURR_BEFORE,
        COL_NEXT_UTXO_ID_BEFORE, COL_SEL_ENTER_CONSTRUCTOR, range_check_layout,
    },
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

fn minimal_constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ])
}

fn incomplete_constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ])
}

fn repeated_constructor_entry_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
    ])
}

fn method_call_trace(enter_method: bool) -> Trace {
    method_call_result_trace(
        enter_method,
        StarstreamValue::default(),
        StarstreamValue::default(),
    )
}

fn method_call_result_trace(
    enter_method: bool,
    expected_result: StarstreamValue,
    actual_result: StarstreamValue,
) -> Trace {
    let method = MethodHash([1, 1, 1, 1]);
    let method_arguments = StarstreamValue::from(vec![1, 2, 3, 4]);
    let mut steps = vec![
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::YieldBegin,
        Step::RegisterMethod { method },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method,
            arguments: method_arguments.clone(),
            result: expected_result.into(),
        },
    ];

    if enter_method {
        steps.push(Step::EnterMethod {
            method,
            arguments: method_arguments,
        });
    }

    steps.extend([
        Step::Return {
            result: actual_result.into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ]);

    Trace::new(steps)
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
fn accepts_minimal_utxo_constructor() {
    verify_sat(&minimal_constructor_trace([0, 1, 2, 3])).unwrap();
}

#[test]
fn rejects_nonempty_terminal_call_stack() {
    assert!(matches!(
        verify_sat(&incomplete_constructor_trace([0, 1, 2, 3])),
        Err(Error::Unsatisfied(
            Unsatisfied::TerminalCallStackNotEmpty { .. }
        ))
    ));
}

#[test]
fn rejects_repeated_constructor_entry() {
    assert!(matches!(
        verify_sat(&repeated_constructor_entry_trace([0, 1, 2, 3])),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 2,
            constraint: "enter constructor constraints",
            ..
        }))
    ));
}

#[test]
fn accepts_method_call_with_expected_result() {
    verify_sat(&method_call_result_trace(
        true,
        vec![7, 8].into(),
        vec![7, 8].into(),
    ))
    .unwrap();
}

#[test]
fn rejects_enter_method_with_wrong_method() {
    let mut trace = method_call_trace(true);
    trace.0[6] = Step::EnterMethod {
        method: MethodHash([2, 1, 1, 1]),
        arguments: vec![1, 2, 3, 4].into(),
    };

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Memory(
            MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedMethod,
                row: 6,
                ..
            }
        )))
    ));
}

#[test]
fn rejects_return_with_wrong_result() {
    assert!(matches!(
        verify_sat(&method_call_result_trace(
            true,
            vec![7, 8].into(),
            vec![7, 9].into(),
        )),
        Err(Error::Unsatisfied(Unsatisfied::Memory(
            MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedResult,
                row: 7,
                ..
            }
        )))
    ));
}

#[test]
fn rejects_return_without_entering_method() {
    assert!(matches!(
        verify_sat(&method_call_trace(false)),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 6,
            constraint: "return constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_return_before_entering_constructor() {
    let trace = Trace::new([
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
            resource: ResourceHandle(0).into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ]);

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 1,
            constraint: "return constraints",
            ..
        }))
    ));
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
fn rejects_out_of_range_witness_column() {
    let trace = constructor_trace([0, 1, 2, 3]);
    let mut rows = build_witness_rows(&trace);

    rows[0][COL_NEXT_UTXO_ID_BEFORE] = F::new(1 << 32);
    range_check_layout().assign_bits(&mut rows[0]).unwrap();

    assert!(matches!(
        verify_witness_rows(&rows),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 0,
            constraint: "COL_NEXT_UTXO_ID_BEFORE",
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
    range_check_layout().assign_bits(&mut rows[1]).unwrap();

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

#[test]
fn rejects_tampered_continuity_value() {
    let trace = constructor_trace([0, 1, 2, 3]);
    let mut rows = build_witness_rows(&trace);
    verify_witness_rows(&rows).unwrap();

    rows[0][COL_CURR_AFTER] += F::ONE;
    range_check_layout().assign_bits(&mut rows[0]).unwrap();

    assert!(matches!(
        verify_witness_rows(&rows),
        Err(Error::Unsatisfied(Unsatisfied::Continuity(
            ContinuityCheckError::Mismatch {
                boundary: 0,
                group_name: "curr_continuity",
                previous_step_column: COL_CURR_AFTER,
                next_step_column: COL_CURR_BEFORE,
                ..
            }
        )))
    ));
}
