mod padding {
    use super::*;
    use crate::{
        ccs::layout::*, memory::build_memory_layout, step::normalize, witness::build_witness_vector,
    };
    use neo_application::MemoryPortActivation;

    #[test]
    fn memory_gates_exclude_padding_structurally() {
        // Explicitly reviewed derived gates, each constrained to execution
        // selectors (or globally zero). New activation schemes need review.
        let derived = [
            COL_CALL_STACK_PUSH,
            COL_CALL_STACK_POP,
            COL_CALL_STACK_TOP,
            COL_METHOD_LOOKUP,
            COL_RESOURCE_RESOLVER_READ,
            COL_RESOURCE_RESOLVER_WRITE,
            COL_UTXO_LIFECYCLE_READ,
            COL_UTXO_LIFECYCLE_WRITE,
        ];
        for memory in build_memory_layout().entries() {
            for port in &memory.ports {
                let excludes_padding = match port.activation {
                    MemoryPortActivation::Always => false,
                    MemoryPortActivation::When(gate) => {
                        crate::opcode::Opcode::all()
                            .iter()
                            .any(|op| op.is_execution() && op.selector() == gate)
                            || derived.contains(&gate)
                    }
                    MemoryPortActivation::Unless(gate) => gate == COL_SEL_PADDING,
                };
                assert!(
                    excludes_padding,
                    "unreviewed padding activation: {:?}: {port:?}",
                    memory.id
                );
            }
        }
    }

    #[test]
    fn padding_does_not_access_memory() {
        let normalized = normalize(&method_call_trace(true));
        let preload = crate::memory::preload_tables(&normalized.method_table);
        let memory = build_memory_layout();
        let mut rows = Vec::new();
        for step in normalized.steps {
            let mut pad = build_witness_vector(&step.padding_after());
            // Assignment regression check; the structural test above audits
            // which selector-derived gates the memory layout may use.
            for entry in memory.entries() {
                for port in &entry.ports {
                    let active = match port.activation {
                        MemoryPortActivation::Always => true,
                        MemoryPortActivation::When(gate) => pad[gate] != F::ZERO,
                        MemoryPortActivation::Unless(gate) => pad[gate] == F::ZERO,
                    };
                    assert!(!active, "padding activates {:?}: {port:?}", entry.id);
                }
            }
            // Unused commitment buses need not match RAM contents.
            for column in COL_IN.into_iter().chain(COL_OUT) {
                pad[column] = F::new(7);
            }
            rows.extend([build_witness_vector(&step), pad]);
        }
        verify_witness_rows(&rows, &preload).unwrap();
    }
}

use neo_application::{ContinuityCheckError, MemoryCheckError};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, StarstreamValue, Step, Trace};

use super::{Error, Unsatisfied, build_witness_rows, verify_sat, verify_witness_rows};
use crate::{
    ccs::layout::{
        COL_CALL_STACK_EXPECTED_ARG_VALUE, COL_NEXT_UTXO_ID_AFTER, COL_NEXT_UTXO_ID_BEFORE,
        COL_SEL_ENTER_CONSTRUCTOR, COL_UTXO_LIFECYCLE_ADDR, range_check_layout,
    },
    memory::MemoryId,
};

pub(super) fn constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
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

fn opcode_after_terminal_return_trace() -> Trace {
    let mut trace = minimal_constructor_trace([0, 1, 2, 3]);
    trace.0.extend([
        Step::NewUtxo {
            arguments: vec![7].into(),
            resource: ResourceHandle(1).into(),
        },
        Step::EnterConstructor {
            arguments: vec![7].into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ]);
    trace
}

pub(super) fn method_call_trace(enter_method: bool) -> Trace {
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

fn repeated_method_call_without_resume_trace() -> Trace {
    let method = MethodHash([1, 1, 1, 1]);
    let arguments = StarstreamValue::from(vec![1, 2, 3, 4]);
    let mut trace = method_call_trace(true);
    let final_coordinator_return = trace.0.len() - 1;

    trace.0.splice(
        final_coordinator_return..final_coordinator_return,
        [
            Step::CallMethod {
                resource: ResourceHandle(0),
                method,
                arguments: arguments.clone(),
                result: StarstreamValue::default().into(),
            },
            Step::EnterMethod { method, arguments },
            Step::Return {
                result: StarstreamValue::default().into(),
            },
        ],
    );

    trace
}

fn unregistered_method_call_trace() -> Trace {
    let mut trace = method_call_trace(true);
    let unregistered_method = MethodHash([2, 1, 1, 1]);

    let Step::CallMethod { method, .. } = &mut trace.0[4] else {
        panic!("method-call trace has a call at step 4");
    };
    *method = unregistered_method;

    let Step::EnterMethod { method, .. } = &mut trace.0[5] else {
        panic!("method-call trace enters the method at step 5");
    };
    *method = unregistered_method;

    trace
}

fn duplicate_method_registration_trace() -> Trace {
    let mut trace = constructor_trace([0, 1, 2, 3]);
    trace.0.insert(
        3,
        Step::RegisterMethod {
            method: MethodHash([1, 1, 1, 1]),
        },
    );
    trace
}

fn method_reyield_trace(final_method: MethodHash) -> Trace {
    let first_method = MethodHash([1, 1, 1, 1]);
    let second_method = MethodHash([2, 1, 1, 1]);
    let arguments = StarstreamValue::from(vec![1, 2, 3, 4]);

    Trace::new([
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::RegisterMethod {
            method: first_method,
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method: first_method,
            arguments: arguments.clone(),
            result: StarstreamValue::default().into(),
        },
        Step::EnterMethod {
            method: first_method,
            arguments: arguments.clone(),
        },
        Step::YieldBegin,
        Step::RegisterMethod {
            method: second_method,
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method: final_method,
            arguments: arguments.clone(),
            result: StarstreamValue::default().into(),
        },
        Step::EnterMethod {
            method: final_method,
            arguments,
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
fn rejects_opcode_after_terminal_return() {
    assert!(matches!(
        verify_sat(&opcode_after_terminal_return_trace()),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 4,
            constraint: "execution requires nonempty call stack",
            ..
        }))
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
fn accepts_repeated_method_call_without_resume() {
    verify_sat(&repeated_method_call_without_resume_trace()).unwrap();
}

#[test]
fn accepts_duplicate_method_registration() {
    verify_sat(&duplicate_method_registration_trace()).unwrap();
}

#[test]
fn accepts_method_replacement_after_yield() {
    verify_sat(&method_reyield_trace(MethodHash([2, 1, 1, 1]))).unwrap();
}

#[test]
fn rejects_enter_method_with_wrong_method() {
    let mut trace = method_call_trace(true);
    trace.0[5] = Step::EnterMethod {
        method: MethodHash([2, 1, 1, 1]),
        arguments: vec![1, 2, 3, 4].into(),
    };

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Memory(
            MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedMethod,
                row: 5,
                ..
            }
        )))
    ));
}

#[test]
fn rejects_call_to_unregistered_method() {
    let error = verify_sat(&unregistered_method_call_trace()).unwrap_err();

    assert!(
        matches!(
            &error,
            Error::Unsatisfied(Unsatisfied::Memory(MemoryCheckError::ZeroReadMismatch {
                memory: MemoryId::EnabledMethodLogUtxo,
                row: 4,
                ..
            }))
        ),
        "{error:?}"
    );
}

#[test]
fn rejects_stale_method_after_yield() {
    let error = verify_sat(&method_reyield_trace(MethodHash([1, 1, 1, 1]))).unwrap_err();

    assert!(
        matches!(
            &error,
            Error::Unsatisfied(Unsatisfied::Memory(MemoryCheckError::ZeroReadMismatch {
                memory: MemoryId::EnabledMethodLogUtxo,
                row: 9,
                ..
            }))
        ),
        "{error:?}"
    );
}

#[test]
fn rejects_call_through_unbound_resource_handle() {
    let mut trace = method_call_trace(true);
    let Step::CallMethod { resource, .. } = &mut trace.0[4] else {
        panic!("method-call trace has a call at step 4");
    };
    *resource = ResourceHandle(1);

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 4,
            constraint: "call method constraints",
            ..
        }))
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
                row: 6,
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
            step: 5,
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
fn rejects_yield_from_coordinator() {
    let trace = Trace::new([
        Step::YieldBegin,
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ]);

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 0,
            constraint: "yield begin constraints",
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
    let (mut rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();

    rows[0][COL_SEL_ENTER_CONSTRUCTOR] = F::ONE;

    assert!(matches!(
        verify_witness_rows(&rows, &preload),
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
    let (mut rows, preload) = build_witness_rows(&trace);

    rows[0][COL_UTXO_LIFECYCLE_ADDR] = F::new(1 << 32);
    range_check_layout().assign_bits(&mut rows[0]).unwrap();

    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 0,
            constraint: "COL_UTXO_LIFECYCLE_ADDR",
            ..
        }))
    ));
}

#[test]
fn rejects_tampered_memory_value() {
    let trace = constructor_trace([0, 1, 2, 3]);
    let (mut rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();

    rows[1][COL_CALL_STACK_EXPECTED_ARG_VALUE[0]] += F::ONE;
    range_check_layout().assign_bits(&mut rows[1]).unwrap();

    let error = verify_witness_rows(&rows, &preload).unwrap_err();
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
    let (mut rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();

    for row in &mut rows[1..] {
        row[COL_NEXT_UTXO_ID_BEFORE] += F::ONE;
        row[COL_NEXT_UTXO_ID_AFTER] += F::ONE;
        range_check_layout().assign_bits(row).unwrap();
    }

    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Continuity(
            ContinuityCheckError::Mismatch {
                boundary: 0,
                group_name: "next_utxo_id_continuity",
                previous_step_column: COL_NEXT_UTXO_ID_AFTER,
                next_step_column: COL_NEXT_UTXO_ID_BEFORE,
                ..
            }
        )))
    ));
}
