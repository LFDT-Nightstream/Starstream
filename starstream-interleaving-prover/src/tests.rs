mod effect_stack;
mod resource_bindings;

mod padding {
    use super::*;
    use crate::{
        ccs::layout::*, memory::build_memory_layout, step::normalize, witness::build_witness_vector,
    };
    use neo_application::MemoryPortActivation;

    #[test]
    fn memory_gates_exclude_padding_structurally() {
        // Explicitly reviewed derived gates, each constrained to non-padding
        // selectors. New activation schemes need review.
        let derived = [
            COL_INSTANCE_PUSH,
            COL_EFFECT_SCOPE_READ,
            COL_CALL_STACK_PUSH,
            COL_CALL_STACK_POP,
            COL_CALL_STACK_TOP,
            COL_METHOD_LOOKUP,
            COL_METHOD_APPEND,
            COL_EVENT_ACTIVE,
            COL_RESOURCE_RESOLVER_READ,
            COL_RESOURCE_RESOLVER_WRITE,
            COL_ABI_METHOD_COUNT_READ,
            COL_TRACE_ROOT_READ,
            COL_ABI_METHOD_COUNT_WRITE,
        ];
        for memory in build_memory_layout().entries() {
            for port in &memory.ports {
                let excludes_padding = match port.activation {
                    MemoryPortActivation::Always => false,
                    MemoryPortActivation::When(gate) => {
                        crate::opcode::Opcode::all().iter().any(|op| {
                            *op != crate::opcode::Opcode::Padding && op.selector() == gate
                        }) || derived.contains(&gate)
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
            crate::commitment::assign_from_bus(&mut pad, crate::opcode::Opcode::Padding);
            range_check_layout().assign_bits(&mut pad).unwrap();
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
        COL_ABI_METHOD_COUNT_ADDR, COL_CALL_SP_AFTER, COL_CALL_STACK_EXPECTED_ARG_VALUE,
        COL_CALL_TARGET_INVERSE, COL_CALL_TARGET_IS_ZERO, COL_EFFECT_HEAD_AFTER,
        COL_NEXT_UTXO_ID_AFTER, COL_NEXT_UTXO_ID_BEFORE, COL_SEL_ENTER_CONSTRUCTOR,
        range_check_layout,
    },
    memory::MemoryId,
    step::normalize,
};

pub(super) fn constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::RegisterMethod {
            method: MethodHash([1, 0, 1, 0, 1, 0, 1, 0]),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

fn minimal_constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

#[test]
fn receive_consumes_the_completed_program_frame() {
    let normalized = normalize(&minimal_constructor_trace([0, 1, 2, 3]));

    let constructor_return = &normalized.steps[2];
    assert_eq!(constructor_return.call_sp_before, F::new(2));
    assert_eq!(constructor_return.call_sp_after, F::new(2));

    let receive = &normalized.steps[3];
    assert_eq!(receive.call_sp_before, F::new(2));
    assert_eq!(receive.call_sp_after, F::new(1));

    let root_return = &normalized.steps[4];
    assert_eq!(root_return.call_sp_before, F::new(1));
    assert_eq!(root_return.call_sp_after, F::ZERO);
}

#[test]
fn rejects_receiving_a_constructor_as_a_method() {
    let mut trace = minimal_constructor_trace([0; 4]);
    trace.0[3] = Step::ReceiveMethodResult {
        result: StarstreamValue::UNIT_VALUE.into(),
    };
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 3,
            constraint: "receive method result constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_receiving_a_method_as_a_constructor() {
    let mut trace = method_call_trace(true);
    trace.0[8] = Step::ReceiveNewUtxoHandle {
        resource: ResourceHandle(99).into(),
    };
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 8,
            constraint: "receive new utxo handle constraints",
            ..
        }))
    ));
}

fn incomplete_constructor_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            arguments: arguments.to_vec().into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

fn repeated_constructor_entry_trace(arguments: [u32; 4]) -> Trace {
    Trace::new([
        Step::NewUtxo {
            arguments: arguments.to_vec().into(),
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
        },
        Step::EnterConstructor {
            arguments: vec![7].into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    trace
}

pub(super) fn method_call_trace(enter_method: bool) -> Trace {
    method_call_trace_with_values(
        enter_method,
        vec![1, 2, 3, 4].into(),
        StarstreamValue::UNIT_VALUE,
        StarstreamValue::UNIT_VALUE,
    )
}

pub(super) fn method_call_trace_with_values(
    enter_method: bool,
    method_arguments: StarstreamValue,
    expected_result: StarstreamValue,
    actual_result: StarstreamValue,
) -> Trace {
    let method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
    let mut steps = vec![
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::EnterConstructor {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::RegisterMethod { method },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method,
            arguments: method_arguments.clone(),
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
        Step::ReceiveMethodResult {
            result: expected_result.into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);

    Trace::new(steps)
}

fn repeated_method_call_without_resume_trace() -> Trace {
    let method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
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
            },
            Step::EnterMethod { method, arguments },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveMethodResult {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
        ],
    );

    trace
}

fn unregistered_method_call_trace() -> Trace {
    let mut trace = method_call_trace(true);
    let unregistered_method = MethodHash([2, 0, 1, 0, 1, 0, 1, 0]);

    let Step::CallMethod { method, .. } = &mut trace.0[5] else {
        panic!("method-call trace has a call at step 5");
    };
    *method = unregistered_method;

    let Step::EnterMethod { method, .. } = &mut trace.0[6] else {
        panic!("method-call trace enters the method at step 6");
    };
    *method = unregistered_method;

    trace
}

fn duplicate_method_registration_trace() -> Trace {
    let mut trace = constructor_trace([0, 1, 2, 3]);
    trace.0.insert(
        3,
        Step::RegisterMethod {
            method: MethodHash([1, 0, 1, 0, 1, 0, 1, 0]),
        },
    );
    trace
}

fn method_reyield_trace(final_method: MethodHash) -> Trace {
    let first_method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
    let second_method = MethodHash([2, 0, 1, 0, 1, 0, 1, 0]);
    let arguments = StarstreamValue::from(vec![1, 2, 3, 4]);

    Trace::new([
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::EnterConstructor {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::RegisterMethod {
            method: first_method,
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method: first_method,
            arguments: arguments.clone(),
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
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveMethodResult {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method: final_method,
            arguments: arguments.clone(),
        },
        Step::EnterMethod {
            method: final_method,
            arguments,
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveMethodResult {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

/// Push a `try` scope registering `effects`.
fn effect_scope(frame_id: u32, effects: &[MethodHash]) -> Vec<Step> {
    std::iter::once(Step::PushEffectHandlerFrame { frame_id })
        .chain(
            effects
                .iter()
                .map(|&effect| Step::AddEffectHandler { effect }),
        )
        .collect()
}

/// Steps preceding the first raise in [`nested_effect_trace`].
const NESTED_EFFECT_PREFIX: usize = 4;

// The outer handler runs with only the scopes outside its own visible, so
// the inner effect is registered on an enclosing scope.
fn nested_effect_trace() -> Trace {
    let outer = MethodHash([10, 0, 1, 0, 1, 0, 1, 0]);
    let inner = MethodHash([11, 0, 1, 0, 1, 0, 1, 0]);
    let mut steps = effect_scope(1, &[inner]);
    steps.extend(effect_scope(2, &[outer]));
    assert_eq!(steps.len(), NESTED_EFFECT_PREFIX);
    steps.extend([
        Step::RaiseEffect {
            effect: outer,
            arguments: vec![1, 2, 3, 4].into(),
        },
        Step::EnterEffectHandler {
            frame_id: 2,
            effect: outer,
            arguments: vec![1, 2, 3, 4].into(),
        },
        Step::RaiseEffect {
            effect: inner,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterEffectHandler {
            frame_id: 1,
            effect: inner,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::ExitEffectHandler {
            handled: true,
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ResumeEffect {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ExitEffectHandler {
            handled: true,
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ResumeEffect {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::PopEffectHandlerFrame,
        Step::PopEffectHandlerFrame,
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    Trace::new(steps)
}

fn constructor_effect_trace(effect_after_registration: bool) -> Trace {
    let effect = MethodHash([12, 0, 1, 0, 1, 0, 1, 0]);
    let mut steps = effect_scope(1, &[effect]);
    steps.extend([
        Step::NewUtxo {
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterConstructor {
            arguments: StarstreamValue::UNIT_VALUE,
        },
    ]);
    if effect_after_registration {
        steps.push(Step::RegisterMethod { method: effect });
    }
    steps.extend([
        Step::RaiseEffect {
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterEffectHandler {
            frame_id: 1,
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::ExitEffectHandler {
            handled: true,
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ResumeEffect {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    if !effect_after_registration {
        steps.push(Step::RegisterMethod { method: effect });
    }
    steps.extend([
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::PopEffectHandlerFrame,
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
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
fn rejects_opcode_after_terminal_return() {
    assert!(matches!(
        verify_sat(&opcode_after_terminal_return_trace()),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 5,
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
    verify_sat(&method_call_trace_with_values(
        true,
        vec![1, 2, 3, 4].into(),
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
fn rejects_return_while_method_result_is_pending() {
    let mut trace = method_call_trace(true);
    let forged_result = StarstreamValue::from(vec![9]);
    trace.0.splice(
        8..9,
        [
            Step::Return {
                result: forged_result.clone().into(),
            },
            Step::ReceiveMethodResult {
                result: forged_result.into(),
            },
        ],
    );
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 8,
            constraint: "return constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_return_while_new_utxo_handle_is_pending() {
    let mut trace = minimal_constructor_trace([0, 1, 2, 3]);
    let Step::ReceiveNewUtxoHandle { resource } = trace.0[3] else {
        panic!("constructor trace must receive its handle at step 3");
    };
    trace.0.splice(
        3..4,
        [
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveNewUtxoHandle { resource },
        ],
    );
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 3,
            constraint: "return constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_effect_while_method_result_is_pending() {
    let mut trace = method_call_trace(true);
    let effect = MethodHash([12, 0, 1, 0, 1, 0, 1, 0]);
    let Step::ReceiveMethodResult { result } = trace.0[8].clone() else {
        panic!("method trace must resume at step 8");
    };
    trace.0.splice(
        8..9,
        [
            Step::RaiseEffect {
                effect,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::EnterEffectHandler {
                frame_id: 1,
                effect,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::ExitEffectHandler {
                handled: true,
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ResumeEffect {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveMethodResult { result },
        ],
    );
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 8,
            constraint: "raise effect constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_effect_while_new_utxo_handle_is_pending() {
    let mut trace = minimal_constructor_trace([0, 1, 2, 3]);
    let effect = MethodHash([12, 0, 1, 0, 1, 0, 1, 0]);
    let Step::ReceiveNewUtxoHandle { resource } = trace.0[3].clone() else {
        panic!("constructor trace must resume at step 3");
    };
    trace.0.splice(
        3..4,
        [
            Step::RaiseEffect {
                effect,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::EnterEffectHandler {
                frame_id: 1,
                effect,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::ExitEffectHandler {
                handled: true,
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ResumeEffect {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveNewUtxoHandle { resource },
        ],
    );
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 3,
            constraint: "raise effect constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_non_unit_constructor_result() {
    let mut trace = minimal_constructor_trace([0, 1, 2, 3]);
    trace.0[2] = Step::Return {
        result: StarstreamValue::from(vec![1]).into(),
    };
    assert!(verify_sat(&trace).is_err());
}

#[test]
fn rejects_non_unit_entrypoint_result() {
    let trace = Trace::new([Step::Return {
        result: StarstreamValue::from(vec![1]).into(),
    }]);
    assert!(verify_sat(&trace).is_err());
}

#[test]
fn accepts_duplicate_method_registration() {
    verify_sat(&duplicate_method_registration_trace()).unwrap();
}

#[test]
fn accepts_method_replacement_after_yield() {
    verify_sat(&method_reyield_trace(MethodHash([2, 0, 1, 0, 1, 0, 1, 0]))).unwrap();
}

#[test]
fn accepts_nested_effect_handler_reentry() {
    verify_sat(&nested_effect_trace()).unwrap();
}

#[test]
fn nested_same_effect_resumes_match_results_by_stack_position() {
    let mut trace = nested_effect_trace();
    let base = NESTED_EFFECT_PREFIX;
    let Step::RaiseEffect { effect, .. } = trace.0[base] else {
        unreachable!()
    };
    for index in [1, base + 2, base + 3] {
        match &mut trace.0[index] {
            Step::AddEffectHandler { effect: inner }
            | Step::RaiseEffect { effect: inner, .. }
            | Step::EnterEffectHandler { effect: inner, .. } => *inner = effect,
            _ => unreachable!(),
        }
    }
    for (index, value) in [(4, 11), (5, 11), (6, 22), (7, 22)] {
        match &mut trace.0[base + index] {
            Step::ExitEffectHandler { result, .. } | Step::ResumeEffect { result } => {
                *result = StarstreamValue::from(vec![value]).into();
            }
            _ => unreachable!(),
        }
    }
    verify_sat(&trace).unwrap();

    trace.0.swap(base + 5, base + 7);
    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
    ));
}

#[test]
fn rejects_skipping_effect_handler_search() {
    let effect = MethodHash([13, 0, 1, 0, 1, 0, 1, 0]);
    let trace = Trace::new([
        Step::RaiseEffect {
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::ResumeEffect {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    assert!(verify_sat(&trace).is_err());
}

#[test]
fn rejects_repeating_effect_handler_after_search_is_exhausted() {
    let effect = MethodHash([14, 0, 1, 0, 1, 0, 1, 0]);
    let mut steps = effect_scope(1, &[effect]);
    steps.extend([
        Step::RaiseEffect {
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterEffectHandler {
            frame_id: 1,
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::ExitEffectHandler {
            handled: false,
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::EnterEffectHandler {
            frame_id: 1,
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
    ]);
    assert!(matches!(
        verify_sat(&Trace::new(steps)),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 5,
            constraint: "effect scope reads",
            ..
        }))
    ));
}

#[test]
fn rejects_resuming_an_unhandled_effect() {
    let effect = MethodHash([15, 0, 1, 0, 1, 0, 1, 0]);
    let mut steps = effect_scope(1, &[effect]);
    steps.extend([
        Step::RaiseEffect {
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterEffectHandler {
            frame_id: 1,
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::ExitEffectHandler {
            handled: false,
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ResumeEffect {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    assert!(verify_sat(&Trace::new(steps)).is_err());
}

#[test]
fn accepts_effect_during_constructor_execution_before_registration() {
    verify_sat(&constructor_effect_trace(false)).unwrap();
}

#[test]
fn rejects_effect_after_registration_starts() {
    assert!(verify_sat(&constructor_effect_trace(true)).is_err());
}

#[test]
fn rejects_effect_after_resume_signal() {
    let mut trace = method_call_trace(true);
    trace.0.insert(7, Step::YieldBegin);
    trace.0.insert(
        8,
        Step::RaiseEffect {
            effect: MethodHash([1, 0, 1, 0, 1, 0, 1, 0]),
            arguments: StarstreamValue::UNIT_VALUE,
        },
    );
    assert!(verify_sat(&trace).is_err());
}

#[test]
fn rejects_effect_resume_with_wrong_result() {
    let mut trace = nested_effect_trace();
    let Step::ResumeEffect { result } = &mut trace.0[NESTED_EFFECT_PREFIX + 5] else {
        unreachable!()
    };
    *result = StarstreamValue::from(vec![99]).into();
    assert!(verify_sat(&trace).is_err());
}

#[test]
fn rejects_enter_method_with_wrong_method() {
    let mut trace = method_call_trace(true);
    trace.0[6] = Step::EnterMethod {
        method: MethodHash([2, 0, 1, 0, 1, 0, 1, 0]),
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
fn rejects_call_to_unregistered_method() {
    let error = verify_sat(&unregistered_method_call_trace()).unwrap_err();

    assert!(
        matches!(
            &error,
            Error::Unsatisfied(Unsatisfied::Memory(MemoryCheckError::ZeroReadMismatch {
                memory: MemoryId::EnabledMethodLogUtxo,
                row: 5,
                ..
            }))
        ),
        "{error:?}"
    );
}

#[test]
fn rejects_stale_method_after_yield() {
    let error =
        verify_sat(&method_reyield_trace(MethodHash([1, 0, 1, 0, 1, 0, 1, 0]))).unwrap_err();

    assert!(
        matches!(
            &error,
            Error::Unsatisfied(Unsatisfied::Memory(MemoryCheckError::ZeroReadMismatch {
                memory: MemoryId::EnabledMethodLogUtxo,
                row: 11,
                ..
            }))
        ),
        "{error:?}"
    );
}

#[test]
fn rejects_call_through_unbound_resource_handle() {
    let mut trace = method_call_trace(true);
    let Step::CallMethod { resource, .. } = &mut trace.0[5] else {
        panic!("method-call trace has a call at step 5");
    };
    *resource = ResourceHandle(1);

    assert!(matches!(
        verify_sat(&trace),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 5,
            constraint: "call method constraints",
            ..
        }))
    ));
}

#[test]
fn rejects_return_with_wrong_result() {
    assert!(matches!(
        verify_sat(&method_call_trace_with_values(
            true,
            vec![1, 2, 3, 4].into(),
            vec![7, 8].into(),
            vec![7, 9].into(),
        )),
        Err(Error::Unsatisfied(Unsatisfied::Memory(
            MemoryCheckError::ReadMismatch {
                memory: MemoryId::CallStackExpectedResult,
                row: 8,
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
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
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
            result: StarstreamValue::UNIT_VALUE.into(),
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
fn rejects_a_nonzero_return_caller_claimed_as_root() {
    let trace = method_call_trace(true);
    let (mut rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();

    // The method Return at step 7 targets its nonzero coordinator caller. A
    // dishonest witness must not classify that caller as the root sentinel.
    rows[7][COL_CALL_TARGET_IS_ZERO] = F::ONE;
    rows[7][COL_CALL_TARGET_INVERSE] = F::ZERO;
    range_check_layout().assign_bits(&mut rows[7]).unwrap();

    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 7,
            constraint: "call target zero test",
            ..
        }))
    ));
}

#[test]
fn rejects_out_of_range_witness_column() {
    let trace = constructor_trace([0, 1, 2, 3]);
    let (mut rows, preload) = build_witness_rows(&trace);

    rows[0][COL_ABI_METHOD_COUNT_ADDR] = F::new(1 << 32);
    range_check_layout().assign_bits(&mut rows[0]).unwrap();

    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 0,
            constraint: "COL_ABI_METHOD_COUNT_ADDR",
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
    crate::commitment::assign_from_bus(&mut rows[1], crate::opcode::Opcode::EnterConstructor);
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

/// Raise `effect` past a single scope that registers something else.
const SKIP_ROW: usize = 3;

fn effect_skip_trace(effect: MethodHash) -> Trace {
    Trace::new([
        Step::PushEffectHandlerFrame { frame_id: 1 },
        Step::AddEffectHandler {
            effect: MethodHash([9; 8]),
        },
        Step::RaiseEffect {
            effect,
            arguments: StarstreamValue([11, 12, 13, 14]),
        },
        Step::SkipEffectHandler,
    ])
}

#[test]
fn effect_skip_accepts_dynamic_ids_and_preserves_pending_arguments() {
    for effect in [
        MethodHash([1, 2, 3, 4, 5, 6, 7, 8]),
        MethodHash([u32::MAX; 8]),
    ] {
        let trace = effect_skip_trace(effect);
        let (rows, preload) = build_witness_rows(&trace);
        // This is a valid execution prefix, not a completed transaction.
        verify_witness_rows(&rows, &preload).unwrap();
        let (raise, skip) = (&rows[SKIP_ROW - 1], &rows[SKIP_ROW]);
        assert_eq!(raise[COL_CALL_SP_AFTER], skip[COL_CALL_SP_AFTER]);
        assert_eq!(skip[COL_EFFECT_HEAD_AFTER], F::ZERO);
        for column in COL_CALL_STACK_EXPECTED_ARG_VALUE {
            assert_eq!(raise[column], skip[column]);
        }
        assert!(matches!(
            verify_sat(&trace),
            Err(Error::Unsatisfied(
                Unsatisfied::TerminalCallStackNotEmpty { .. }
            ))
        ));
    }
}

#[test]
fn effect_skip_searches_for_the_pending_effect() {
    // The inner scope handles the raised effect. A forged skip claims to
    // search for the outer scope's effect instead, which the inner scope
    // does not register, but the pending frame records the raised one.
    let (raised, other) = (MethodHash([1; 8]), MethodHash([2; 8]));
    let trace = Trace::new([
        Step::PushEffectHandlerFrame { frame_id: 1 },
        Step::AddEffectHandler { effect: other },
        Step::PushEffectHandlerFrame { frame_id: 2 },
        Step::AddEffectHandler { effect: raised },
        Step::RaiseEffect {
            effect: raised,
            arguments: StarstreamValue([11, 12, 13, 14]),
        },
        Step::SkipEffectHandler,
    ]);
    let (mut rows, preload) = build_witness_rows(&trace);
    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 5,
            constraint: "effect scope reads",
            ..
        }))
    ));
    use crate::ccs::layout::*;
    let add_other = rows[1].clone();
    let skip = &mut rows[5];
    for column in COL_METHOD_HASH_VALUE
        .into_iter()
        .chain(COL_METHOD_TABLE_ADDR)
        .chain([COL_METHOD_INDEX])
    {
        skip[column] = add_other[column];
    }
    skip[COL_EFFECT_REGISTERED] = F::ZERO;
    crate::witness::assign_stride_columns(skip);
    range_check_layout().assign_bits(skip).unwrap();
    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory(_)))
    ));
}

#[test]
fn exhausted_search_cannot_skip_handle_resume_or_return() {
    let effect = MethodHash([1; 8]);
    for next in [
        Step::SkipEffectHandler,
        Step::EnterEffectHandler {
            frame_id: 1,
            effect,
            arguments: StarstreamValue([11, 12, 13, 14]),
        },
        Step::ResumeEffect {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ] {
        let mut trace = effect_skip_trace(effect);
        trace.0.push(next);
        let (rows, preload) = build_witness_rows(&trace);
        assert!(verify_witness_rows(&rows, &preload).is_err());
    }
    let (rows, preload) = build_witness_rows(&Trace::new([Step::SkipEffectHandler]));
    assert!(verify_witness_rows(&rows, &preload).is_err());
}
