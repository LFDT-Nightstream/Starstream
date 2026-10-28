use starstream_interleaving_prover::{Error, verify_sat, verify_sat_batched};
use starstream_interleaving_spec::{
    MethodHash, QuintError, QuintVerifier, ResourceHandle, StarstreamValue, Step, Trace,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Outcome {
    Accept,
    Reject,
}

struct Case {
    name: &'static str,
    trace: Trace,
    expected: Outcome,
    rejected_step: Option<usize>,
}

fn constructor_trace(arguments: [u32; 4]) -> Trace {
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

fn effect_handler_trace() -> Trace {
    let effect = MethodHash([9, 0, 1, 0, 1, 0, 1, 0]);
    Trace::new([
        Step::PushEffectHandlerFrame { frame_id: 1 },
        Step::AddEffectHandler { effect },
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
        Step::PopEffectHandlerFrame,
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

fn effect_after_registration_trace() -> Trace {
    let effect = MethodHash([9, 0, 1, 0, 1, 0, 1, 0]);
    Trace::new([
        Step::NewUtxo {
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterConstructor {
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::RegisterMethod { method: effect },
        Step::RaiseEffect {
            effect,
            arguments: StarstreamValue::UNIT_VALUE,
        },
    ])
}

fn effect_after_resume_signal_trace() -> Trace {
    let mut trace = method_call_trace(true);
    trace.0.insert(7, Step::YieldBegin);
    trace.0.insert(
        8,
        Step::RaiseEffect {
            effect: MethodHash([1, 0, 1, 0, 1, 0, 1, 0]),
            arguments: StarstreamValue::UNIT_VALUE,
        },
    );
    trace
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

fn method_call_trace(enter_method: bool) -> Trace {
    method_call_result_trace(
        enter_method,
        StarstreamValue::UNIT_VALUE,
        StarstreamValue::UNIT_VALUE,
    )
}

fn method_call_result_trace(
    enter_method: bool,
    expected_result: StarstreamValue,
    actual_result: StarstreamValue,
) -> Trace {
    let method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
    let method_arguments = StarstreamValue::from(vec![1, 2, 3, 4]);
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

// Leave the coordinator active after constructing two UTXOs with independent ABIs.
fn two_utxo_trace(first_method: MethodHash, second_method: MethodHash) -> Trace {
    let mut steps = Vec::new();
    for (handle, method) in [(0, first_method), (1, second_method)] {
        steps.extend([
            Step::NewUtxo {
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::EnterConstructor {
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::RegisterMethod { method },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveNewUtxoHandle {
                resource: ResourceHandle(handle).into(),
            },
        ]);
    }
    Trace::new(steps)
}

fn call_other_utxos_method_trace() -> Trace {
    let first_method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
    let second_method = MethodHash([2, 0, 1, 0, 1, 0, 1, 0]);
    let mut trace = two_utxo_trace(first_method, second_method);
    trace.0.extend([
        Step::CallMethod {
            resource: ResourceHandle(1),
            method: first_method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterMethod {
            method: first_method,
            arguments: StarstreamValue::UNIT_VALUE,
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
    ]);
    trace
}

fn yield_preserves_other_utxos_abi_trace() -> Trace {
    let method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
    let mut trace = two_utxo_trace(method, method);
    trace.0.extend([
        Step::CallMethod {
            resource: ResourceHandle(0),
            method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterMethod {
            method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::YieldBegin,
        Step::RegisterMethod {
            method: MethodHash([2, 0, 1, 0, 1, 0, 1, 0]),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveMethodResult {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(1),
            method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterMethod {
            method,
            arguments: StarstreamValue::UNIT_VALUE,
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
    ]);
    trace
}

fn cases() -> [Case; 26] {
    let accepted = constructor_trace([0, 1, 2, 3]);
    let repeated_arguments = constructor_trace([7, 7, 7, 7]);
    let full_root = StarstreamValue([0xffff_ffff_0000_0000, 1 << 50, (1 << 32) + 1, 0]);
    let full_result = method_call_result_trace(true, full_root.clone(), full_root.clone());
    let mut changed_high = full_root.clone();
    changed_high.0[3] = 1 << 32;
    let wrong_high = method_call_result_trace(true, full_root, changed_high);
    let mut noncanonical = constructor_trace([0; 4]);
    noncanonical.0[0] = Step::NewUtxo {
        arguments: StarstreamValue([0xffff_ffff_0000_0001, 0, 0, 0]),
    };
    let minimal_constructor = minimal_constructor_trace([0, 1, 2, 3]);
    let mut mismatched_arguments = constructor_trace([1, 2, 3, 4]);
    mismatched_arguments.0[1] = Step::EnterConstructor {
        arguments: vec![0, 1, 2, 3].into(),
    };
    let return_before_constructor_entry = Trace::new([
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    let mut wrong_enter_method = method_call_trace(true);
    wrong_enter_method.0[6] = Step::EnterMethod {
        method: MethodHash([2, 0, 1, 0, 1, 0, 1, 0]),
        arguments: vec![1, 2, 3, 4].into(),
    };
    let yield_from_coordinator = Trace::new([
        Step::YieldBegin,
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);

    [
        Case {
            name: "utxo constructor",
            trace: accepted,
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "effect handler re-entry",
            trace: effect_handler_trace(),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "effect after registration starts",
            trace: effect_after_registration_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(3),
        },
        Case {
            name: "effect after resume signal",
            trace: effect_after_resume_signal_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(8),
        },
        Case {
            name: "utxo constructor with repeated arguments",
            trace: repeated_arguments,
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "minimal utxo constructor",
            trace: minimal_constructor,
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "nonempty terminal call stack",
            trace: incomplete_constructor_trace([0, 1, 2, 3]),
            expected: Outcome::Reject,
            rejected_step: None,
        },
        Case {
            name: "opcode after terminal return",
            trace: opcode_after_terminal_return_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(5),
        },
        Case {
            name: "constructor arguments disagree",
            trace: mismatched_arguments,
            expected: Outcome::Reject,
            rejected_step: Some(1),
        },
        Case {
            name: "return before entering constructor",
            trace: return_before_constructor_entry,
            expected: Outcome::Reject,
            rejected_step: Some(1),
        },
        Case {
            name: "repeated constructor entry",
            trace: repeated_constructor_entry_trace([0, 1, 2, 3]),
            expected: Outcome::Reject,
            rejected_step: Some(2),
        },
        Case {
            name: "method call with expected result",
            trace: method_call_result_trace(true, vec![7, 8].into(), vec![7, 8].into()),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "method call without resume preserves ABI",
            trace: repeated_method_call_without_resume_trace(),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "enter method with wrong method",
            trace: wrong_enter_method,
            expected: Outcome::Reject,
            rejected_step: Some(6),
        },
        Case {
            name: "call to unregistered method",
            trace: unregistered_method_call_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(5),
        },
        Case {
            name: "duplicate method registration is idempotent",
            trace: duplicate_method_registration_trace(),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "method replacement after yield",
            trace: method_reyield_trace(MethodHash([2, 0, 1, 0, 1, 0, 1, 0])),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "stale method after yield",
            trace: method_reyield_trace(MethodHash([1, 0, 1, 0, 1, 0, 1, 0])),
            expected: Outcome::Reject,
            rejected_step: Some(11),
        },
        Case {
            name: "method registered only by another UTXO",
            trace: call_other_utxos_method_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(10),
        },
        Case {
            name: "yield preserves another UTXO's ABI",
            trace: yield_preserves_other_utxos_abi_trace(),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "return with wrong result",
            trace: method_call_result_trace(true, vec![7, 8].into(), vec![7, 9].into()),
            expected: Outcome::Reject,
            rejected_step: Some(8),
        },
        Case {
            name: "return without entering method",
            trace: method_call_trace(false),
            expected: Outcome::Reject,
            rejected_step: Some(6),
        },
        Case {
            name: "yield from coordinator",
            trace: yield_from_coordinator,
            expected: Outcome::Reject,
            rejected_step: Some(0),
        },
        Case {
            name: "full-field opaque result",
            trace: full_result,
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "opaque result differs in high limb",
            trace: wrong_high,
            expected: Outcome::Reject,
            rejected_step: Some(8),
        },
        Case {
            name: "noncanonical opaque root",
            trace: noncanonical,
            expected: Outcome::Reject,
            rejected_step: Some(0),
        },
    ]
}

fn circuit_outcome(case: &Case) -> Outcome {
    match verify_sat(&case.trace) {
        Ok(()) => Outcome::Accept,
        Err(Error::Unsatisfied(_)) => Outcome::Reject,
        Err(error) => panic!("{}: circuit check failed: {error}", case.name),
    }
}

fn quint_outcome(verifier: &QuintVerifier, case: &Case) -> Outcome {
    match verifier.verify(&case.trace) {
        Ok(()) => Outcome::Accept,
        Err(QuintError::RejectedStep { index, .. }) => {
            assert_eq!(
                Some(index),
                case.rejected_step,
                "{}: Quint rejected an unexpected step",
                case.name
            );
            Outcome::Reject
        }
        Err(QuintError::IncompleteExecution(_)) => {
            assert_eq!(
                case.rejected_step, None,
                "{}: expected a step rejection, but Quint rejected completion",
                case.name
            );
            Outcome::Reject
        }
        Err(error) => panic!("{}: Quint invocation failed: {error}", case.name),
    }
}

#[test]
#[ignore = "requires Quint; run `npm test` in starstream-interleaving-spec"]
fn circuit_and_quint_agree() {
    let verifier = QuintVerifier::new().expect("repository-pinned Quint is available");

    for case in cases() {
        // Exercise both internal RAM dependencies and cross-batch boundaries,
        // with non-divisors also testing final padding.
        for size in [1, 3, 8] {
            let outcome = match verify_sat_batched(&case.trace, size) {
                Ok(()) => Outcome::Accept,
                Err(Error::Unsatisfied(_)) => Outcome::Reject,
                Err(error) => panic!("{}: batch size {size}: {error}", case.name),
            };
            assert_eq!(outcome, case.expected, "{}: batch size {size}", case.name);
        }
        let quint = quint_outcome(&verifier, &case);
        let circuit = circuit_outcome(&case);

        assert_eq!(
            quint, case.expected,
            "{}: unexpected Quint outcome",
            case.name
        );
        assert_eq!(
            circuit, case.expected,
            "{}: unexpected circuit outcome",
            case.name
        );
    }
}

// One case per scope rule: dispatch order, invocation ownership and balance.
fn effect_scope_cases() -> Vec<Case> {
    let effect = MethodHash([1, 2, 3, 4, 5, 6, 7, 8]);
    let other = MethodHash([8, 7, 6, 5, 4, 3, 2, 1]);
    let args = StarstreamValue([11, 12, 13, 14]);
    let unit = || StarstreamValue::UNIT_VALUE;
    let push = |frame_id| Step::PushEffectHandlerFrame { frame_id };
    let add = |effect| Step::AddEffectHandler { effect };
    let raise = |effect| Step::RaiseEffect {
        effect,
        arguments: args.clone(),
    };
    let enter = |frame_id, effect| Step::EnterEffectHandler {
        frame_id,
        effect,
        arguments: args.clone(),
    };
    let exit = |handled| Step::ExitEffectHandler {
        handled,
        result: unit().into(),
    };
    let resume = || Step::ResumeEffect {
        result: unit().into(),
    };
    let ret = || Step::Return {
        result: unit().into(),
    };
    let new_utxo = || Step::NewUtxo { arguments: unit() };
    let enter_constructor = || Step::EnterConstructor { arguments: unit() };
    let accept = |name, steps: Vec<Step>| Case {
        name,
        trace: Trace::new(steps),
        expected: Outcome::Accept,
        rejected_step: None,
    };
    // `None` means Quint accepts every step but not the completion.
    let reject = |name, steps: Vec<Step>, rejected_step| Case {
        name,
        trace: Trace::new(steps),
        expected: Outcome::Reject,
        rejected_step,
    };
    vec![
        accept(
            "skip, decline, then accept an outer scope",
            vec![
                push(1),
                add(effect),
                push(2),
                add(effect),
                push(3),
                add(other),
                raise(effect),
                Step::SkipEffectHandler,
                enter(2, effect),
                exit(false),
                enter(1, effect),
                exit(true),
                resume(),
                Step::PopEffectHandlerFrame,
                Step::PopEffectHandlerFrame,
                Step::PopEffectHandlerFrame,
                ret(),
            ],
        ),
        reject(
            "matching scope cannot be skipped",
            vec![push(1), add(effect), raise(effect), Step::SkipEffectHandler],
            Some(3),
        ),
        reject(
            "empty chain cannot be skipped",
            vec![raise(effect), Step::SkipEffectHandler],
            Some(1),
        ),
        reject(
            "wrong frame id",
            vec![push(1), add(effect), raise(effect), enter(2, effect)],
            Some(3),
        ),
        reject(
            "unhandled effect is incomplete",
            vec![push(1), add(other), raise(effect), Step::SkipEffectHandler],
            None,
        ),
        reject(
            "sibling clause is not visible inside its handler",
            vec![
                push(1),
                add(effect),
                add(other),
                raise(effect),
                enter(1, effect),
                raise(other),
                enter(1, other),
            ],
            Some(6),
        ),
        reject(
            "callee cannot register on its caller's scope",
            vec![push(1), new_utxo(), enter_constructor(), add(effect)],
            Some(3),
        ),
        reject(
            "callee cannot pop its caller's scope",
            vec![
                push(1),
                new_utxo(),
                enter_constructor(),
                Step::PopEffectHandlerFrame,
            ],
            Some(3),
        ),
        reject(
            "return requires balanced scopes",
            vec![new_utxo(), enter_constructor(), push(1), ret()],
            Some(3),
        ),
        reject(
            "handler exit requires balanced scopes",
            vec![
                push(1),
                add(effect),
                raise(effect),
                enter(1, effect),
                push(2),
                exit(false),
            ],
            Some(5),
        ),
        reject(
            "scopes cannot change during a pending search",
            vec![
                push(1),
                add(effect),
                raise(effect),
                Step::PopEffectHandlerFrame,
            ],
            Some(3),
        ),
    ]
}

#[test]
#[ignore = "requires Quint; run with --include-ignored"]
fn effect_scopes_circuit_and_quint_agree() {
    let verifier = QuintVerifier::new().expect("repository-pinned Quint is available");
    for case in effect_scope_cases() {
        assert_eq!(
            quint_outcome(&verifier, &case),
            case.expected,
            "{}",
            case.name
        );
        assert_eq!(circuit_outcome(&case), case.expected, "{}", case.name);
        if case.expected == Outcome::Reject {
            for size in [1, 3] {
                assert!(
                    matches!(
                        verify_sat_batched(&case.trace, size),
                        Err(Error::Unsatisfied(_))
                    ),
                    "{}",
                    case.name
                );
            }
        }
    }
}
