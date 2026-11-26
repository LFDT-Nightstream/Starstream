use starstream_interleaving_prover::{Error, verify_sat, verify_sat_batched};
use starstream_interleaving_spec::{
    EntryCall, MethodHash, QuintError, QuintVerifier, ResourceHandle, StarstreamValue, Step, Trace,
};

/// The entry script call that every case runs.
const ENTRY: EntryCall = EntryCall {
    script: MethodHash([0xe, 0, 0xe, 0, 0xe, 0, 0xe, 0]),
    arguments: StarstreamValue::UNIT_VALUE,
};

/// The constructor identity that every case uses unless it tests a mismatch.
const CONSTRUCTOR: MethodHash = MethodHash([0xc; 8]);

/// An execution whose entry coordinator runs `body`, after the entry call
/// and its entry event (steps 0 and 1).
fn entered(body: impl IntoIterator<Item = Step>) -> Trace {
    Trace::with_entry(&ENTRY, body)
}

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
    entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::RegisterMethod {
            method: MethodHash([1, 0, 1, 0, 1, 0, 1, 0]),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewResourceHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

fn minimal_constructor_trace(arguments: [u32; 4]) -> Trace {
    entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewResourceHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

fn effect_handler_trace() -> Trace {
    let effect = MethodHash([9, 0, 1, 0, 1, 0, 1, 0]);
    entered([
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
    entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
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
    trace.0.insert(9, Step::YieldBegin);
    trace.0.insert(
        10,
        Step::RaiseEffect {
            effect: MethodHash([1, 0, 1, 0, 1, 0, 1, 0]),
            arguments: StarstreamValue::UNIT_VALUE,
        },
    );
    trace
}

fn incomplete_constructor_trace(arguments: [u32; 4]) -> Trace {
    entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ])
}

fn repeated_constructor_entry_trace(arguments: [u32; 4]) -> Trace {
    entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: arguments.to_vec().into(),
        },
    ])
}

fn opcode_after_terminal_return_trace() -> Trace {
    let mut trace = minimal_constructor_trace([0, 1, 2, 3]);
    trace.0.extend([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: vec![7].into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: vec![7].into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    trace
}

fn method_call_trace(enter_call: bool) -> Trace {
    method_call_result_trace(
        enter_call,
        StarstreamValue::UNIT_VALUE,
        StarstreamValue::UNIT_VALUE,
    )
}

fn method_call_result_trace(
    enter_call: bool,
    expected_result: StarstreamValue,
    actual_result: StarstreamValue,
) -> Trace {
    let method = MethodHash([1, 0, 1, 0, 1, 0, 1, 0]);
    let method_arguments = StarstreamValue::from(vec![1, 2, 3, 4]);
    let mut steps = vec![
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::RegisterMethod { method },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewResourceHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method,
            arguments: method_arguments.clone(),
        },
    ];

    if enter_call {
        steps.push(Step::EnterCall {
            method,
            arguments: method_arguments,
        });
    }

    steps.extend([
        Step::Return {
            result: actual_result.into(),
        },
        Step::ReceiveCallResult {
            result: expected_result.into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);

    entered(steps)
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
            Step::EnterCall { method, arguments },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveCallResult {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
        ],
    );

    trace
}

fn unregistered_method_call_trace() -> Trace {
    let mut trace = method_call_trace(true);
    let unregistered_method = MethodHash([2, 0, 1, 0, 1, 0, 1, 0]);

    let Step::CallMethod { method, .. } = &mut trace.0[7] else {
        panic!("method-call trace has a call at step 5");
    };
    *method = unregistered_method;

    let Step::EnterCall { method, .. } = &mut trace.0[8] else {
        panic!("method-call trace enters the method at step 6");
    };
    *method = unregistered_method;

    trace
}

fn duplicate_method_registration_trace() -> Trace {
    let mut trace = constructor_trace([0, 1, 2, 3]);
    trace.0.insert(
        5,
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

    entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::RegisterMethod {
            method: first_method,
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveNewResourceHandle {
            resource: ResourceHandle(0).into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method: first_method,
            arguments: arguments.clone(),
        },
        Step::EnterCall {
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
        Step::ReceiveCallResult {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(0),
            method: final_method,
            arguments: arguments.clone(),
        },
        Step::EnterCall {
            method: final_method,
            arguments,
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveCallResult {
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
                constructor: CONSTRUCTOR,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::EnterConstructor {
                constructor: CONSTRUCTOR,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::RegisterMethod { method },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveNewResourceHandle {
                resource: ResourceHandle(handle).into(),
            },
        ]);
    }
    entered(steps)
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
        Step::EnterCall {
            method: first_method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveCallResult {
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
        Step::EnterCall {
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
        Step::ReceiveCallResult {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::CallMethod {
            resource: ResourceHandle(1),
            method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::EnterCall {
            method,
            arguments: StarstreamValue::UNIT_VALUE,
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::ReceiveCallResult {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    trace
}

fn cases() -> [Case; 32] {
    let accepted = constructor_trace([0, 1, 2, 3]);
    let repeated_arguments = constructor_trace([7, 7, 7, 7]);
    let full_root = StarstreamValue([0xffff_ffff_0000_0000, 1 << 50, (1 << 32) + 1, 0]);
    let full_result = method_call_result_trace(true, full_root.clone(), full_root.clone());
    let mut changed_high = full_root.clone();
    changed_high.0[3] = 1 << 32;
    let wrong_high = method_call_result_trace(true, full_root, changed_high);
    let mut noncanonical = constructor_trace([0; 4]);
    noncanonical.0[2] = Step::NewUtxo {
        constructor: CONSTRUCTOR,
        arguments: StarstreamValue([0xffff_ffff_0000_0001, 0, 0, 0]),
    };
    let minimal_constructor = minimal_constructor_trace([0, 1, 2, 3]);
    let mut mismatched_arguments = constructor_trace([1, 2, 3, 4]);
    mismatched_arguments.0[3] = Step::EnterConstructor {
        constructor: CONSTRUCTOR,
        arguments: vec![0, 1, 2, 3].into(),
    };
    let mut mismatched_constructor = constructor_trace([1, 2, 3, 4]);
    mismatched_constructor.0[3] = Step::EnterConstructor {
        constructor: MethodHash([0xd; 8]),
        arguments: vec![1, 2, 3, 4].into(),
    };
    let return_before_constructor_entry = entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: vec![0, 1, 2, 3].into(),
        },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    let mut wrong_enter_call = method_call_trace(true);
    wrong_enter_call.0[8] = Step::EnterCall {
        method: MethodHash([2, 0, 1, 0, 1, 0, 1, 0]),
        arguments: vec![1, 2, 3, 4].into(),
    };
    let ret = || Step::Return {
        result: StarstreamValue::UNIT_VALUE.into(),
    };
    let begin = |script, arguments| Step::BeginExecution { script, arguments };
    let enter = |method, arguments| Step::EnterCall { method, arguments };
    let other_script = MethodHash([2, 0, 1, 0, 1, 0, 1, 0]);
    let entry_arguments = StarstreamValue([5, 6, 7, 8]);
    let execution_without_entry = Trace::new([ret()]);
    let entry_arguments_bound = Trace::new([
        begin(ENTRY.script, entry_arguments.clone()),
        enter(ENTRY.script, entry_arguments.clone()),
        ret(),
    ]);
    let wrong_entry_script = Trace::new([
        begin(ENTRY.script, StarstreamValue::UNIT_VALUE),
        enter(other_script, StarstreamValue::UNIT_VALUE),
        ret(),
    ]);
    let wrong_entry_arguments = Trace::new([
        begin(ENTRY.script, entry_arguments),
        enter(ENTRY.script, StarstreamValue::UNIT_VALUE),
        ret(),
    ]);
    let repeated_entry = entered([begin(ENTRY.script, StarstreamValue::UNIT_VALUE), ret()]);
    let yield_from_coordinator = entered([
        Step::YieldBegin,
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);

    [
        Case {
            name: "execution without the entry call",
            trace: execution_without_entry,
            expected: Outcome::Reject,
            rejected_step: Some(0),
        },
        Case {
            name: "entry arguments are bound",
            trace: entry_arguments_bound,
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "entry event names another script",
            trace: wrong_entry_script,
            expected: Outcome::Reject,
            rejected_step: Some(1),
        },
        Case {
            name: "entry event carries other arguments",
            trace: wrong_entry_arguments,
            expected: Outcome::Reject,
            rejected_step: Some(1),
        },
        Case {
            name: "entry call cannot repeat",
            trace: repeated_entry,
            expected: Outcome::Reject,
            rejected_step: Some(2),
        },
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
            rejected_step: Some(5),
        },
        Case {
            name: "effect after resume signal",
            trace: effect_after_resume_signal_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(10),
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
            rejected_step: Some(7),
        },
        Case {
            name: "constructor arguments disagree",
            trace: mismatched_arguments,
            expected: Outcome::Reject,
            rejected_step: Some(3),
        },
        Case {
            name: "constructor identities disagree",
            trace: mismatched_constructor,
            expected: Outcome::Reject,
            rejected_step: Some(3),
        },
        Case {
            name: "return before entering constructor",
            trace: return_before_constructor_entry,
            expected: Outcome::Reject,
            rejected_step: Some(3),
        },
        Case {
            name: "repeated constructor entry",
            trace: repeated_constructor_entry_trace([0, 1, 2, 3]),
            expected: Outcome::Reject,
            rejected_step: Some(4),
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
            trace: wrong_enter_call,
            expected: Outcome::Reject,
            rejected_step: Some(8),
        },
        Case {
            name: "call to unregistered method",
            trace: unregistered_method_call_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(7),
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
            rejected_step: Some(13),
        },
        Case {
            name: "method registered only by another UTXO",
            trace: call_other_utxos_method_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(12),
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
            rejected_step: Some(10),
        },
        Case {
            name: "return without entering method",
            trace: method_call_trace(false),
            expected: Outcome::Reject,
            rejected_step: Some(8),
        },
        Case {
            name: "yield from coordinator",
            trace: yield_from_coordinator,
            expected: Outcome::Reject,
            rejected_step: Some(2),
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
            rejected_step: Some(10),
        },
        Case {
            name: "noncanonical opaque root",
            trace: noncanonical,
            expected: Outcome::Reject,
            rejected_step: Some(2),
        },
    ]
}

fn circuit_outcome(case: &Case) -> Outcome {
    match verify_sat(&case.trace, &Default::default()) {
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
            let outcome = match verify_sat_batched(&case.trace, size, &Default::default()) {
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
    let new_utxo = || Step::NewUtxo {
        constructor: CONSTRUCTOR,
        arguments: unit(),
    };
    let enter_constructor = || Step::EnterConstructor {
        constructor: CONSTRUCTOR,
        arguments: unit(),
    };
    let accept = |name, steps: Vec<Step>| Case {
        name,
        trace: entered(steps),
        expected: Outcome::Accept,
        rejected_step: None,
    };
    // `None` means Quint accepts every step but not the completion.
    let reject = |name, steps: Vec<Step>, rejected_step| Case {
        name,
        trace: entered(steps),
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
            Some(5),
        ),
        reject(
            "empty chain cannot be skipped",
            vec![raise(effect), Step::SkipEffectHandler],
            Some(3),
        ),
        reject(
            "wrong frame id",
            vec![push(1), add(effect), raise(effect), enter(2, effect)],
            Some(5),
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
            Some(8),
        ),
        reject(
            "callee cannot register on its caller's scope",
            vec![push(1), new_utxo(), enter_constructor(), add(effect)],
            Some(5),
        ),
        reject(
            "callee cannot pop its caller's scope",
            vec![
                push(1),
                new_utxo(),
                enter_constructor(),
                Step::PopEffectHandlerFrame,
            ],
            Some(5),
        ),
        reject(
            "return requires balanced scopes",
            vec![new_utxo(), enter_constructor(), push(1), ret()],
            Some(5),
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
            Some(7),
        ),
        reject(
            "scopes cannot change during a pending search",
            vec![
                push(1),
                add(effect),
                raise(effect),
                Step::PopEffectHandlerFrame,
            ],
            Some(5),
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
                        verify_sat_batched(&case.trace, size, &Default::default()),
                        Err(Error::Unsatisfied(_))
                    ),
                    "{}",
                    case.name
                );
            }
        }
    }
}

// Script calls: the callee boundary is bound to the caller's frame, callees
// are fresh coordinators, and UTXOs are leaves.
fn script_call_cases() -> Vec<Case> {
    let a = MethodHash([0xa, 1, 0xa, 2, 0xa, 3, 0xa, 4]);
    let b = MethodHash([0xb, 1, 0xb, 2, 0xb, 3, 0xb, 4]);
    let effect = MethodHash([31; 8]);
    let unit = || StarstreamValue::UNIT_VALUE;
    let args = || StarstreamValue([11, 12, 13, 14]);
    let result = || StarstreamValue([31, 32, 33, 34]);
    let other = || StarstreamValue([9; 4]);
    let call = |script| Step::CallCoordinator {
        script,
        arguments: args(),
    };
    let enter = |method, arguments| Step::EnterCall { method, arguments };
    let ret = |result: StarstreamValue| Step::Return {
        result: result.into(),
    };
    let receive = |result: StarstreamValue| Step::ReceiveCallResult {
        result: result.into(),
    };
    let accept = |name, body: Vec<Step>| Case {
        name,
        trace: entered(body),
        expected: Outcome::Accept,
        rejected_step: None,
    };
    // `rejected_step` counts the entry call and entry event (steps 0 and 1).
    let reject = |name, body: Vec<Step>, rejected_step| Case {
        name,
        trace: entered(body),
        expected: Outcome::Reject,
        rejected_step,
    };
    vec![
        accept(
            "nested and repeated script calls",
            vec![
                call(a),
                enter(a, args()),
                call(b),
                enter(b, args()),
                ret(result()),
                receive(result()),
                ret(unit()),
                receive(unit()),
                call(a),
                enter(a, args()),
                ret(result()),
                receive(result()),
                ret(unit()),
            ],
        ),
        accept(
            "a callee's effect reaches the caller, whose handler calls a script",
            vec![
                Step::PushEffectHandlerFrame { frame_id: 7 },
                Step::AddEffectHandler { effect },
                call(a),
                enter(a, args()),
                Step::RaiseEffect {
                    effect,
                    arguments: args(),
                },
                Step::EnterEffectHandler {
                    frame_id: 7,
                    effect,
                    arguments: args(),
                },
                call(b),
                enter(b, args()),
                ret(result()),
                receive(result()),
                Step::ExitEffectHandler {
                    handled: true,
                    result: result().into(),
                },
                Step::ResumeEffect {
                    result: result().into(),
                },
                ret(unit()),
                receive(unit()),
                Step::PopEffectHandlerFrame,
                ret(unit()),
            ],
        ),
        reject(
            "callee enters under another identity",
            vec![call(a), enter(b, args())],
            Some(3),
        ),
        reject(
            "callee enters with other arguments",
            vec![call(a), enter(a, other())],
            Some(3),
        ),
        reject(
            "callee omits its entry event",
            vec![call(a), ret(result())],
            Some(3),
        ),
        reject(
            "caller receives another result",
            vec![call(a), enter(a, args()), ret(result()), receive(other())],
            Some(5),
        ),
        reject(
            "caller continues without receiving the result",
            vec![call(a), enter(a, args()), ret(result()), ret(unit())],
            Some(5),
        ),
        reject(
            "callee never returns",
            vec![call(a), enter(a, args())],
            None,
        ),
        reject(
            "a UTXO cannot call a script",
            vec![
                Step::NewUtxo {
                    constructor: CONSTRUCTOR,
                    arguments: unit(),
                },
                Step::EnterConstructor {
                    constructor: CONSTRUCTOR,
                    arguments: unit(),
                },
                call(a),
            ],
            Some(4),
        ),
        reject(
            "callee returns with its own scope open",
            vec![
                call(a),
                enter(a, args()),
                Step::PushEffectHandlerFrame { frame_id: 8 },
                ret(result()),
            ],
            Some(5),
        ),
        reject(
            "callee cannot pop the scope it inherited",
            vec![
                Step::PushEffectHandlerFrame { frame_id: 7 },
                call(a),
                enter(a, args()),
                Step::PopEffectHandlerFrame,
            ],
            Some(5),
        ),
    ]
}

#[test]
#[ignore = "requires Quint; run with --include-ignored"]
fn script_calls_circuit_and_quint_agree() {
    let verifier = QuintVerifier::new().expect("repository-pinned Quint is available");
    for case in script_call_cases() {
        assert_eq!(
            quint_outcome(&verifier, &case),
            case.expected,
            "{}",
            case.name
        );
        for size in [1, 3, 8] {
            let outcome = match verify_sat_batched(&case.trace, size, &Default::default()) {
                Ok(()) => Outcome::Accept,
                Err(Error::Unsatisfied(_)) => Outcome::Reject,
                Err(error) => panic!("{}: batch size {size}: {error}", case.name),
            };
            assert_eq!(outcome, case.expected, "{}: batch size {size}", case.name);
        }
    }
}
