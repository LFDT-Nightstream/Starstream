use starstream_interleaving_prover::{Error, verify_sat};
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

fn cases() -> [Case; 18] {
    let accepted = constructor_trace([0, 1, 2, 3]);
    let repeated_arguments = constructor_trace([7, 7, 7, 7]);
    let minimal_constructor = minimal_constructor_trace([0, 1, 2, 3]);
    let mut mismatched_arguments = constructor_trace([1, 2, 3, 4]);
    mismatched_arguments.0[1] = Step::EnterConstructor {
        arguments: vec![0, 1, 2, 3].into(),
    };
    let return_before_constructor_entry = Trace::new([
        Step::NewUtxo {
            arguments: vec![0, 1, 2, 3].into(),
            resource: ResourceHandle(0).into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ]);
    let mut wrong_enter_method = method_call_trace(true);
    wrong_enter_method.0[5] = Step::EnterMethod {
        method: MethodHash([2, 1, 1, 1]),
        arguments: vec![1, 2, 3, 4].into(),
    };
    let yield_from_coordinator = Trace::new([
        Step::YieldBegin,
        Step::Return {
            result: StarstreamValue::default().into(),
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
            rejected_step: Some(4),
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
            rejected_step: Some(5),
        },
        Case {
            name: "call to unregistered method",
            trace: unregistered_method_call_trace(),
            expected: Outcome::Reject,
            rejected_step: Some(4),
        },
        Case {
            name: "duplicate method registration is idempotent",
            trace: duplicate_method_registration_trace(),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "method replacement after yield",
            trace: method_reyield_trace(MethodHash([2, 1, 1, 1])),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "stale method after yield",
            trace: method_reyield_trace(MethodHash([1, 1, 1, 1])),
            expected: Outcome::Reject,
            rejected_step: Some(9),
        },
        Case {
            name: "return with wrong result",
            trace: method_call_result_trace(true, vec![7, 8].into(), vec![7, 9].into()),
            expected: Outcome::Reject,
            rejected_step: Some(6),
        },
        Case {
            name: "return without entering method",
            trace: method_call_trace(false),
            expected: Outcome::Reject,
            rejected_step: Some(5),
        },
        Case {
            name: "yield from coordinator",
            trace: yield_from_coordinator,
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
