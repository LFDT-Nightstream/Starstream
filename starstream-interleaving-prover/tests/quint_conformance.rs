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
            result: StarstreamValue::default().into(),
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
            result: StarstreamValue::default().into(),
        },
        Step::Return {
            result: StarstreamValue::default().into(),
        },
    ]);

    Trace::new(steps)
}

fn cases() -> [Case; 8] {
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
            name: "method call after entering method",
            trace: method_call_trace(true),
            expected: Outcome::Accept,
            rejected_step: None,
        },
        Case {
            name: "return without entering method",
            trace: method_call_trace(false),
            expected: Outcome::Reject,
            rejected_step: Some(6),
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
