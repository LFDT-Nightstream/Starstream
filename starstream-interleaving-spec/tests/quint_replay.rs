use starstream_interleaving_spec::{
    ExecutionEvent, ExecutionTrace, MethodHash, QuintError, QuintVerifier, ResourceHandle,
    StarstreamValue, VerificationFailure, interleave_traces,
};

fn method(n: u64) -> MethodHash {
    MethodHash([n, 0, 0, 0])
}

fn score_like_trace() -> ExecutionTrace {
    let resource = ResourceHandle(7);
    let coordinator = ExecutionTrace::new([
        ExecutionEvent::NewUtxo {
            arguments: StarstreamValue(vec![55]),
            resource,
        },
        ExecutionEvent::CallMethod {
            resource,
            method: method(1),
            arguments: StarstreamValue(vec![13]),
            result: StarstreamValue(vec![21]),
        },
        ExecutionEvent::CallMethod {
            resource,
            method: method(2),
            arguments: StarstreamValue(vec![]),
            result: StarstreamValue::default(),
        },
        ExecutionEvent::CallMethod {
            resource,
            method: method(3),
            arguments: StarstreamValue(vec![42, 0]),
            result: StarstreamValue(vec![99]),
        },
        ExecutionEvent::CoordReturn,
    ]);
    let utxo = ExecutionTrace::new([
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(1) },
        ExecutionEvent::AdvertiseMethod { method: method(2) },
        ExecutionEvent::ReturnControl {
            result: StarstreamValue::default(),
        },
        ExecutionEvent::EnterMethod {
            arguments: StarstreamValue(vec![13]),
        },
        ExecutionEvent::ReturnControl {
            result: StarstreamValue(vec![21]),
        },
        ExecutionEvent::EnterMethod {
            arguments: StarstreamValue::default(),
        },
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(3) },
        ExecutionEvent::ReturnControl {
            result: StarstreamValue::default(),
        },
        ExecutionEvent::EnterMethod {
            arguments: StarstreamValue(vec![42, 0]),
        },
        ExecutionEvent::CoroutineReturn,
        ExecutionEvent::ReturnControl {
            result: StarstreamValue(vec![99]),
        },
    ]);

    interleave_traces(&[coordinator, utxo]).expect("score process traces should interleave")
}

fn assert_model_rejection(error: QuintError) -> VerificationFailure {
    let QuintError::Rejected(failure) = error else {
        panic!("expected Quint model rejection, got {error:?}");
    };
    let diagnostics = format!("{}\n{}", failure.stdout, failure.stderr);
    for marker in [
        "parsing failed",
        "typechecking failed",
        "static analysis error",
    ] {
        assert!(
            !diagnostics.contains(marker),
            "expected a model rejection, but Quint reported a static-analysis failure:\n\
             {diagnostics}"
        );
    }
    failure
}

#[test]
fn accepts_a_score_like_execution() {
    QuintVerifier::default()
        .verify(&score_like_trace())
        .expect("valid execution should satisfy the Quint specification");
}

#[test]
fn example_score_trace_satisfies_the_specification() {
    let trace: ExecutionTrace = serde_json::from_str(include_str!("../examples/score-trace.json"))
        .expect("example score trace should be valid JSON");
    assert_eq!(
        trace,
        score_like_trace(),
        "the checked-in example should match the canonical score-like trace"
    );

    QuintVerifier::default()
        .verify(&trace)
        .expect("example score trace should satisfy the Quint specification");
}

#[test]
fn rejects_a_constructor_that_terminates_before_its_initial_yield() {
    let resource = ResourceHandle(5);
    let coordinator = ExecutionTrace::new([
        ExecutionEvent::NewUtxo {
            arguments: StarstreamValue::default(),
            resource,
        },
        ExecutionEvent::CoordReturn,
    ]);
    let terminated_constructor = ExecutionTrace::new([
        ExecutionEvent::CoroutineReturn,
        ExecutionEvent::ReturnControl {
            result: StarstreamValue::default(),
        },
    ]);
    let trace = interleave_traces(&[coordinator, terminated_constructor])
        .expect("the scheduler can merge the trace before semantic validation");

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("a constructor must reach its initial yield before returning");
    assert_model_rejection(error);
}

#[test]
fn reports_generated_quint_static_errors_separately() {
    let verifier = QuintVerifier::default().with_spec(format!(
        "{}/tests/invalid-replay-spec.qnt",
        env!("CARGO_MANIFEST_DIR")
    ));
    let error = verifier
        .verify(&ExecutionTrace::new([ExecutionEvent::Init]))
        .expect_err("the invalid Quint fixture should fail typechecking");

    let QuintError::Typecheck(failure) = error else {
        panic!("expected a Quint typechecking failure, got {error:?}");
    };
    assert!(
        failure.stderr.contains("typechecking failed")
            || failure.stdout.contains("typechecking failed"),
        "missing Quint typechecking diagnostic: {failure:?}"
    );
}

#[test]
fn rejects_a_trace_truncated_before_coord_return() {
    let mut trace = score_like_trace();
    assert_eq!(trace.0.pop(), Some(ExecutionEvent::CoordReturn));

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("a trace truncated before coordination-script return should be rejected");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_call_to_an_unadvertised_method() {
    let mut trace = score_like_trace();
    trace.0[6] = ExecutionEvent::CallMethod {
        resource: ResourceHandle(7),
        method: method(999),
        arguments: StarstreamValue(vec![13]),
        result: StarstreamValue(vec![21]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("invalid execution should be rejected");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_call_through_an_unknown_resource_handle() {
    let mut trace = score_like_trace();
    trace.0[6] = ExecutionEvent::CallMethod {
        resource: ResourceHandle(8),
        method: method(1),
        arguments: StarstreamValue(vec![13]),
        result: StarstreamValue(vec![21]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("an unbound resource handle should be rejected");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_method_result_that_differs_between_caller_and_callee() {
    let mut trace = score_like_trace();
    trace.0[8] = ExecutionEvent::ReturnControl {
        result: StarstreamValue(vec![999]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("the callee result must match the atomic import result");
    assert_model_rejection(error);
}

#[test]
fn rejects_method_arguments_that_differ_between_caller_and_callee() {
    let mut trace = score_like_trace();
    trace.0[7] = ExecutionEvent::EnterMethod {
        arguments: StarstreamValue(vec![999]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("the callee arguments must match the atomic import arguments");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_call_before_the_constructor_returns_control() {
    let mut trace = score_like_trace();
    trace.0.remove(5);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("the resource must remain pending until the constructor returns control");
    assert_model_rejection(error);
}

#[test]
fn clear_abi_replaces_the_previous_method_set() {
    let mut trace = score_like_trace();
    trace.0[14] = ExecutionEvent::CallMethod {
        resource: ResourceHandle(7),
        method: method(1),
        arguments: StarstreamValue(vec![42, 0]),
        result: StarstreamValue(vec![99]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("a method from the previous ABI epoch should be rejected");
    assert_model_rejection(error);
}

#[test]
fn coroutine_return_waits_for_the_export_to_return_control() {
    let mut trace = score_like_trace();
    trace.0.remove(17);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("coroutine return alone must not return control to the coordinator");
    assert_model_rejection(error);
}

#[test]
fn coroutine_return_rejects_intervening_semantic_actions() {
    let mut trace = score_like_trace();
    trace.0.insert(17, ExecutionEvent::ClearAbi);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("only ReturnControl may follow CoroutineReturn");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_second_init() {
    let mut trace = score_like_trace();
    trace.0.insert(1, ExecutionEvent::Init);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("invalid execution should be rejected");
    assert!(
        matches!(error, QuintError::RepeatedInit { index: 1 }),
        "expected structural rejection of the repeated Init, got {error:?}"
    );
}
