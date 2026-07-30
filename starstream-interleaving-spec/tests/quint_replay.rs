use starstream_interleaving_spec::{
    ExecutionEvent, ExecutionTrace, MethodHash, QuintError, QuintVerifier, ResourceHandle,
    StarstreamValue, VerificationFailure,
};

fn method(n: u64) -> MethodHash {
    MethodHash([n, 0, 0, 0])
}

fn score_like_trace() -> ExecutionTrace {
    ExecutionTrace::new([
        ExecutionEvent::Init,
        ExecutionEvent::BeginNewUtxo {
            arguments: StarstreamValue(vec![55]),
        },
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(1) },
        ExecutionEvent::AdvertiseMethod { method: method(2) },
        ExecutionEvent::ReturnControl,
        ExecutionEvent::NewUtxoReturn {
            resource: ResourceHandle(7),
        },
        ExecutionEvent::CallMethod {
            resource: ResourceHandle(7),
            method: method(1),
            arguments: StarstreamValue(vec![13]),
        },
        ExecutionEvent::ReturnControl,
        ExecutionEvent::CallMethod {
            resource: ResourceHandle(7),
            method: method(2),
            arguments: StarstreamValue(vec![]),
        },
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method: method(3) },
        ExecutionEvent::ReturnControl,
        ExecutionEvent::CallMethod {
            resource: ResourceHandle(7),
            method: method(3),
            arguments: StarstreamValue(vec![42, 0]),
        },
        ExecutionEvent::CoroutineReturn,
        ExecutionEvent::ReturnControl,
        ExecutionEvent::CoordReturn,
    ])
}

fn assert_model_rejection(error: QuintError) -> VerificationFailure {
    let QuintError::Rejected(failure) = error else {
        panic!("expected Quint model rejection, got {error:?}");
    };
    assert!(
        !failure.stderr.contains("parsing failed"),
        "generated Quint failed to parse:\n{}",
        failure.stderr
    );
    failure
}

#[test]
fn accepts_a_score_like_execution() {
    QuintVerifier::default()
        .verify(&score_like_trace())
        .expect("valid execution should satisfy the Quint specification");
}

#[test]
fn rejects_an_incomplete_execution() {
    let trace = ExecutionTrace::new([ExecutionEvent::Init]);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("an initialized but running execution should be rejected");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_call_to_an_unadvertised_method() {
    let mut trace = score_like_trace();
    trace.0[7] = ExecutionEvent::CallMethod {
        resource: ResourceHandle(7),
        method: method(999),
        arguments: StarstreamValue(vec![13]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("invalid execution should be rejected");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_call_through_an_unknown_resource_handle() {
    let mut trace = score_like_trace();
    trace.0[7] = ExecutionEvent::CallMethod {
        resource: ResourceHandle(8),
        method: method(1),
        arguments: StarstreamValue(vec![13]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("an unbound resource handle should be rejected");
    assert_model_rejection(error);
}

#[test]
fn rejects_a_call_before_the_constructor_handle_is_returned() {
    let mut trace = score_like_trace();
    trace.0.remove(6);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("the resource must be bound before it can be called");
    assert_model_rejection(error);
}

#[test]
fn clear_abi_replaces_the_previous_method_set() {
    let mut trace = score_like_trace();
    trace.0[13] = ExecutionEvent::CallMethod {
        resource: ResourceHandle(7),
        method: method(1),
        arguments: StarstreamValue(vec![42, 0]),
    };

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("a method from the previous ABI epoch should be rejected");
    assert_model_rejection(error);
}

#[test]
fn coroutine_return_waits_for_the_export_to_return_control() {
    let mut trace = score_like_trace();
    trace.0.remove(15);

    let error = QuintVerifier::default()
        .verify(&trace)
        .expect_err("coroutine return alone must not return control to the coordinator");
    assert_model_rejection(error);
}

#[test]
fn coroutine_return_rejects_intervening_semantic_actions() {
    let mut trace = score_like_trace();
    trace.0.insert(15, ExecutionEvent::ClearAbi);

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
    assert_model_rejection(error);
}
