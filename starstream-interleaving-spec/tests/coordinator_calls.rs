use starstream_interleaving_spec::{
    MethodHash, Out, StarstreamValue, Step, Trace, events,
    interleaver::{InterleavingError, interleave_transaction},
};

const ENTRY: MethodHash = MethodHash([7; 8]);

fn traces() -> Vec<Trace> {
    let script = MethodHash([42; 8]);
    let arguments = StarstreamValue([1, 2, 3, 4]);
    let result = Out(StarstreamValue([5, 6, 7, 8]));
    vec![
        Trace::new([
            Step::EnterCall {
                method: ENTRY,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::CallCoordinator {
                script,
                arguments: arguments.clone(),
            },
            Step::ReceiveCallResult {
                result: result.clone(),
            },
            Step::Return {
                result: Out(StarstreamValue::UNIT_VALUE),
            },
        ]),
        // The callee enters and returns exactly like a called UTXO method.
        Trace::new([
            Step::EnterCall {
                method: script,
                arguments,
            },
            Step::Return { result },
        ]),
    ]
}

#[test]
fn coordinator_events_roundtrip_and_finalization_covers_both_instances() {
    let traces = traces();
    for step in traces.iter().flat_map(|t| &t.0) {
        let kind = events::EventKind::for_step(step).unwrap();
        assert_eq!(
            kind.decode(&events::encode(step), None, None).unwrap(),
            *step
        );
    }
    let transaction = interleave_transaction(&traces).unwrap();
    assert_eq!(
        transaction.trace.0[transaction.execution.0.len()..],
        [
            Step::FinalizeCoordinator,
            Step::FinalizeCoordinator,
            Step::FinishTransaction
        ]
    );
    assert_eq!(
        transaction.execution.0,
        vec![
            Step::BeginExecution {
                script: ENTRY,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            traces[0].0[0].clone(),
            traces[0].0[1].clone(),
            traces[1].0[0].clone(),
            traces[1].0[1].clone(),
            traces[0].0[2].clone(),
            traces[0].0[3].clone()
        ]
    );
}

// A handler in the caller runs for an effect raised by the callee, declines
// once, and calls a second script from the accepting activation.
fn effect_traces() -> Vec<Trace> {
    let effect = MethodHash([9; 8]);
    let script = MethodHash([42; 8]);
    let helper = MethodHash([43; 8]);
    let unit = StarstreamValue::UNIT_VALUE;
    let enter = |method| Step::EnterCall {
        method,
        arguments: unit.clone(),
    };
    let call = |script| Step::CallCoordinator {
        script,
        arguments: unit.clone(),
    };
    let ret = || Step::Return {
        result: Out(unit.clone()),
    };
    let receive = || Step::ReceiveCallResult {
        result: Out(unit.clone()),
    };
    let handler = |frame_id| Step::EnterEffectHandler {
        frame_id,
        effect,
        arguments: unit.clone(),
    };
    let exit = |handled| Step::ExitEffectHandler {
        handled,
        result: Out(unit.clone()),
    };
    let root = Trace::new([
        enter(ENTRY),
        Step::PushEffectHandlerFrame { frame_id: 1 },
        Step::AddEffectHandler { effect },
        Step::PushEffectHandlerFrame { frame_id: 2 },
        Step::AddEffectHandler { effect },
        call(script),
        handler(2),
        exit(false),
        handler(1),
        call(helper),
        receive(),
        exit(true),
        receive(),
        Step::PopEffectHandlerFrame,
        Step::PopEffectHandlerFrame,
        ret(),
    ]);
    let callee = Trace::new([
        enter(script),
        Step::RaiseEffect {
            effect,
            arguments: unit.clone(),
        },
        Step::ResumeEffect {
            result: Out(unit.clone()),
        },
        ret(),
    ]);
    vec![root, callee, Trace::new([enter(helper), ret()])]
}

#[test]
fn effects_cross_coordinator_boundaries_and_handlers_can_call_scripts() {
    let traces = effect_traces();
    let transaction = interleave_transaction(&traces).unwrap();
    let steps = &transaction.execution.0;
    // begin, 6 root steps, callee entry and raise, then the two activations
    // (root steps 6..12, with the helper's whole trace nested in the second).
    let mut expected = traces[0].0[6..10].to_vec();
    expected.extend(traces[2].0.iter().cloned());
    expected.extend(traces[0].0[10..12].iter().cloned());
    assert_eq!(steps[9..17], expected[..]);
    assert_eq!(
        transaction
            .trace
            .0
            .iter()
            .filter(|step| matches!(step, Step::FinalizeCoordinator))
            .count(),
        3
    );
}

#[test]
fn missing_and_unconsumed_coordinator_traces_are_rejected() {
    let mut traces = traces();
    assert!(matches!(
        interleave_transaction(&traces[..1]),
        Err(InterleavingError::MissingCoordinatorTrace { .. })
    ));
    traces[1].0.push(Step::Return {
        result: Out(StarstreamValue::UNIT_VALUE),
    });
    assert!(matches!(
        interleave_transaction(&traces),
        Err(InterleavingError::UnconsumedSteps { .. })
    ));
}

#[test]
#[ignore = "requires Quint; run through npm test"]
fn coordinator_call_interleavings_satisfy_quint() {
    let verifier = starstream_interleaving_spec::QuintVerifier::new().unwrap();
    for traces in [traces(), effect_traces()] {
        let transaction = interleave_transaction(&traces).unwrap();
        verifier.verify(&transaction.execution).unwrap();
        verifier
            .verify_transaction(&transaction.trace, &transaction.statement)
            .unwrap();
        // The statement binds the entry call.
        let mut wrong = transaction.statement.clone();
        wrong.entry.script.0[0] ^= 1;
        assert!(
            verifier
                .verify_transaction(&transaction.trace, &wrong)
                .is_err()
        );
    }
}
