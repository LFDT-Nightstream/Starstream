use super::*;
use crate::ivc_state::CoroutineId::{self, Coord, Utxo};
use crate::{ccs::layout::*, verify_sat_batched};
use starstream_interleaving_spec::{Out, QuintVerifier, TransactionStatement};

const A: MethodHash = MethodHash([0xa, 1, 0xa, 2, 0xa, 3, 0xa, 4]);
const B: MethodHash = MethodHash([0xb, 1, 0xb, 2, 0xb, 3, 0xb, 4]);
const EFFECT: MethodHash = MethodHash([31; 8]);
const ARGS_A: StarstreamValue = StarstreamValue([11, 12, 13, 14]);
const ARGS_B: StarstreamValue = StarstreamValue([21, 22, 23, 24]);
const RESULT_A: StarstreamValue = StarstreamValue([31, 32, 33, 34]);
const RESULT_B: StarstreamValue = StarstreamValue([41, 42, 43, 44]);

/// Offsets into the body of [`nested_calls`].
const A_ENTER: usize = ENTRY_STEPS + 1;
const B_CALL: usize = ENTRY_STEPS + 2;
const B_RECEIVE: usize = ENTRY_STEPS + 5;
const A_RECEIVE: usize = ENTRY_STEPS + 7;

fn call(script: MethodHash, arguments: StarstreamValue) -> Step {
    Step::CallCoordinator { script, arguments }
}

fn enter(method: MethodHash, arguments: StarstreamValue) -> Step {
    Step::EnterCall { method, arguments }
}

fn ret(result: StarstreamValue) -> Step {
    Step::Return {
        result: Out(result),
    }
}

fn receive(result: StarstreamValue) -> Step {
    Step::ReceiveCallResult {
        result: Out(result),
    }
}

// The entry script, Coord(1), calls A as Coord(2), which calls B as Coord(3).
// Calling A again runs a fresh instance, Coord(4).
fn nested_calls() -> Trace {
    entered([
        call(A, ARGS_A),
        enter(A, ARGS_A),
        call(B, ARGS_B),
        enter(B, ARGS_B),
        ret(RESULT_B),
        receive(RESULT_B),
        ret(RESULT_A),
        receive(RESULT_A),
        call(A, ARGS_A),
        enter(A, ARGS_A),
        ret(RESULT_A),
        receive(RESULT_A),
        ret(StarstreamValue::UNIT_VALUE),
    ])
}

/// Event owners of [`nested_calls`], stated independently of normalization.
fn nested_owners() -> Vec<CoroutineId> {
    [1, 1, 2, 2, 3, 3, 2, 2, 1, 1, 4, 4, 1, 1]
        .map(Coord)
        .to_vec()
}

fn transaction(finalized: usize) -> Trace {
    let mut trace = nested_calls();
    trace
        .0
        .extend(std::iter::repeat_n(Step::FinalizeCoordinator, finalized));
    trace.0.push(Step::FinishTransaction);
    trace
}

fn statement() -> TransactionStatement {
    empty_statement()
}

// The entry script handles EFFECT. A raises it, and the handler itself calls
// B before resuming A: scripts inherit the caller's visible scopes, and a
// handler body is an ordinary place to call from.
fn effect_across_calls() -> Trace {
    entered([
        Step::PushEffectHandlerFrame { frame_id: 7 },
        Step::AddEffectHandler { effect: EFFECT },
        call(A, ARGS_A),
        enter(A, ARGS_A),
        Step::RaiseEffect {
            effect: EFFECT,
            arguments: ARGS_B,
        },
        Step::EnterEffectHandler {
            frame_id: 7,
            effect: EFFECT,
            arguments: ARGS_B,
        },
        call(B, ARGS_B),
        enter(B, ARGS_B),
        ret(RESULT_B),
        receive(RESULT_B),
        Step::ExitEffectHandler {
            handled: true,
            result: Out(RESULT_B),
        },
        Step::ResumeEffect {
            result: Out(RESULT_B),
        },
        ret(RESULT_A),
        receive(RESULT_A),
        Step::PopEffectHandlerFrame,
        ret(StarstreamValue::UNIT_VALUE),
    ])
}

#[test]
fn nested_and_repeated_script_calls_run_fresh_instances() {
    let trace = nested_calls();
    for size in [1, 3, 8] {
        verify_sat_batched(&trace, size, &Default::default()).unwrap();
    }
    let normalized = crate::step::normalize(&trace);
    let owners: Vec<_> = normalized
        .steps
        .iter()
        .filter(|step| step.opcode.has_event())
        .map(|step| step.event_owner)
        .collect();
    assert_eq!(owners, nested_owners());
    let targets: Vec<_> = normalized
        .steps
        .iter()
        .filter(|step| step.opcode.allocates_coordinator())
        .map(|step| step.call_target)
        .collect();
    assert_eq!(targets, [1, 2, 3, 4].map(Coord));
    // The entry coordinator is not on the invocation stack; callees are.
    let depths: Vec<_> = normalized.steps[ENTRY_STEPS..]
        .iter()
        .map(|step| step.instance_depth_after)
        .collect();
    assert_eq!(depths, [1, 1, 2, 2, 2, 1, 1, 0, 1, 1, 1, 0, 0]);
}

#[test]
fn callee_boundaries_are_bound_to_the_caller() {
    let other = StarstreamValue([9; 4]);
    for (index, forged) in [
        // The callee enters under another identity, or with other arguments.
        (A_ENTER, enter(B, ARGS_A)),
        (A_ENTER, enter(A, other.clone())),
        // The caller observes a result the callee did not return.
        (B_RECEIVE, receive(other.clone())),
        (A_RECEIVE, receive(other)),
    ] {
        let mut trace = nested_calls();
        trace.0[index] = forged;
        assert!(
            matches!(
                verify_sat(&trace, &Default::default()),
                Err(Error::Unsatisfied(Unsatisfied::Constraint { step, .. })) if step == index
            ),
            "step {index}"
        );
    }
    // A callee must open with its entry event, and its caller must consume
    // the result before continuing.
    for missing in [A_ENTER, B_RECEIVE] {
        let mut trace = nested_calls();
        trace.0.remove(missing);
        assert!(
            verify_sat(&trace, &Default::default()).is_err(),
            "step {missing}"
        );
    }
}

#[test]
fn only_coordinators_call_scripts() {
    let unit = StarstreamValue::UNIT_VALUE;
    let trace = entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        call(A, ARGS_A),
        enter(A, ARGS_A),
        ret(RESULT_A),
        receive(RESULT_A),
        ret(unit.clone()),
        Step::ReceiveNewUtxoHandle {
            resource: Out(ResourceHandle(0)),
        },
        ret(unit),
    ]);
    assert!(matches!(
        verify_sat(&trace, &Default::default()),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            constraint,
            ..
        })) if constraint == "call coordinator constraints"));
}

#[test]
fn a_script_call_cannot_reenter_an_existing_instance() {
    let trace = nested_calls();
    let (rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();
    // A's call to B instead targets the entry coordinator or A itself.
    for target in [Coord(1), Coord(2), Utxo(0)] {
        let mut forged = rows.clone();
        let row = &mut forged[B_CALL];
        row[COL_CALL_TARGET] = target.field();
        row[COL_CURR_AFTER] = target.field();
        crate::ccs::CALL_TARGET_ZERO_TEST.assign(row);
        range_check_layout().assign_bits(row).unwrap();
        assert!(
            matches!(
                verify_witness_rows(&forged, &preload),
                Err(Error::Unsatisfied(Unsatisfied::Constraint {
                    step: B_CALL,
                    constraint,
                    ..
                })) if constraint == "script calls target a fresh coordinator"),
            "{target:?}"
        );
    }
    // Nor can the allocator stay put, handing the same id to the next call.
    let mut stalled = rows;
    stalled[B_CALL][COL_NEXT_COORD_ID_AFTER] = stalled[B_CALL][COL_NEXT_COORD_ID_BEFORE];
    range_check_layout()
        .assign_bits(&mut stalled[B_CALL])
        .unwrap();
    assert!(matches!(
        verify_witness_rows(&stalled, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: B_CALL,
            constraint,
            ..
        })) if constraint == "next coordinator allocator transition"));
}

#[test]
fn effects_cross_script_boundaries() {
    let trace = effect_across_calls();
    for size in [1, 3, 8] {
        verify_sat_batched(&trace, size, &Default::default()).unwrap();
    }
    let normalized = crate::step::normalize(&trace);
    // The handler runs in the entry coordinator, which owns the scope, and
    // the resume is attributed to the raiser.
    let handler = ENTRY_STEPS + 5;
    assert_eq!(normalized.steps[handler].event_owner, Coord(1));
    assert_eq!(normalized.steps[handler + 6].event_owner, Coord(2));
    // A callee cannot mutate the scope it inherited.
    for mutation in [
        Step::AddEffectHandler { effect: A },
        Step::PopEffectHandlerFrame,
    ] {
        let mut trace = effect_across_calls();
        trace.0.insert(ENTRY_STEPS + 4, mutation);
        assert!(matches!(
            verify_sat(&trace, &Default::default()),
            Err(Error::Unsatisfied(Unsatisfied::Constraint {
                constraint,
                ..
            })) if constraint == "effect scope reads"));
    }
    // Nor return with a scope of its own still open.
    let mut trace = effect_across_calls();
    trace.0.insert(
        ENTRY_STEPS + 12,
        Step::PushEffectHandlerFrame { frame_id: 8 },
    );
    assert!(matches!(
        verify_sat(&trace, &Default::default()),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            constraint,
            ..
        })) if constraint == "local effect depth"));
}

#[test]
fn every_coordinator_root_is_enumerated_once() {
    let roots = crate::commitment::tests::expected(&nested_calls(), &nested_owners());
    assert_eq!(roots.len(), 4);
    for size in [1, 3, 8] {
        crate::verify_transaction_sat(&transaction(4), size, &statement(), &roots).unwrap();
    }
    // The scan can neither stop early nor run past the allocator.
    for finalized in [0, 3, 5] {
        assert!(
            crate::verify_transaction_sat(&transaction(finalized), 1, &statement(), &roots)
                .is_err(),
            "{finalized} coordinators finalized"
        );
    }
    // Each callee's final root is part of the transaction digest.
    for id in 1..=4 {
        let mut changed = roots.clone();
        changed.get_mut(&Coord(id).encoded()).unwrap()[0] += 1;
        assert!(
            crate::verify_transaction_sat(&transaction(4), 1, &statement(), &changed).is_err(),
            "Coord({id})"
        );
    }
    let mut missing = roots.clone();
    missing.remove(&Coord(4).encoded());
    assert!(crate::verify_transaction_sat(&transaction(4), 1, &statement(), &missing).is_err());
}

#[test]
fn coordinator_scan_reads_each_root_in_allocation_order() {
    let trace = transaction(4);
    let (rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();
    let first = trace
        .0
        .iter()
        .position(|step| matches!(step, Step::FinalizeCoordinator))
        .unwrap();
    for (offset, row) in rows[first..first + 4].iter().enumerate() {
        assert_eq!(row[COL_EVENT_OWNER], Coord(offset as u32 + 1).field());
    }
    // Reading another coordinator's root, or the same one twice, is rejected.
    for forged in [Coord(1), Coord(4)] {
        let mut changed = rows.clone();
        let row = &mut changed[first + 1];
        row[COL_EVENT_OWNER] = forged.field();
        crate::witness::assign_stride_columns(row);
        range_check_layout().assign_bits(row).unwrap();
        assert!(matches!(
            verify_witness_rows(&changed, &preload),
            Err(Error::Unsatisfied(Unsatisfied::Constraint { step, .. })) if step == first + 1
        ));
    }
}

#[test]
#[ignore = "requires Quint; run with --include-ignored"]
fn script_calls_match_quint() {
    let verifier = QuintVerifier::new().unwrap();
    verifier.verify(&nested_calls()).unwrap();
    verifier.verify(&effect_across_calls()).unwrap();
    verifier
        .verify_transaction(&transaction(4), &statement())
        .unwrap();
    for finalized in [3, 5] {
        assert!(
            verifier
                .verify_transaction(&transaction(finalized), &statement())
                .is_err()
        );
    }
}
