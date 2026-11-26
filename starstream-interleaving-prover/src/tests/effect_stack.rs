use super::*;
use crate::CoroutineId::{Coord, Utxo};
use crate::{ccs::layout::*, verify_sat_batched};
use starstream_interleaving_spec::{Out, QuintVerifier};

const FIRST: MethodHash = MethodHash([31; 8]);
const SECOND: MethodHash = MethodHash([47; 8]);
const ARGS: StarstreamValue = StarstreamValue([11, 12, 13, 14]);
const RESULT: StarstreamValue = StarstreamValue([21, 22, 23, 24]);

/// Row of B's raise in [`nested_invocations`].
const B_RAISE: usize = ENTRY_STEPS + 13;
/// Row of the handler entry that B's raise selects.
const B_ENTER: usize = ENTRY_STEPS + 14;

// The coordinator handles FIRST from A by constructing B. A's own scope also
// registers SECOND, but it is suspended: B's SECOND must reach the scope the
// coordinator's handler pushed, which branches from outside A's scopes.
fn nested_invocations() -> Trace {
    let unit = StarstreamValue::UNIT_VALUE;
    let ret = || Step::Return {
        result: Out(StarstreamValue::UNIT_VALUE),
    };
    let exit = || Step::ExitEffectHandler {
        handled: true,
        result: Out(RESULT),
    };
    let resume = || Step::ResumeEffect {
        result: Out(RESULT),
    };
    entered([
        // 2: coordinator scope 1 handles FIRST.
        Step::PushEffectHandlerFrame { frame_id: 1 },
        Step::AddEffectHandler { effect: FIRST },
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        // 6: A's scope 2 handles only SECOND, so FIRST skips it.
        Step::PushEffectHandlerFrame { frame_id: 2 },
        Step::AddEffectHandler { effect: SECOND },
        Step::RaiseEffect {
            effect: FIRST,
            arguments: ARGS,
        },
        Step::SkipEffectHandler,
        Step::EnterEffectHandler {
            frame_id: 1,
            effect: FIRST,
            arguments: ARGS,
        },
        // 11: the coordinator handler opens scope 3 and constructs B.
        Step::PushEffectHandlerFrame { frame_id: 3 },
        Step::AddEffectHandler { effect: SECOND },
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        Step::RaiseEffect {
            effect: SECOND,
            arguments: ARGS,
        },
        Step::EnterEffectHandler {
            frame_id: 3,
            effect: SECOND,
            arguments: ARGS,
        },
        exit(),
        resume(),
        ret(),
        Step::ReceiveNewResourceHandle {
            resource: Out(ResourceHandle(1)),
        },
        Step::PopEffectHandlerFrame,
        // 22: back in A after the coordinator handler.
        exit(),
        resume(),
        Step::PopEffectHandlerFrame,
        ret(),
        Step::ReceiveNewResourceHandle {
            resource: Out(ResourceHandle(0)),
        },
        Step::PopEffectHandlerFrame,
        ret(),
    ])
}

#[test]
fn search_follows_scopes_and_returns_to_each_raiser() {
    let trace = nested_invocations();
    for size in [1, 3, 8] {
        verify_sat_batched(&trace, size, &Default::default()).unwrap();
    }
    let normalized = crate::step::normalize(&trace);
    let owners: Vec<_> = normalized
        .steps
        .iter()
        .map(|step| step.event_owner)
        .collect();
    // The skip has no observer; handler entries are attributed to the owner.
    assert_eq!(&owners[8..11], &[Utxo(0), Coord(0), Coord(1)]);
    assert_eq!(
        &owners[B_RAISE..=18],
        &[Utxo(1), Coord(1), Coord(1), Utxo(1)]
    );
    // Program events split per instance; the scheduler resynthesizes skips.
    let mut per_instance = vec![Trace::default(); 3];
    for (step, owner) in trace.0.iter().zip(&owners) {
        let process = match owner {
            Coord(0) => continue,
            Coord(1) => 0,
            Utxo(0) => 1,
            Utxo(1) => 2,
            _ => unreachable!(),
        };
        per_instance[process].0.push(step.clone());
    }
    assert_eq!(
        starstream_interleaving_spec::interleaver::interleave_execution_only(&per_instance)
            .unwrap(),
        trace
    );
}

#[test]
fn suspended_scope_cannot_handle_a_nested_effect() {
    // A's scope 2 registers SECOND, but the handler's head skips past it.
    let mut trace = nested_invocations();
    trace.0[B_ENTER] = Step::EnterEffectHandler {
        frame_id: 2,
        effect: SECOND,
        arguments: ARGS,
    };
    assert!(verify_sat(&trace, &Default::default()).is_err());
    // Skipping the active scope would require it not to register SECOND.
    let mut skipped = nested_invocations();
    skipped.0.insert(B_ENTER, Step::SkipEffectHandler);
    // The honest registration read is 1, which a skip forbids...
    assert!(matches!(
        verify_sat(&skipped, &Default::default()),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: B_ENTER,
            constraint,
            ..
        })) if constraint == "effect scope reads"));
    // ...and claiming 0 contradicts the registration RAM. Stop after the
    // skip, since the search would then have no candidate left.
    skipped.0.truncate(B_ENTER + 1);
    let (mut rows, preload) = build_witness_rows(&skipped);
    verify_witness_rows(&rows, &preload).unwrap_err();
    rows[B_ENTER][COL_EFFECT_REGISTERED] = F::ZERO;
    range_check_layout()
        .assign_bits(&mut rows[B_ENTER])
        .unwrap();
    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory(_)))
    ));
}

#[test]
fn handler_owner_and_frame_id_come_from_the_selected_scope() {
    let trace = nested_invocations();
    let (rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();
    for (column, forged) in [
        // B and A instead of the coordinator owner.
        (COL_CALL_TARGET, u64::from(Utxo(1).encoded())),
        (COL_CALL_TARGET, u64::from(Utxo(0).encoded())),
        // A's frame id instead of the selected scope's.
        (COL_EFFECT_FRAME_ID, 2),
    ] {
        let mut changed = rows.clone();
        let row = &mut changed[B_ENTER];
        row[column] = F::new(forged);
        if column == COL_CALL_TARGET {
            row[COL_CURR_AFTER] = F::new(forged);
            row[COL_EVENT_OWNER] = F::new(forged);
            crate::ccs::CALL_TARGET_ZERO_TEST.assign(row);
            crate::witness::assign_stride_columns(row);
        }
        crate::commitment::assign_from_bus(row, crate::opcode::Opcode::EnterEffectHandler);
        range_check_layout().assign_bits(row).unwrap();
        assert!(
            matches!(
                verify_witness_rows(&changed, &preload),
                Err(Error::Unsatisfied(Unsatisfied::Memory(_)))
            ),
            "forged {column} = {forged}"
        );
    }
}

#[test]
fn scope_mutation_is_limited_to_the_current_invocation() {
    // A callee cannot register on, or pop, its caller's scope.
    for mutation in [
        Step::AddEffectHandler { effect: SECOND },
        Step::PopEffectHandlerFrame,
    ] {
        let mut trace = nested_invocations();
        trace.0.insert(6, mutation);
        assert!(matches!(
            verify_sat(&trace, &Default::default()),
            Err(Error::Unsatisfied(Unsatisfied::Constraint {
                step: 6,
                constraint,
                ..
            })) if constraint == "effect scope reads"));
    }
    // Nor return, or exit a handler, with its own scope still open.
    for unpopped in [21, 24] {
        let mut trace = nested_invocations();
        trace.0.remove(unpopped);
        assert!(matches!(
            verify_sat(&trace, &Default::default()),
            Err(Error::Unsatisfied(Unsatisfied::Constraint {
                constraint,
                ..
            })) if constraint == "local effect depth"));
    }
}

#[test]
fn scopes_cannot_change_during_a_pending_search() {
    let mut trace = nested_invocations();
    trace.0.insert(9, Step::PopEffectHandlerFrame);
    assert!(matches!(
        verify_sat(&trace, &Default::default()),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 9,
            constraint,
            ..
        })) if constraint == "scope changes and signer assertions require a live guest body"));
}

#[test]
fn scope_mutation_cannot_skip_the_handler_exit() {
    // A handler that resumes without exiting, hiding the missing exit behind
    // a pop that claims to leave the raiser waiting for its resume.
    let trace = entered([
        Step::PushEffectHandlerFrame { frame_id: 1 },
        Step::AddEffectHandler { effect: FIRST },
        Step::RaiseEffect {
            effect: FIRST,
            arguments: ARGS,
        },
        Step::EnterEffectHandler {
            frame_id: 1,
            effect: FIRST,
            arguments: ARGS,
        },
        Step::PushEffectHandlerFrame { frame_id: 2 },
        Step::PopEffectHandlerFrame,
        Step::ResumeEffect {
            result: Out(RESULT),
        },
    ]);
    let (mut rows, preload) = build_witness_rows(&trace);
    assert!(verify_witness_rows(&rows, &preload).is_err());
    let resume_pending = F::new(crate::ivc_state::CurrPhase::EffectResumePending.value() as u64);
    rows[7][COL_CURR_PHASE_AFTER] = resume_pending;
    rows[8][COL_CURR_PHASE_BEFORE] = resume_pending;
    rows[8][COL_PHASE_BEFORE_BITS_1_AND_2] = F::ONE;
    for row in &mut rows[7..=8] {
        range_check_layout().assign_bits(row).unwrap();
    }
    assert!(matches!(
        verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint {
            step: 7,
            constraint,
            ..
        })) if constraint == "scope changes and signer assertions require a live guest body"));
}

#[test]
#[ignore = "requires Quint; run with --include-ignored"]
fn scope_walk_matches_quint() {
    let trace = nested_invocations();
    QuintVerifier::new().unwrap().verify(&trace).unwrap();
    verify_sat_batched(&trace, 3, &Default::default()).unwrap();
}

#[test]
fn instance_stack_keeps_callee_until_receive() {
    let trace = nested_invocations();
    let normalized = crate::step::normalize(&trace);
    for (index, before, after) in [(19, 2, 2), (20, 2, 1), (25, 1, 1), (26, 1, 0), (27, 0, 0)] {
        assert_eq!(normalized.steps[index].instance_depth_before, before);
        assert_eq!(normalized.steps[index].instance_depth_after, after);
    }
    let (rows, preload) = build_witness_rows(&trace);
    verify_witness_rows(&rows, &preload).unwrap();
    let mut forged = rows.clone();
    // B's handle must resolve to B (3), not the suspended A (1).
    forged[20][COL_RESOURCE_RESOLVER_VALUE] = F::new(1);
    range_check_layout().assign_bits(&mut forged[20]).unwrap();
    assert!(matches!(
        verify_witness_rows(&forged, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory(_)))
    ));
    let mut popped_early = rows;
    popped_early[19][COL_INSTANCE_DEPTH_AFTER] = F::ONE;
    range_check_layout()
        .assign_bits(&mut popped_early[19])
        .unwrap();
    assert!(verify_witness_rows(&popped_early, &preload).is_err());
}

#[test]
fn cannot_raise_effect_between_constructor_return_and_receive() {
    let mut trace = nested_invocations();
    trace.0.insert(
        20,
        Step::RaiseEffect {
            effect: SECOND,
            arguments: StarstreamValue::UNIT_VALUE,
        },
    );
    assert!(verify_sat(&trace, &Default::default()).is_err());
}
