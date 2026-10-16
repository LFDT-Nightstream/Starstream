use std::iter;

use neo_application::{ApplicationRelation, TaggedR1csBuilder, ZeroTest};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;

pub(crate) use crate::ccs::layout::{COL_ONE, PUBLIC_INPUTS};
use crate::{
    ccs::{
        layout::{
            COL_ABI_GENERATION_ADDR, COL_ABI_GENERATION_AFTER, COL_ABI_GENERATION_BEFORE,
            COL_CALL_FRAME_CALLEE, COL_CALL_FRAME_CONSTRUCTOR, COL_CALL_FRAME_IS_EFFECT,
            COL_CALL_FRAME_SAVED_PHASE, COL_CALL_SP_AFTER, COL_CALL_SP_BEFORE,
            COL_CALL_SP_BEFORE_INVERSE, COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8,
            COL_CALL_STACK_MUL_STRIDE_8, COL_CALL_STACK_POP, COL_CALL_STACK_PUSH,
            COL_CALL_STACK_TOP, COL_CALL_TARGET, COL_CURR_AFTER, COL_CURR_BEFORE,
            COL_CURR_PHASE_AFTER, COL_CURR_PHASE_BEFORE, COL_EFFECT_HANDLED,
            COL_ENABLED_METHOD_LOG_ADDR, COL_ENABLED_METHOD_LOG_GENERATION,
            COL_ENABLED_METHOD_LOG_LEN_AFTER, COL_ENABLED_METHOD_LOG_LEN_BEFORE,
            COL_ENABLED_METHOD_LOG_UTXO, COL_EVENT_OWNER_STRIDE_8,
            COL_HANDLER_SEARCH_REMAINING_AFTER, COL_HANDLER_SEARCH_REMAINING_BEFORE,
            COL_METHOD_INDEX, COL_METHOD_LOOKUP, COL_METHOD_TABLE_ADDR, COL_NEXT_UTXO_ID_AFTER,
            COL_NEXT_UTXO_ID_BEFORE, COL_RESOURCE_RESOLVER_ADDR_CID, COL_RESOURCE_RESOLVER_READ,
            COL_RESOURCE_RESOLVER_VALUE, COL_RESOURCE_RESOLVER_WRITE, COL_RESULT_ROOT,
            COL_RETURN_REQUIRES_UNIT, COL_SEL_CALL_METHOD, COL_SEL_NEW_UTXO, COL_SEL_RAISE_EFFECT,
            COL_SEL_RECEIVE_NEW_UTXO_HANDLE, SELECTORS, range_check_layout,
        },
        tags::{ConstraintScope, always, opcode_tag, opcode_tags},
    },
    ivc_state::CoroutineKind,
    opcode::Opcode,
};

pub(crate) const CALL_TARGET_ZERO_TEST: ZeroTest = ZeroTest::column(
    COL_CALL_TARGET,
    layout::COL_CALL_TARGET_INVERSE,
    layout::COL_CALL_TARGET_IS_ZERO,
);

pub(crate) mod layout;
pub(crate) mod tags;

type R1csBuilder = neo_application::R1csBuilder<ConstraintScope>;

pub fn build_relation() -> Result<ApplicationRelation<ConstraintScope>, crate::Error> {
    let range_checks = range_check_layout();
    let column_registry = range_checks.columns().clone();

    let mut builder = R1csBuilder::new(column_registry.column_count(), PUBLIC_INPUTS, COL_ONE)?;
    let mut b = builder.tagged(always("unlabeled"));

    b.with_tag(always("resource bindings are fresh"), |b| {
        b.push_gated_linear_zero(
            COL_RESOURCE_RESOLVER_WRITE,
            [(layout::COL_RESOURCE_RESOLVER_BEFORE, F::ONE)],
        );
    });

    b.with_tag(always("opcode selectors are one-hot"), |b| {
        b.push_row(
            SELECTORS.iter().map(|col| (*col, F::ONE)),
            [(COL_ONE, F::ONE)],
            [(COL_ONE, F::ONE)],
        );
    });

    b.with_tag(always("execution requires nonempty call stack"), |b| {
        b.push_row(
            [(COL_CALL_SP_BEFORE, F::ONE)],
            [(COL_CALL_SP_BEFORE_INVERSE, F::ONE)],
            Opcode::all()
                .into_iter()
                .filter(Opcode::is_execution)
                .map(|opcode| (opcode.selector(), F::ONE)),
        );
    });

    b.with_tag(
        opcode_tag("padding preserves state", Opcode::Padding),
        |b| {
            // The commitment write port is disabled on padding. The event
            // gadget stutters its chain locally, without reading/writing RAM.
            for group in crate::ivc_state::build_ivc_state_continuity_links() {
                for link in group.links {
                    require_equal(
                        b,
                        Opcode::Padding,
                        link.previous_step_column,
                        link.next_step_column,
                    );
                }
            }
        },
    );

    b.with_tag(always("non-execution preserves coroutine state"), |b| {
        // Padding already preserves these carried columns above.
        let non_execution = Opcode::all()
            .into_iter()
            .filter(|op| !op.is_execution() && *op != Opcode::Padding)
            .map(|op| (op.selector(), F::ONE))
            .collect::<Vec<_>>();
        b.push_row(
            non_execution.iter().copied(),
            [
                (COL_CURR_PHASE_AFTER, F::ONE),
                (COL_CURR_PHASE_BEFORE, -F::ONE),
            ],
            [],
        );
    });

    crate::transaction::constraints(&mut b);

    let curr_switching_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(Opcode::switches_curr)
        .collect::<Vec<_>>();

    b.with_tag(always("current coroutine transition"), |b| {
        b.push_row(
            curr_switching_opcodes
                .iter()
                .map(|opcode| (opcode.selector(), F::ONE)),
            [(COL_CALL_TARGET, F::ONE), (COL_CURR_BEFORE, -F::ONE)],
            [(COL_CURR_AFTER, F::ONE), (COL_CURR_BEFORE, -F::ONE)],
        );
    });

    b.with_tag(always("call target zero test"), |b| {
        CALL_TARGET_ZERO_TEST.push_constraints(b);
    });

    b.with_tag(
        opcode_tags(
            "call stack push global condition",
            &[Opcode::NewUtxo, Opcode::CallMethod, Opcode::RaiseEffect],
        ),
        |b| {
            b.push_linear_zero([
                (COL_SEL_NEW_UTXO, F::ONE),
                (COL_SEL_CALL_METHOD, F::ONE),
                (COL_SEL_RAISE_EFFECT, F::ONE),
                (COL_CALL_STACK_PUSH, -F::ONE),
            ]);
        },
    );

    // One-hot selectors make the RHS the active opcode's required flag value.
    let effect_frame_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(|op| op.required_effect_frame().is_some())
        .collect::<Vec<_>>();
    b.with_tag(
        opcode_tags("effect frame requirements", &effect_frame_opcodes),
        |b| {
            b.push_row(
                effect_frame_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE)),
                [(COL_CALL_FRAME_IS_EFFECT, F::ONE)],
                effect_frame_opcodes
                    .iter()
                    .filter(|op| op.required_effect_frame() == Some(true))
                    .map(|op| (op.selector(), F::ONE)),
            );
        },
    );

    let constructor_frame_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(|op| op.required_constructor_frame().is_some())
        .collect::<Vec<_>>();
    b.with_tag(
        opcode_tags("constructor frame requirements", &constructor_frame_opcodes),
        |b| {
            b.push_row(
                constructor_frame_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE)),
                [(COL_CALL_FRAME_CONSTRUCTOR, F::ONE)],
                constructor_frame_opcodes
                    .iter()
                    .filter(|op| op.required_constructor_frame() == Some(true))
                    .map(|op| (op.selector(), F::ONE)),
            );
        },
    );

    b.with_tag(always("next UTXO allocator transition"), |b| {
        b.push_linear_zero([
            (COL_NEXT_UTXO_ID_AFTER, F::ONE),
            (COL_NEXT_UTXO_ID_BEFORE, -F::ONE),
            (COL_SEL_NEW_UTXO, -F::ONE),
            (layout::COL_SEL_SET_STORAGE, -F::ONE),
        ]);
    });

    b.with_tag(always("resource resolver access flags"), |b| {
        b.push_linear_zero([
            (COL_RESOURCE_RESOLVER_READ, F::ONE),
            (COL_SEL_CALL_METHOD, -F::ONE),
        ]);
        b.push_linear_zero([
            (COL_RESOURCE_RESOLVER_WRITE, F::ONE),
            (COL_SEL_RECEIVE_NEW_UTXO_HANDLE, -F::ONE),
            (layout::COL_SEL_SET_STORAGE, -F::ONE),
        ]);
    });

    b.with_tag(always("enabled method log transition"), |b| {
        b.push_linear_zero([
            (COL_METHOD_LOOKUP, F::ONE),
            (Opcode::RegisterMethod.selector(), -F::ONE),
            (Opcode::PreloadMethod.selector(), -F::ONE),
            (Opcode::CallMethod.selector(), -F::ONE),
            (Opcode::ReadAbi.selector(), -F::ONE),
            (Opcode::RaiseEffect.selector(), -F::ONE),
            (Opcode::EnterEffectHandler.selector(), -F::ONE),
        ]);
        b.push_linear_zero([
            (COL_ENABLED_METHOD_LOG_LEN_AFTER, F::ONE),
            (COL_ENABLED_METHOD_LOG_LEN_BEFORE, -F::ONE),
            (Opcode::RegisterMethod.selector(), -F::ONE),
            (Opcode::PreloadMethod.selector(), -F::ONE),
        ]);
    });

    b.with_tag(always("method table lookup addresses"), |b| {
        for (offset, address) in COL_METHOD_TABLE_ADDR.iter().enumerate() {
            b.push_row(
                [(COL_METHOD_LOOKUP, F::ONE)],
                [
                    (COL_METHOD_INDEX, F::new(8)),
                    (COL_ONE, F::new(offset as u64)),
                ],
                [(*address, F::ONE)],
            );
        }
    });

    // TODO(perf): Consider gating these call-stack address derivations and
    // assigning zero on rows without a call-stack memory access. Nightstream's
    // Ajtai commitment path has sparse/zero fast paths, so at large batch sizes
    // the commitment savings may outweigh the cost of gated constraints.
    // Benchmark both layouts once proof batching is wired.
    //
    // A push writes at the next free address (`sp`), while peeks and pops read
    // the current top (`sp - 1`). The corresponding access flag, or Return's
    // selector for its data-dependent pop, selects the latter address.
    b.with_tag(always("call stack expected-address stride (8)"), |b| {
        COL_CALL_STACK_MUL_STRIDE_8
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (COL_CALL_SP_BEFORE, F::new(8)),
                    (COL_CALL_STACK_TOP, -F::new(8)),
                    (COL_CALL_STACK_POP, -F::new(8)),
                    (layout::COL_SEL_RETURN, -F::new(8)),
                    (COL_ONE, F::new(i as u64)),
                    (*col, -F::ONE),
                ]);
            });
    });

    b.with_tag(always("call stack expected-address stride (8)"), |b| {
        COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (COL_CALL_SP_BEFORE, F::new(8)),
                    (COL_CALL_STACK_TOP, -F::new(8)),
                    (COL_CALL_STACK_POP, -F::new(8)),
                    (layout::COL_SEL_RETURN, -F::new(8)),
                    (COL_ONE, F::new(i as u64)),
                    (*col, -F::ONE),
                ]);
            });
    });

    b.with_tag(always("event owner mul stride (8)"), |b| {
        COL_EVENT_OWNER_STRIDE_8
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (layout::COL_EVENT_OWNER, F::new(8)),
                    (COL_ONE, F::new(i as u64)),
                    (*col, -F::ONE),
                ]);
            });
    });

    let call_stack_pushing_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(|op| op.pushes_to_call_stack())
        .collect::<Vec<_>>();

    b.with_tag(
        opcode_tags("sp' = sp+1 on push", &call_stack_pushing_opcodes),
        |b| {
            b.push_row(
                call_stack_pushing_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE)),
                [
                    (COL_CALL_SP_AFTER, F::ONE),
                    (COL_CALL_SP_BEFORE, -F::ONE),
                    (COL_ONE, -F::ONE),
                ],
                [],
            );

            b.push_linear_zero(
                call_stack_pushing_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE))
                    .chain(iter::once((COL_CALL_STACK_PUSH, -F::ONE))),
            );
        },
    );

    let call_stack_poping_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(|op| op.pops_from_call_stack_unconditionally())
        .collect::<Vec<_>>();

    b.with_tag(
        opcode_tags("sp' = sp-1 on pop", &call_stack_poping_opcodes),
        |b| {
            b.push_row(
                call_stack_poping_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE)),
                [
                    (COL_CALL_SP_AFTER, F::ONE),
                    (COL_CALL_SP_BEFORE, -F::ONE),
                    (COL_ONE, F::ONE),
                ],
                [],
            );

            b.push_linear_zero(
                call_stack_poping_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE))
                    .chain(iter::once((COL_CALL_STACK_POP, -F::ONE))),
            );
        },
    );

    let call_stack_preserving_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(|op| {
            !op.pushes_to_call_stack()
                && !op.pops_from_call_stack_unconditionally()
                && *op != Opcode::Return
        })
        .collect::<Vec<_>>();

    b.with_tag(
        opcode_tags(
            "sp' = sp when the call stack is preserved",
            &call_stack_preserving_opcodes,
        ),
        |b| {
            b.push_row(
                call_stack_preserving_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE)),
                [(COL_CALL_SP_AFTER, F::ONE), (COL_CALL_SP_BEFORE, -F::ONE)],
                [],
            );
        },
    );

    let call_stack_peeking_opcodes = Opcode::all()
        .iter()
        .copied()
        .filter(|op| op.peeks_call_stack_top())
        .collect::<Vec<_>>();

    b.with_tag(
        opcode_tags(
            "reads the top of the stack without popping",
            &call_stack_peeking_opcodes,
        ),
        |b| {
            b.push_linear_zero(
                call_stack_peeking_opcodes
                    .iter()
                    .map(|op| (op.selector(), F::ONE))
                    .chain(iter::once((COL_CALL_STACK_TOP, -F::ONE))),
            );
        },
    );

    b.with_tag(
        opcode_tag("new utxo constraints", Opcode::NewUtxo),
        visit_new_utxo,
    );
    b.with_tag(
        opcode_tag(
            "receive new utxo handle constraints",
            Opcode::ReceiveNewUtxoHandle,
        ),
        visit_receive_new_utxo_handle,
    );

    b.with_tag(
        opcode_tag("enter constructor constraints", Opcode::EnterConstructor),
        visit_enter_constructor,
    );
    b.with_tag(
        opcode_tag("yield begin constraints", Opcode::YieldBegin),
        visit_yield_begin,
    );
    b.with_tag(
        opcode_tag("register method constraints", Opcode::RegisterMethod),
        visit_register_method,
    );
    b.with_tag(
        opcode_tag("return constraints", Opcode::Return),
        visit_return,
    );
    b.with_tag(
        opcode_tag("call method constraints", Opcode::CallMethod),
        visit_call_method,
    );
    b.with_tag(
        opcode_tag(
            "receive method result constraints",
            Opcode::ReceiveMethodResult,
        ),
        visit_receive_method_result,
    );
    b.with_tag(
        opcode_tag("enter method constraints", Opcode::EnterMethod),
        visit_enter_method,
    );
    b.with_tag(
        opcode_tag("raise effect constraints", Opcode::RaiseEffect),
        visit_raise_effect,
    );
    b.with_tag(
        opcode_tag(
            "enter effect handler constraints",
            Opcode::EnterEffectHandler,
        ),
        visit_enter_effect_handler,
    );
    b.with_tag(
        opcode_tag("exit effect handler constraints", Opcode::ExitEffectHandler),
        visit_exit_effect_handler,
    );
    b.with_tag(
        opcode_tag("resume effect constraints", Opcode::ResumeEffect),
        visit_resume_effect,
    );

    crate::commitment::constraints(&mut b);
    range_checks.push_constraints(&mut b, ConstraintScope::Always);

    let r1cs = builder.build()?;

    Ok(ApplicationRelation::new(r1cs, column_registry)?)
}

fn require_phase_before(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    opcode: Opcode,
    phase: crate::ivc_state::CurrPhase,
) {
    b.push_gated_linear_zero(
        opcode.selector(),
        [
            (COL_CURR_PHASE_BEFORE, F::ONE),
            (COL_ONE, -F::new(phase.value() as u64)),
        ],
    );
}

fn require_phase_after(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    opcode: Opcode,
    phase: crate::ivc_state::CurrPhase,
) {
    b.push_gated_linear_zero(
        opcode.selector(),
        [
            (COL_CURR_PHASE_AFTER, F::ONE),
            (COL_ONE, -F::new(phase.value() as u64)),
        ],
    );
}

fn phase_bits(column: usize) -> [usize; 4] {
    assert!(
        [COL_CURR_PHASE_BEFORE, COL_CURR_PHASE_AFTER].contains(&column),
        "this function should only be called on the phase columns"
    );

    let bits = range_check_layout()
        .bit_columns_for(column)
        .expect("phase column has decomposition bits");

    [bits.start, bits.start + 1, bits.start + 2, bits.start + 3]
}

fn low_bit(column: usize) -> usize {
    range_check_layout()
        .bit_columns_for(column)
        .expect("packed coroutine ID column has decomposition bits")
        .start
}

fn require_coroutine_kind(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    opcode: Opcode,
    column: usize,
    expected: CoroutineKind,
) {
    b.push_gated_linear_zero(
        opcode.selector(),
        [
            (low_bit(column), F::ONE),
            (COL_ONE, -F::new(u64::from(expected.tag()))),
        ],
    );
}

fn require_equal(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    opcode: Opcode,
    left: usize,
    right: usize,
) {
    b.push_gated_linear_zero(opcode.selector(), [(left, F::ONE), (right, -F::ONE)]);
}

fn visit_enter_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_coroutine_kind(b, Opcode::EnterMethod, COL_CURR_BEFORE, CoroutineKind::Utxo);
    require_phase_before(
        b,
        Opcode::EnterMethod,
        crate::ivc_state::CurrPhase::MethodEnterPending,
    );
    require_phase_after(
        b,
        Opcode::EnterMethod,
        crate::ivc_state::CurrPhase::Executing,
    );
}

fn visit_call_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_coroutine_kind(b, Opcode::CallMethod, COL_CURR_BEFORE, CoroutineKind::Coord);
    require_coroutine_kind(b, Opcode::CallMethod, COL_CALL_TARGET, CoroutineKind::Utxo);
    require_phase_before(
        b,
        Opcode::CallMethod,
        crate::ivc_state::CurrPhase::Executing,
    );
    require_phase_after(
        b,
        Opcode::CallMethod,
        crate::ivc_state::CurrPhase::MethodEnterPending,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_RESOURCE_RESOLVER_ADDR_CID,
        COL_CURR_BEFORE,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_RESOURCE_RESOLVER_VALUE,
        COL_CALL_TARGET,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_CALL_FRAME_CALLEE,
        COL_CALL_TARGET,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_CALL_FRAME_SAVED_PHASE,
        COL_CURR_PHASE_BEFORE,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_ABI_GENERATION_ADDR,
        COL_CALL_TARGET,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_ENABLED_METHOD_LOG_UTXO,
        COL_CALL_TARGET,
    );
    require_equal(
        b,
        Opcode::CallMethod,
        COL_ENABLED_METHOD_LOG_GENERATION,
        COL_ABI_GENERATION_BEFORE,
    );
}

fn visit_return(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    // Bits 2 and 3 are both clear exactly for Executing,
    // ConstructorExecuting, and Registering.
    let phase_before = phase_bits(COL_CURR_PHASE_BEFORE);
    b.push_gated_linear_zero(
        Opcode::Return.selector(),
        [(phase_before[2], F::ONE), (phase_before[3], F::ONE)],
    );
    require_coroutine_kind(b, Opcode::Return, COL_CURR_AFTER, CoroutineKind::Coord);
    // Constructors and the root coordinator have statically unit return
    // types. A nonzero frame caller means that a caller-side Receive* event
    // still has to consume this frame. Constructor and root indicators are
    // mutually exclusive, so this relation is linear.
    b.push_gated_linear_zero(
        Opcode::Return.selector(),
        [
            (COL_RETURN_REQUIRES_UNIT, F::ONE),
            (COL_CALL_FRAME_CONSTRUCTOR, -F::ONE),
            (layout::COL_CALL_TARGET_IS_ZERO, -F::ONE),
        ],
    );
    for limb in COL_RESULT_ROOT {
        b.push_row([(COL_RETURN_REQUIRES_UNIT, F::ONE)], [(limb, F::ONE)], []);
    }
    // A root return pops immediately. Other program returns leave their frame
    // live for ReceiveMethodResult or ReceiveNewUtxoHandle to consume.
    b.push_gated_linear_zero(
        Opcode::Return.selector(),
        [
            (COL_CALL_SP_AFTER, F::ONE),
            (COL_CALL_SP_BEFORE, -F::ONE),
            (layout::COL_CALL_TARGET_IS_ZERO, F::ONE),
        ],
    );
    let root_phase = F::new(crate::ivc_state::CurrPhase::Executing.value() as u64);
    let method_phase = F::new(crate::ivc_state::CurrPhase::MethodResultPending.value() as u64);
    let constructor_phase =
        F::new(crate::ivc_state::CurrPhase::NewUtxoHandlePending.value() as u64);
    // Start with the method receive phase, then select the root or constructor
    // phase using their mutually exclusive indicators.
    b.push_gated_linear_zero(
        Opcode::Return.selector(),
        [
            (COL_CURR_PHASE_AFTER, F::ONE),
            (COL_ONE, -method_phase),
            (layout::COL_CALL_TARGET_IS_ZERO, method_phase - root_phase),
            (COL_CALL_FRAME_CONSTRUCTOR, method_phase - constructor_phase),
        ],
    );
}

fn visit_register_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_coroutine_kind(
        b,
        Opcode::RegisterMethod,
        COL_CURR_BEFORE,
        CoroutineKind::Utxo,
    );
    // ConstructorExecuting (0001) and Registering (0011) differ only in bit 1.
    let phase_before = phase_bits(COL_CURR_PHASE_BEFORE);
    b.push_gated_linear_zero(
        Opcode::RegisterMethod.selector(),
        [(phase_before[0], F::ONE), (COL_ONE, -F::ONE)],
    );
    b.push_gated_linear_zero(
        Opcode::RegisterMethod.selector(),
        [(phase_before[2], F::ONE), (phase_before[3], F::ONE)],
    );
    require_phase_after(
        b,
        Opcode::RegisterMethod,
        crate::ivc_state::CurrPhase::Registering,
    );
    require_equal(
        b,
        Opcode::RegisterMethod,
        COL_ABI_GENERATION_ADDR,
        COL_CURR_BEFORE,
    );
    require_equal(
        b,
        Opcode::RegisterMethod,
        COL_ENABLED_METHOD_LOG_ADDR,
        COL_ENABLED_METHOD_LOG_LEN_BEFORE,
    );
    require_equal(
        b,
        Opcode::RegisterMethod,
        COL_ENABLED_METHOD_LOG_UTXO,
        COL_CURR_BEFORE,
    );
    require_equal(
        b,
        Opcode::RegisterMethod,
        COL_ENABLED_METHOD_LOG_GENERATION,
        COL_ABI_GENERATION_BEFORE,
    );
}

fn visit_yield_begin(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_coroutine_kind(b, Opcode::YieldBegin, COL_CURR_BEFORE, CoroutineKind::Utxo);
    require_phase_before(
        b,
        Opcode::YieldBegin,
        crate::ivc_state::CurrPhase::Executing,
    );
    require_phase_after(
        b,
        Opcode::YieldBegin,
        crate::ivc_state::CurrPhase::Registering,
    );
    require_equal(
        b,
        Opcode::YieldBegin,
        COL_ABI_GENERATION_ADDR,
        COL_CURR_BEFORE,
    );
    b.push_gated_linear_zero(
        Opcode::YieldBegin.selector(),
        [
            (COL_ABI_GENERATION_AFTER, F::ONE),
            (COL_ABI_GENERATION_BEFORE, -F::ONE),
            (COL_ONE, -F::ONE),
        ],
    );
}

fn visit_enter_constructor(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_coroutine_kind(
        b,
        Opcode::EnterConstructor,
        COL_CURR_BEFORE,
        CoroutineKind::Utxo,
    );
    require_phase_before(
        b,
        Opcode::EnterConstructor,
        crate::ivc_state::CurrPhase::CtorEnterPending,
    );
    require_phase_after(
        b,
        Opcode::EnterConstructor,
        crate::ivc_state::CurrPhase::ConstructorExecuting,
    );
}

fn visit_new_utxo(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_coroutine_kind(b, Opcode::NewUtxo, COL_CURR_BEFORE, CoroutineKind::Coord);
    require_phase_before(b, Opcode::NewUtxo, crate::ivc_state::CurrPhase::Executing);
    require_phase_after(
        b,
        Opcode::NewUtxo,
        crate::ivc_state::CurrPhase::CtorEnterPending,
    );
    b.push_gated_linear_zero(
        Opcode::NewUtxo.selector(),
        [
            (COL_CALL_TARGET, F::ONE),
            (COL_NEXT_UTXO_ID_BEFORE, -F::new(2)),
            (COL_ONE, -F::ONE),
        ],
    );
    require_equal(b, Opcode::NewUtxo, COL_CALL_FRAME_CALLEE, COL_CALL_TARGET);
    require_equal(
        b,
        Opcode::NewUtxo,
        COL_CALL_FRAME_SAVED_PHASE,
        COL_CURR_PHASE_BEFORE,
    );
}

fn visit_receive_new_utxo_handle(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::ReceiveNewUtxoHandle,
        crate::ivc_state::CurrPhase::NewUtxoHandlePending,
    );
    require_equal(
        b,
        Opcode::ReceiveNewUtxoHandle,
        COL_CALL_TARGET,
        COL_CURR_BEFORE,
    );
    require_equal(
        b,
        Opcode::ReceiveNewUtxoHandle,
        COL_CURR_PHASE_AFTER,
        COL_CALL_FRAME_SAVED_PHASE,
    );
    require_equal(
        b,
        Opcode::ReceiveNewUtxoHandle,
        COL_RESOURCE_RESOLVER_ADDR_CID,
        COL_CURR_BEFORE,
    );
    require_equal(
        b,
        Opcode::ReceiveNewUtxoHandle,
        COL_RESOURCE_RESOLVER_VALUE,
        COL_CALL_FRAME_CALLEE,
    );
}

fn visit_receive_method_result(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::ReceiveMethodResult,
        crate::ivc_state::CurrPhase::MethodResultPending,
    );
    require_equal(
        b,
        Opcode::ReceiveMethodResult,
        COL_CALL_TARGET,
        COL_CURR_BEFORE,
    );
    require_equal(
        b,
        Opcode::ReceiveMethodResult,
        COL_CURR_PHASE_AFTER,
        COL_CALL_FRAME_SAVED_PHASE,
    );
}

fn visit_raise_effect(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    // Among reachable phases, bits 1 and 3 are both clear exactly for
    // Executing, ConstructorExecuting, and handler execution.
    let phase_before = phase_bits(COL_CURR_PHASE_BEFORE);
    b.push_gated_linear_zero(
        Opcode::RaiseEffect.selector(),
        [(phase_before[1], F::ONE), (phase_before[3], F::ONE)],
    );
    require_phase_after(
        b,
        Opcode::RaiseEffect,
        crate::ivc_state::CurrPhase::EffectHandlerPending,
    );
    require_equal(b, Opcode::RaiseEffect, COL_CALL_TARGET, COL_CURR_BEFORE);
    b.push_gated_linear_zero(
        Opcode::RaiseEffect.selector(),
        [
            (COL_HANDLER_SEARCH_REMAINING_AFTER, F::ONE),
            (COL_ONE, -F::ONE),
        ],
    );
    require_equal(
        b,
        Opcode::RaiseEffect,
        COL_CALL_FRAME_SAVED_PHASE,
        COL_CURR_PHASE_BEFORE,
    );
}

fn visit_enter_effect_handler(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::EnterEffectHandler,
        crate::ivc_state::CurrPhase::EffectHandlerPending,
    );
    require_phase_after(
        b,
        Opcode::EnterEffectHandler,
        crate::ivc_state::CurrPhase::EffectHandlerExecuting,
    );
    b.push_gated_linear_zero(
        Opcode::EnterEffectHandler.selector(),
        [
            (COL_HANDLER_SEARCH_REMAINING_BEFORE, F::ONE),
            (COL_ONE, -F::ONE),
        ],
    );
}

fn visit_exit_effect_handler(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::ExitEffectHandler,
        crate::ivc_state::CurrPhase::EffectHandlerExecuting,
    );
    b.push_gated_linear_zero(
        Opcode::ExitEffectHandler.selector(),
        [
            (COL_CURR_PHASE_AFTER, F::ONE),
            (
                COL_ONE,
                -F::new(crate::ivc_state::CurrPhase::EffectHandlerPending.value() as u64),
            ),
            (COL_EFFECT_HANDLED, -F::ONE),
        ],
    );
    b.push_gated_linear_zero(
        Opcode::ExitEffectHandler.selector(),
        [
            (COL_HANDLER_SEARCH_REMAINING_BEFORE, F::ONE),
            (COL_ONE, -F::ONE),
        ],
    );
    b.push_gated_linear_zero(
        Opcode::ExitEffectHandler.selector(),
        [(COL_HANDLER_SEARCH_REMAINING_AFTER, F::ONE)],
    );
}

fn visit_resume_effect(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::ResumeEffect,
        crate::ivc_state::CurrPhase::EffectResumePending,
    );
    require_equal(
        b,
        Opcode::ResumeEffect,
        COL_CURR_PHASE_AFTER,
        COL_CALL_FRAME_SAVED_PHASE,
    );
    require_equal(b, Opcode::ResumeEffect, COL_CALL_TARGET, COL_CURR_BEFORE);
    b.push_gated_linear_zero(
        Opcode::ResumeEffect.selector(),
        [(COL_HANDLER_SEARCH_REMAINING_BEFORE, F::ONE)],
    );
}
