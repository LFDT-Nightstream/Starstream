use std::iter;

use neo_application::{ApplicationRelation, TaggedR1csBuilder};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;

pub(crate) use crate::ccs::layout::{COL_ONE, PUBLIC_INPUTS};
use crate::{
    ccs::{
        layout::{
            COL_CALL_SP_AFTER, COL_CALL_SP_BEFORE, COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8,
            COL_CALL_STACK_MUL_STRIDE_4, COL_CALL_STACK_POP, COL_CALL_STACK_PUSH,
            COL_CALL_STACK_TOP, COL_CURR_BEFORE, COL_CURR_BEFORE_STRIDE_4, COL_CURR_PHASE_AFTER,
            COL_CURR_PHASE_BEFORE, COL_SEL_CALL_METHOD, COL_SEL_NEW_UTXO, SELECTORS,
            range_check_layout,
        },
        tags::{ConstraintScope, always, opcode_tag, opcode_tags},
    },
    opcode::Opcode,
};

pub(crate) mod layout;
mod tags;

type R1csBuilder = neo_application::R1csBuilder<ConstraintScope>;

pub fn build_relation() -> Result<ApplicationRelation<ConstraintScope>, crate::Error> {
    let range_checks = range_check_layout();
    let column_registry = range_checks.columns().clone();

    let mut builder = R1csBuilder::new(column_registry.column_count(), PUBLIC_INPUTS, COL_ONE)?;
    let mut b = builder.tagged(always("unlabeled"));

    b.with_tag(always("opcode selectors are one-hot"), |b| {
        b.push_row(
            SELECTORS.iter().map(|col| (*col, F::ONE)),
            [(COL_ONE, F::ONE)],
            [(COL_ONE, F::ONE)],
        );
    });

    b.with_tag(
        opcode_tags(
            "call stack push global condition: true if new_utxo or call_method",
            &[Opcode::NewUtxo, Opcode::CallMethod],
        ),
        |b| {
            b.push_linear_zero([
                (COL_SEL_NEW_UTXO, F::ONE),
                (COL_SEL_CALL_METHOD, F::ONE),
                (COL_CALL_STACK_PUSH, -F::ONE),
            ]);
        },
    );

    // TODO(perf): Consider gating these call-stack address derivations and
    // assigning zero on rows without a call-stack memory access. Nightstream's
    // Ajtai commitment path has sparse/zero fast paths, so at large batch sizes
    // the commitment savings may outweigh the cost of gated constraints.
    // Benchmark both layouts once proof batching is wired.
    //
    // A push writes at the next free address (`sp`), while peeks and pops read
    // the current top (`sp - 1`). The corresponding access flag selects the
    // latter address.
    b.with_tag(always("call stack mul stride (4)"), |b| {
        COL_CALL_STACK_MUL_STRIDE_4
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (COL_CALL_SP_BEFORE, F::new(4)),
                    (COL_CALL_STACK_TOP, -F::new(4)),
                    (COL_CALL_STACK_POP, -F::new(4)),
                    (COL_ONE, F::new(i as u64)),
                    (*col, -F::ONE),
                ]);
            });
    });

    b.with_tag(always("call stack mul stride (8)"), |b| {
        COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (COL_CALL_SP_BEFORE, F::new(8)),
                    (COL_CALL_STACK_TOP, -F::new(8)),
                    (COL_ONE, F::new(i as u64)),
                    (*col, -F::ONE),
                ]);
            });
    });

    b.with_tag(always("curr mul stride (4)"), |b| {
        COL_CURR_BEFORE_STRIDE_4
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (COL_CURR_BEFORE, F::new(4)),
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
        .filter(|op| op.pops_from_call_stack())
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
        .filter(|op| !op.pushes_to_call_stack() && !op.pops_from_call_stack())
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
        opcode_tag("enter method constraints", Opcode::EnterMethod),
        visit_enter_method,
    );

    range_checks.push_constraints(&mut b, ConstraintScope::Always);

    let r1cs = builder.build()?;

    Ok(ApplicationRelation::new(r1cs, column_registry)?)
}

fn push_gated_linear_zero(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    gate: usize,
    terms: impl IntoIterator<Item = (usize, F)>,
) {
    b.push_row([(gate, F::ONE)], terms, []);
}

fn require_phase_before(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    opcode: Opcode,
    phase: crate::ivc_state::CurrPhase,
) {
    push_gated_linear_zero(
        b,
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
    push_gated_linear_zero(
        b,
        opcode.selector(),
        [
            (COL_CURR_PHASE_AFTER, F::ONE),
            (COL_ONE, -F::new(phase.value() as u64)),
        ],
    );
}

fn phase_bits(column: usize) -> [usize; 2] {
    assert!(
        [COL_CURR_PHASE_BEFORE, COL_CURR_PHASE_AFTER].contains(&column),
        "this function should only be called on the phase columns"
    );

    let bits = range_check_layout()
        .bit_columns_for(column)
        .expect("phase column has decomposition bits");

    [bits.start, bits.start + 1]
}

fn visit_enter_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
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
}

fn visit_return(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    // Return is allowed in Executing (00) and Yield (11).
    let phase_before = phase_bits(COL_CURR_PHASE_BEFORE);
    push_gated_linear_zero(
        b,
        Opcode::Return.selector(),
        [(phase_before[0], F::ONE), (phase_before[1], -F::ONE)],
    );
    require_phase_after(b, Opcode::Return, crate::ivc_state::CurrPhase::Executing);
}

fn visit_register_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::RegisterMethod,
        crate::ivc_state::CurrPhase::Yield,
    );
    require_phase_after(
        b,
        Opcode::RegisterMethod,
        crate::ivc_state::CurrPhase::Yield,
    );
}

fn visit_yield_begin(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::YieldBegin,
        crate::ivc_state::CurrPhase::Executing,
    );
    require_phase_after(b, Opcode::YieldBegin, crate::ivc_state::CurrPhase::Yield);
}

fn visit_enter_constructor(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(
        b,
        Opcode::EnterConstructor,
        crate::ivc_state::CurrPhase::CtorEnterPending,
    );
    require_phase_after(
        b,
        Opcode::EnterConstructor,
        crate::ivc_state::CurrPhase::Executing,
    );
}

fn visit_new_utxo(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    require_phase_before(b, Opcode::NewUtxo, crate::ivc_state::CurrPhase::Executing);
    require_phase_after(
        b,
        Opcode::NewUtxo,
        crate::ivc_state::CurrPhase::CtorEnterPending,
    );
}
