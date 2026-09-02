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
            COL_CALL_STACK_TOP, COL_CURR_BEFORE, COL_CURR_BEFORE_STRIDE_4, COL_SEL_CALL_METHOD,
            COL_SEL_NEW_UTXO, SELECTORS, full_column_registry,
        },
        tags::{ConstraintScope, always, opcode_tag, opcode_tags},
    },
    opcode::Opcode,
};

pub(crate) mod layout;
mod tags;

type R1csBuilder = neo_application::R1csBuilder<ConstraintScope>;

pub fn build_relation() -> Result<ApplicationRelation<ConstraintScope>, crate::Error> {
    let column_registry = full_column_registry();

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

    // TODO: gate these?
    b.with_tag(always("call stack mul stride (4)"), |b| {
        COL_CALL_STACK_MUL_STRIDE_4
            .iter()
            .enumerate()
            .for_each(|(i, col)| {
                b.push_linear_zero([
                    (COL_CALL_SP_BEFORE, F::new(4)),
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

    b.with_tag(opcode_tag("new utxo constraints", Opcode::NewUtxo), |b| {
        visit_new_utxo(b)
    });

    b.with_tag(
        opcode_tag("enter constructor constraints", Opcode::EnterConstructor),
        |b| visit_enter_constructor(b),
    );
    b.with_tag(
        opcode_tag("yield begin constraints", Opcode::YieldBegin),
        |b| visit_yield_begin(b),
    );
    b.with_tag(
        opcode_tag("register method constraints", Opcode::RegisterMethod),
        |b| visit_register_method(b),
    );
    b.with_tag(opcode_tag("return constraints", Opcode::Return), |b| {
        visit_return(b)
    });
    b.with_tag(
        opcode_tag("call method constraints", Opcode::CallMethod),
        |b| visit_call_method(b),
    );
    b.with_tag(
        opcode_tag("enter method constraints", Opcode::EnterMethod),
        |b| visit_enter_method(b),
    );

    let r1cs = builder.build()?;

    Ok(ApplicationRelation::new(r1cs, column_registry)?)
}

fn visit_enter_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}

fn visit_call_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}

fn visit_return(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}

fn visit_register_method(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}

fn visit_yield_begin(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}

fn visit_enter_constructor(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}

fn visit_new_utxo(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {}
