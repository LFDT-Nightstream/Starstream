use crate::{ccs::layout::*, opcode::Opcode, step::Wit};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;

pub fn build_witness_vector(input: &Wit) -> Vec<F> {
    let range_checks = range_check_layout();
    let mut wit = vec![F::ZERO; range_checks.base_column_count()];

    wit[COL_ONE] = F::ONE;

    match input.opcode {
        Opcode::NewUtxo => wit[COL_SEL_NEW_UTXO] = F::ONE,
        Opcode::EnterConstructor => wit[COL_SEL_ENTER_CONSTRUCTOR] = F::ONE,
        Opcode::YieldBegin => wit[COL_SEL_YIELD_BEGIN] = F::ONE,
        Opcode::RegisterMethod => wit[COL_SEL_REGISTER_METHOD] = F::ONE,
        Opcode::Return => wit[COL_SEL_RETURN] = F::ONE,
        Opcode::CallMethod => wit[COL_SEL_CALL_METHOD] = F::ONE,
        Opcode::EnterMethod => wit[COL_SEL_ENTER_METHOD] = F::ONE,
    }

    wit[COL_CURR_PHASE_BEFORE] = F::new(input.curr_phase_before.value() as u64);
    wit[COL_CURR_PHASE_AFTER] = F::new(input.curr_phase_after.value() as u64);
    wit[COL_CALL_SP_BEFORE] = input.call_sp_before;
    wit[COL_CALL_SP_AFTER] = input.call_sp_after;

    if input.opcode.pushes_to_call_stack() {
        wit[COL_CALL_STACK_PUSH] = F::ONE;
    }

    if input.opcode.pops_from_call_stack() {
        wit[COL_CALL_STACK_POP] = F::ONE;
    }

    if input.opcode.peeks_call_stack_top() {
        wit[COL_CALL_STACK_TOP] = F::ONE;
    }

    COL_CALL_STACK_MUL_STRIDE_4
        .iter()
        .enumerate()
        .for_each(|(i, col)| {
            wit[*col] =
                (wit[COL_CALL_SP_BEFORE] - wit[COL_CALL_STACK_TOP] - wit[COL_CALL_STACK_POP])
                    * F::new(4)
                    + F::new(i as u64)
        });

    COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8
        .iter()
        .enumerate()
        .for_each(|(i, col)| {
            wit[*col] =
                (wit[COL_CALL_SP_BEFORE] - wit[COL_CALL_STACK_TOP]) * F::new(8) + F::new(i as u64)
        });

    COL_CURR_BEFORE_STRIDE_4
        .iter()
        .enumerate()
        .for_each(|(i, col)| wit[*col] = wit[COL_CURR_BEFORE] * F::new(4) + F::new(i as u64));

    if let Some(expected_arg) = &input.expected_arguments {
        for (offset, col) in COL_CALL_STACK_EXPECTED_ARG_VALUE.iter().enumerate() {
            // TODO: tmp? limitation
            //
            // probably need to implement the opaque value support in
            // nightstream for bigger than 4
            debug_assert!(expected_arg.len() <= 4);
            wit[*col] = expected_arg.get(offset).copied().unwrap_or(F::ZERO)
        }
    }

    if let Some(expected_method) = &input.expected_method {
        for (col, value) in COL_CALL_STACK_EXPECTED_METHOD_VALUE
            .iter()
            .zip(expected_method)
        {
            wit[*col] = *value;
        }
    }

    if let Some(expected_result) = &input.expected_result {
        debug_assert!(expected_result.len() <= COL_CALL_STACK_EXPECTED_RESULT_VALUE.len());
        for (offset, col) in COL_CALL_STACK_EXPECTED_RESULT_VALUE.iter().enumerate() {
            wit[*col] = expected_result.get(offset).copied().unwrap_or(F::ZERO);
        }
    }

    range_checks
        .assign_bits(&mut wit)
        .expect("base witness matches the range-check layout");

    wit
}
