use crate::{ccs::layout::*, opcode::Opcode, step::Wit};
use neo_math::F;
use p3_field::{Field, PrimeCharacteristicRing};

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
        Opcode::Padding => wit[COL_SEL_PADDING] = F::ONE,
    }

    wit[COL_CURR_BEFORE] = input.curr_before.field();
    wit[COL_CURR_AFTER] = input.curr_after.field();
    wit[COL_CURR_PHASE_BEFORE] = F::new(input.curr_phase_before.value() as u64);
    wit[COL_CURR_PHASE_AFTER] = F::new(input.curr_phase_after.value() as u64);
    wit[COL_CALL_SP_BEFORE] = input.call_sp_before;
    wit[COL_CALL_SP_AFTER] = input.call_sp_after;
    if input.opcode.is_execution() {
        wit[COL_CALL_SP_BEFORE_INVERSE] = input.call_sp_before.try_inverse().unwrap_or(F::ZERO);
    }
    wit[COL_CALL_TARGET] = input.call_target.field();
    wit[COL_NEXT_UTXO_ID_BEFORE] = F::new(u64::from(input.next_utxo_id_before));
    wit[COL_NEXT_UTXO_ID_AFTER] = F::new(u64::from(input.next_utxo_id_after));
    wit[COL_ENABLED_METHOD_LOG_LEN_BEFORE] = F::new(u64::from(input.enabled_method_log_len_before));
    wit[COL_ENABLED_METHOD_LOG_LEN_AFTER] = F::new(u64::from(input.enabled_method_log_len_after));
    wit[COL_METHOD_INDEX] = F::new(u64::from(input.method_index));
    wit[COL_ABI_GENERATION_ADDR] = input.abi_generation_address.field();
    wit[COL_ABI_GENERATION_BEFORE] = F::new(u64::from(input.abi_generation_before));
    wit[COL_ABI_GENERATION_AFTER] = F::new(u64::from(input.abi_generation_after));
    wit[COL_ENABLED_METHOD_LOG_ADDR] = F::new(u64::from(input.enabled_method_log_address));
    wit[COL_ENABLED_METHOD_LOG_UTXO] = input.enabled_method_log_utxo.field();
    wit[COL_ENABLED_METHOD_LOG_GENERATION] = F::new(u64::from(input.enabled_method_log_generation));

    assign_pending_constructor_key(
        &mut wit,
        input.pending_ctor_key_before,
        COL_PENDING_CTOR_PRESENT_BEFORE,
        COL_PENDING_CTOR_HOLDER_BEFORE,
        COL_PENDING_CTOR_HANDLE_BEFORE,
    );
    assign_pending_constructor_key(
        &mut wit,
        input.pending_ctor_key_after,
        COL_PENDING_CTOR_PRESENT_AFTER,
        COL_PENDING_CTOR_HOLDER_AFTER,
        COL_PENDING_CTOR_HANDLE_AFTER,
    );

    wit[COL_RESOURCE_RESOLVER_ADDR_CID] = input.resolver_address.0.field();
    wit[COL_RESOURCE_RESOLVER_ADDR_HANDLE] = F::new(u64::from(input.resolver_address.1.0));
    wit[COL_RESOURCE_RESOLVER_VALUE] = input.resolver_value.field();
    wit[COL_RESOURCE_RESOLVER_READ] = if input.resolver_read { F::ONE } else { F::ZERO };
    wit[COL_RESOURCE_RESOLVER_WRITE] = if input.resolver_write {
        F::ONE
    } else {
        F::ZERO
    };

    if matches!(input.opcode, Opcode::RegisterMethod | Opcode::CallMethod) {
        wit[COL_METHOD_LOOKUP] = F::ONE;
    }

    if wit[COL_METHOD_LOOKUP] == F::ONE {
        for (offset, address) in COL_METHOD_TABLE_ADDR.iter().enumerate() {
            wit[*address] = F::new(u64::from(input.method_index) * 8 + offset as u64);
        }
    }

    if input.opcode.pushes_to_call_stack() {
        wit[COL_CALL_STACK_PUSH] = F::ONE;
    }

    if input.opcode.pops_from_call_stack() {
        wit[COL_CALL_STACK_POP] = F::ONE;
    }

    if input.opcode.peeks_call_stack_top() {
        wit[COL_CALL_STACK_TOP] = F::ONE;
    }

    assign_stride_columns(&mut wit);

    if let Some(expected_arg) = &input.expected_arguments {
        for (offset, col) in COL_CALL_STACK_EXPECTED_ARG_VALUE.iter().enumerate() {
            debug_assert_eq!(expected_arg.len(), 8);
            wit[*col] = expected_arg.get(offset).copied().unwrap_or(F::ZERO)
        }
    }

    if let Some(method_hash) = &input.method_hash {
        for (col, value) in COL_METHOD_HASH_VALUE.iter().zip(method_hash) {
            wit[*col] = *value;
        }
    }

    if let Some(expected_result) = &input.expected_result {
        debug_assert!(expected_result.len() <= COL_CALL_STACK_EXPECTED_RESULT_VALUE.len());
        for (offset, col) in COL_CALL_STACK_EXPECTED_RESULT_VALUE.iter().enumerate() {
            wit[*col] = expected_result.get(offset).copied().unwrap_or(F::ZERO);
        }
    }

    crate::commitment::assign(&mut wit, input);
    range_checks
        .assign_bits(&mut wit)
        .expect("base witness matches the range-check layout");

    wit
}

/// Derive the memory address columns from the call-stack pointer, its access
/// flags, and the current coroutine, matching the stride constraints in
/// `ccs.rs`. Shared with batch padding so the two cannot drift apart.
pub(crate) fn assign_stride_columns(wit: &mut [F]) {
    let call_stack_top = wit[COL_CALL_SP_BEFORE] - wit[COL_CALL_STACK_TOP];
    let call_stack_access = call_stack_top - wit[COL_CALL_STACK_POP];

    for (i, col) in COL_CALL_STACK_MUL_STRIDE_8.iter().enumerate() {
        wit[*col] = call_stack_access * F::new(8) + F::new(i as u64);
    }
    for (i, col) in COL_CALL_STACK_EXPECTED_ADDR_STRIDE_8.iter().enumerate() {
        wit[*col] = call_stack_top * F::new(8) + F::new(i as u64);
    }
    for (i, col) in COL_CURR_BEFORE_STRIDE_8.iter().enumerate() {
        wit[*col] = wit[COL_CURR_BEFORE] * F::new(8) + F::new(i as u64);
    }
}

fn assign_pending_constructor_key(
    wit: &mut [F],
    key: Option<(
        crate::ivc_state::CoroutineId,
        starstream_interleaving_spec::ResourceHandle,
    )>,
    present_column: usize,
    holder_column: usize,
    handle_column: usize,
) {
    if let Some((holder, handle)) = key {
        wit[present_column] = F::ONE;
        wit[holder_column] = holder.field();
        wit[handle_column] = F::new(u64::from(handle.0));
    }
}
