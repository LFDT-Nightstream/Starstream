use crate::{ccs::layout::*, opcode::Opcode, step::Wit};
use neo_math::F;
use p3_field::{Field, PrimeCharacteristicRing};

pub fn build_witness_vector(input: &Wit) -> Vec<F> {
    let mut wit = assign_base_columns(input);
    crate::commitment::assign_from_bus(&mut wit, input.opcode);
    range_check_layout()
        .assign_bits(&mut wit)
        .expect("base witness matches the range-check layout");
    wit
}

/// Shared by normalization and full witness assignment; no hash advice or range bits.
pub(crate) fn assign_base_columns(input: &Wit) -> Vec<F> {
    let range_checks = range_check_layout();
    let mut wit = vec![F::ZERO; range_checks.base_column_count()];

    wit[COL_ONE] = F::ONE;

    wit[input.opcode.selector()] = F::ONE;
    wit[COL_EVENT_ACTIVE] = F::from_bool(input.opcode.has_event());
    wit[COL_EVENT_OWNER] = input.event_owner.field();
    wit[COL_BOUNDARY_UTXO] = input.boundary_utxo.field();
    wit[COL_TX_PHASE_BEFORE] = F::new(input.tx_before.phase as u64);
    wit[COL_TX_PHASE_AFTER] = F::new(input.tx_after.phase as u64);
    wit[COL_LAST_INPUT_HAS_ABI_BEFORE] = F::from_bool(input.tx_before.last_input_has_abi);
    wit[COL_LAST_INPUT_HAS_ABI_AFTER] = F::from_bool(input.tx_after.last_input_has_abi);
    wit[COL_OUTPUT_CURSOR_BEFORE] = F::new(u64::from(input.tx_before.output_cursor));
    wit[COL_OUTPUT_CURSOR_AFTER] = F::new(u64::from(input.tx_after.output_cursor));
    wit[COL_COORD_FINALIZED_BEFORE] = F::from_bool(input.tx_before.coordinator_finalized);
    wit[COL_COORD_FINALIZED_AFTER] = F::from_bool(input.tx_after.coordinator_finalized);
    wit[COL_TRACE_ROOT_READ] = F::from_bool(input.opcode.reads_trace_root());
    wit[COL_ABI_READ_REMAINING_BEFORE] = F::new(u64::from(input.tx_before.abi_read_remaining));
    wit[COL_ABI_READ_REMAINING_AFTER] = F::new(u64::from(input.tx_after.abi_read_remaining));
    wit[COL_ABI_READ_ORDINAL_BEFORE] = F::new(u64::from(input.tx_before.abi_read_ordinal));
    wit[COL_ABI_READ_ORDINAL_AFTER] = F::new(u64::from(input.tx_after.abi_read_ordinal));
    if input.opcode.scans_output() {
        wit[COL_OUTPUT_REMAINING] = F::new(u64::from(input.next_utxo_id_before))
            - F::new(u64::from(input.tx_before.output_cursor))
            - F::ONE;
    }
    wit[COL_METHOD_APPEND] = F::from_bool(input.opcode.appends_method());
    let count_write = input.opcode.appends_method() || input.opcode == Opcode::YieldBegin;
    let count_read = input.opcode.scans_output();
    wit[COL_ABI_METHOD_COUNT_WRITE] = F::from_bool(count_write);
    wit[COL_ABI_METHOD_COUNT_READ] = F::from_bool(count_read);
    if count_write || count_read {
        wit[COL_ABI_METHOD_COUNT_ADDR] = if count_read {
            input.boundary_utxo.field()
        } else {
            input.abi_generation_address.field()
        };
        wit[COL_ABI_METHOD_COUNT_BEFORE] = F::new(u64::from(input.abi_method_count_before));
        wit[COL_ABI_METHOD_COUNT_AFTER] = F::new(u64::from(input.abi_method_count_after));
    }
    if input.opcode.appends_method() {
        wit[COL_ENABLED_METHOD_LOG_ORDINAL] = wit[COL_ABI_METHOD_COUNT_BEFORE];
    }
    if input.opcode == Opcode::ReadAbi {
        wit[COL_ENABLED_METHOD_LOG_ORDINAL] = wit[COL_ABI_READ_ORDINAL_BEFORE];
    }
    if input.opcode == Opcode::GetStorage {
        wit[COL_ABI_METHOD_COUNT_INVERSE] = wit[COL_ABI_METHOD_COUNT_BEFORE]
            .try_inverse()
            .unwrap_or(F::ZERO);
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

    if input.opcode.appends_method() || matches!(input.opcode, Opcode::CallMethod | Opcode::ReadAbi)
    {
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

    for (c, value) in COL_IN.into_iter().zip(input.commitment_before) {
        wit[c] = value;
    }
    for (c, value) in COL_IO_BEFORE.into_iter().zip(input.io_before) {
        wit[c] = value;
    }
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
    for (i, col) in COL_EVENT_OWNER_STRIDE_8.iter().enumerate() {
        wit[*col] = wit[COL_EVENT_OWNER] * F::new(8) + F::new(i as u64);
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
