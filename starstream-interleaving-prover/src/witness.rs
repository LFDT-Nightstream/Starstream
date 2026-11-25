use crate::{ccs::layout::*, opcode::Opcode, step::Wit};
use neo_math::F;
use p3_field::{Field, PrimeCharacteristicRing, PrimeField64};

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
    wit[COL_INSTANCE_DEPTH_BEFORE] = F::new(u64::from(input.instance_depth_before));
    wit[COL_INSTANCE_DEPTH_AFTER] = F::new(u64::from(input.instance_depth_after));
    wit[COL_INSTANCE_PUSH] = F::from_bool(input.opcode.pushes_instance());
    wit[COL_INSTANCE_POP] = F::from_bool(matches!(
        input.opcode,
        Opcode::ReceiveNewUtxoHandle | Opcode::ReceiveCallResult
    ));
    wit[COL_EFFECT_HEAD_BEFORE] = F::new(u64::from(input.effect_head_before));
    wit[COL_EFFECT_HEAD_AFTER] = F::new(u64::from(input.effect_head_after));
    wit[COL_EFFECT_DEPTH_BEFORE] = F::new(u64::from(input.effect_depth_before));
    wit[COL_EFFECT_DEPTH_AFTER] = F::new(u64::from(input.effect_depth_after));
    wit[COL_EFFECT_ARENA_LEN_BEFORE] = F::new(u64::from(input.effect_arena_len_before));
    wit[COL_EFFECT_ARENA_LEN_AFTER] = F::new(u64::from(input.effect_arena_len_after));
    wit[COL_EFFECT_FRAME_ID] = F::new(u64::from(input.effect_frame_id));
    wit[COL_EFFECT_REGISTERED] = F::from_bool(input.effect_registered);
    wit[COL_EFFECT_SCOPE_READ] = F::from_bool(matches!(
        input.opcode,
        Opcode::PopEffectHandlerFrame | Opcode::SkipEffectHandler | Opcode::EnterEffectHandler
    ));
    if matches!(
        input.opcode,
        Opcode::SkipEffectHandler | Opcode::EnterEffectHandler
    ) {
        wit[COL_EFFECT_HEAD_INVERSE] = wit[COL_EFFECT_HEAD_BEFORE].try_inverse().unwrap_or(F::ZERO);
    }
    if matches!(
        input.opcode,
        Opcode::AddEffectHandler | Opcode::PopEffectHandlerFrame
    ) {
        wit[COL_EFFECT_DEPTH_INVERSE] = wit[COL_EFFECT_DEPTH_BEFORE]
            .try_inverse()
            .unwrap_or(F::ZERO);
    }
    wit[COL_CALL_FRAME_SAVED_EFFECT_HEAD] = F::new(u64::from(input.saved_effect_head));
    wit[COL_CALL_FRAME_SAVED_EFFECT_DEPTH] = F::new(u64::from(input.saved_effect_depth));

    wit[input.opcode.selector()] = F::ONE;
    if input.opcode == Opcode::SetStorage {
        let load = match input.boundary_coroutine_id {
            crate::CoroutineId::Token(_) => COL_TOKEN_LOAD,
            crate::CoroutineId::Utxo(_) => COL_UTXO_LOAD,
            crate::CoroutineId::Coord(_) => unreachable!("coordinators have no storage setter"),
        };
        wit[load] = F::ONE;
    }
    wit[COL_EVENT_ACTIVE] = F::from_bool(input.opcode.has_event());
    wit[COL_EVENT_OWNER] = input.event_owner.field();
    wit[COL_BOUNDARY_COROUTINE_ID] = input.boundary_coroutine_id.field();
    wit[COL_TX_PHASE_BEFORE] = F::new(input.tx_before.phase as u64);
    wit[COL_TX_PHASE_AFTER] = F::new(input.tx_after.phase as u64);
    wit[COL_LAST_INPUT_HAS_ABI_BEFORE] = F::from_bool(input.tx_before.last_input_has_abi);
    wit[COL_LAST_INPUT_HAS_ABI_AFTER] = F::from_bool(input.tx_after.last_input_has_abi);
    wit[COL_OUTPUT_CURSOR_BEFORE] = F::new(u64::from(input.tx_before.output_cursor));
    wit[COL_OUTPUT_CURSOR_AFTER] = F::new(u64::from(input.tx_after.output_cursor));
    wit[COL_NEXT_COORD_ID_BEFORE] = F::new(u64::from(input.next_coordinator_id_before));
    wit[COL_NEXT_COORD_ID_AFTER] = F::new(u64::from(input.next_coordinator_id_after));
    wit[COL_COORD_CURSOR_BEFORE] = F::new(u64::from(input.tx_before.coordinator_cursor));
    wit[COL_COORD_CURSOR_AFTER] = F::new(u64::from(input.tx_after.coordinator_cursor));
    wit[COL_ABI_READ_REMAINING_BEFORE] = F::new(u64::from(input.tx_before.abi_read_remaining));
    wit[COL_ABI_READ_REMAINING_AFTER] = F::new(u64::from(input.tx_after.abi_read_remaining));
    wit[COL_ABI_READ_ORDINAL_BEFORE] = F::new(u64::from(input.tx_before.abi_read_ordinal));
    wit[COL_ABI_READ_ORDINAL_AFTER] = F::new(u64::from(input.tx_after.abi_read_ordinal));
    if input.opcode.scans_utxo_output() {
        wit[COL_OUTPUT_REMAINING] = F::new(u64::from(input.next_utxo_id_before))
            - F::new(u64::from(input.tx_before.output_cursor))
            - F::ONE;
    }
    if input.opcode == Opcode::GetTokenStorage {
        wit[COL_OUTPUT_REMAINING] = F::from_u32(input.next_utxo_id_before)
            + F::from_u32(input.next_token_id_before)
            - F::from_u32(input.tx_before.output_cursor)
            - F::ONE;
    }
    if input.opcode == Opcode::FinalizeCoordinator {
        wit[COL_OUTPUT_REMAINING] = F::new(u64::from(input.next_coordinator_id_before))
            - F::new(u64::from(input.tx_before.coordinator_cursor))
            - F::ONE;
    }
    wit[COL_METHOD_APPEND] = F::from_bool(input.opcode.appends_method());
    let count_write = input.opcode.appends_method() || input.opcode == Opcode::YieldBegin;
    let count_read = input.opcode.scans_utxo_output();
    wit[COL_ABI_METHOD_COUNT_WRITE] = F::from_bool(count_write);
    wit[COL_ABI_METHOD_COUNT_READ] = F::from_bool(count_read);
    if count_write || count_read {
        wit[COL_ABI_METHOD_COUNT_ADDR] = if count_read {
            input.boundary_coroutine_id.local_index()
        } else {
            input.abi_generation_address.local_index()
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
    let phase = input.curr_phase_before.value();
    wit[COL_PHASE_BEFORE_BITS_1_AND_2] = F::from_bool(phase & 0b0110 == 0b0110);
    wit[COL_CURR_PHASE_AFTER] = F::new(input.curr_phase_after.value() as u64);
    wit[COL_CALL_SP_BEFORE] = input.call_sp_before;
    wit[COL_CALL_SP_AFTER] = input.call_sp_after;
    if input.opcode.is_execution() {
        wit[COL_CALL_SP_BEFORE_INVERSE] = input.call_sp_before.try_inverse().unwrap_or(F::ZERO);
    }
    wit[COL_CALL_TARGET] = input.call_target.field();
    crate::ccs::CALL_TARGET_ZERO_TEST.assign(&mut wit);
    wit[COL_CALL_FRAME_IS_EFFECT] = F::from_bool(input.call_frame_is_effect);
    wit[COL_CALL_FRAME_CONSTRUCTOR] = F::from_bool(input.call_frame_constructor);
    wit[COL_RETURN_REQUIRES_UNIT] = F::from_bool(input.return_requires_unit);
    wit[COL_CALL_FRAME_SAVED_PHASE] = F::new(u64::from(input.call_frame_saved_phase.value()));
    wit[COL_EFFECT_HANDLED] = F::from_bool(input.effect_handled);
    wit[COL_EFFECT_RESTORE_DELTA] = wit[COL_EFFECT_HANDLED]
        * (wit[COL_CALL_FRAME_SAVED_EFFECT_HEAD] - wit[COL_EFFECT_HEAD_BEFORE]);
    wit[COL_NEXT_TOKEN_ID_BEFORE] = F::from_u32(input.next_token_id_before);
    wit[COL_NEXT_TOKEN_ID_AFTER] = F::from_u32(input.next_token_id_after);
    wit[COL_NEXT_UTXO_ID_BEFORE] = F::new(u64::from(input.next_utxo_id_before));
    wit[COL_NEXT_UTXO_ID_AFTER] = F::new(u64::from(input.next_utxo_id_after));
    wit[COL_ENABLED_METHOD_LOG_LEN_BEFORE] = F::new(u64::from(input.enabled_method_log_len_before));
    wit[COL_ENABLED_METHOD_LOG_LEN_AFTER] = F::new(u64::from(input.enabled_method_log_len_after));
    wit[COL_METHOD_INDEX] = F::new(u64::from(input.method_index));
    wit[COL_ABI_GENERATION_ADDR] = input.abi_generation_address.local_index();
    wit[COL_ABI_GENERATION_BEFORE] = F::new(u64::from(input.abi_generation_before));
    wit[COL_ABI_GENERATION_AFTER] = F::new(u64::from(input.abi_generation_after));
    wit[COL_ENABLED_METHOD_LOG_ADDR] = F::new(u64::from(input.enabled_method_log_address));
    wit[COL_ENABLED_METHOD_LOG_UTXO] = input.enabled_method_log_utxo.field();
    wit[COL_ENABLED_METHOD_LOG_GENERATION] = F::new(u64::from(input.enabled_method_log_generation));

    wit[COL_TOKEN_INTERFACE] = F::from_u32(input.token_interface);
    wit[COL_RESOLVER_SECOND_READ] =
        F::from_bool(matches!(input.opcode, Opcode::Upcast | Opcode::Attach));
    wit[COL_RESOURCE_RESOLVER_SECOND_ADDR_HANDLE] = F::from_u32(input.second_handle.0);
    wit[COL_RESOLVER_SECOND_VALUE] = input.second_target.field();
    wit[COL_TOKEN_EVENT] = F::from_bool(
        input.opcode.has_event() && matches!(input.event_owner, crate::CoroutineId::Token(_)),
    );
    wit[COL_RESOURCE_RESOLVER_ADDR_CID] = input.resolver_address.0.field();
    wit[COL_RESOURCE_RESOLVER_ADDR_HANDLE] = F::new(u64::from(input.resolver_address.1.0));
    wit[COL_RESOURCE_RESOLVER_VALUE] = input.resolver_value.field();
    if input.opcode == Opcode::Upcast {
        wit[COL_UPCAST_HANDLE_DIFF_INVERSE] = (wit[COL_RESOURCE_RESOLVER_ADDR_HANDLE]
            - wit[COL_RESOURCE_RESOLVER_SECOND_ADDR_HANDLE])
            .try_inverse()
            .unwrap_or(F::ZERO);
    }
    wit[COL_RESOURCE_RESOLVER_READ] = if input.resolver_read { F::ONE } else { F::ZERO };
    wit[COL_RESOURCE_RESOLVER_WRITE] = if input.resolver_write {
        F::ONE
    } else {
        F::ZERO
    };

    if input.opcode.appends_method()
        || matches!(
            input.opcode,
            Opcode::CallMethod
                | Opcode::ReadAbi
                | Opcode::RaiseEffect
                | Opcode::EnterEffectHandler
                | Opcode::SkipEffectHandler
                | Opcode::AddEffectHandler
        )
    {
        wit[COL_METHOD_LOOKUP] = F::ONE;
    }

    wit[COL_ABI_GENERATION_READ] = F::from_bool(
        input.opcode.appends_method()
            || matches!(input.opcode, Opcode::CallMethod | Opcode::ReadAbi),
    );

    if wit[COL_METHOD_LOOKUP] == F::ONE {
        for (offset, address) in COL_METHOD_TABLE_ADDR.iter().enumerate() {
            wit[*address] = F::new(u64::from(input.method_index) * 8 + offset as u64);
        }
    }

    if input.opcode == Opcode::AssertSignedBy {
        let base = wit[COL_SIGNER_INDEX] * F::new(SIGNER_KEY_LIMBS as u64);
        for (offset, address) in COL_SIGNER_KEY_ADDR.into_iter().enumerate() {
            wit[address] = base + F::new(offset as u64);
        }
    }

    if input.opcode.pushes_to_call_stack() {
        wit[COL_CALL_STACK_PUSH] = F::ONE;
    }

    if input.opcode.pops_from_call_stack_unconditionally() {
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

    if let Some(event_identity) = &input.event_identity {
        for (col, value) in COL_METHOD_HASH_VALUE.iter().zip(event_identity) {
            wit[*col] = *value;
        }
    }

    if let Some(expected_result) = &input.expected_result {
        debug_assert_eq!(expected_result.len(), 8);
        for (col, limbs) in COL_RESULT_ROOT.iter().zip(expected_result.chunks(2)) {
            wit[*col] = limbs[0] + F::new(1 << 32) * limbs[1];
        }
    }

    for (cols, values) in [
        (COL_PENDING_VALUE_BEFORE, input.pending_value_before),
        (COL_PENDING_VALUE_AFTER, input.pending_value_after),
    ] {
        for (col, value) in cols.into_iter().zip(values) {
            wit[col] = value;
        }
    }
    for (cols, values) in [
        (COL_PENDING_METHOD_BEFORE, input.pending_method_before),
        (COL_PENDING_METHOD_AFTER, input.pending_method_after),
    ] {
        for (col, value) in cols.into_iter().zip(values) {
            wit[col] = value;
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
    let call_stack_access = wit[COL_CALL_SP_BEFORE]
        - wit[COL_CALL_STACK_TOP]
        - wit[COL_CALL_STACK_POP]
        - wit[COL_SEL_RETURN];

    wit[COL_CALL_STACK_FRAME] = call_stack_access;
    for (i, col) in COL_CALL_STACK_MUL_STRIDE_8.iter().enumerate() {
        wit[*col] = call_stack_access * F::new(8) + F::new(i as u64);
    }
    // The owner's kind tag selects the trace commitment memory, and the
    // rest of its packed id is the address there.
    let event_owner = wit[COL_EVENT_OWNER].as_canonical_u64();
    let event_owner_index = event_owner / 4;
    let utxo_owner = event_owner % 4 == u64::from(crate::ivc_state::CoroutineKind::Utxo.tag());
    wit[COL_UTXO_EVENT] = wit[COL_EVENT_ACTIVE] * F::from_bool(utxo_owner);
    wit[COL_COORD_EVENT] = wit[COL_EVENT_ACTIVE] * F::from_bool(event_owner.is_multiple_of(4));
    for (i, col) in COL_EVENT_OWNER_STRIDE_8.iter().enumerate() {
        wit[*col] = F::new(event_owner_index * 8 + i as u64);
    }
}
