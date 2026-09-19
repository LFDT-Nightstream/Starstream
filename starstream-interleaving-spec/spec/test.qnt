module replay_trace {
  import starstream.* from "./starstream"

  action init = { state' = new_tx }

  def method = "0000000000000001000000000000000100000000000000010000000000000001";
  def other_method = "0000000000000002000000000000000100000000000000010000000000000001";

  action init_transaction = { state' = new_transaction(
    List({ storage: UNIT_VALUE, methods: List(method) }),
    List({ utxo: 0, storage: UNIT_VALUE, methods: List(method) })) }

  run test_storage_roundtrip = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(method))
    .then(finalize_coordinator)
    .then(finish_transaction)
    .then(transaction_complete)

  action check_abi_count: bool = all {
    assert(abi_method_count_matches_registrations),
    state' = state,
  }

  action init_empty_transaction = { state' = new_transaction(List(), List()) }

  run test_constructor_cannot_rebind_even_consumed_resource = init
    .then(new_utxo(UNIT_VALUE, { value: 0 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(return({ value: UNIT_VALUE }))
    .then(new_utxo(UNIT_VALUE, { value: 0 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(return({ value: UNIT_VALUE }))
    .fail()

  run test_constructor_cannot_rebind_input = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(new_utxo(UNIT_VALUE, { value: 0 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(return({ value: UNIT_VALUE }))
    .fail()

  run test_input_cannot_rebind_input = {
    val input = { storage: UNIT_VALUE, methods: List(method) }
    val initial = new_transaction(List(input, input), List())
    { state' = initial }
      .then(set_storage(UNIT_VALUE, 0))
      .then(preload_method(method))
      .then(set_storage(UNIT_VALUE, 0))
      .fail()
  }

  run test_coordinator_finalization_required = init_empty_transaction
    .then(return({ value: UNIT_VALUE }))
    .then(finish_transaction)
    .fail()

  run test_coordinator_finalization_only_once = init_empty_transaction
    .then(return({ value: UNIT_VALUE }))
    .then(finalize_coordinator)
    .then(finalize_coordinator)
    .fail()

  run test_coordinator_finalization_after_execution = init_empty_transaction
    .then(finalize_coordinator)
    .fail()

  run test_coordinator_finalization_after_utxos = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(finalize_coordinator)
    .fail()

  run test_coordinator_finalization_after_abis = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(finalize_coordinator)
    .fail()

  run test_abi_count_tracks_duplicates_and_reset = init_transaction
    .then(check_abi_count)
    .then(set_storage(UNIT_VALUE, 0))
    .then(check_abi_count)
    .then(preload_method(method))
    .then(preload_method(method))
    .then(check_abi_count)
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(enter_method(method, UNIT_VALUE))
    .then(yield_begin)
    .then(check_abi_count)
    .then(register_method(other_method))
    .then(register_method(other_method))
    .then(check_abi_count)

  action init_abi_sequence(methods: List[MethodHash]): bool = {
    init_abi_sequences(methods, methods)
  }

  run test_missing_output_abi_read_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(finish_transaction)
    .fail()

  run test_extra_output_abi_read_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(method))
    .then(read_abi(method))
    .fail()

  run test_output_abi_order_fails = init_abi_sequences(List(method, other_method), List(other_method, method))
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(preload_method(other_method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(other_method))
    .fail()

  action init_abi_sequences(inputs: List[MethodHash], outputs: List[MethodHash]): bool = {
    state' = new_transaction(
      List({ storage: UNIT_VALUE, methods: inputs }),
      List({ utxo: 0, storage: UNIT_VALUE, methods: outputs }))
  }

  run test_preload_count_bound = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(255.reps(i => preload_method(method)))
    .then(all { assert(state.abi_method_count.get(Utxo(0)) == 255), state' = state })
    .then(preload_method(method).fail())

  run test_registration_count_bound_and_reset = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(255.reps(i => preload_method(method)))
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(enter_method(method, UNIT_VALUE))
    .then(yield_begin)
    .then(all { assert(state.abi_method_count.get(Utxo(0)) == 0), state' = state })
    .then(255.reps(i => register_method(method)))
    .then(all { assert(state.abi_method_count.get(Utxo(0)) == 255), state' = state })
    .then(register_method(method).fail())

  run test_missing_preload_rejected_at_finalization = init_abi_sequences(List(method, other_method), List(method))
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(method))
    .then(finalize_coordinator)
    .then(finish_transaction)
    .fail()

  run test_preload_order_rejected_at_finalization = init_abi_sequences(List(method, other_method), List(other_method, method))
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(other_method))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(other_method))
    .then(read_abi(method))
    .then(finalize_coordinator)
    .then(finish_transaction)
    .fail()

  run test_extra_preload_rejected_at_finalization = init_abi_sequences(List(method), List(method, other_method))
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(preload_method(other_method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(method))
    .then(read_abi(other_method))
    .then(finalize_coordinator)
    .then(finish_transaction)
    .fail()

  run test_preload_duplicates_preserved = init_abi_sequence(List(method, method))
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(method))
    .then(read_abi(method))
    .then(finalize_coordinator)
    .then(finish_transaction)
    .then(transaction_complete)

  run test_execution_does_not_change_initial_abi_binding = {
    state' = new_transaction(
      List({ storage: UNIT_VALUE, methods: List(method) }),
      List({ utxo: 0, storage: UNIT_VALUE, methods: List(other_method) }))
  }
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(enter_method(method, UNIT_VALUE))
    .then(yield_begin)
    .then(register_method(other_method))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .then(read_abi(other_method))
    .then(finalize_coordinator)
    .then(finish_transaction)
    .then(transaction_complete)

  run test_multiple_input_utxos = {
    val first_storage = List(11, 12, 13, 14)
    val second_storage = List(21, 22, 23, 24)
    val inputs = List(
      { storage: first_storage, methods: List(method) },
      { storage: second_storage, methods: List(other_method) }
    )
    val outputs = List(
      { utxo: 0, storage: first_storage, methods: List(method) },
      { utxo: 1, storage: second_storage, methods: List(other_method) }
    )
    val first_handle = 7
    val second_handle = 8
    { state' = new_transaction(inputs, outputs) }
      .then(set_storage(first_storage, first_handle))
      .then(preload_method(method))
      .then(set_storage(second_storage, second_handle))
      .then(preload_method(other_method))
      // Both handles belong to the same coordinator, but resolve to different UTXOs.
      .then(call_method(first_handle, method, UNIT_VALUE, { value: UNIT_VALUE }))
      .then(enter_method(method, UNIT_VALUE))
      .then(return({ value: UNIT_VALUE }))
      .then(call_method(second_handle, other_method, UNIT_VALUE, { value: UNIT_VALUE }))
      .then(enter_method(other_method, UNIT_VALUE))
      .then(return({ value: UNIT_VALUE }))
      .then(return({ value: UNIT_VALUE }))
      .then(get_storage({ value: first_storage }))
      .then(read_abi(method))
      .then(get_storage({ value: second_storage }))
      .then(read_abi(other_method))
      .then(finalize_coordinator)
      .then(finish_transaction)
      .then(transaction_complete)
  }

  run test_storage_during_execution_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(set_storage(UNIT_VALUE, 0))
    .fail()

  run test_execution_before_inputs_loaded_fails = init_transaction
    .then(return({ value: UNIT_VALUE }))
    .fail()

  run test_export_during_execution_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(get_storage({ value: UNIT_VALUE }))
    .fail()

  run test_preload_after_execution_starts_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(preload_method(method))
    .fail()

  run test_execution_before_input_abi_loaded_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(return({ value: UNIT_VALUE }))
    .fail()

  run test_missing_storage_export_fails = init_transaction
    .then(set_storage(UNIT_VALUE, 0))
    .then(preload_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(finish_transaction)
    .fail()

  run test_centered_field_boundaries = init
    .then(new_utxo(List(-FIELD_HALF, FIELD_HALF, -1, 0), { value: 0 }))
    .then(enter_constructor(List(-FIELD_HALF, FIELD_HALF, -1, 0)))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run test_above_centered_field_bound_fails = init
    .then(new_utxo(List(FIELD_HALF + 1, 0, 0, 0), { value: 0 }))
    .fail()

  run test_below_centered_field_bound_fails = init
    .then(new_utxo(List(-FIELD_HALF - 1, 0, 0, 0), { value: 0 }))
    .fail()

  run test_execution_satisfies_spec = init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run test_enter_constructor_twice_fails = init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .fail()

  run test_incomplete_execution_fails = init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)
    .fail()

  run test_opcode_after_terminal_return_fails = init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(new_utxo(List(7, 0, 0, 0), { value: 1 }))
    .fail()

  run test_yield_from_coordinator_fails = init
    .then(yield_begin)
    .fail()

  run test_call_method_success =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), {value: List(7, 8, 0, 0)}))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .then(return({ value: List(7, 8, 0, 0) }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  // Returning from a method without `resume` preserves the current ABI.
  run test_method_call_without_resume_preserves_abi =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), {value: UNIT_VALUE}))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), {value: UNIT_VALUE}))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run test_enter_method_twice_fails =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), {value: UNIT_VALUE}))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .fail()

  run test_enter_method_with_wrong_method_fails =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), {value: UNIT_VALUE}))
    .then(enter_method(other_method, List(42, 0, 0, 0)))
    .fail()

  run test_call_unregistered_method_fails =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, other_method, List(42, 0, 0, 0), { value: UNIT_VALUE }))
    .fail()

  // A shared method dictionary does not make registrations global.
  run test_method_registered_only_by_another_utxo_fails = init
    .then(new_utxo(UNIT_VALUE, { value: 0 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(new_utxo(UNIT_VALUE, { value: 1 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(register_method(other_method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(1, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .fail()

  // Both UTXOs expose the same hash; resuming A must not reset B's ABI.
  run test_yield_preserves_another_utxos_abi = init
    .then(new_utxo(UNIT_VALUE, { value: 0 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(new_utxo(UNIT_VALUE, { value: 1 }))
    .then(enter_constructor(UNIT_VALUE))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(enter_method(method, UNIT_VALUE))
    .then(yield_begin)
    .then(register_method(other_method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(1, method, UNIT_VALUE, { value: UNIT_VALUE }))
    .then(enter_method(method, UNIT_VALUE))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run test_duplicate_method_registration_is_idempotent =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run test_stale_method_after_yield_fails =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), { value: UNIT_VALUE }))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .then(yield_begin)
    .then(register_method(other_method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), { value: UNIT_VALUE }))
    .fail()

  run test_return_with_wrong_result_fails =
    init
    .then(new_utxo(List(42, 0, 0, 0), { value: 0 }))
    .then(enter_constructor(List(42, 0, 0, 0)))
    .then(register_method(method))
    .then(return({ value: UNIT_VALUE }))
    .then(call_method(0, method, List(42, 0, 0, 0), {value: List(7, 8, 0, 0)}))
    .then(enter_method(method, List(42, 0, 0, 0)))
    .then(return({ value: List(7, 9, 0, 0) }))
    .fail()
}
