module replay_trace {
  import starstream.* from "./starstream"

  action init = { state' = new_tx }

  def method = "0000000000000001000000000000000100000000000000010000000000000001";
  def other_method = "0000000000000002000000000000000100000000000000010000000000000001";

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
