module replay_trace {
  import starstream.* from "./starstream"

  action init = { state' = new_tx }

  def method = "0000000000000001000000000000000100000000000000010000000000000001";
  def other_method = "0000000000000002000000000000000100000000000000010000000000000001";

  run test_execution_satisfies_spec = init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(yield_begin)
    .then(register_method(method))
    .then(return({ value: List() }))
    .then(return({ value: List() }))
    .then(execution_complete)

  run test_enter_constructor_twice_fails = init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(enter_constructor(List(42)))
    .fail()

  run test_call_method_success =
    init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(yield_begin)
    .then(register_method(method))
    .then(return({ value: List() }))
    .then(call_method(0, method, List(42), {value: List(7, 8)}))
    .then(enter_method(method, List(42)))
    .then(return({ value: List(7, 8) }))
    .then(return({ value: List() }))
    .then(execution_complete)

  run test_enter_method_twice_fails =
    init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(yield_begin)
    .then(register_method(method))
    .then(return({ value: List() }))
    .then(call_method(0, method, List(42), {value: List()}))
    .then(enter_method(method, List(42)))
    .then(enter_method(method, List(42)))
    .fail()

  run test_enter_method_with_wrong_method_fails =
    init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(yield_begin)
    .then(register_method(method))
    .then(return({ value: List() }))
    .then(call_method(0, method, List(42), {value: List()}))
    .then(enter_method(other_method, List(42)))
    .fail()

  run test_return_with_wrong_result_fails =
    init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(yield_begin)
    .then(register_method(method))
    .then(return({ value: List() }))
    .then(call_method(0, method, List(42), {value: List(7, 8)}))
    .then(enter_method(method, List(42)))
    .then(return({ value: List(7, 9) }))
    .fail()
}
