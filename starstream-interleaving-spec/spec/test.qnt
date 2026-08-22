module replay_trace {
  import starstream.* from "./starstream"

  action init = { state' = new_tx }

  run execution_satisfies_spec = init
    .then(new_utxo(List(42), { value: 0 }))
    .then(enter_constructor(List(42)))
    .then(yield_begin)
    .then(register_method("0000000000000001000000000000000100000000000000010000000000000001"))
    .then(return({ value: List(42) }))
    .then(return({ value: List() }))
    .then(execution_complete)
}
