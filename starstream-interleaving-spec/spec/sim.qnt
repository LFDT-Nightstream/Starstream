// Small sampled domains for random exploration. This module only chooses
// concrete arguments for the semantic actions above; it does not change their
// guards or transition semantics.
module starstream_sim {
  import starstream.* from "./starstream"
  import utils.* from "./utils"

  pure val SIM_METHODS = Set("m0", "m1", "m2")
  pure val SIM_VALUES: Set[StarstreamValue] =
    Set(List(), List(0), List(1), List(1, 2))
  pure val SIM_RESOURCE_HANDLES = Set(0, 1, 2, 3)
  pure val SIM_MAX_UTXOS = 4

  action simulate_new_utxo: bool = all {
    state.next_utxo_id < SIM_MAX_UTXOS,
    nondet arguments = SIM_VALUES.oneOf()
    nondet resource = SIM_RESOURCE_HANDLES.oneOf()
    new_utxo(arguments, {value: resource }),
  }

  action simulate_register_method: bool = {
    nondet method = SIM_METHODS.oneOf()
    register_method(method)
  }

  action simulate_enter_constructor: bool = {
    val expected_arguments =
      if (state.call_stack.length() == 0)
        List()
      else
        state.call_stack.last().expected_arguments

    nondet arguments = Set(expected_arguments).oneOf()
    enter_constructor(arguments)
  }

  action simulate_call_method: bool = {
    val callable_resources = state.resource_resolver.keys().filter(key =>
      key.holder == state.curr
        and state.utxo_lifecycle.get(state.resource_resolver.get(key)) == Live
    )
    nondet key = callable_resources.oneOf()
    val target = state.resource_resolver.get(key)
    nondet method = state.enabled_methods.get(target).oneOf()
    nondet arguments = SIM_VALUES.oneOf()
    nondet result = SIM_VALUES.oneOf()
    call_method(key.handle, method, arguments, { value: result })
  }

  action simulate_return: bool = {
    val expected_result =
      if (state.call_stack.length() == 0)
        List()
      else
        state.call_stack.last().expected_result

    nondet result = Set(expected_result).oneOf()
    return({ value: result })
  }

  action simulate_enter_method: bool = {
    val expected_arguments =
      if (state.call_stack.length() == 0)
        List()
      else
        state.call_stack.last().expected_arguments

    nondet arguments = Set(expected_arguments).oneOf()
    nondet method = SIM_METHODS.oneOf()
    enter_method(method, arguments)
  }

  // ghost variable used to verify transition invariants using next() inside
  // always() seems to not be well supported by the quint tla generation (orKeep
  // wants a non temporal action, so nesting doesn't work)
  var prev_lifecycle: CoroutineId -> UtxoLifecycle

  // Choose one enabled semantic event at each simulated transition.
  action step: bool = all {
    prev_lifecycle' = state.utxo_lifecycle,
    any {
      simulate_new_utxo,
      simulate_enter_constructor,
      yield_begin,
      simulate_register_method,
      simulate_return,
      simulate_call_method,
      simulate_enter_method,
      // consume_utxo,
    }
  }

  action init = {
    val fresh_state = new_tx
    all {
      state' = fresh_state,
      prev_lifecycle' = fresh_state.utxo_lifecycle,
    }
  }

  // transition invariant
  val consumed_utxo_stays_consumed =
    prev_lifecycle.keys().forall(u =>
        prev_lifecycle.get(u) == Consumed
          implies state.utxo_lifecycle.get(u) == Consumed
    )

  val tx_finished = state.call_stack.length() == 0

  // Reachability witnesses make it visible when random exploration stops
  // exercising an important part of the protocol.
  val has_bound_resource = state.resource_resolver.keys().size() > 0

  val has_yielded_utxo =
    state.enabled_methods.keys().contains(state.curr)

  val has_dead_utxo = state.utxo_lifecycle.keys().exists(cid =>
    state.utxo_lifecycle.get(cid) == Consumed
  )

  // val has_entered_method =
  //   state.active_turn == Executing and current_method_frame_is_well_formed
  // val has_yielded_utxo = state.utxo_lifecycle.keys().exists(cid =>
  //   state.utxo_lifecycle.get(cid) == Live
  // )
  // val has_started_coroutine_return = state.active_turn == ReturningTurn
}
