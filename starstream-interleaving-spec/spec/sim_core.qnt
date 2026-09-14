// Generate consistent transactions: inputs are chosen up front, outputs are
// synthesized when execution ends. Only that boundary step is sim-specific;
// fixed-statement rejection tests use the unchanged core spec.
module starstream_sim_core {
  import starstream.* from "./starstream"
  export starstream.*
  import utils.* from "./utils"

  const SIM_METHODS: Set[MethodHash]
  const SIM_VALUES: Set[StarstreamValue]
  const SIM_RESOURCE_HANDLES: Set[ResourceHandle]
  const SIM_MAX_UTXOS: int
  const SIM_MAX_METHODS: int
  const SIM_MAX_INPUTS: int
  const SIM_ABIS: Set[List[MethodHash]]
  pure val SIM_INPUTS = SIM_VALUES.map(storage =>
    SIM_ABIS.map(methods => { storage: storage, methods: methods })).flatten()

  def last_input_loaded: bool = if (state.next_utxo_id == 0) true else
    state.abi_method_count.get(Utxo(state.next_utxo_id - 1)) ==
      state.tx_inputs[state.next_utxo_id - 1].methods.length()

  action simulate_set_storage: bool = all {
    last_input_loaded,
    state.next_utxo_id < state.tx_inputs.length(),
    nondet resource = SIM_RESOURCE_HANDLES.oneOf()
    set_storage(state.tx_inputs[state.next_utxo_id].storage, { value: resource }),
  }

  action simulate_preload_method: bool = all {
    state.next_utxo_id > 0,
    not(last_input_loaded),
    preload_method(state.tx_inputs[state.next_utxo_id - 1].methods[
      state.abi_method_count.get(Utxo(state.next_utxo_id - 1))]),
  }

  // Sim-only statement installation; it leaves the core phase unchanged.
  action simulate_install_outputs: bool = all {
    not(outputs_installed),
    state.tx_phase == Running,
    state.call_stack.length() == 0,
    // Apalache requires a constant domain for this function-set choice.
    nondet storages = 0.to(SIM_MAX_UTXOS - 1).setOfMaps(SIM_VALUES).oneOf()
    state' = {
      tx_outputs: range(0, SIM_MAX_UTXOS).foldl(List(), (outputs, id) =>
        if (id >= state.next_utxo_id) outputs
        else if (state.utxo_lifecycle.get(Utxo(id)) == Live)
          outputs.append({ utxo: id, storage: storages.get(id),
            methods: state.abi_registrations.get(Utxo(id)) })
        else outputs),
      ...state
    },
    outputs_installed' = true,
  }

  action simulate_get_storage: bool = all {
    state.outputs_seen < state.tx_outputs.length(),
    get_storage({ value: state.tx_outputs[state.outputs_seen].storage }),
  }

  action simulate_read_abi: bool = all {
    state.abi_read_remaining > 0,
    read_abi(state.tx_outputs[state.outputs_seen - 1].methods[state.abi_read_ordinal]),
  }

  action simulate_new_utxo: bool = all {
    state.next_utxo_id < SIM_MAX_UTXOS,
    nondet arguments = SIM_VALUES.oneOf()
    nondet resource = SIM_RESOURCE_HANDLES.oneOf()
    new_utxo(arguments, {value: resource }),
  }

  action simulate_register_method: bool = all {
    state.curr.is_utxo(),
    state.abi_method_count.get(state.curr) < SIM_MAX_METHODS,
    nondet method = SIM_METHODS.oneOf()
    register_method(method),
  }

  action simulate_enter_constructor: bool = {
    val expected_arguments =
      if (state.call_stack.length() == 0)
        UNIT_VALUE
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
        UNIT_VALUE
      else
        state.call_stack.last().expected_result

    nondet result = Set(expected_result).oneOf()
    return({ value: result })
  }

  action simulate_enter_method: bool = {
    val expected_arguments =
      if (state.call_stack.length() == 0)
        UNIT_VALUE
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
  var prev_tx_phase: TxPhase
  var outputs_installed: bool

  // Choose one enabled semantic event at each simulated transition.
  action step: bool = all {
    prev_lifecycle' = state.utxo_lifecycle,
    prev_tx_phase' = state.tx_phase,
    any {
      simulate_install_outputs,
      all {
        outputs_installed' = outputs_installed,
        match state.tx_phase {
          | Loading => any {
              simulate_set_storage,
              simulate_preload_method,
              all { last_input_loaded, state.next_utxo_id == state.tx_inputs.length(), execution_step },
            }
          | Running => any {
              execution_step,
              all { outputs_installed,
                any { simulate_get_storage, simulate_read_abi, skip_consumed, finish_transaction } },
            }
          | Finished => transaction_complete
        },
      },
    },
  }

  action execution_step: bool = any {
      simulate_new_utxo,
      simulate_enter_constructor,
      yield_begin,
      simulate_register_method,
      simulate_return,
      simulate_call_method,
      simulate_enter_method,
  }

  action init = {
    nondet input_count = 0.to(SIM_MAX_INPUTS).oneOf()
    // Constant-size domain keeps this choice compatible with Apalache.
    nondet candidates = 0.to(SIM_MAX_INPUTS - 1).setOfMaps(SIM_INPUTS).oneOf()
    val inputs = range(0, SIM_MAX_INPUTS).foldl(List(), (inputs, i) =>
      if (i < input_count) inputs.append(candidates.get(i)) else inputs)
    val fresh_state = new_transaction(inputs, List())
    all {
      state' = fresh_state,
      prev_lifecycle' = fresh_state.utxo_lifecycle,
      prev_tx_phase' = fresh_state.tx_phase,
      outputs_installed' = false,
    }
  }

  // transition invariant
  val cannot_return_to_loading =
    prev_tx_phase != Loading implies state.tx_phase != Loading

  // transition invariant
  val consumed_utxo_stays_consumed =
    prev_lifecycle.keys().forall(u =>
        prev_lifecycle.get(u) == Consumed
          implies state.utxo_lifecycle.get(u) == Consumed
    )

  val tx_finished = state.tx_phase == Finished
  val has_loaded_input = state.next_utxo_id > 0 and state.tx_inputs.length() > 0
  val has_exported_abi = state.abi_read_ordinal > 0
  val finished_without_inputs = tx_finished and state.tx_inputs.length() == 0
  val finished_with_new_output = tx_finished and state.tx_outputs.indices().exists(i =>
    state.tx_outputs[i].utxo >= state.tx_inputs.length())
  val finished_outputs_match_live_utxos = state.tx_phase == Finished implies
    state.outputs_seen == state.utxo_lifecycle.keys().filter(u => state.utxo_lifecycle.get(u) == Live).size()

  // Reachability witnesses make it visible when random exploration stops
  // exercising an important part of the protocol.
  val has_bound_resource = state.resource_resolver.keys().size() > 0

  val has_yielded_utxo =
    state.enabled_methods.keys().contains(state.curr)

  val has_dead_utxo = state.utxo_lifecycle.keys().exists(cid =>
    state.utxo_lifecycle.get(cid) == Consumed
  )

}
