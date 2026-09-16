module starstream {
  // a spec that is meant to be as close to the circuit as possible
  //
  // execution actions in this module correspond roughly with one host call
  // (core wasm host call)
  //
  // similarly, each action should map to a single opcode in the circuit
  //
  // invariants are for things that we DO NOT prove, we can use Quint's
  // simulation or model checking to have confidence that those hold
  //
  // (conditional on whether the bounded domains for model checking and the
  // simulator are representative)
  import utils.* from "./utils"

  // Opaque object root: four centered Goldilocks representatives in
  // [-(p-1)/2, (p-1)/2], which fit the evaluator's signed 64-bit integers.
  // The program proof owns schema/length framing and payload hashing.
  type StarstreamValue = List[int]
  // Unit is emitted as four constants, not as an empty-object hash.
  pure val UNIT_VALUE = List(0, 0, 0, 0)
  // (p-1)/2, written directly to avoid evaluating 2^64 or p in Quint.
  pure val FIELD_HALF = 9223372034707292160
  type ResourceHandle = int
  type MethodHash = str
  type CoroutineId = Coord(int) | Utxo(int)

  type ResourceKey = {
    holder: CoroutineId,
    handle: ResourceHandle,
  }

  type CallFrame = {
    caller: CoroutineId,
    expected_arguments: StarstreamValue,
    expected_result: StarstreamValue,
    expected_method: Option[MethodHash],
  }

  type CurrPhase =
      CtorEnterPending
    | MethodEnterPending
    | Executing
    | Yield

  type UtxoLifecycle = Live | Consumed
  // Running includes output processing once the call stack is empty.
  type TxPhase = Loading | Running | Finished
  type InputUtxo = {
    storage: StarstreamValue,
    methods: List[MethodHash]
  }
  type OutputUtxo = { utxo: int, storage: StarstreamValue, methods: List[MethodHash] }

  // just a tagged type so that the action signature reflects what's input and
  // output (in the corresponding wasm function)
  type Output[t] = { value: t }

  type State = {
    tx_phase: TxPhase,
    tx_inputs: List[InputUtxo],
    // Initial preload sequences, framed per input and unchanged by execution.
    loaded_input_methods: List[List[MethodHash]],
    tx_outputs: List[OutputUtxo],
    output_cursor: int,
    outputs_seen: int,
    coordinator_finalized: bool,
    curr: CoroutineId,
    call_stack: List[CallFrame],
    curr_cid_phase: CurrPhase,
    utxo_lifecycle: CoroutineId -> UtxoLifecycle,
    next_utxo_id: int,
    enabled_methods: CoroutineId -> Set[MethodHash],
    // Current-generation registrations, including duplicates (maximum 255).
    // The pre-increment count is the append-log ordinal in the circuit.
    abi_method_count: CoroutineId -> int,
    abi_registrations: CoroutineId -> List[MethodHash],
    abi_read_remaining: int,
    abi_read_ordinal: int,
    // ResourceKey is an instance(coroutine)-dependent id
    //
    // this maps from local handles to instances
    //
    // this map is not injective
    resource_resolver: ResourceKey -> CoroutineId,
    // stores the id of the caller to utxo new
    //
    // we don't need a stack for this because utxo constructor can't call other
    // constructors (for now)
    //
    // this gets consumed when the constructors returns, but the data is not
    // available in that event, since only the coord has the coord-scoped
    // resource
    //
    // because of that, we need to keep track of this until then
    pending_ctor_key: Option[ResourceKey],
  }

  var state: State

  type Opcode =
      NewUtxo
    | EnterConstructor
    | CallMethod
    | EnterMethod
    | Return
    | YieldBegin
    | RegisterMethod

  def action_allowed(op: Opcode): bool = all {
    state.tx_phase == Running or (state.tx_phase == Loading and inputs_loaded),
    // the call stack starts with length = 1 (as a sentinel/top level coord
    // script to make the logic more uniform)
    // so an empty call stack is a termination condition
    state.call_stack.length() > 0,
    match op {
      | NewUtxo => state.curr_cid_phase == Executing
      | EnterConstructor => state.curr_cid_phase == CtorEnterPending
      | CallMethod => state.curr_cid_phase == Executing
      | EnterMethod => state.curr_cid_phase == MethodEnterPending
      | Return =>
          state.curr_cid_phase == Executing or state.curr_cid_phase == Yield
      | YieldBegin => state.curr_cid_phase == Executing
      | RegisterMethod => state.curr_cid_phase == Yield
    }
  }

  pure def is_utxo(cid: CoroutineId): bool =
    match cid {
      | Utxo(_) => true
      | Coord(_) => false
    }

  pure def is_coord(cid: CoroutineId): bool =
    not(is_utxo(cid))

  pure def is_valid(value: StarstreamValue): bool =
    value.length() == 4 and
      value.foldl(true, (valid, word) => valid and -FIELD_HALF <= word and word <= FIELD_HALF)

  // the init action is in sim.qnt, or in the generated test modules
  //
  // that makes it easier to add ghost variables to the state later for
  // verification/tracing purposes
  pure def new_tx(): State = {
    val cid = Coord(1)

    {
      tx_phase: Running,
      tx_inputs: List(),
      loaded_input_methods: List(),
      tx_outputs: List(),
      output_cursor: 0,
      outputs_seen: 0,
      coordinator_finalized: false,
      curr: cid,
      call_stack: List({
        caller: Coord(0),
        expected_arguments: UNIT_VALUE,
        expected_result: UNIT_VALUE,
        expected_method: None,
      }),
      curr_cid_phase: Executing,
      utxo_lifecycle: Map(),
      next_utxo_id: 0,
      enabled_methods: Map(),
      abi_method_count: Map(),
      abi_registrations: Map(),
      abi_read_remaining: 0,
      abi_read_ordinal: 0,
      resource_resolver: Map(),
      pending_ctor_key: None,
    }
  }

  // Execution-only tests use new_tx; transaction replay includes the boundaries.
  pure def new_transaction(inputs: List[InputUtxo], outputs: List[OutputUtxo]): State = {
    tx_phase: Loading, tx_inputs: inputs, tx_outputs: outputs, ...new_tx
  }

  def last_input_has_abi: bool = state.next_utxo_id == 0 or
    not(state.enabled_methods.get(Utxo(state.next_utxo_id - 1)).is_empty_set())

  action set_storage(storage: StarstreamValue, resource: Output[ResourceHandle]): bool = all {
    state.tx_phase == Loading,
    last_input_has_abi,
    state.next_utxo_id < state.tx_inputs.length(),
    storage.is_valid(),
    // Keep statement binding local for simulation. The circuit needs no per-step
    // equality to tx_inputs: the transaction IO commitment binds this storage.
    // TODO(proof): Authenticate the digest endpoints through the proof statement.
    storage == state.tx_inputs[state.next_utxo_id].storage,
    val target = Utxo(state.next_utxo_id)
    state' = {
      next_utxo_id: state.next_utxo_id + 1,
      utxo_lifecycle: state.utxo_lifecycle.put(target, Live),
      enabled_methods: state.enabled_methods.put(target, Set()),
      abi_method_count: state.abi_method_count.put(target, 0),
      abi_registrations: state.abi_registrations.put(target, List()),
      loaded_input_methods: state.loaded_input_methods.append(List()),
      resource_resolver: state.resource_resolver.put({
        holder: state.curr,
        handle: resource.value
      }, target),
      ...state
    }
  }

  // Host-synthesized from the input's stored ABI, not emitted by wasm.
  action preload_method(method: MethodHash): bool = all {
    state.tx_phase == Loading,
    state.next_utxo_id > 0,
    state.abi_method_count.get(Utxo(state.next_utxo_id - 1)) < 255,
    state' = {
      abi_method_count: state.abi_method_count.setBy(Utxo(state.next_utxo_id - 1), count => count + 1),
      abi_registrations: state.abi_registrations.setBy(Utxo(state.next_utxo_id - 1), methods => methods.append(method)),
      loaded_input_methods: state.loaded_input_methods.replaceAt(
        state.next_utxo_id - 1,
        state.loaded_input_methods.last().append(method)
      ),
      enabled_methods: state.enabled_methods.setBy(
        Utxo(state.next_utxo_id - 1),
        prev => prev.insert(method)
      ),
      ...state
    }
  }

  def inputs_loaded: bool = all {
    last_input_has_abi,
    state.next_utxo_id == state.tx_inputs.length(),
  }

  def finalization_allowed: bool = all {
    state.tx_phase == Running,
    state.call_stack.length() == 0,
  }

  action get_storage(storage: Output[StarstreamValue]): bool = all {
    state.abi_read_remaining == 0,
    finalization_allowed,
    state.output_cursor < state.next_utxo_id,
    state.utxo_lifecycle.get(Utxo(state.output_cursor)) == Live,
    state.outputs_seen < state.tx_outputs.length(),
    state.tx_outputs[state.outputs_seen].utxo == state.output_cursor,
    storage.value.is_valid(),
    storage.value == state.tx_outputs[state.outputs_seen].storage,
    state.tx_outputs[state.outputs_seen].methods.length() == state.abi_method_count.get(Utxo(state.output_cursor)),
    state' = { output_cursor: state.output_cursor + 1, outputs_seen: state.outputs_seen + 1,
      abi_read_remaining: state.abi_method_count.get(Utxo(state.output_cursor)), abi_read_ordinal: 0, ...state }
  }

  // Host-synthesized final ABI enumeration, not a wasm event.
  action read_abi(method: MethodHash): bool = all {
    state.tx_phase == Running,
    state.abi_read_remaining > 0,
    state.abi_registrations.get(Utxo(state.output_cursor - 1))[state.abi_read_ordinal] == method,
    state.tx_outputs[state.outputs_seen - 1].methods[state.abi_read_ordinal] == method,
    state' = { abi_read_remaining: state.abi_read_remaining - 1,
      abi_read_ordinal: state.abi_read_ordinal + 1, ...state }
  }

  action skip_consumed: bool = all {
    state.abi_read_remaining == 0,
    finalization_allowed,
    state.output_cursor < state.next_utxo_id,
    state.utxo_lifecycle.get(Utxo(state.output_cursor)) == Consumed,
    state' = { output_cursor: state.output_cursor + 1, ...state }
  }

  // Host-synthesized root enumeration; hashing belongs to the circuit.
  // TODO: Enumerate all coordinators once the model supports coordinator calls.
  action finalize_coordinator: bool = all {
    finalization_allowed,
    state.abi_read_remaining == 0,
    state.output_cursor == state.next_utxo_id,
    not(state.coordinator_finalized),
    state' = { coordinator_finalized: true, ...state }
  }

  action finish_transaction: bool = all {
    state.coordinator_finalized,
    state.abi_read_remaining == 0,
    finalization_allowed,
    state.output_cursor == state.next_utxo_id,
    state.outputs_seen == state.tx_outputs.length(),
    // Deferred statement binding; a framed preload digest can implement this.
    state.loaded_input_methods == state.tx_inputs.foldl(
      List(), (methods, input) => methods.append(input.methods)),
    state' = { tx_phase: Finished, ...state }
  }

  action transaction_complete: bool = all {
    state.tx_phase == Finished,
    state' = state,
  }

  // triggers on a call to a utxo main function, in a coord script context
  action new_utxo(
    arguments: StarstreamValue,
    resource: Output[ResourceHandle]
  ): bool = {
    val caller = state.curr
    val target = Utxo(state.next_utxo_id)

    all {
      action_allowed(NewUtxo),
      arguments.is_valid(),
      caller.is_coord(),
      state' = {
        curr: target,
        tx_phase: Running,
        call_stack: state.call_stack.append(
          {
            caller: caller,
            expected_arguments: arguments,
            expected_result: UNIT_VALUE,
            expected_method: None,
          }
        ),
        curr_cid_phase: CtorEnterPending,
        utxo_lifecycle: state.utxo_lifecycle.put(target, Live),
        next_utxo_id: state.next_utxo_id + 1,
        enabled_methods: state.enabled_methods.put(target, Set()),
        abi_method_count: state.abi_method_count.put(target, 0),
        abi_registrations: state.abi_registrations.put(target, List()),
        pending_ctor_key: Some({
          holder: caller,
          handle: resource.value
        }),
        ...state
      }
    }
  }

  action enter_constructor(arguments: StarstreamValue): bool = all {
    all {
      action_allowed(EnterConstructor),
      state.curr.is_utxo(),
      arguments.is_valid(),
      state.call_stack.last().expected_arguments == arguments,

      state.pending_ctor_key.is_some(),

      state' = {
        tx_phase: Running,
        // Constructors establish generation zero without a preceding runtime
        // `resume` call, so constructor entry opens the initial ABI window.
        curr_cid_phase: Yield,
        ...state
      }
    }
  }

  // triggers with utxo-context.resume()
  action yield_begin(): bool = all {
    action_allowed(YieldBegin),
    state.curr.is_utxo(),
    // this forbids "yielding twice" (because we set it to Yield in post)
    //
    // it may or may not be too restrictive, review later with the runtime/lang
    // integration

    state' = {
      abi_method_count: state.abi_method_count.put(state.curr, 0),
      abi_registrations: state.abi_registrations.put(state.curr, List()),
      curr_cid_phase: Yield,
      tx_phase: Running,
      enabled_methods:
        // begin yield (a `resume` keyword in a utxo method) resets the abi set
        state.enabled_methods.put(state.curr, Set()),
      ...state
    }
  }

  // triggers with utxo-context.implements-method(hash)
  action register_method(method: MethodHash): bool = all {
    action_allowed(RegisterMethod),
    state.curr.is_utxo(),
    state.abi_method_count.get(state.curr) < 255,

    state' = {
      abi_method_count: state.abi_method_count.setBy(state.curr, count => count + 1),
      abi_registrations: state.abi_registrations.setBy(state.curr, methods => methods.append(method)),
      tx_phase: Running,
      // Set insertion makes repeated registrations semantically idempotent.
      enabled_methods:
        state.enabled_methods.setBy(state.curr, prev => prev.insert(method)),
      ...state
    }
  }

  // triggers on the exit-side of an exported function
  //
  // currently this is applied to every function, but we could split it if we
  // need different semantics
  action return(result: Output[StarstreamValue]): bool = all {
    action_allowed(Return),
    result.value.is_valid(),
    val frame = state.call_stack.last()

    val resource_resolver = match state.pending_ctor_key {
      | Some(key) =>
        state.resource_resolver.put(key, state.curr)
      | None => state.resource_resolver
    }

    val utxo_lifecycle = if (state.curr.is_utxo()) {
      val yield_empty_abi = state.enabled_methods.get(state.curr) == Set()

      val new_lifecycle = if (yield_empty_abi) {
        Consumed
      } else {
        Live
      }

      state.utxo_lifecycle.put(state.curr, new_lifecycle)
    }
    else {
      // presumably we can use the same opcode for utxo and coord return
      state.utxo_lifecycle
    }

    all {
      frame.expected_result == result.value,
      state' = {
        curr: frame.caller,
        tx_phase: Running,
        resource_resolver: resource_resolver,
        call_stack: state.call_stack.pop_last(),
        pending_ctor_key: None,
        utxo_lifecycle: utxo_lifecycle,
        curr_cid_phase: Executing,
        ...state
      },
    }
  }

  // triggers on a coord making a call to a resource that is associated with
  // a utxo
  action call_method(
    resource: ResourceHandle,
    method: MethodHash,
    arguments: StarstreamValue,
    result: Output[StarstreamValue]
  ): bool = {
    val target = state.resource_resolver.get({
      holder: state.curr,
      handle: resource
    })

    all {
      action_allowed(CallMethod),
      arguments.is_valid(),
      result.value.is_valid(),
      // utxos can't call utxos
      state.curr.is_coord(),
      state.resource_resolver.keys().contains({ holder: state.curr, handle: resource }),
      state.enabled_methods.get(target).contains(method),
      state.utxo_lifecycle.get(target) == Live,

      val frame = {
        caller: state.curr,
        expected_arguments: arguments,
        expected_result: result.value,
        expected_method: Some(method)
      }
      state' = {
        curr: target,
        call_stack: state.call_stack.append(frame),
        tx_phase: Running,
        curr_cid_phase: MethodEnterPending,

        ...state
      }
    }
  }

  // triggers immediately before entering the body of a utxo function that is
  // registered as a method
  //
  // this is attributed to the receiver
  action enter_method(
    method: MethodHash,
    arguments: StarstreamValue
  ): bool = all {
    action_allowed(EnterMethod),
    arguments.is_valid(),

    // the utxo "calls" this
    state.curr.is_utxo(),
    state.call_stack.length() > 0,
    state.call_stack.last().expected_arguments == arguments,
    state.call_stack.last().expected_method == Some(method),

    state' = {
      curr_cid_phase: Executing,
      tx_phase: Running,

      ...state
    }
  }

  action execution_complete: bool = all {
    state.tx_phase == Running,
    state.curr.is_coord(),
    state.call_stack.length() == 0,
    state' = state,
  }

  val coord_stack =
    state.curr.is_utxo() implies state.call_stack.length() > 1

  // The scalar count mirrors the circuit's RAM counter; it is redundant here.
  val abi_method_count_matches_registrations = all {
    state.abi_method_count.keys() == state.abi_registrations.keys(),
    state.abi_method_count.keys().forall(u => all {
      state.abi_method_count.get(u) == state.abi_registrations.get(u).length(),
      state.abi_method_count.get(u) >= 0,
      state.abi_method_count.get(u) <= 255,
    }),
  }

  // Once execution ends, the ABI count determines which UTXOs survive.
  val completed_lifecycle_matches_abi_count =
    state.call_stack.length() == 0 implies
      state.utxo_lifecycle.keys().forall(u =>
        (state.utxo_lifecycle.get(u) == Live) == (state.abi_method_count.get(u) > 0)
      )

  val consumed_utxos_have_no_methods =
    state.utxo_lifecycle.keys().forall(u =>
      state.utxo_lifecycle.get(u) == Consumed
        implies state.enabled_methods.get(u).is_empty_set()
    )
}
