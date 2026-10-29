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

  type CurrPhase =
      CtorEnterPending
    | MethodEnterPending
    | Executing
    | ConstructorExecuting
    | Registering
    | EffectHandlerPending
    | EffectHandlerExecuting
    | EffectResumePending
    | MethodResultPending
    | NewUtxoHandlePending

  type CallFrameKind = ProgramCall | ConstructorCall | EffectCall

  type EffectScope = {
    parent: Option[int],
    owner: CoroutineId,
    frame_id: int,
    effects: Set[MethodHash],
  }

  type CallFrame = {
    kind: CallFrameKind,
    caller: CoroutineId,
    expected_arguments: StarstreamValue,
    expected_result: StarstreamValue,
    expected_method: Option[MethodHash],
    saved_phase: CurrPhase,
    // Caller restoration context; the current head doubles as the search cursor.
    saved_effect_head: Option[int],
    saved_effect_depth: int,
  }

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
    effect_arena: List[EffectScope],
    // Visible chain during execution; candidate cursor during pending search.
    effect_head: Option[int],
    // Scopes pushed by the current invocation and not yet popped; these are
    // the top nodes of the visible chain. During a pending search this is
    // still the raiser's count.
    effect_local_depth: int,
    // Program invocations remain through Receive* for resource-handle binding.
    // Handler entry changes curr, but not this stack; search follows effect_arena parent links.
    instance_stack: List[CoroutineId],
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
  }

  var state: State

  type Opcode =
      NewUtxo
    | ReceiveNewUtxoHandle
    | EnterConstructor
    | CallMethod
    | ReceiveMethodResult
    | EnterMethod
    | RaiseEffect
    | PushEffectHandlerFrame
    | AddEffectHandler
    | PopEffectHandlerFrame
    | SkipEffectHandler
    | EnterEffectHandler
    | ExitEffectHandler
    | ResumeEffect
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
      | NewUtxo => state.curr_cid_phase == Executing or state.curr_cid_phase == EffectHandlerExecuting
      | ReceiveNewUtxoHandle => state.curr_cid_phase == NewUtxoHandlePending
      | EnterConstructor => state.curr_cid_phase == CtorEnterPending
      | CallMethod => state.curr_cid_phase == Executing or state.curr_cid_phase == EffectHandlerExecuting
      | ReceiveMethodResult => state.curr_cid_phase == MethodResultPending
      | EnterMethod => state.curr_cid_phase == MethodEnterPending
      | Return =>
          state.curr_cid_phase == Executing or
          state.curr_cid_phase == ConstructorExecuting or
          state.curr_cid_phase == Registering
      // The first `utxo-context.resume()` moves the invocation out of
      // Executing, so this also forbids a second resume in that invocation.
      // Revisit if the runtime/language is intended to allow that.
      | YieldBegin => state.curr_cid_phase == Executing
      | RegisterMethod =>
          state.curr_cid_phase == ConstructorExecuting or
          state.curr_cid_phase == Registering
      | RaiseEffect =>
          state.curr_cid_phase == Executing or
          state.curr_cid_phase == ConstructorExecuting or
          state.curr_cid_phase == EffectHandlerExecuting
      | PushEffectHandlerFrame => guest_executing
      | AddEffectHandler => guest_executing
      | PopEffectHandlerFrame => guest_executing
      | SkipEffectHandler => state.curr_cid_phase == EffectHandlerPending
      | EnterEffectHandler => state.curr_cid_phase == EffectHandlerPending
      | ExitEffectHandler => state.curr_cid_phase == EffectHandlerExecuting
      | ResumeEffect => state.curr_cid_phase == EffectResumePending
    }
  }

  // Registration calls can execute in a live guest body, including cleanup
  // after registration. They cannot interleave with pending call boundaries.
  def guest_executing: bool =
    state.curr_cid_phase == Executing or
    state.curr_cid_phase == ConstructorExecuting or
    state.curr_cid_phase == Registering or
    state.curr_cid_phase == EffectHandlerExecuting

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
      instance_stack: List(cid),
      effect_arena: List(),
      effect_head: None,
      effect_local_depth: 0,
      call_stack: List({
        kind: ProgramCall,
        caller: Coord(0),
        expected_arguments: UNIT_VALUE,
        expected_result: UNIT_VALUE,
        expected_method: None,
        saved_phase: Executing,
        saved_effect_head: None,
        saved_effect_depth: 0,
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
    }
  }

  // Execution-only tests use new_tx; transaction replay includes the boundaries.
  pure def new_transaction(inputs: List[InputUtxo], outputs: List[OutputUtxo]): State = {
    tx_phase: Loading, tx_inputs: inputs, tx_outputs: outputs, ...new_tx
  }

  def last_input_has_abi: bool = state.next_utxo_id == 0 or
    not(state.enabled_methods.get(Utxo(state.next_utxo_id - 1)).is_empty_set())

  // coordinator_handle is host metadata, not a return value of the UTXO export.
  action set_storage(storage: StarstreamValue, coordinator_handle: ResourceHandle): bool = all {
    state.tx_phase == Loading,
    not(state.resource_resolver.keys().contains({ holder: state.curr, handle: coordinator_handle })),
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
        handle: coordinator_handle
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
  action new_utxo(arguments: StarstreamValue): bool = {
    val caller = state.curr
    val target = Utxo(state.next_utxo_id)

    all {
      action_allowed(NewUtxo),
      arguments.is_valid(),
      caller.is_coord(),
      state' = {
        curr: target,
        effect_local_depth: 0,
        instance_stack: state.instance_stack.append(target),
        tx_phase: Running,
        call_stack: state.call_stack.append(
          {
            kind: ConstructorCall,
            caller: caller,
            expected_arguments: arguments,
            expected_result: UNIT_VALUE,
            expected_method: None,
            saved_phase: state.curr_cid_phase,
            saved_effect_head: state.effect_head,
            saved_effect_depth: state.effect_local_depth,
          }
        ),
        curr_cid_phase: CtorEnterPending,
        utxo_lifecycle: state.utxo_lifecycle.put(target, Live),
        next_utxo_id: state.next_utxo_id + 1,
        enabled_methods: state.enabled_methods.put(target, Set()),
        abi_method_count: state.abi_method_count.put(target, 0),
        abi_registrations: state.abi_registrations.put(target, List()),
        ...state
      }
    }
  }

  action enter_constructor(arguments: StarstreamValue): bool = all {
    all {
      action_allowed(EnterConstructor),
      state.curr.is_utxo(),
      arguments.is_valid(),
      state.call_stack.last().kind == ConstructorCall,
      state.call_stack.last().expected_arguments == arguments,

      state' = {
        tx_phase: Running,
        // Constructors establish generation zero without a preceding runtime
        // `resume` call. Their main body runs before its first registration.
        curr_cid_phase: ConstructorExecuting,
        ...state
      }
    }
  }

  // triggers with utxo-context.resume()
  action yield_begin(): bool = all {
    action_allowed(YieldBegin),
    state.effect_local_depth == 0,
    state.curr.is_utxo(),

    state' = {
      abi_method_count: state.abi_method_count.put(state.curr, 0),
      abi_registrations: state.abi_registrations.put(state.curr, List()),
      curr_cid_phase: Registering,
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
      curr_cid_phase: Registering,
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
      frame.kind == ProgramCall or frame.kind == ConstructorCall,
      state.effect_local_depth == 0,
      if (frame.kind == ConstructorCall or frame.caller == Coord(0))
        frame.expected_result == result.value
      else true,
      val returning_to_import = frame.caller != Coord(0)
      state' = {
        curr: frame.caller,
        effect_local_depth: frame.saved_effect_depth,
        tx_phase: Running,
        call_stack:
          if (returning_to_import)
            state.call_stack.replaceAt(
              state.call_stack.length() - 1,
              { expected_result: result.value, ...frame })
          else state.call_stack.pop_last(),
        utxo_lifecycle: utxo_lifecycle,
        curr_cid_phase:
          if (not(returning_to_import)) Executing
          else if (frame.kind == ConstructorCall) NewUtxoHandlePending
          else MethodResultPending,
        ...state
      },
    }
  }

  action receive_new_utxo_handle(resource: Output[ResourceHandle]): bool = {
    val frame = state.call_stack.last()
    val key = { holder: state.curr, handle: resource.value }
    all {
      action_allowed(ReceiveNewUtxoHandle),
      frame.caller == state.curr,
      not(state.resource_resolver.keys().contains(key)),
      state' = {
        instance_stack: state.instance_stack.pop_last(),
        call_stack: state.call_stack.pop_last(),
        resource_resolver: state.resource_resolver.put(key, state.instance_stack.last()),
        curr_cid_phase: frame.saved_phase,
        ...state
      }
    }
  }

  // triggers on a coord making a call to a resource that is associated with
  // a utxo
  action call_method(
    resource: ResourceHandle,
    method: MethodHash,
    arguments: StarstreamValue
  ): bool = {
    val target = state.resource_resolver.get({
      holder: state.curr,
      handle: resource
    })

    all {
      action_allowed(CallMethod),
      arguments.is_valid(),
      // utxos can't call utxos
      state.curr.is_coord(),
      state.resource_resolver.keys().contains({ holder: state.curr, handle: resource }),
      state.enabled_methods.get(target).contains(method),
      state.utxo_lifecycle.get(target) == Live,

      val frame = {
        kind: ProgramCall,
        caller: state.curr,
        expected_arguments: arguments,
        expected_result: UNIT_VALUE,
        expected_method: Some(method),
        saved_phase: state.curr_cid_phase,
        saved_effect_head: state.effect_head,
        saved_effect_depth: state.effect_local_depth,
      }
      state' = {
        curr: target,
        effect_local_depth: 0,
        instance_stack: state.instance_stack.append(target),
        call_stack: state.call_stack.append(frame),
        tx_phase: Running,
        curr_cid_phase: MethodEnterPending,

        ...state
      }
    }
  }


  action receive_method_result(result: Output[StarstreamValue]): bool = {
    val frame = state.call_stack.last()
    all {
      action_allowed(ReceiveMethodResult),
      result.value.is_valid(),
      frame.caller == state.curr,
      frame.expected_result == result.value,
      state' = {
        instance_stack: state.instance_stack.pop_last(),
        call_stack: state.call_stack.pop_last(),
        curr: frame.caller,
        curr_cid_phase: frame.saved_phase,
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
    state.call_stack.last().kind != ConstructorCall,
    state.call_stack.last().expected_arguments == arguments,
    state.call_stack.last().expected_method == Some(method),

    state' = {
      curr_cid_phase: Executing,
      tx_phase: Running,

      ...state
    }
  }

  // These three actions correspond to the explicit builtin host calls.
  action push_effect_handler_frame(frame_id: int): bool = all {
    action_allowed(PushEffectHandlerFrame),
    0 <= frame_id and frame_id < 4294967296,
    state' = {
      effect_head: Some(state.effect_arena.length()),
      effect_arena: state.effect_arena.append({
        parent: state.effect_head,
        owner: state.curr,
        frame_id: frame_id,
        effects: Set(),
      }),
      effect_local_depth: state.effect_local_depth + 1,
      tx_phase: Running,
      ...state
    },
  }

  // Only scopes pushed by this invocation may be mutated, so a nonzero local
  // depth is required even when an inherited head has the same owner.
  action add_effect_handler(effect: MethodHash): bool = all {
    action_allowed(AddEffectHandler),
    state.effect_local_depth > 0,
    match state.effect_head {
      | Some(id) => {
          val scope = state.effect_arena[id]
          state' = {
            effect_arena: state.effect_arena.replaceAt(
              id,
              {
                effects: scope.effects.insert(effect),
                ...scope
              }
            ),
            tx_phase: Running,
            ...state
          }
        }
      | None => all { false, state' = state }
    },
  }

  action pop_effect_handler_frame: bool = all {
    action_allowed(PopEffectHandlerFrame),
    state.effect_local_depth > 0,
    match state.effect_head {
      | Some(id) => state' = {
          effect_head: state.effect_arena[id].parent,
          effect_local_depth: state.effect_local_depth - 1,
          tx_phase: Running,
          ...state
        }
      | None => all { false, state' = state }
    },
  }

  // A raise saves the visible head and local depth; search follows parent links.
  action raise_effect(effect: MethodHash, arguments: StarstreamValue): bool = all {
    action_allowed(RaiseEffect),
    arguments.is_valid(),
    state' = {
      call_stack: state.call_stack.append({
        kind: EffectCall,
        caller: state.curr,
        expected_arguments: arguments,
        expected_result: UNIT_VALUE,
        expected_method: Some(effect),
        saved_phase: state.curr_cid_phase,
        saved_effect_head: state.effect_head,
        saved_effect_depth: state.effect_local_depth,
      }),
      curr_cid_phase: EffectHandlerPending,
      tx_phase: Running,
      ...state
    }
  }

  // Internal host search step, not a guest export. A candidate with no matching
  // registration is skipped without calling its instance. A matching candidate
  // must be entered; a false handler result can then continue the search.
  action skip_effect_handler: bool = {
    val frame = state.call_stack.last()
    all {
      action_allowed(SkipEffectHandler),
      frame.kind == EffectCall,
      match frame.expected_method {
        | Some(effect) => match state.effect_head {
            | Some(id) => all {
                not(state.effect_arena[id].effects.contains(effect)),
                state' = { effect_head: state.effect_arena[id].parent, ...state },
              }
            | None => all { false, state' = state }
          }
        | None => all { false, state' = state }
      },
    }
  }


  action enter_effect_handler(frame_id: int, effect: MethodHash, arguments: StarstreamValue): bool = {
    val frame = state.call_stack.last()
    all {
      action_allowed(EnterEffectHandler),
      arguments.is_valid(),
      frame.kind == EffectCall,
      frame.expected_method == Some(effect),
      frame.expected_arguments == arguments,
      match state.effect_head {
        | Some(id) => {
            val scope = state.effect_arena[id]
            all {
              scope.effects.contains(effect),
              scope.frame_id == frame_id,
              state' = {
                curr: scope.owner,
                // Excludes both the selected try scope (including sibling with
                // clauses) and every inner scope. A nested try branches from
                // the selected scope's parent.
                effect_head: scope.parent,
                effect_local_depth: 0,
                curr_cid_phase: EffectHandlerExecuting,
                ...state
              }
            }
          }
        | None => all { false, state' = state }
      },
    }
  }

  action exit_effect_handler(handled: bool, result: Output[StarstreamValue]): bool = {
    val frame = state.call_stack.last()
    all {
      action_allowed(ExitEffectHandler),
      result.value.is_valid(),
      frame.kind == EffectCall,
      // The handler popped every scope it pushed, so the head is back at the
      // selected scope's parent: the next search candidate on decline.
      state.effect_local_depth == 0,
      state' = {
        call_stack: state.call_stack.replaceAt(state.call_stack.length() - 1,
          { expected_result: result.value, ...frame }),
        // A decline continues from the current head; acceptance restores the
        // raiser's chain. Pending phases prevent guest code running in between.
        effect_head: if (handled) frame.saved_effect_head else state.effect_head,
        effect_local_depth: frame.saved_effect_depth,
        curr: frame.caller,
        curr_cid_phase: if (handled) EffectResumePending else EffectHandlerPending,
        ...state
      }
    }
  }

  action resume_effect(result: Output[StarstreamValue]): bool = {
    val frame = state.call_stack.last()
    all {
      action_allowed(ResumeEffect),
      result.value.is_valid(),
      frame.kind == EffectCall,
      frame.expected_result == result.value,
      state' = {
        call_stack: state.call_stack.pop_last(),
        curr: frame.caller,
        curr_cid_phase: frame.saved_phase,
        ...state
      }
    }
  }

  action execution_complete: bool = all {
    state.tx_phase == Running,
    state.curr.is_coord(),
    state.call_stack.length() == 0,
    state' = state,
  }

  // Derived views are for assertions/exploration only, not stored snapshots.
  // Scope IDs on the chain ending at start, outermost first.
  pure def scope_ids(arena: List[EffectScope], start: Option[int]): List[int] =
    arena.foldl({ cursor: start, ids: List() }, (acc, ignored) =>
      match acc.cursor {
        | Some(id) => { cursor: arena[id].parent, ids: List(id).concat(acc.ids) }
        | None => acc
      }).ids

  // The node reached by following `steps` parent links from start.
  pure def scope_ancestor(arena: List[EffectScope], start: Option[int], steps: int): Option[int] =
    range(0, steps).foldl(start, (cursor, ignored) =>
      match cursor {
        | Some(id) => arena[id].parent
        | None => None
      })

  pure def scope_id_in_arena(arena: List[EffectScope], id: Option[int]): bool =
    match id {
      | Some(i) => 0 <= i and i < arena.length()
      | None => true
    }

  val effect_stack = scope_ids(state.effect_arena, state.effect_head)
    .foldl(List(), (scopes, id) => scopes.append(state.effect_arena[id]))

  // Outside a pending search, effect_head is the current invocation's chain
  // and effect_local_depth counts its own nodes at the top of that chain.
  def effect_head_is_visible_chain: bool =
    state.curr_cid_phase != EffectHandlerPending

  val effect_arena_is_well_formed = all {
    state.effect_arena.indices().forall(i =>
      match state.effect_arena[i].parent {
        | Some(p) => 0 <= p and p < i
        | None => true
      }),
    scope_id_in_arena(state.effect_arena, state.effect_head),
    state.effect_local_depth >= 0,
    state.call_stack.indices().forall(i => all {
      scope_id_in_arena(state.effect_arena, state.call_stack[i].saved_effect_head),
      state.call_stack[i].saved_effect_depth >= 0,
    }),
  }

  // Popping is only guarded by the counter, so it must never exceed the chain.
  val effect_local_depth_fits_chain =
    effect_head_is_visible_chain implies
      state.effect_local_depth <= scope_ids(state.effect_arena, state.effect_head).length()

  val mutable_effect_scopes_belong_to_current =
    effect_head_is_visible_chain implies {
      val ids = scope_ids(state.effect_arena, state.effect_head)
      ids.indices().forall(i =>
        i >= ids.length() - state.effect_local_depth implies
          state.effect_arena[ids[i]].owner == state.curr)
    }

  // Stripping the local scopes of an executing program invocation recovers the
  // head it was entered with, which is also the head its caller resumes with.
  val program_scopes_rest_on_entry_head = {
    val executing = state.curr_cid_phase == Executing or
      state.curr_cid_phase == ConstructorExecuting or
      state.curr_cid_phase == Registering
    (executing and state.call_stack.length() > 0) implies {
      val frame = state.call_stack.last()
      frame.kind != EffectCall implies
        scope_ancestor(state.effect_arena, state.effect_head, state.effect_local_depth) ==
          frame.saved_effect_head
    }
  }

  // Only the top pending effect uses the head as a cursor. Suspended effect
  // calls retain their restoration heads, not independent search cursors.
  val effect_search_is_saved_ancestor =
    if (state.curr_cid_phase != EffectHandlerPending) true
    else if (state.call_stack.length() == 0) false
    else {
      val frame = state.call_stack.last()
      all {
        frame.kind == EffectCall,
        match state.effect_head {
          | Some(id) => scope_ids(state.effect_arena, frame.saved_effect_head).to_set().contains(id)
          | None => true
        },
      }
    }

  // Recover the entry boundary for diagnostics only, by stripping local nodes.
  // No selected-node or entry-head field is needed by the execution actions.
  val executing_handler_has_selected_owner =
    if (state.curr_cid_phase != EffectHandlerExecuting) true
    else if (state.call_stack.length() == 0) false
    else {
      val frame = state.call_stack.last()
      val entry_head = scope_ancestor(state.effect_arena, state.effect_head, state.effect_local_depth)
      all {
        frame.kind == EffectCall,
        scope_ids(state.effect_arena, frame.saved_effect_head).to_set().exists(id => all {
          state.effect_arena[id].parent == entry_head,
          state.effect_arena[id].owner == state.curr,
        }),
      }
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
