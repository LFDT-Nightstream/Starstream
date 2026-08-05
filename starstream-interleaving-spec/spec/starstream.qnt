module starstream_execution {
  type CoroutineId = Coord(int) | Utxo(int)

  type ResourceKey = {
    holder: CoroutineId,
    handle: int,
  }

  type ConstructorCallFrame = {
    caller: CoroutineId,
    target: CoroutineId,
    key: ResourceKey,
  }

  type MethodCallFrame = {
    caller: CoroutineId,
    target: CoroutineId,
    expected_arguments: List[int],
    expected_result: List[int],
  }

  type CallFrame =
      ConstructorFrame(ConstructorCallFrame)
    | MethodFrame(MethodCallFrame)

  // Only a UTXO's lifecycle is persistent. The finer-grained execution state
  // belongs to the one coroutine identified by `curr`, so it is represented
  // once in `active_turn` instead of once per coroutine.
  type UtxoLifecycle = Live | Dead

  type ActiveTurn =
      CoordinatorTurn
    | ConstructorTurn
    | AwaitingMethodEntry
    | MethodTurn
    | PublishingAbi
    | ReturningTurn
    | ExecutionFinished

  var state: {
    curr: CoroutineId,
    call_stack: List[CallFrame],
    active_turn: ActiveTurn,
    utxo_lifecycle: CoroutineId -> UtxoLifecycle,
    next_utxo_id: int,
    enabled_methods: CoroutineId -> Set[str],
    resource_targets: ResourceKey -> CoroutineId,
  }

  pure def is_coord(cid: CoroutineId): bool =
    match cid {
      | Coord(_) => true
      | Utxo(_) => false
    }

  pure def is_starstream_value(value: List[int]): bool =
    value.foldl(true, (valid, limb) =>
      valid and 0 <= limb and limb < 4294967296
    )

  pure def caller(frame: CallFrame): CoroutineId =
    match frame {
      | ConstructorFrame(call) => call.caller
      | MethodFrame(call) => call.caller
    }

  pure def target(frame: CallFrame): CoroutineId =
    match frame {
      | ConstructorFrame(call) => call.target
      | MethodFrame(call) => call.target
    }

  pure def last(items: List[a]): a = items[items.length() - 1]

  pure def pop_last(items: List[a]): List[a] =
    items.slice(0, items.length() - 1)

  pure def to_set(items: List[a]): Set[a] =
    items.foldl(Set(), (result, elem) => result.union(Set(elem)))

  action init: bool = {
    val cid = Coord(0)
    state' = {
      curr: cid,
      call_stack: List(),
      active_turn: CoordinatorTurn,
      utxo_lifecycle: Map(),
      next_utxo_id: 0,
      enabled_methods: Map(),
      resource_targets: Map(),
    }
  }

  // Neo-Wasm observes a constructor import atomically, including both its
  // arguments and returned caller-local resource handle. Keep the handle
  // pending while the constructed UTXO executes its main continuation; its
  // initial ReturnControl makes the binding visible to the caller.
  action new_utxo(arguments: List[int], resource: int): bool = {
    val caller = state.curr
    val target = Utxo(state.next_utxo_id)
    val key = { holder: caller, handle: resource }

    all {
      caller.is_coord(),
      state.active_turn == CoordinatorTurn,
      arguments.is_starstream_value(),
      0 <= resource,
      resource < 4294967296,
      not(state.resource_targets.keys().contains(key)),
      state' = {
        curr: target,
        call_stack: state.call_stack.append(ConstructorFrame({
          caller: caller,
          target: target,
          key: key,
        })),
        active_turn: ConstructorTurn,
        next_utxo_id: state.next_utxo_id + 1,
        enabled_methods: state.enabled_methods.put(target, Set()),
        ...state,
      },
    }
  }

  // The intended protocol makes `abis-clear` explicit at every yield,
  // including yields with no advertised methods. During construction it
  // starts the initial enabled-method set; from a method handler it proves
  // that execution reached a new yield point rather than returning normally.
  action abis_clear: bool = all {
    not(state.curr.is_coord()),
    or {
      state.active_turn == ConstructorTurn,
      state.active_turn == MethodTurn,
    },
    state' = {
      active_turn: PublishingAbi,
      enabled_methods: state.enabled_methods.set(state.curr, Set()),
      ...state,
    },
  }

  // `implements-method` calls after `abis-clear` construct the enabled-method
  // set for the new yield point.
  action advertise_method(method: str): bool = all {
    not(state.curr.is_coord()),
    state.active_turn == PublishingAbi,
    not(state.enabled_methods.get(state.curr).contains(method)),
    state' = {
      enabled_methods: state.enabled_methods.set(
        state.curr,
        state.enabled_methods.get(state.curr).union(Set(method))
      ),
      ...state,
    },
  }

  // Every UTXO export exit returns control to its caller. If `abis-clear`
  // occurred during the turn, this closes a fresh yield point; otherwise a
  // method handler returned while preserving the previous enabled-method set.
  // An explicit coroutine return also waits for this boundary to return. The
  // exit does not repeat the caller-local constructor handle: an atomic
  // NewUtxo already committed it, so an initial-yield return installs the
  // pending binding here.
  action return_control(result: List[int]): bool = {
    val frame =
      if (state.call_stack.length() > 0)
        state.call_stack.last()
      else
        MethodFrame({
          caller: state.curr,
          target: state.curr,
          expected_arguments: List(),
          expected_result: List(),
        })
    val resource_targets =
      match frame {
        | ConstructorFrame(call) =>
            state.resource_targets.put(call.key, call.target)
        | MethodFrame(_) => state.resource_targets
      }
    val utxo_lifecycle =
      if (state.active_turn == ReturningTurn)
        state.utxo_lifecycle.set(state.curr, Dead)
      else
        match frame {
          | ConstructorFrame(_) => state.utxo_lifecycle.put(state.curr, Live)
          | MethodFrame(_) => state.utxo_lifecycle
        }

    all {
      not(state.curr.is_coord()),
      result.is_starstream_value(),
      state.call_stack.length() > 0,
      frame.target() == state.curr,
      frame.caller().is_coord(),
      match frame {
        | ConstructorFrame(_) => and {
            state.active_turn == PublishingAbi,
            result == List(),
          }
        | MethodFrame(call) => and {
            or {
              state.active_turn == PublishingAbi,
              state.active_turn == MethodTurn,
              state.active_turn == ReturningTurn,
            },
            result == call.expected_result,
          }
      },
      state' = {
        curr: frame.caller(),
        call_stack: state.call_stack.pop_last(),
        active_turn: CoordinatorTurn,
        utxo_lifecycle: utxo_lifecycle,
        resource_targets: resource_targets,
        ...state,
      },
    }
  }

  // Calling an exported method enters its handler. It does not consume the ABI:
  // a handler may return normally while the UTXO stays at the same yield point.
  action call_method(
    resource: int,
    method: str,
    arguments: List[int],
    result: List[int],
  ): bool = {
    val caller = state.curr
    val key = { holder: caller, handle: resource }
    val target =
      if (state.resource_targets.keys().contains(key))
        state.resource_targets.get(key)
      else
        caller

    all {
      caller.is_coord(),
      state.active_turn == CoordinatorTurn,
      arguments.is_starstream_value(),
      result.is_starstream_value(),
      state.resource_targets.keys().contains(key),
      state.utxo_lifecycle.keys().contains(target),
      state.utxo_lifecycle.get(target) == Live,
      state.enabled_methods.get(target).contains(method),
      state' = {
        curr: target,
        call_stack: state.call_stack.append(MethodFrame({
          caller: caller,
          target: target,
          expected_arguments: arguments,
          expected_result: result,
        })),
        active_turn: AwaitingMethodEntry,
        ...state,
      },
    }
  }

  // The receiver-side export boundary publishes the flattened user
  // arguments before the first method instruction. The callee's internal
  // resource representation is bootstrap advice and is intentionally absent.
  action enter_method(arguments: List[int]): bool = {
    val frame =
      if (state.call_stack.length() > 0)
        state.call_stack.last()
      else
        ConstructorFrame({
          caller: state.curr,
          target: state.curr,
          key: { holder: state.curr, handle: 0 },
        })

    all {
      not(state.curr.is_coord()),
      state.active_turn == AwaitingMethodEntry,
      arguments.is_starstream_value(),
      state.call_stack.length() > 0,
      match frame {
        | ConstructorFrame(_) => false
        | MethodFrame(call) => and {
            call.target == state.curr,
            arguments == call.expected_arguments,
          }
      },
      state' = {
        active_turn: MethodTurn,
        ...state,
      },
    }
  }

  // Provisional semantics: after its required initial yield, coroutine return
  // is an explicit terminal host call (`return` or `burn`), not an inference
  // from a particular return PC. It marks the UTXO here; the export boundary
  // performs the control transfer. Constructors cannot terminate before their
  // initial yield, because their component ABI must return a live resource.
  action coroutine_return: bool = {
    all {
      not(state.curr.is_coord()),
      state.active_turn == MethodTurn,
      state.call_stack.length() > 0,
      state' = {
        active_turn: ReturningTurn,
        ...state,
      },
    }
  }

  action coord_return: bool = all {
    state.curr.is_coord(),
    state.active_turn == CoordinatorTurn,
    state.call_stack.length() == 0,
    state' = {
      active_turn: ExecutionFinished,
      ...state,
    },
  }

  // A complete trace has returned from the entrypoint coordination script and
  // has no unfinished constructor or caller waiting for control.
  action execution_complete: bool = all {
    state.curr.is_coord(),
    state.active_turn == ExecutionFinished,
    state.call_stack.length() == 0,
    state' = state,
  }

  val next_utxo_id_is_nonnegative = state.next_utxo_id >= 0

  val current_coroutine_is_known = or {
    state.curr.is_coord(),
    state.utxo_lifecycle.keys().contains(state.curr),
    state.call_stack.length() > 0
      and state.call_stack.last().target() == state.curr,
  }

  // The stack is the canonical representation of waiting coordinators. There
  // is no duplicate per-coordinator waiting state to keep synchronized.
  val call_stack_is_well_formed = {
    val callers = state.call_stack.foldl(Set(), (result, frame) =>
      result.union(Set(frame.caller()))
    )

    and {
      state.call_stack.length() == callers.size(),
      callers.forall(caller => caller.is_coord()),
      not(callers.contains(state.curr)),
      state.call_stack.to_set().forall(frame => and {
        frame.caller().is_coord(),
        not(frame.target().is_coord()),
      }),
    }
  }

  val call_frames_are_well_formed =
    state.call_stack.to_set().forall(frame =>
      match frame {
        | ConstructorFrame(call) => and {
            call.key.holder == call.caller,
            not(state.utxo_lifecycle.keys().contains(call.target)),
            not(state.resource_targets.keys().contains(call.key)),
          }
        | MethodFrame(call) => and {
            call.expected_arguments.is_starstream_value(),
            call.expected_result.is_starstream_value(),
            state.utxo_lifecycle.keys().contains(call.target),
            state.utxo_lifecycle.get(call.target) == Live,
          }
      }
    )

  val utxo_lifecycle_is_well_scoped =
    state.utxo_lifecycle.keys().forall(cid =>
      match cid {
        | Coord(_) => false
        | Utxo(id) => 0 <= id and id < state.next_utxo_id
      }
    )

  val enabled_methods_are_well_scoped =
    state.enabled_methods.keys().forall(cid =>
      not(cid.is_coord()) and or {
        state.utxo_lifecycle.keys().contains(cid),
        state.call_stack.to_set().exists(frame =>
          match frame {
            | ConstructorFrame(call) => call.target == cid
            | MethodFrame(_) => false
          }
        ),
      }
    )

  val resource_bindings_are_well_scoped =
    state.resource_targets.keys().forall(key =>
      key.holder.is_coord()
        and not(state.resource_targets.get(key).is_coord())
        and state.utxo_lifecycle.keys().contains(state.resource_targets.get(key))
    )

  val active_turn_is_well_formed =
    match state.active_turn {
      | CoordinatorTurn => and {
          state.curr.is_coord(),
        }
      | ConstructorTurn => and {
          not(state.curr.is_coord()),
          state.call_stack.length() > 0,
          match state.call_stack.last() {
            | ConstructorFrame(call) => call.target == state.curr
            | MethodFrame(_) => false
          },
        }
      | MethodTurn => and {
          not(state.curr.is_coord()),
          state.call_stack.length() > 0,
          match state.call_stack.last() {
            | ConstructorFrame(_) => false
            | MethodFrame(call) => call.target == state.curr
          },
          state.utxo_lifecycle.keys().contains(state.curr),
          state.utxo_lifecycle.get(state.curr) == Live,
        }
      | AwaitingMethodEntry => and {
          not(state.curr.is_coord()),
          state.call_stack.length() > 0,
          match state.call_stack.last() {
            | ConstructorFrame(_) => false
            | MethodFrame(call) => call.target == state.curr
          },
          state.utxo_lifecycle.keys().contains(state.curr),
          state.utxo_lifecycle.get(state.curr) == Live,
        }
      | PublishingAbi => and {
          not(state.curr.is_coord()),
          state.call_stack.length() > 0,
          state.call_stack.last().target() == state.curr,
          match state.call_stack.last() {
            | ConstructorFrame(call) =>
                not(state.utxo_lifecycle.keys().contains(call.target))
            | MethodFrame(call) => and {
                state.utxo_lifecycle.keys().contains(call.target),
                state.utxo_lifecycle.get(call.target) == Live,
              }
          },
        }
      | ReturningTurn => and {
          not(state.curr.is_coord()),
          state.call_stack.length() > 0,
          match state.call_stack.last() {
            | ConstructorFrame(_) => false
            | MethodFrame(call) => call.target == state.curr
          },
          state.utxo_lifecycle.keys().contains(state.curr),
          state.utxo_lifecycle.get(state.curr) == Live,
        }
      | ExecutionFinished => and {
          state.curr.is_coord(),
          state.call_stack.length() == 0,
        }
    }

  val control_is_well_formed = and {
    next_utxo_id_is_nonnegative,
    current_coroutine_is_known,
    call_stack_is_well_formed,
    call_frames_are_well_formed,
    utxo_lifecycle_is_well_scoped,
    enabled_methods_are_well_scoped,
    resource_bindings_are_well_scoped,
    active_turn_is_well_formed,
  }
}

// Small sampled domains for random exploration. This module only chooses
// concrete arguments for the semantic actions above; it does not change their
// guards or transition semantics.
module starstream_sim {
  import starstream_execution.*

  pure val SIM_METHODS = Set("m0", "m1", "m2")
  pure val SIM_VALUES: Set[List[int]] =
    Set(List(), List(0), List(1), List(1, 2))
  pure val SIM_RESOURCE_HANDLES = Set(0, 1, 2, 3)
  pure val SIM_MAX_UTXOS = 4

  action simulate_new_utxo: bool = all {
    state.next_utxo_id < SIM_MAX_UTXOS,
    nondet arguments = SIM_VALUES.oneOf()
    nondet resource = SIM_RESOURCE_HANDLES.oneOf()
    new_utxo(arguments, resource),
  }

  action simulate_advertise_method: bool = {
    nondet method = SIM_METHODS.oneOf()
    advertise_method(method)
  }

  action simulate_call_method: bool = {
    val callable_resources = state.resource_targets.keys().filter(key =>
      key.holder == state.curr
        and state.utxo_lifecycle.get(state.resource_targets.get(key)) == Live
    )
    nondet key = callable_resources.oneOf()
    val target = state.resource_targets.get(key)
    nondet method = state.enabled_methods.get(target).oneOf()
    nondet arguments = SIM_VALUES.oneOf()
    nondet result = SIM_VALUES.oneOf()
    call_method(key.handle, method, arguments, result)
  }

  action simulate_return_control: bool = {
    val expected_result =
      if (state.call_stack.length() == 0)
        List()
      else
        match state.call_stack.last() {
          | ConstructorFrame(_) => List()
          | MethodFrame(call) => call.expected_result
        }
    nondet result = Set(expected_result).oneOf()
    return_control(result)
  }

  action simulate_enter_method: bool = {
    val expected_arguments =
      if (state.call_stack.length() == 0)
        List()
      else
        match state.call_stack.last() {
          | ConstructorFrame(_) => List()
          | MethodFrame(call) => call.expected_arguments
        }
    nondet arguments = Set(expected_arguments).oneOf()
    enter_method(arguments)
  }

  // Choose one enabled semantic event at each simulated transition.
  action step: bool = any {
    simulate_new_utxo,
    abis_clear,
    simulate_advertise_method,
    simulate_return_control,
    simulate_call_method,
    simulate_enter_method,
    coroutine_return,
    coord_return,
  }

  // Reachability witnesses make it visible when random exploration stops
  // exercising an important part of the protocol.
  val has_bound_resource = state.resource_targets.keys().size() > 0
  val has_entered_method = state.active_turn == MethodTurn
  val has_yielded_utxo = state.utxo_lifecycle.keys().exists(cid =>
    state.utxo_lifecycle.get(cid) == Live
  )
  val has_returned_coroutine = state.utxo_lifecycle.keys().exists(cid =>
    state.utxo_lifecycle.get(cid) == Dead
  )
}
