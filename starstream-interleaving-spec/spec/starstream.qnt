module starstream_execution {
  type Option[a] = Some(a) | None

  type CoroutineId = Coord(int) | Utxo(int)

  type ResourceKey = {
    holder: CoroutineId,
    handle: int,
  }

  type PendingNewUtxo = {
    target: CoroutineId,
    key: ResourceKey,
  }

  type Phase =
      CoordRunning
    | CoordWaiting
    | Constructing
    | Yielded
    | HandlerRunning
    | PreparingYield
    | Returning
    | Completed

  var state: {
    curr: CoroutineId,
    call_stack: List[CoroutineId],
    phases: CoroutineId -> Phase,
    next_utxo_id: int,
    enabled_methods: CoroutineId -> Set[str],
    resource_targets: ResourceKey -> CoroutineId,
    pending_new_utxo: Option[PendingNewUtxo],
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
      phases: Map(cid -> CoordRunning),
      next_utxo_id: 0,
      enabled_methods: Map(),
      resource_targets: Map(),
      pending_new_utxo: None,
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
      state.phases.get(caller) == CoordRunning,
      arguments.is_starstream_value(),
      0 <= resource,
      resource < 4294967296,
      state.pending_new_utxo == None,
      not(state.resource_targets.keys().contains(key)),
      state' = {
        curr: target,
        call_stack: state.call_stack.append(caller),
        phases: state.phases
          .set(caller, CoordWaiting)
          .put(target, Constructing),
        next_utxo_id: state.next_utxo_id + 1,
        enabled_methods: state.enabled_methods.put(target, Set()),
        pending_new_utxo: Some({ target: target, key: key }),
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
      state.phases.get(state.curr) == Constructing,
      state.phases.get(state.curr) == HandlerRunning,
    },
    state' = {
      phases: state.phases.set(state.curr, PreparingYield),
      enabled_methods: state.enabled_methods.set(state.curr, Set()),
      ...state,
    },
  }

  // `implements-method` calls after `abis-clear` construct the enabled-method
  // set for the new yield point.
  action advertise_method(method: str): bool = all {
    not(state.curr.is_coord()),
    state.phases.get(state.curr) == PreparingYield,
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
  action return_control: bool = {
    val caller =
      if (state.call_stack.length() > 0) state.call_stack.last()
      else state.curr
    val resource_targets =
      match state.pending_new_utxo {
        | Some(pending) =>
            state.resource_targets.put(pending.key, pending.target)
        | None => state.resource_targets
      }

    all {
      not(state.curr.is_coord()),
      or {
        state.phases.get(state.curr) == PreparingYield,
        state.phases.get(state.curr) == HandlerRunning,
        state.phases.get(state.curr) == Returning,
      },
      state.call_stack.length() > 0,
      state.phases.get(caller) == CoordWaiting,
      match state.pending_new_utxo {
        | Some(pending) => pending.target == state.curr
        | None => true
      },
      state' = {
        curr: caller,
        call_stack: state.call_stack.pop_last(),
        phases: state.phases
          .set(
            state.curr,
            // Completed is the model's logical retirement boundary. The
            // runtime may physically drop the Component Model resource after
            // this export has returned and its borrow scope has ended.
            if (state.phases.get(state.curr) == Returning) Completed
            else Yielded
          )
          .set(caller, CoordRunning),
        resource_targets: resource_targets,
        pending_new_utxo: None,
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
      state.phases.get(caller) == CoordRunning,
      arguments.is_starstream_value(),
      state.pending_new_utxo == None,
      state.resource_targets.keys().contains(key),
      state.phases.keys().contains(target),
      state.phases.get(target) == Yielded,
      state.enabled_methods.get(target).contains(method),
      state' = {
        curr: target,
        call_stack: state.call_stack.append(caller),
        phases: state.phases
          .set(caller, CoordWaiting)
          .set(target, HandlerRunning),
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
      state.phases.get(state.curr) == HandlerRunning,
      state.call_stack.length() > 0,
      state' = {
        phases: state.phases.set(state.curr, Returning),
        ...state,
      },
    }
  }

  action coord_return: bool = all {
    state.curr.is_coord(),
    state.phases.get(state.curr) == CoordRunning,
    state.call_stack.length() == 0,
    state.pending_new_utxo == None,
    state' = {
      phases: state.phases.set(state.curr, Completed),
      ...state,
    },
  }

  // A complete trace has returned from the entrypoint coordination script and
  // has no unfinished constructor or caller waiting for control.
  action execution_complete: bool = all {
    state.curr.is_coord(),
    state.phases.get(state.curr) == Completed,
    state.call_stack.length() == 0,
    state.pending_new_utxo == None,
    state' = state,
  }

  pure def owns_turn(phase: Phase): bool =
    match phase {
      | CoordRunning => true
      | Constructing => true
      | HandlerRunning => true
      | PreparingYield => true
      | Returning => true
      | CoordWaiting => false
      | Yielded => false
      | Completed => false
    }

  val turn_owners =
    state.phases.keys().filter(cid => state.phases.get(cid).owns_turn())

  // Before the coordination script returns, exactly `curr` owns the turn.
  // Afterwards no coroutine owns it.
  val current_coroutine_uniquely_owns_turn = {
    if (
      state.phases.keys().contains(state.curr)
        and state.phases.get(state.curr) == Completed
    )
      state.curr.is_coord() and turn_owners == Set()
    else
      turn_owners == Set(state.curr)
  }

  val waiting_coordinators_are_callers = {
    val waiting_coords = state.phases.keys().filter(cid =>
      cid.is_coord() and state.phases.get(cid) == CoordWaiting
    )
    val callers = state.call_stack.to_set()

    and {
      state.call_stack.length() == callers.size(),
      callers.forall(caller =>
        state.phases.keys().contains(caller) and caller.is_coord()
      ),
      waiting_coords == callers,
    }
  }

  val pending_constructor_is_well_formed =
    match state.pending_new_utxo {
      | Some(pending) => and {
          not(pending.target.is_coord()),
          state.phases.keys().contains(pending.target),
          or {
            state.phases.get(pending.target) == Constructing,
            state.phases.get(pending.target) == PreparingYield,
          },
          pending.key.holder.is_coord(),
          state.phases.keys().contains(pending.key.holder),
          state.phases.get(pending.key.holder) == CoordWaiting,
          state.call_stack.length() > 0,
          state.call_stack.last() == pending.key.holder,
          not(state.resource_targets.keys().contains(pending.key)),
      }
      | None => true
    }

  val enabled_methods_are_well_scoped =
    state.enabled_methods.keys().forall(cid =>
      not(cid.is_coord()) and state.phases.keys().contains(cid)
    )

  val resource_bindings_are_well_scoped =
    state.resource_targets.keys().forall(key =>
      key.holder.is_coord()
        and state.phases.keys().contains(key.holder)
        and not(state.resource_targets.get(key).is_coord())
        and state.phases.keys().contains(state.resource_targets.get(key))
    )

  val current_phase_is_well_formed =
    match state.phases.get(state.curr) {
      | CoordRunning => state.curr.is_coord()
      | Constructing => not(state.curr.is_coord())
      | HandlerRunning => not(state.curr.is_coord())
      | PreparingYield => not(state.curr.is_coord())
      | Returning =>
          not(state.curr.is_coord()) and state.call_stack.length() > 0
      | Completed => state.curr.is_coord() and state.call_stack.length() == 0
      | CoordWaiting => false
      | Yielded => false
    }

  val control_is_well_formed = and {
    state.next_utxo_id >= 0,
    state.phases.keys().contains(state.curr),
    current_coroutine_uniquely_owns_turn,
    waiting_coordinators_are_callers,
    pending_constructor_is_well_formed,
    enabled_methods_are_well_scoped,
    resource_bindings_are_well_scoped,
    current_phase_is_well_formed,
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
        and state.phases.get(state.resource_targets.get(key)) == Yielded
    )
    nondet key = callable_resources.oneOf()
    val target = state.resource_targets.get(key)
    nondet method = state.enabled_methods.get(target).oneOf()
    nondet arguments = SIM_VALUES.oneOf()
    call_method(key.handle, method, arguments)
  }

  // Choose one enabled semantic event at each simulated transition.
  action step: bool = any {
    simulate_new_utxo,
    abis_clear,
    simulate_advertise_method,
    return_control,
    simulate_call_method,
    coroutine_return,
    coord_return,
  }

  // Reachability witnesses make it visible when random exploration stops
  // exercising an important part of the protocol.
  val has_bound_resource = state.resource_targets.keys().size() > 0
  val has_entered_method = state.phases.keys().exists(cid =>
    state.phases.get(cid) == HandlerRunning
  )
  val has_yielded_utxo = state.phases.keys().exists(cid =>
    not(cid.is_coord()) and state.phases.get(cid) == Yielded
  )
  val has_returned_coroutine = state.phases.keys().exists(cid =>
    not(cid.is_coord()) and state.phases.get(cid) == Completed
  )
}
