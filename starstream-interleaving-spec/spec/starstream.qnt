module starstream {
  // a spec that is meant to be as close to the circuit as possible
  //
  // every action in this module should correspond roughly with one host call
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

  type StarstreamValue = List[int]
  type ResourceHandle = int
  type MethodHash = str
  type CoroutineId = Coord(int) | Utxo(int)

  type ResourceKey = {
    holder: CoroutineId,
    handle: ResourceHandle,
  }

  type CallFrame = {
    caller: CoroutineId,
    expected_arguments: List[int],
    expected_result: List[int],
    expected_method: Option[MethodHash],
  }

  type CurrPhase =
      CtorEnterPending
    | MethodEnterPending
    | Executing
    | Yield

  type UtxoLifecycle = Live | Consumed

  // just a tagged type so that the action signature reflects what's input and
  // output (in the corresponding wasm function)
  type Output[t] = { value: t }

  type State = {
    curr: CoroutineId,
    call_stack: List[CallFrame],
    curr_cid_phase: CurrPhase,
    utxo_lifecycle: CoroutineId -> UtxoLifecycle,
    next_utxo_id: int,
    enabled_methods: CoroutineId -> Set[MethodHash],
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

  def action_allowed(op: Opcode): bool =
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

  pure def is_utxo(cid: CoroutineId): bool =
    match cid {
      | Utxo(_) => true
      | Coord(_) => false
    }

  pure def is_coord(cid: CoroutineId): bool =
    not(is_utxo(cid))

  pure def is_valid(value: StarstreamValue): bool =
    // because of Goldilocks <64bit restriction, values needing more than 32bits
    // are split in 32bit limbs
    //
    // this is more of a circuit detail than a behaviour one, but better to
    // enforce it here for the trace generation
    value.foldl(true, (valid, limb) => valid and 0 <= limb and limb < 2^32)

  // the init action is in sim.qnt, or in the generated test modules
  //
  // that makes it easier to add ghost variables to the state later for
  // verification/tracing purposes
  pure def new_tx(): State = {
    val cid = Coord(1)

    {
      curr: cid,
      call_stack: List({
        caller: Coord(0),
        expected_arguments: List(),
        expected_result: List(),
        expected_method: None,
      }),
      curr_cid_phase: Executing,
      utxo_lifecycle: Map(),
      next_utxo_id: 0,
      enabled_methods: Map(),
      resource_resolver: Map(),
      pending_ctor_key: None,
    }
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
      caller.is_coord(),
      state.call_stack.length() > 0,
      state' = {
        curr: target,
        call_stack: state.call_stack.append(
          {
            caller: caller,
            expected_arguments: arguments,
            expected_result: List(),
            expected_method: None,
          }
        ),
        curr_cid_phase: CtorEnterPending,
        utxo_lifecycle: state.utxo_lifecycle.put(target, Live),
        next_utxo_id: state.next_utxo_id + 1,
        enabled_methods: state.enabled_methods.put(target, Set()),
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
        curr_cid_phase: Executing,
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
      curr_cid_phase: Yield,
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
    // this forbids registering the same method twice
    //
    // it may or may not be too restrictive, review later with the runtime/lang
    // integration
    not(state.enabled_methods.get(state.curr).contains(method)),

    state' = {
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
    state.call_stack.length() > 0,
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
      // utxos can't call utxos
      state.curr.is_coord(),
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

    // the utxo "calls" this
    state.curr.is_utxo(),
    state.call_stack.length() > 0,
    state.call_stack.last().expected_arguments == arguments,
    state.call_stack.last().expected_method == Some(method),

    state' = {
      curr_cid_phase: Executing,

      ...state
    }
  }

  action execution_complete: bool = all {
    state.curr.is_coord(),
    state.call_stack.length() == 0,
    state' = state,
  }

  val coord_stack =
    state.curr.is_utxo() implies state.call_stack.length() > 1

  val consumed_utxos_have_no_methods =
    state.utxo_lifecycle.keys().forall(u =>
      state.utxo_lifecycle.get(u) == Consumed
        implies state.enabled_methods.get(u).is_empty_set()
    )
}
