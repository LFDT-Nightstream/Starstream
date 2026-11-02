module effect_scopes_test {
  import starstream.* from "./starstream"
  import utils.* from "./utils"

  action init = { state' = new_tx }
  action check(condition: bool): bool = all { assert(condition), state' = state }

  run root_e = init
    .then(push_effect_handler_frame(1))
    .then(add_effect_handler("E"))

  run two_e = root_e
    .then(push_effect_handler_frame(2))
    .then(add_effect_handler("E"))

  run child_with_f = root_e
    .then(new_utxo(UNIT_VALUE))
    .then(enter_constructor(UNIT_VALUE))
    .then(push_effect_handler_frame(2))
    .then(add_effect_handler("F"))

  run handling_child_e = child_with_f
    .then(raise_effect("E", UNIT_VALUE))
    .then(skip_effect_handler)
    .then(enter_effect_handler(1, "E", UNIT_VALUE))

  run test_basic_handler_and_scope_cleanup = root_e
    .then(raise_effect("E", List(1, 2, 3, 4)))
    .then(enter_effect_handler(1, "E", List(1, 2, 3, 4)))
    .then(check(effect_stack == List()))
    .then(exit_effect_handler(true, { value: List(5, 6, 7, 8) }))
    .then(resume_effect({ value: List(5, 6, 7, 8) }))
    .then(check(effect_stack.length() == 1))
    .then(pop_effect_handler_frame)
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run test_wrong_arguments_rejected = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", List(1, 0, 0, 0)).fail())

  run test_wrong_effect_rejected = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "F", UNIT_VALUE).fail())

  run test_wrong_result_rejected = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: List(1, 0, 0, 0) }).fail())

  run test_matching_scope_cannot_be_skipped = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(skip_effect_handler.fail())

  run test_innermost_scope_must_be_tried_first = two_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE).fail())

  run test_skip_requires_pending_phase = root_e
    .then(skip_effect_handler.fail())

  run test_empty_stack_cannot_dispatch = init
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE).fail())

  run test_exhausted_search_cannot_skip_again = root_e
    .then(raise_effect("F", UNIT_VALUE))
    .then(skip_effect_handler)
    .then(skip_effect_handler.fail())

  run test_declined_handler_falls_back_to_outer_scope = two_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(2, "E", UNIT_VALUE))
    .then(exit_effect_handler(false, { value: UNIT_VALUE }))
    .then(check(effect_stack.length() == 1 and state.effect_head == Some(0)))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(effect_stack.length() == 2))

  run test_last_handler_declining_does_not_allow_resume = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(exit_effect_handler(false, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }).fail())

  // Recursive Hello example: the handler can re-raise the same effect, which
  // reaches the outer try in the same instance, not itself. Frame ids name
  // compiled handler code, not dynamic scopes, so reentry repeats them: only
  // the head walk tells the two scopes apart.
  run test_recursion_walks_outer_scopes_in_same_instance = root_e
    .then(push_effect_handler_frame(1))
    .then(add_effect_handler("E"))
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(check(effect_stack.length() == 1))
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(check(effect_stack.length() == 0))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.curr_cid_phase == EffectHandlerExecuting))
    .then(check(effect_stack.length() == 1))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.curr_cid_phase == Executing and effect_stack.length() == 2))

  run test_sibling_clause_is_not_visible_inside_handler = root_e
    .then(add_effect_handler("F"))
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(raise_effect("F", UNIT_VALUE))
    .then(enter_effect_handler(1, "F", UNIT_VALUE).fail())

  run test_suspended_child_cannot_handle_parents_nested_effect = handling_child_e
    .then(raise_effect("F", UNIT_VALUE))
    .then(enter_effect_handler(2, "F", UNIT_VALUE).fail())

  // A scope belongs to the instance running the `try`, here the handler's
  // (curr), not the suspended raiser. The runtime gets this by pushing handler
  // calls onto its instance_stack; this model leaves instance_stack to program
  // invocations and uses curr.
  run test_nested_try_uses_current_handler_owner_and_restores_child_scopes = handling_child_e
    .then(check(state.curr == Coord(1) and state.instance_stack.last() == Utxo(0)))
    .then(push_effect_handler_frame(3))
    .then(add_effect_handler("F"))
    .then(check(effect_stack.last().owner == Coord(1)))
    .then(raise_effect("F", UNIT_VALUE))
    .then(enter_effect_handler(3, "F", UNIT_VALUE))
    .then(check(state.curr == Coord(1)))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(pop_effect_handler_frame)
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.curr == Utxo(0) and effect_stack.length() == 2))
    .then(raise_effect("F", UNIT_VALUE))
    .then(enter_effect_handler(2, "F", UNIT_VALUE))
    .then(check(state.curr == Utxo(0)))

  run test_program_call_from_handler_sees_active_scope_not_suspended_child = handling_child_e
    .then(push_effect_handler_frame(3))
    .then(add_effect_handler("F"))
    .then(new_utxo(UNIT_VALUE))
    .then(enter_constructor(UNIT_VALUE))
    .then(raise_effect("F", UNIT_VALUE))
    .then(enter_effect_handler(3, "F", UNIT_VALUE))
    .then(check(state.curr == Coord(1)))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.curr == Utxo(1)))
    .then(return({ value: UNIT_VALUE }))
    .then(receive_new_utxo_handle({ value: 1 }))
    .then(check(state.curr == Coord(1) and state.curr_cid_phase == EffectHandlerExecuting))
    .then(pop_effect_handler_frame)
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.curr == Utxo(0)))

  run test_frame_id_must_fit_u32 = init.then(push_effect_handler_frame(-1).fail())
  run test_frame_id_cannot_overflow_u32 = init.then(push_effect_handler_frame(4294967296).fail())

  run test_scope_mutation_cannot_interleave_with_pending_dispatch = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(pop_effect_handler_frame.fail())

  run test_return_rejects_unpopped_scope = init
    .then(new_utxo(UNIT_VALUE))
    .then(enter_constructor(UNIT_VALUE))
    .then(push_effect_handler_frame(2))
    .then(return({ value: UNIT_VALUE }).fail())

  // Handler invocations must balance their own scopes too.
  run test_handler_exit_rejects_unpopped_local_scope = root_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(push_effect_handler_frame(2))
    .then(add_effect_handler("F"))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }).fail())

  run test_nested_tries_count_toward_invocation_depth = root_e
    .then(add_effect_handler("E"))
    .then(check(effect_stack.last().effects == Set("E")))
    .then(push_effect_handler_frame(2))
    .then(check(state.effect_local_depth == 2 and state.effect_head == Some(1)))
    .then(pop_effect_handler_frame)
    .then(check(state.effect_head == Some(0) and state.effect_local_depth == 1))
    .then(pop_effect_handler_frame)
    .then(pop_effect_handler_frame.fail())

  run test_popped_scopes_leave_the_chain_and_keep_their_ids = root_e
    .then(pop_effect_handler_frame)
    .then(push_effect_handler_frame(2))
    .then(check(state.effect_head == Some(1) and state.effect_arena[1].parent == None))
    .then(check(state.effect_arena[0].effects == Set("E")))
    .then(raise_effect("E", UNIT_VALUE))
    .then(skip_effect_handler)
    .then(enter_effect_handler(1, "E", UNIT_VALUE).fail())

  run same_owner_handler = two_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(2, "E", UNIT_VALUE))
    .then(check(state.effect_local_depth == 0 and state.effect_head == Some(0)))

  run test_same_owner_handler_cannot_pop_inherited_scope = same_owner_handler
    .then(pop_effect_handler_frame.fail())

  run test_handler_branch_preserves_suspended_nodes_and_restores_depth = same_owner_handler
    .then(push_effect_handler_frame(3))
    .then(add_effect_handler("F"))
    .then(check(state.effect_head == Some(2) and state.effect_arena[2].parent == Some(0)))
    .then(check(state.effect_arena[1].effects == Set("E")))
    .then(pop_effect_handler_frame)
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.effect_head == Some(1) and state.effect_local_depth == 2))
    .then(add_effect_handler("G"))
    .then(check(state.effect_arena[1].effects == Set("E", "G")))

  run test_declined_handler_next_invocation_gets_fresh_depth = same_owner_handler
    .then(push_effect_handler_frame(3))
    .then(pop_effect_handler_frame)
    .then(exit_effect_handler(false, { value: UNIT_VALUE }))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(check(state.effect_local_depth == 0 and state.effect_head == None))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.effect_local_depth == 2 and state.effect_head == Some(1)))

  run test_constructor_restores_callers_depth = root_e
    .then(new_utxo(UNIT_VALUE))
    .then(enter_constructor(UNIT_VALUE))
    .then(check(state.effect_local_depth == 0))
    .then(push_effect_handler_frame(2))
    .then(pop_effect_handler_frame)
    .then(return({ value: UNIT_VALUE }))
    .then(receive_new_utxo_handle({ value: 0 }))
    .then(check(state.effect_local_depth == 1 and state.effect_head == Some(0)))
    .then(add_effect_handler("F"))
    .then(pop_effect_handler_frame)

  run method_invocation = root_e
    .then(new_utxo(UNIT_VALUE))
    .then(enter_constructor(UNIT_VALUE))
    .then(register_method("M"))
    .then(return({ value: UNIT_VALUE }))
    .then(receive_new_utxo_handle({ value: 0 }))
    .then(call_method(0, "M", UNIT_VALUE))
    .then(enter_method("M", UNIT_VALUE))
    .then(check(state.effect_local_depth == 0))

  run test_method_cannot_modify_inherited_scope = method_invocation
    .then(add_effect_handler("F").fail())

  run test_yield_requires_balanced_scopes = method_invocation
    .then(push_effect_handler_frame(2))
    .then(yield_begin.fail())

  run test_method_restores_callers_depth = method_invocation
    .then(push_effect_handler_frame(2))
    .then(pop_effect_handler_frame)
    .then(yield_begin)
    .then(return({ value: UNIT_VALUE }))
    .then(receive_method_result({ value: UNIT_VALUE }))
    .then(check(state.effect_local_depth == 1 and state.effect_head == Some(0)))
    .then(pop_effect_handler_frame)

  run test_search_moves_head_but_preserves_raiser_context = child_with_f
    .then(raise_effect("E", UNIT_VALUE))
    .then(check(state.effect_head == Some(1) and state.effect_local_depth == 1))
    .then(skip_effect_handler)
    .then(check(state.effect_head == Some(0)))
    // this is probably not a concern even if it happens
    // (in the sense that nothing should use the intermediate curr anyway)
    .then(check(state.curr == Utxo(0)))
    .then(check(state.call_stack.last().saved_effect_head == Some(1)))
    .then(check(state.call_stack.last().saved_effect_depth == 1))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(check(state.curr == Coord(1)))
    .then(check(state.effect_head == None and state.effect_local_depth == 0))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(check(state.effect_head == Some(1) and state.effect_local_depth == 1))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.curr == Utxo(0) and state.curr_cid_phase == ConstructorExecuting))
    .then(pop_effect_handler_frame)
    .then(check(state.effect_head == Some(0)))

  run test_nested_handler_then_decline_continues_from_parent = two_e
    .then(raise_effect("E", UNIT_VALUE))
    .then(enter_effect_handler(2, "E", UNIT_VALUE))
    .then(push_effect_handler_frame(3))
    .then(add_effect_handler("F"))
    .then(raise_effect("F", UNIT_VALUE))
    .then(enter_effect_handler(3, "F", UNIT_VALUE))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.effect_head == Some(2) and state.effect_local_depth == 1))
    .then(pop_effect_handler_frame)
    .then(exit_effect_handler(false, { value: UNIT_VALUE }))
    .then(check(state.effect_head == Some(0) and state.effect_local_depth == 2))
    .then(check(state.call_stack.last().saved_effect_head == Some(1)))
    .then(enter_effect_handler(1, "E", UNIT_VALUE))
    .then(check(state.effect_head == None and state.effect_local_depth == 0))
    .then(exit_effect_handler(true, { value: UNIT_VALUE }))
    .then(resume_effect({ value: UNIT_VALUE }))
    .then(check(state.effect_head == Some(1) and state.effect_local_depth == 2))

}
