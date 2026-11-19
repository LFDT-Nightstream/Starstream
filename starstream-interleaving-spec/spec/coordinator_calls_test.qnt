module coordinator_calls_test {
  import starstream.* from "./starstream"
  import utils.* from "./utils"

  pure val ENTRY = "entry"
  action fresh = { state' = new_tx }
  run init = fresh
    .then(begin_execution(ENTRY, UNIT_VALUE))
    .then(enter_call(ENTRY, UNIT_VALUE))
  action check(condition: bool): bool = all { assert(condition), state' = state }
  pure val ARG = List(1, 2, 3, 4)
  pure val RESULT = List(5, 6, 7, 8)

  run called = init.then(call_coordinator("A", ARG))
  run entered = called.then(enter_call("A", ARG))
  run returned = entered.then(return({ value: RESULT }))

  run completed_calls = entered
    .then(check(state.curr == Coord(2)))
    .then(call_coordinator("A", ARG))
    .then(enter_call("A", ARG))
    .then(check(state.curr == Coord(3)))
    .then(return({ value: RESULT }))
    .then(receive_call_result({ value: RESULT }))
    .then(check(state.curr == Coord(2)))
    .then(return({ value: RESULT }))
    .then(receive_call_result({ value: RESULT }))
    .then(call_coordinator("A", ARG))
    .then(enter_call("A", ARG))
    .then(check(state.curr == Coord(4)))
    .then(return({ value: RESULT }))
    .then(receive_call_result({ value: RESULT }))
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)

  run all_finalized = completed_calls
    .then(finalize_coordinator)
    .then(finalize_coordinator)
    .then(finalize_coordinator)
    .then(finalize_coordinator)

  run test_nested_and_repeated_calls_allocate_fresh_instances = all_finalized
    .then(finish_transaction)
    .then(transaction_complete)

  run test_early_finish = completed_calls.then(finalize_coordinator).then(finish_transaction.fail())
  run test_duplicate_finalization = all_finalized.then(finalize_coordinator.fail())

  run test_wrong_script = called.then(enter_call("B", ARG).fail())
  run test_wrong_arguments = called.then(enter_call("A", RESULT).fail())
  run test_return_before_entry = called.then(return({ value: RESULT }).fail())
  run test_wrong_result = returned.then(receive_call_result({ value: ARG }).fail())
  run test_wrong_receive_kind = returned.then(receive_new_utxo_handle({ value: 0 }).fail())
  // The entry script is entered exactly like an imported one.
  run test_entry_uses_coordinator_call_protocol = fresh
    .then(check(state.curr == Coord(0) and state.next_coordinator_id == 1))
    .then(begin_execution("A", ARG))
    .then(check(state.curr == Coord(1) and state.curr_cid_phase == CallEnterPending))
    .then(enter_call("A", ARG))
    .then(return({ value: UNIT_VALUE }))
    .then(check(state.curr == Coord(0)))
    .then(execution_complete)
    .then(finalize_coordinator)
    .then(finish_transaction)
  run test_cannot_continue_before_receive = returned.then(call_coordinator("B", ARG).fail())
  run test_duplicate_receive = returned
    .then(receive_call_result({ value: RESULT }))
    .then(receive_call_result({ value: RESULT }).fail())
  run test_no_call_from_utxo = init
    .then(new_utxo(UNIT_VALUE)).then(enter_constructor(UNIT_VALUE))
    .then(call_coordinator("A", ARG).fail())
  run test_unbalanced_scope_cannot_return = entered
    .then(push_effect_handler_frame(9))
    .then(return({ value: RESULT }).fail())

  run test_effect_crosses_coordinator_boundary_and_handler_can_call_script = init
    .then(push_effect_handler_frame(1))
    .then(add_effect_handler("E"))
    .then(call_coordinator("A", ARG))
    .then(enter_call("A", ARG))
    .then(raise_effect("E", ARG))
    .then(enter_effect_handler(1, "E", ARG))
    .then(check(state.curr == Coord(1)))
    .then(call_coordinator("B", ARG))
    .then(enter_call("B", ARG))
    .then(return({ value: RESULT }))
    .then(receive_call_result({ value: RESULT }))
    .then(check(state.curr_cid_phase == EffectHandlerExecuting))
    .then(exit_effect_handler(true, { value: RESULT }))
    .then(resume_effect({ value: RESULT }))
    .then(check(state.curr == Coord(2)))
    .then(return({ value: RESULT }))
    .then(receive_call_result({ value: RESULT }))
    .then(check(state.effect_local_depth == 1 and state.effect_head == Some(0)))
    .then(pop_effect_handler_frame)
    .then(return({ value: UNIT_VALUE }))
    .then(execution_complete)
}
