use std::collections::{HashMap, HashSet};

use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{
    EventIdentity, MethodHash, ResourceHandle, StarstreamValue, Step, Trace,
};

use crate::{
    ivc_state::{CoroutineId, CurrPhase, TxPhase},
    opcode::Opcode,
};

fn identity_limbs(identity: EventIdentity) -> [F; 8] {
    identity.0.map(|word| F::new(u64::from(word)))
}

pub(crate) fn method_table(trace: &Trace) -> Vec<MethodHash> {
    let mut method_table = trace
        .0
        .iter()
        .filter_map(|step| match step {
            Step::RegisterMethod { method }
            | Step::PreloadMethod { method }
            | Step::ReadAbi { method }
            | Step::CallMethod { method, .. }
            | Step::EnterCall { method, .. }
            | Step::RaiseEffect { effect: method, .. }
            | Step::AddEffectHandler { effect: method }
            | Step::EnterEffectHandler { effect: method, .. } => Some(*method),
            _ => None,
        })
        .collect::<Vec<_>>();
    method_table.sort_unstable_by_key(|method| method.0);
    method_table.dedup();

    method_table
}

#[cfg(test)]
pub(crate) fn normalize(trace: &Trace) -> NormalizedTrace {
    normalize_with_inputs(trace, &Default::default())
}

pub(crate) fn normalize_with_inputs(
    trace: &Trace,
    signers: &std::collections::BTreeSet<u64>,
) -> NormalizedTrace {
    let mut io_chain = [F::ZERO; 4];
    let signer_indices: HashMap<_, _> = signers.iter().copied().zip(0u32..).collect();
    let method_table = method_table(trace);
    let method_indices = method_table
        .iter()
        .enumerate()
        .map(|(index, method)| {
            (
                *method,
                u32::try_from(index).expect("the trace-local method table fits in u32"),
            )
        })
        .collect::<HashMap<_, _>>();

    let mut wit: Vec<Wit> = vec![];
    let mut tx = TransactionState {
        phase: TxPhase::Loading,
        last_input_has_abi: true,
        output_cursor: 0,
        coordinator_cursor: 1,
        abi_read_remaining: 0,
        abi_read_ordinal: 0,
    };
    // The transaction context has the turn until BeginExecution calls the
    // entry script.
    let mut curr = CoroutineId::Coord(0);
    let mut curr_phase = CurrPhase::Executing;
    // Program invocations remain until Receive*, which binds the callee handle.
    // The circuit's depth excludes the entry coordinator, pushed on entry.
    let mut instance_stack: Vec<CoroutineId> = vec![];
    // Append-only scope arena, indexed by node id; heads are encoded id + 1.
    let mut arena: Vec<EffectScope> = vec![];
    let mut effect_head = 0u32;
    let mut effect_depth = 0u32;
    // BeginExecution pushes the entry frame, whose caller is the context.
    let mut callers: Vec<CallFrame> = vec![];
    let mut call_sp = F::ZERO;
    let mut next_token_id = 0u32;
    let mut next_utxo_id = 0u32;
    // Coord(0) is the transaction context; scripts are allocated from Coord(1).
    let mut next_coordinator_id = 1u32;
    let mut resource_resolver = HashMap::new();
    let mut abi_generations: HashMap<CoroutineId, u32> = HashMap::new();
    let mut abi_method_counts: HashMap<CoroutineId, u32> = HashMap::new();
    let mut enabled_method_log: Vec<(CoroutineId, u32, u32, u32)> = vec![];
    // Preserve first exact-entry / last membership lookup, including duplicates.
    let mut log_address_by_entry = HashMap::new();
    let mut last_log_membership = HashMap::new();
    let mut commitments = HashMap::new();

    // Hand-off registers: what a call, return or handler exit leaves for the
    // step that must follow it.
    let mut pending_value = [F::ZERO; 4];
    let mut pending_method = [F::ZERO; 8];

    for step in &trace.0 {
        let opcode = Opcode::from(step);
        let tx_before = tx;
        let boundary_coroutine_id = match opcode {
            Opcode::SetStorage
                if matches!(
                    step,
                    Step::SetStorage {
                        kind: starstream_interleaving_spec::StorageKind::Token { .. },
                        ..
                    }
                ) =>
            {
                CoroutineId::Token(next_token_id)
            }
            Opcode::SetStorage => CoroutineId::Utxo(next_utxo_id),
            Opcode::PreloadMethod => CoroutineId::Utxo(next_utxo_id.saturating_sub(1)),
            Opcode::GetStorage | Opcode::SkipConsumed => CoroutineId::Utxo(tx.output_cursor),
            Opcode::ReadAbi => CoroutineId::Utxo(tx.output_cursor.saturating_sub(1)),
            _ => CoroutineId::Coord(0),
        };
        let curr_before = curr;
        let instance_depth_before = instance_stack.len().saturating_sub(1) as u32;
        let curr_phase_before = curr_phase;
        let next_token_id_before = next_token_id;
        let next_utxo_id_before = next_utxo_id;
        let next_coordinator_id_before = next_coordinator_id;
        let enabled_method_log_len_before =
            u32::try_from(enabled_method_log.len()).expect("the enabled-method log fits in u32");
        let effect_head_before = effect_head;
        let effect_depth_before = effect_depth;
        let effect_arena_len_before =
            u32::try_from(arena.len()).expect("the effect arena fits in u32");
        let mut dynamic_phase_after = None;

        let call_sp_before = call_sp;
        let return_has_receiver = matches!(step, Step::Return { .. })
            && callers
                .last()
                .is_some_and(|frame| frame.caller != CoroutineId::Coord(0));
        let call_sp_after = if opcode.pushes_to_call_stack() {
            call_sp_before + F::new(1)
        } else if opcode.pops_from_call_stack_unconditionally()
            || (opcode == Opcode::Return && !return_has_receiver)
        {
            call_sp_before - F::new(1)
        } else {
            call_sp_before
        };
        call_sp = call_sp_after;

        let mut expected_arguments = None;
        let mut event_identity = None;
        let mut expected_result = None;
        let mut method_index = 0;
        let mut curr_after = curr_before;
        let mut call_target = CoroutineId::Coord(0);
        let mut token_interface = 0;
        let mut second_handle = ResourceHandle(0);
        let mut second_target = CoroutineId::Coord(0);
        let mut resolver_address = (CoroutineId::Coord(0), ResourceHandle(0));
        let mut resolver_value = CoroutineId::Coord(0);
        let mut resolver_read = false;
        let mut resolver_write = false;
        let mut abi_generation_address = CoroutineId::Coord(0);
        let mut abi_generation_before = 0;
        let mut abi_generation_after = 0;
        let mut enabled_method_log_address = 0;
        let mut enabled_method_log_utxo = CoroutineId::Coord(0);
        let mut enabled_method_log_generation = 0;
        let mut call_frame_is_effect = false;
        let mut call_frame_constructor = false;
        let mut call_frame_saved_phase = CurrPhase::Executing;
        let mut return_requires_unit = false;
        let mut effect_handled = false;
        let mut effect_frame_id = 0;
        let mut effect_registered = false;
        let mut saved_effect_head = 0;
        let mut saved_effect_depth = 0;

        match step {
            // Both call a script as the next fresh coordinator; only the
            // entry call leaves loading, and its callee has no receiver.
            Step::BeginExecution { script, arguments }
            | Step::CallCoordinator { script, arguments } => {
                expected_arguments =
                    Some(arguments.words32().map(|x| F::new(u64::from(x))).to_vec());
                event_identity = Some(identity_limbs(*script));
                let target = CoroutineId::Coord(next_coordinator_id_before);
                next_coordinator_id = next_coordinator_id_before
                    .checked_add(1)
                    .expect("the coordinator allocator fits in u32");
                callers.push(CallFrame {
                    caller: curr_before,
                    saved_phase: curr_phase_before,
                    kind: CallFrameKind::Program,
                    saved_effect_head: effect_head_before,
                    saved_effect_depth: effect_depth_before,
                    arguments: arguments.clone(),
                    effect: None,
                });
                effect_depth = 0;
                instance_stack.push(target);
                curr_after = target;
                call_target = target;
                call_frame_saved_phase = curr_phase_before;
                if opcode == Opcode::BeginExecution {
                    tx.phase = TxPhase::Running;
                }
            }
            Step::ReadAbi { method } => {
                event_identity = Some(identity_limbs(*method));
                method_index = method_indices[method];
                abi_generation_address = boundary_coroutine_id;
                abi_generation_before = abi_generations
                    .get(&boundary_coroutine_id)
                    .copied()
                    .unwrap_or(0);
                enabled_method_log_utxo = boundary_coroutine_id;
                enabled_method_log_generation = abi_generation_before;
                enabled_method_log_address = log_address_by_entry
                    .get(&(
                        boundary_coroutine_id,
                        method_index,
                        abi_generation_before,
                        tx.abi_read_ordinal,
                    ))
                    .copied()
                    .unwrap_or(enabled_method_log_len_before);
                tx.abi_read_ordinal = tx.abi_read_ordinal.wrapping_add(1);
                tx.abi_read_remaining = tx.abi_read_remaining.wrapping_sub(1);
            }
            Step::SetStorage {
                kind,
                storage,
                entry_handle,
            } => {
                expected_arguments = Some(storage.words32().map(|x| F::new(u64::from(x))).to_vec());
                match kind {
                    starstream_interleaving_spec::StorageKind::Token { interface } => {
                        token_interface = *interface;
                        next_token_id = next_token_id
                            .checked_add(1)
                            .expect("token allocator fits u32")
                    }
                    starstream_interleaving_spec::StorageKind::Utxo => {
                        next_utxo_id = next_utxo_id
                            .checked_add(1)
                            .expect("UTXO allocator fits u32");
                        tx.last_input_has_abi = false;
                    }
                }
                resolver_address = (CoroutineId::Coord(1), *entry_handle);
                resolver_value = boundary_coroutine_id;
                resolver_write = true;
                resource_resolver.insert(resolver_address, resolver_value);
            }
            Step::FinishTransaction => tx.phase = TxPhase::Finished,
            Step::GetStorage { storage } => {
                tx.abi_read_remaining = abi_method_counts
                    .get(&boundary_coroutine_id)
                    .copied()
                    .unwrap_or(0);
                tx.abi_read_ordinal = 0;
                expected_result = Some(storage.0.words32().map(|x| F::new(u64::from(x))).to_vec());
                tx.output_cursor = tx
                    .output_cursor
                    .checked_add(1)
                    .expect("output cursor fits u32");
            }
            Step::SkipConsumed => {
                tx.output_cursor = tx
                    .output_cursor
                    .checked_add(1)
                    .expect("output cursor fits u32");
            }
            Step::FinalizeCoordinator => {
                tx.coordinator_cursor = tx.coordinator_cursor.wrapping_add(1);
            }
            Step::NewUtxo {
                constructor,
                arguments,
            } => {
                event_identity = Some(identity_limbs(*constructor));
                expected_arguments.replace(
                    arguments
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );

                let target = CoroutineId::Utxo(next_utxo_id_before);
                next_utxo_id = next_utxo_id_before
                    .checked_add(1)
                    .expect("the UTXO allocator fits in u32");
                callers.push(CallFrame {
                    caller: curr_before,
                    saved_phase: curr_phase_before,
                    kind: CallFrameKind::Constructor,
                    saved_effect_head: effect_head_before,
                    saved_effect_depth: effect_depth_before,
                    arguments: arguments.clone(),
                    effect: None,
                });
                effect_depth = 0;
                instance_stack.push(target);
                curr_after = target;
                call_target = target;
                call_frame_constructor = true;
                call_frame_saved_phase = curr_phase_before;
            }
            Step::EnterConstructor {
                constructor,
                arguments,
            } => {
                event_identity = Some(identity_limbs(*constructor));
                call_frame_constructor = callers
                    .last()
                    .map(|frame| frame.kind == CallFrameKind::Constructor)
                    .unwrap_or(false);
                expected_arguments.replace(
                    arguments
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );
            }
            Step::YieldBegin => {
                abi_generation_address = curr_before;
                abi_generation_before = abi_generations.get(&curr_before).copied().unwrap_or(0);
                abi_generation_after = abi_generation_before
                    .checked_add(1)
                    .expect("the UTXO ABI generation fits in u32");
                abi_generations.insert(curr_before, abi_generation_after);
            }
            Step::RegisterMethod { method } | Step::PreloadMethod { method } => {
                let owner = if opcode == Opcode::PreloadMethod {
                    tx.last_input_has_abi = true;
                    boundary_coroutine_id
                } else {
                    curr_before
                };
                event_identity.replace(identity_limbs(*method));
                method_index = method_indices[method];
                abi_generation_address = owner;
                abi_generation_before = abi_generations.get(&owner).copied().unwrap_or(0);
                enabled_method_log_address = enabled_method_log_len_before;
                enabled_method_log_utxo = owner;
                enabled_method_log_generation = abi_generation_before;
                let entry = (
                    owner,
                    method_index,
                    abi_generation_before,
                    abi_method_counts.get(&owner).copied().unwrap_or(0),
                );
                log_address_by_entry
                    .entry(entry)
                    .or_insert(enabled_method_log_address);
                last_log_membership.insert(
                    (owner, method_index, abi_generation_before),
                    enabled_method_log_address,
                );
                enabled_method_log.push(entry);
            }
            Step::Return { result } => {
                expected_result.replace(
                    result
                        .0
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );

                // Malformed traces still normalize to a witness; the call-SP
                // range check rejects an underflow instead of normalization
                // panicking before the relation is checked.
                let frame = callers
                    .last()
                    .cloned()
                    .unwrap_or_else(|| CallFrame::placeholder(CallFrameKind::Program));
                call_frame_is_effect = frame.kind == CallFrameKind::Effect;
                call_frame_constructor = frame.kind == CallFrameKind::Constructor;
                saved_effect_depth = frame.saved_effect_depth;
                effect_depth = frame.saved_effect_depth;
                return_requires_unit = frame.kind == CallFrameKind::Constructor
                    || !return_has_receiver
                    || matches!(curr_before, CoroutineId::Token(_));
                curr_after = frame.caller;
                call_target = frame.caller;
                if return_has_receiver {
                    dynamic_phase_after = Some(if frame.kind == CallFrameKind::Constructor {
                        CurrPhase::NewUtxoHandlePending
                    } else {
                        CurrPhase::CallResultPending
                    });
                } else {
                    callers.pop();
                    dynamic_phase_after = Some(CurrPhase::Executing);
                }
            }
            Step::Upcast { source, result } => {
                second_handle = *source;
                second_target = resource_resolver
                    .get(&(curr_before, *source))
                    .copied()
                    .unwrap_or(CoroutineId::Coord(0));
                resolver_address = (curr_before, result.0);
                resolver_value = second_target;
                resolver_write = true;
                resource_resolver.insert(resolver_address, resolver_value);
            }
            Step::Attach { token, destination } => {
                resolver_address = (curr_before, *token);
                resolver_read = true;
                resolver_value = resource_resolver
                    .get(&resolver_address)
                    .copied()
                    .unwrap_or(CoroutineId::Coord(0));
                second_handle = *destination;
                second_target = resource_resolver
                    .get(&(curr_before, *destination))
                    .copied()
                    .unwrap_or(CoroutineId::Coord(0));
                let arguments = StarstreamValue([u64::from(second_target.encoded()), 0, 0, 0]);
                expected_arguments = Some(arguments.words32().map(F::from_u32).to_vec());
                callers.push(CallFrame {
                    caller: curr_before,
                    saved_phase: curr_phase_before,
                    kind: CallFrameKind::Program,
                    saved_effect_head: effect_head_before,
                    saved_effect_depth: effect_depth_before,
                    arguments,
                    effect: None,
                });
                effect_depth = 0;
                instance_stack.push(resolver_value);
                curr_after = resolver_value;
                call_target = resolver_value;
                call_frame_saved_phase = curr_phase_before;
            }
            Step::EnterAttach {
                destination,
                interface,
            } => {
                token_interface = *interface;
                resolver_address = (curr_before, *destination);
                resolver_value = callers
                    .last()
                    .and_then(|frame| u32::try_from(frame.arguments.0[0]).ok())
                    .and_then(CoroutineId::from_packed)
                    .unwrap_or(CoroutineId::Coord(0));
                resolver_write = true;
                resource_resolver.insert(resolver_address, resolver_value);
            }
            Step::CallMethod {
                resource,
                method,
                arguments,
            } => {
                expected_arguments.replace(
                    arguments
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );
                event_identity.replace(identity_limbs(*method));
                method_index = method_indices[method];

                let key = (curr_before, *resource);
                let target = resource_resolver
                    .get(&key)
                    .copied()
                    .unwrap_or(CoroutineId::Coord(0));
                callers.push(CallFrame {
                    caller: curr_before,
                    saved_phase: curr_phase_before,
                    kind: CallFrameKind::Program,
                    saved_effect_head: effect_head_before,
                    saved_effect_depth: effect_depth_before,
                    arguments: arguments.clone(),
                    effect: None,
                });
                effect_depth = 0;
                instance_stack.push(target);
                curr_after = target;
                call_target = target;
                resolver_address = key;
                resolver_value = target;
                resolver_read = true;
                call_frame_saved_phase = curr_phase_before;

                abi_generation_address = target;
                abi_generation_before = abi_generations.get(&target).copied().unwrap_or(0);
                enabled_method_log_utxo = target;
                enabled_method_log_generation = abi_generation_before;
                enabled_method_log_address = last_log_membership
                    .get(&(target, method_index, abi_generation_before))
                    .copied()
                    .unwrap_or(enabled_method_log_len_before);
            }
            Step::EnterCall { method, arguments } => {
                expected_arguments.replace(
                    arguments
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );
                event_identity.replace(identity_limbs(*method));
            }
            Step::ReceiveNewUtxoHandle { resource } => {
                let frame = callers
                    .pop()
                    .unwrap_or_else(|| CallFrame::placeholder(CallFrameKind::Constructor));
                call_frame_saved_phase = frame.saved_phase;
                call_target = frame.caller;
                resolver_address = (curr_before, resource.0);
                resolver_value = instance_stack.pop().unwrap_or(CoroutineId::Coord(0));
                resolver_write = true;
                resource_resolver.insert(resolver_address, resolver_value);
                dynamic_phase_after = Some(frame.saved_phase);
            }
            Step::ReceiveCallResult { result } => {
                instance_stack.pop();
                expected_result = Some(result.0.words32().map(|x| F::new(u64::from(x))).to_vec());
                let frame = callers
                    .pop()
                    .unwrap_or_else(|| CallFrame::placeholder(CallFrameKind::Program));
                call_frame_saved_phase = frame.saved_phase;
                call_target = frame.caller;
                dynamic_phase_after = Some(frame.saved_phase);
            }
            Step::RaiseEffect { effect, arguments } => {
                expected_arguments =
                    Some(arguments.words32().map(|x| F::new(u64::from(x))).to_vec());
                expected_result = Some(vec![F::ZERO; 8]);
                event_identity = Some(identity_limbs(*effect));
                method_index = method_indices[effect];
                call_target = curr_before;
                call_frame_is_effect = true;
                call_frame_saved_phase = curr_phase_before;
                callers.push(CallFrame {
                    caller: curr_before,
                    saved_phase: curr_phase_before,
                    kind: CallFrameKind::Effect,
                    saved_effect_head: effect_head_before,
                    saved_effect_depth: effect_depth_before,
                    arguments: arguments.clone(),
                    effect: Some(*effect),
                });
            }
            Step::AssertSignedBy { key } => {
                event_identity = Some(identity_limbs(EventIdentity::from_u64_words([
                    *key, 0, 0, 0,
                ])));
                method_index = signer_indices.get(key).copied().unwrap_or(0);
            }
            Step::PushEffectHandlerFrame { frame_id } => {
                arena.push(EffectScope {
                    parent: effect_head_before,
                    owner: curr_before,
                    effects: HashSet::new(),
                });
                effect_frame_id = *frame_id;
                effect_head = u32::try_from(arena.len()).expect("the effect arena fits in u32");
                effect_depth = effect_depth_before.wrapping_add(1);
            }
            Step::AddEffectHandler { effect } => {
                event_identity = Some(identity_limbs(*effect));
                method_index = method_indices[effect];
                effect_registered = true;
                if let Some(scope) = scope_mut(&mut arena, effect_head_before) {
                    scope.effects.insert(*effect);
                }
            }
            Step::PopEffectHandlerFrame => {
                // An invalid pop keeps the depth representable as a field
                // element; the inverse constraint rejects it.
                effect_head = scope(&arena, effect_head_before).map_or(0, |scope| scope.parent);
                effect_depth = effect_depth_before.wrapping_sub(1);
            }
            Step::SkipEffectHandler => {
                // The searched effect is the pending raise's, which the
                // circuit reads back from the call frame.
                let frame = callers.last();
                let effect = frame.and_then(|frame| frame.effect);
                if let Some(effect) = effect {
                    event_identity = Some(identity_limbs(effect));
                    method_index = method_indices[&effect];
                }
                call_frame_is_effect = true;
                expected_arguments = frame.map(|frame| {
                    frame
                        .arguments
                        .words32()
                        .map(|x| F::new(u64::from(x)))
                        .to_vec()
                });
                let selected = scope(&arena, effect_head_before);
                effect_registered = selected
                    .zip(effect)
                    .is_some_and(|(scope, effect)| scope.effects.contains(&effect));
                effect_head = selected.map_or(0, |scope| scope.parent);
            }
            Step::EnterEffectHandler {
                frame_id,
                effect,
                arguments,
            } => {
                expected_arguments =
                    Some(arguments.words32().map(|x| F::new(u64::from(x))).to_vec());
                event_identity = Some(identity_limbs(*effect));
                method_index = method_indices[effect];
                call_frame_is_effect = true;
                // The claimed frame id is committed and checked against the
                // arena by the frame-id read port.
                let selected = scope(&arena, effect_head_before);
                effect_registered = selected.is_some_and(|scope| scope.effects.contains(effect));
                effect_frame_id = *frame_id;
                effect_head = selected.map_or(0, |scope| scope.parent);
                effect_depth = 0;
                call_target = selected.map_or(CoroutineId::Coord(0), |scope| scope.owner);
                curr_after = call_target;
            }
            Step::ExitEffectHandler { handled, result } => {
                if let Some(frame) = callers.last_mut() {
                    expected_arguments = Some(
                        frame
                            .arguments
                            .words32()
                            .map(|x| F::new(u64::from(x)))
                            .to_vec(),
                    );
                    saved_effect_head = frame.saved_effect_head;
                    saved_effect_depth = frame.saved_effect_depth;
                }
                if *handled {
                    effect_head = saved_effect_head;
                }
                effect_depth = saved_effect_depth;
                expected_result = Some(result.0.words32().map(|x| F::new(u64::from(x))).to_vec());
                call_frame_is_effect = true;
                effect_handled = *handled;
                dynamic_phase_after = Some(if *handled {
                    CurrPhase::EffectResumePending
                } else {
                    CurrPhase::EffectHandlerPending
                });
            }
            Step::ResumeEffect { result } => {
                expected_result = Some(result.0.words32().map(|x| F::new(u64::from(x))).to_vec());
                let frame = callers
                    .pop()
                    .unwrap_or_else(|| CallFrame::placeholder(CallFrameKind::Effect));
                call_frame_is_effect = frame.kind == CallFrameKind::Effect;
                call_frame_saved_phase = frame.saved_phase;
                curr_after = frame.caller;
                call_target = frame.caller;
                dynamic_phase_after = Some(frame.saved_phase);
            }
        }

        let curr_phase_after = dynamic_phase_after
            .or_else(|| opcode.fixed_phase_after(curr_phase_before))
            .expect("normalization assigns every data-dependent phase transition");

        curr = curr_after;
        curr_phase = curr_phase_after;

        let mut abi_method_count_before = 0;
        let mut abi_method_count_after = 0;
        if opcode.appends_method() || opcode == Opcode::YieldBegin {
            abi_method_count_before = abi_method_counts
                .get(&abi_generation_address)
                .copied()
                .unwrap_or(0);
            if opcode.appends_method() {
                // Keep invalid counts representable: the declared range rejects overflow.
                abi_method_count_after = abi_method_count_before
                    .checked_add(1)
                    .expect("registration count fits u32");
            }
            abi_method_counts.insert(abi_generation_address, abi_method_count_after);
        } else if opcode.scans_output() {
            abi_method_count_before = abi_method_counts
                .get(&boundary_coroutine_id)
                .copied()
                .unwrap_or(0);
        }
        let event_owner = if opcode == Opcode::FinalizeCoordinator {
            CoroutineId::Coord(tx_before.coordinator_cursor)
        } else if matches!(
            opcode,
            Opcode::SetStorage | Opcode::GetStorage | Opcode::SkipConsumed
        ) {
            boundary_coroutine_id
        } else if matches!(opcode, Opcode::EnterEffectHandler | Opcode::ResumeEffect) {
            call_target
        } else if opcode.has_event() {
            curr_before
        } else {
            CoroutineId::Coord(0)
        };
        let commitment_before = commitments
            .get(&event_owner)
            .copied()
            .filter(|_| opcode.has_event() || opcode.reads_trace_root())
            .unwrap_or([F::ZERO; 4]);
        let root_of = |limbs: &Option<Vec<F>>| -> [F; 4] {
            let limbs = limbs.as_deref().unwrap_or(&[]);
            std::array::from_fn(|i| {
                let limb = |j: usize| limbs.get(j).copied().unwrap_or(F::ZERO);
                limb(2 * i) + F::new(1 << 32) * limb(2 * i + 1)
            })
        };
        let pending_value_before = pending_value;
        let pending_method_before = pending_method;
        match opcode {
            Opcode::Attach => pending_value = root_of(&expected_arguments),
            Opcode::CallMethod
            | Opcode::CallCoordinator
            | Opcode::BeginExecution
            | Opcode::NewUtxo => {
                pending_value = root_of(&expected_arguments);
                pending_method = event_identity.unwrap_or([F::ZERO; 8]);
            }
            Opcode::Return | Opcode::ExitEffectHandler => {
                pending_value = root_of(&expected_result);
            }
            _ => {}
        }
        let mut row = Wit {
            pending_value_before,
            pending_value_after: pending_value,
            pending_method_before,
            pending_method_after: pending_method,
            instance_depth_before,
            instance_depth_after: instance_stack.len().saturating_sub(1) as u32,
            io_before: io_chain,
            io_after: io_chain,
            abi_method_count_before,
            abi_method_count_after,
            tx_before,
            tx_after: tx,
            boundary_coroutine_id,
            event_owner,
            commitment_before,
            opcode,
            expected_arguments,
            event_identity,
            expected_result,
            method_index,
            curr_before,
            curr_after,
            curr_phase_before,
            curr_phase_after,
            call_sp_before,
            call_sp_after,
            call_target,
            call_frame_is_effect,
            call_frame_constructor,
            call_frame_saved_phase,
            return_requires_unit,
            effect_handled,
            effect_frame_id,
            effect_registered,
            saved_effect_head: if opcode.pushes_to_call_stack() {
                effect_head_before
            } else {
                saved_effect_head
            },
            saved_effect_depth: if opcode.pushes_to_call_stack() {
                effect_depth_before
            } else {
                saved_effect_depth
            },
            effect_head_before,
            effect_head_after: effect_head,
            effect_depth_before,
            effect_depth_after: effect_depth,
            effect_arena_len_before,
            effect_arena_len_after: u32::try_from(arena.len())
                .expect("the effect arena fits in u32"),
            next_token_id_before,
            next_token_id_after: next_token_id,
            next_utxo_id_before,
            next_utxo_id_after: next_utxo_id,
            next_coordinator_id_before,
            next_coordinator_id_after: next_coordinator_id,
            enabled_method_log_len_before,
            enabled_method_log_len_after: u32::try_from(enabled_method_log.len())
                .expect("the enabled-method log fits in u32"),
            token_interface,
            second_handle,
            second_target,
            resolver_address,
            resolver_value,
            resolver_read,
            resolver_write,
            abi_generation_address,
            abi_generation_before,
            abi_generation_after,
            enabled_method_log_address,
            enabled_method_log_utxo,
            enabled_method_log_generation,
        };
        let mut bus = crate::witness::assign_base_columns(&row);
        crate::commitment::assign_native_from_bus(&mut bus, opcode);
        io_chain = crate::ccs::layout::COL_IO_AFTER.map(|c| bus[c]);
        if opcode.has_event() {
            commitments.insert(event_owner, crate::ccs::layout::COL_OUT.map(|c| bus[c]));
        }
        row.io_after = io_chain;
        wit.push(row);
    }

    NormalizedTrace {
        steps: wit,
        method_table,
    }
}

pub(crate) struct NormalizedTrace {
    pub(crate) steps: Vec<Wit>,
    pub(crate) method_table: Vec<MethodHash>,
}

struct EffectScope {
    /// Encoded parent head: node id + 1, or 0 for the root.
    parent: u32,
    owner: CoroutineId,
    effects: HashSet<MethodHash>,
}

fn scope(arena: &[EffectScope], head: u32) -> Option<&EffectScope> {
    arena.get(head.checked_sub(1)? as usize)
}

fn scope_mut(arena: &mut [EffectScope], head: u32) -> Option<&mut EffectScope> {
    arena.get_mut(head.checked_sub(1)? as usize)
}

// The circuit encodes these variants with separate effect and constructor bits.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CallFrameKind {
    Program,
    Constructor,
    Effect,
}

#[derive(Clone)]
struct CallFrame {
    caller: CoroutineId,
    saved_phase: CurrPhase,
    kind: CallFrameKind,
    saved_effect_head: u32,
    saved_effect_depth: u32,
    arguments: starstream_interleaving_spec::StarstreamValue,
    /// The pending effect of an effect frame, which the search looks up.
    effect: Option<MethodHash>,
}

impl CallFrame {
    fn placeholder(kind: CallFrameKind) -> Self {
        Self {
            caller: CoroutineId::Coord(0),
            saved_phase: CurrPhase::Executing,
            kind,
            saved_effect_head: 0,
            saved_effect_depth: 0,
            arguments: starstream_interleaving_spec::StarstreamValue::UNIT_VALUE,
            effect: None,
        }
    }
}

pub(crate) struct Wit {
    pub(crate) pending_value_before: [F; 4],
    pub(crate) pending_value_after: [F; 4],
    pub(crate) pending_method_before: [F; 8],
    pub(crate) pending_method_after: [F; 8],
    pub(crate) io_before: [F; 4],
    pub(crate) io_after: [F; 4],
    pub(crate) tx_before: TransactionState,
    pub(crate) tx_after: TransactionState,
    pub(crate) boundary_coroutine_id: CoroutineId,
    pub(crate) event_owner: CoroutineId,
    pub(crate) commitment_before: [F; 4],
    pub(crate) opcode: Opcode,
    pub(crate) expected_arguments: Option<Vec<F>>,
    pub(crate) event_identity: Option<[F; 8]>,
    pub(crate) expected_result: Option<Vec<F>>,
    pub(crate) method_index: u32,
    pub(crate) curr_before: CoroutineId,
    pub(crate) curr_after: CoroutineId,
    pub(crate) curr_phase_before: CurrPhase,
    pub(crate) curr_phase_after: CurrPhase,
    pub(crate) call_sp_before: F,
    pub(crate) call_sp_after: F,
    pub(crate) call_target: CoroutineId,
    pub(crate) call_frame_is_effect: bool,
    pub(crate) call_frame_constructor: bool,
    pub(crate) call_frame_saved_phase: CurrPhase,
    pub(crate) return_requires_unit: bool,
    pub(crate) effect_handled: bool,
    pub(crate) effect_frame_id: u32,
    pub(crate) effect_registered: bool,
    pub(crate) saved_effect_head: u32,
    pub(crate) saved_effect_depth: u32,
    pub(crate) effect_head_before: u32,
    pub(crate) effect_head_after: u32,
    pub(crate) effect_depth_before: u32,
    pub(crate) effect_depth_after: u32,
    pub(crate) effect_arena_len_before: u32,
    pub(crate) effect_arena_len_after: u32,
    pub(crate) instance_depth_before: u32,
    pub(crate) instance_depth_after: u32,
    pub(crate) next_token_id_before: u32,
    pub(crate) next_token_id_after: u32,
    pub(crate) next_utxo_id_before: u32,
    pub(crate) next_utxo_id_after: u32,
    pub(crate) next_coordinator_id_before: u32,
    pub(crate) next_coordinator_id_after: u32,
    pub(crate) enabled_method_log_len_before: u32,
    pub(crate) enabled_method_log_len_after: u32,
    pub(crate) token_interface: u32,
    pub(crate) second_handle: ResourceHandle,
    pub(crate) second_target: CoroutineId,
    pub(crate) resolver_address: (CoroutineId, ResourceHandle),
    pub(crate) resolver_value: CoroutineId,
    pub(crate) resolver_read: bool,
    pub(crate) resolver_write: bool,
    pub(crate) abi_generation_address: CoroutineId,
    pub(crate) abi_generation_before: u32,
    pub(crate) abi_method_count_before: u32,
    pub(crate) abi_method_count_after: u32,
    pub(crate) abi_generation_after: u32,
    pub(crate) enabled_method_log_address: u32,
    pub(crate) enabled_method_log_utxo: CoroutineId,
    pub(crate) enabled_method_log_generation: u32,
}

#[derive(Clone, Copy)]
pub(crate) struct TransactionState {
    pub(crate) phase: TxPhase,
    pub(crate) last_input_has_abi: bool,
    pub(crate) output_cursor: u32,
    /// Next coordinator whose final trace root is enumerated.
    pub(crate) coordinator_cursor: u32,
    pub(crate) abi_read_remaining: u32,
    pub(crate) abi_read_ordinal: u32,
}

impl Wit {
    /// A circuit-only fixed point of the carried state, with inactive buses
    /// assigned zero. It goes through the same column assigner as execution.
    pub(crate) fn padding_after(&self) -> Self {
        Self {
            pending_value_before: self.pending_value_after,
            pending_value_after: self.pending_value_after,
            pending_method_before: self.pending_method_after,
            pending_method_after: self.pending_method_after,
            instance_depth_before: self.instance_depth_after,
            instance_depth_after: self.instance_depth_after,
            tx_before: self.tx_after,
            io_before: self.io_after,
            io_after: self.io_after,
            tx_after: self.tx_after,
            boundary_coroutine_id: CoroutineId::Coord(0),
            event_owner: CoroutineId::Coord(0),
            commitment_before: [F::ZERO; 4],
            opcode: Opcode::Padding,
            expected_arguments: None,
            event_identity: None,
            expected_result: None,
            method_index: 0,
            curr_before: self.curr_after,
            curr_after: self.curr_after,
            curr_phase_before: self.curr_phase_after,
            curr_phase_after: self.curr_phase_after,
            call_sp_before: self.call_sp_after,
            call_sp_after: self.call_sp_after,
            call_target: CoroutineId::Coord(0),
            call_frame_is_effect: false,
            call_frame_constructor: false,
            call_frame_saved_phase: CurrPhase::Executing,
            return_requires_unit: false,
            effect_handled: false,
            effect_frame_id: 0,
            effect_registered: false,
            saved_effect_head: 0,
            saved_effect_depth: 0,
            effect_head_before: self.effect_head_after,
            effect_head_after: self.effect_head_after,
            effect_depth_before: self.effect_depth_after,
            effect_depth_after: self.effect_depth_after,
            effect_arena_len_before: self.effect_arena_len_after,
            effect_arena_len_after: self.effect_arena_len_after,
            next_token_id_before: self.next_token_id_after,
            next_token_id_after: self.next_token_id_after,
            next_utxo_id_before: self.next_utxo_id_after,
            next_utxo_id_after: self.next_utxo_id_after,
            next_coordinator_id_before: self.next_coordinator_id_after,
            next_coordinator_id_after: self.next_coordinator_id_after,
            enabled_method_log_len_before: self.enabled_method_log_len_after,
            enabled_method_log_len_after: self.enabled_method_log_len_after,
            token_interface: 0,
            second_handle: ResourceHandle(0),
            second_target: CoroutineId::Coord(0),
            resolver_address: (CoroutineId::Coord(0), ResourceHandle(0)),
            resolver_value: CoroutineId::Coord(0),
            resolver_read: false,
            resolver_write: false,
            abi_generation_address: CoroutineId::Coord(0),
            abi_generation_before: 0,
            abi_method_count_before: 0,
            abi_method_count_after: 0,
            abi_generation_after: 0,
            enabled_method_log_address: 0,
            enabled_method_log_utxo: CoroutineId::Coord(0),
            enabled_method_log_generation: 0,
        }
    }
}
