use std::collections::HashMap;

use neo_math::F;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, Step, Trace};

use crate::{
    ivc_state::{CoroutineId, CurrPhase},
    opcode::Opcode,
};

pub(crate) fn method_hash_words(method: MethodHash) -> [u32; 8] {
    std::array::from_fn(|word| {
        let limb = method.0[word / 2];
        let shift = (word % 2) * 32;
        ((limb >> shift) & u64::from(u32::MAX)) as u32
    })
}

fn encode_method_hash(method: MethodHash) -> [F; 8] {
    method_hash_words(method).map(|word| F::new(u64::from(word)))
}

pub(crate) fn normalize(trace: &Trace) -> NormalizedTrace {
    let mut method_table = trace
        .0
        .iter()
        .filter_map(|step| match step {
            Step::RegisterMethod { method }
            | Step::CallMethod { method, .. }
            | Step::EnterMethod { method, .. } => Some(*method),
            _ => None,
        })
        .collect::<Vec<_>>();
    method_table.sort_unstable_by_key(|method| method.0);
    method_table.dedup();

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
    let mut curr = CoroutineId::Coord(1);
    let mut curr_phase = CurrPhase::Executing;
    // Quint's initial coordinator frame occupies the first stack slot.
    let mut callers = vec![CoroutineId::Coord(0)];
    let mut call_sp = F::new(1);
    let mut next_utxo_id = 0u32;
    let mut pending_ctor_key = None;
    let mut resource_resolver = HashMap::new();
    let mut abi_generations: HashMap<CoroutineId, u32> = HashMap::new();
    let mut enabled_method_log: Vec<(CoroutineId, u32, u32)> = vec![];

    for step in &trace.0 {
        let opcode = Opcode::from(step);
        let curr_before = curr;
        let curr_phase_before = curr_phase;
        let next_utxo_id_before = next_utxo_id;
        let enabled_method_log_len_before =
            u32::try_from(enabled_method_log.len()).expect("the enabled-method log fits in u32");
        let pending_ctor_key_before = pending_ctor_key;
        let curr_phase_after = opcode.phase_after(curr_phase_before);
        curr_phase = curr_phase_after;

        let call_sp_before = call_sp;
        let call_sp_after = if opcode.pushes_to_call_stack() {
            call_sp_before + F::new(1)
        } else if opcode.pops_from_call_stack() {
            call_sp_before - F::new(1)
        } else {
            call_sp_before
        };
        call_sp = call_sp_after;

        let mut expected_arguments = None;
        let mut method_hash = None;
        let mut expected_result = None;
        let mut method_index = 0;
        let mut curr_after = curr_before;
        let mut call_target = CoroutineId::Coord(0);
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

        match step {
            Step::NewUtxo {
                arguments,
                resource,
            } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());

                let target = CoroutineId::Utxo(next_utxo_id_before);
                next_utxo_id = next_utxo_id_before
                    .checked_add(1)
                    .expect("the UTXO allocator fits in u32");
                callers.push(curr_before);
                pending_ctor_key = Some((curr_before, resource.0));
                curr_after = target;
                call_target = target;
                resolver_address = (curr_before, resource.0);
            }
            Step::EnterConstructor { arguments } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
            }
            Step::YieldBegin => {
                abi_generation_address = curr_before;
                abi_generation_before = abi_generations.get(&curr_before).copied().unwrap_or(0);
                abi_generation_after = abi_generation_before
                    .checked_add(1)
                    .expect("the UTXO ABI generation fits in u32");
                abi_generations.insert(curr_before, abi_generation_after);
            }
            Step::RegisterMethod { method } => {
                method_hash.replace(encode_method_hash(*method));
                method_index = method_indices[method];
                abi_generation_address = curr_before;
                abi_generation_before = abi_generations.get(&curr_before).copied().unwrap_or(0);
                enabled_method_log_address = enabled_method_log_len_before;
                enabled_method_log_utxo = curr_before;
                enabled_method_log_generation = abi_generation_before;
                enabled_method_log.push((curr_before, method_index, abi_generation_before));
            }
            Step::Return { result } => {
                expected_result.replace(result.0.0.iter().map(|&x| F::new(x as u64)).collect());

                if let Some(key) = pending_ctor_key_before {
                    resource_resolver.insert(key, curr_before);
                    resolver_address = key;
                    resolver_value = curr_before;
                    resolver_write = true;
                }
                pending_ctor_key = None;

                // Malformed traces still normalize to a witness; the call-SP
                // range check rejects an underflow instead of normalization
                // panicking before the relation is checked.
                let target = callers.pop().unwrap_or(CoroutineId::Coord(0));
                curr_after = target;
                call_target = target;
            }
            Step::CallMethod {
                resource,
                method,
                arguments,
                result,
            } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
                method_hash.replace(encode_method_hash(*method));
                expected_result.replace(result.0.0.iter().map(|&x| F::new(x as u64)).collect());
                method_index = method_indices[method];

                let key = (curr_before, *resource);
                let target = resource_resolver
                    .get(&key)
                    .copied()
                    .unwrap_or(CoroutineId::Coord(0));
                callers.push(curr_before);
                curr_after = target;
                call_target = target;
                resolver_address = key;
                resolver_value = target;
                resolver_read = true;

                abi_generation_address = target;
                abi_generation_before = abi_generations.get(&target).copied().unwrap_or(0);
                enabled_method_log_utxo = target;
                enabled_method_log_generation = abi_generation_before;
                enabled_method_log_address = enabled_method_log
                    .iter()
                    .rposition(|entry| *entry == (target, method_index, abi_generation_before))
                    .map(|index| u32::try_from(index).expect("the enabled-method log fits in u32"))
                    .unwrap_or(enabled_method_log_len_before);
            }
            Step::EnterMethod { method, arguments } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
                method_hash.replace(encode_method_hash(*method));
            }
        }

        curr = curr_after;

        wit.push(Wit {
            opcode,
            expected_arguments,
            method_hash,
            expected_result,
            method_index,
            curr_before,
            curr_after,
            curr_phase_before,
            curr_phase_after,
            call_sp_before,
            call_sp_after,
            call_target,
            next_utxo_id_before,
            next_utxo_id_after: next_utxo_id,
            enabled_method_log_len_before,
            enabled_method_log_len_after: u32::try_from(enabled_method_log.len())
                .expect("the enabled-method log fits in u32"),
            pending_ctor_key_before,
            pending_ctor_key_after: pending_ctor_key,
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
        })
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

pub(crate) struct Wit {
    pub(crate) opcode: Opcode,
    pub(crate) expected_arguments: Option<Vec<F>>,
    pub(crate) method_hash: Option<[F; 8]>,
    pub(crate) expected_result: Option<Vec<F>>,
    pub(crate) method_index: u32,
    pub(crate) curr_before: CoroutineId,
    pub(crate) curr_after: CoroutineId,
    pub(crate) curr_phase_before: CurrPhase,
    pub(crate) curr_phase_after: CurrPhase,
    pub(crate) call_sp_before: F,
    pub(crate) call_sp_after: F,
    pub(crate) call_target: CoroutineId,
    pub(crate) next_utxo_id_before: u32,
    pub(crate) next_utxo_id_after: u32,
    pub(crate) enabled_method_log_len_before: u32,
    pub(crate) enabled_method_log_len_after: u32,
    pub(crate) pending_ctor_key_before: Option<(CoroutineId, ResourceHandle)>,
    pub(crate) pending_ctor_key_after: Option<(CoroutineId, ResourceHandle)>,
    pub(crate) resolver_address: (CoroutineId, ResourceHandle),
    pub(crate) resolver_value: CoroutineId,
    pub(crate) resolver_read: bool,
    pub(crate) resolver_write: bool,
    pub(crate) abi_generation_address: CoroutineId,
    pub(crate) abi_generation_before: u32,
    pub(crate) abi_generation_after: u32,
    pub(crate) enabled_method_log_address: u32,
    pub(crate) enabled_method_log_utxo: CoroutineId,
    pub(crate) enabled_method_log_generation: u32,
}

impl Wit {
    /// A circuit-only fixed point of the carried state, with inactive buses
    /// assigned zero. It goes through the same column assigner as execution.
    pub(crate) fn padding_after(&self) -> Self {
        Self {
            opcode: Opcode::Padding,
            expected_arguments: None,
            method_hash: None,
            expected_result: None,
            method_index: 0,
            curr_before: self.curr_after,
            curr_after: self.curr_after,
            curr_phase_before: self.curr_phase_after,
            curr_phase_after: Opcode::Padding.phase_after(self.curr_phase_after),
            call_sp_before: self.call_sp_after,
            call_sp_after: self.call_sp_after,
            call_target: CoroutineId::Coord(0),
            next_utxo_id_before: self.next_utxo_id_after,
            next_utxo_id_after: self.next_utxo_id_after,
            enabled_method_log_len_before: self.enabled_method_log_len_after,
            enabled_method_log_len_after: self.enabled_method_log_len_after,
            pending_ctor_key_before: self.pending_ctor_key_after,
            pending_ctor_key_after: self.pending_ctor_key_after,
            resolver_address: (CoroutineId::Coord(0), ResourceHandle(0)),
            resolver_value: CoroutineId::Coord(0),
            resolver_read: false,
            resolver_write: false,
            abi_generation_address: CoroutineId::Coord(0),
            abi_generation_before: 0,
            abi_generation_after: 0,
            enabled_method_log_address: 0,
            enabled_method_log_utxo: CoroutineId::Coord(0),
            enabled_method_log_generation: 0,
        }
    }
}
