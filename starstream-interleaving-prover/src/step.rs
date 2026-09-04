use std::collections::HashMap;

use neo_math::F;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, Step, Trace};

use crate::{
    ivc_state::{CoroutineId, CurrPhase},
    opcode::Opcode,
};

fn encode_method_hash(method: MethodHash) -> [F; 8] {
    std::array::from_fn(|word| {
        let limb = method.0[word / 2];
        let shift = (word % 2) * 32;
        F::new((limb >> shift) & u64::from(u32::MAX))
    })
}

pub(crate) fn normalize(trace: &Trace) -> Vec<Wit> {
    let mut wit: Vec<Wit> = vec![];
    let mut curr = CoroutineId::Coord(1);
    let mut curr_phase = CurrPhase::Executing;
    // Quint's initial coordinator frame occupies the first stack slot.
    let mut callers = vec![CoroutineId::Coord(0)];
    let mut call_sp = F::new(1);
    let mut next_utxo_id = 0u32;
    let mut pending_ctor_key = None;
    let mut resource_resolver = HashMap::new();

    for step in &trace.0 {
        let opcode = Opcode::from(step);
        let curr_before = curr;
        let curr_phase_before = curr_phase;
        let next_utxo_id_before = next_utxo_id;
        let pending_ctor_key_before = pending_ctor_key;
        let curr_phase_after = match opcode {
            Opcode::NewUtxo => CurrPhase::CtorEnterPending,
            Opcode::CallMethod => CurrPhase::MethodEnterPending,
            Opcode::EnterConstructor | Opcode::EnterMethod | Opcode::Return => CurrPhase::Executing,
            Opcode::YieldBegin => CurrPhase::Yield,
            Opcode::RegisterMethod => curr_phase_before,
        };
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
        let mut expected_method = None;
        let mut expected_result = None;
        let mut curr_after = curr_before;
        let mut call_target = CoroutineId::Coord(0);
        let mut resolver_address = (CoroutineId::Coord(0), ResourceHandle(0));
        let mut resolver_value = CoroutineId::Coord(0);
        let mut resolver_read = false;
        let mut resolver_write = false;

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
            Step::YieldBegin => {}
            Step::RegisterMethod { method: _ } => {}
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
                expected_method.replace(encode_method_hash(*method));
                expected_result.replace(result.0.0.iter().map(|&x| F::new(x as u64)).collect());

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
            }
            Step::EnterMethod { method, arguments } => {
                expected_arguments.replace(arguments.0.iter().map(|&x| F::new(x as u64)).collect());
                expected_method.replace(encode_method_hash(*method));
            }
        }

        curr = curr_after;

        wit.push(Wit {
            opcode,
            expected_arguments,
            expected_method,
            expected_result,
            curr_before,
            curr_after,
            curr_phase_before,
            curr_phase_after,
            call_sp_before,
            call_sp_after,
            call_target,
            next_utxo_id_before,
            next_utxo_id_after: next_utxo_id,
            pending_ctor_key_before,
            pending_ctor_key_after: pending_ctor_key,
            resolver_address,
            resolver_value,
            resolver_read,
            resolver_write,
        })
    }

    wit
}

pub(crate) struct Wit {
    pub(crate) opcode: Opcode,
    pub(crate) expected_arguments: Option<Vec<F>>,
    pub(crate) expected_method: Option<[F; 8]>,
    pub(crate) expected_result: Option<Vec<F>>,
    pub(crate) curr_before: CoroutineId,
    pub(crate) curr_after: CoroutineId,
    pub(crate) curr_phase_before: CurrPhase,
    pub(crate) curr_phase_after: CurrPhase,
    pub(crate) call_sp_before: F,
    pub(crate) call_sp_after: F,
    pub(crate) call_target: CoroutineId,
    pub(crate) next_utxo_id_before: u32,
    pub(crate) next_utxo_id_after: u32,
    pub(crate) pending_ctor_key_before: Option<(CoroutineId, ResourceHandle)>,
    pub(crate) pending_ctor_key_after: Option<(CoroutineId, ResourceHandle)>,
    pub(crate) resolver_address: (CoroutineId, ResourceHandle),
    pub(crate) resolver_value: CoroutineId,
    pub(crate) resolver_read: bool,
    pub(crate) resolver_write: bool,
}
