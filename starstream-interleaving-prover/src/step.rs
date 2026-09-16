use std::collections::HashMap;

use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::{MethodHash, ResourceHandle, Step, Trace};

use crate::{
    ivc_state::{CoroutineId, CurrPhase, TxPhase},
    opcode::Opcode,
};

fn encode_method_hash(method: MethodHash) -> [F; 8] {
    method.0.map(|word| F::new(u64::from(word)))
}

pub(crate) fn normalize(trace: &Trace) -> NormalizedTrace {
    let mut method_table = trace
        .0
        .iter()
        .filter_map(|step| match step {
            Step::RegisterMethod { method }
            | Step::PreloadMethod { method }
            | Step::ReadAbi { method }
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
    let mut tx = TransactionState {
        phase: TxPhase::Loading,
        last_input_has_abi: true,
        output_cursor: 0,
        coordinator_finalized: false,
        abi_read_remaining: 0,
        abi_read_ordinal: 0,
    };
    let mut curr = CoroutineId::Coord(1);
    let mut curr_phase = CurrPhase::Executing;
    // Quint's initial coordinator frame occupies the first stack slot.
    let mut callers = vec![CoroutineId::Coord(0)];
    let mut call_sp = F::new(1);
    let mut next_utxo_id = 0u32;
    let mut pending_ctor_key = None;
    let mut resource_resolver = HashMap::new();
    let mut abi_generations: HashMap<CoroutineId, u32> = HashMap::new();
    let mut abi_method_counts: HashMap<CoroutineId, u32> = HashMap::new();
    let mut enabled_method_log: Vec<(CoroutineId, u32, u32, u32)> = vec![];
    // Preserve first exact-entry / last membership lookup, including duplicates.
    let mut log_address_by_entry = HashMap::new();
    let mut last_log_membership = HashMap::new();
    let mut commitments = HashMap::new();
    let mut io_chain = [F::ZERO; 4];

    for step in &trace.0 {
        let opcode = Opcode::from(step);
        let tx_before = tx;
        if opcode.is_execution() {
            tx.phase = TxPhase::Running;
        }
        let boundary_utxo = match opcode {
            Opcode::SetStorage => CoroutineId::Utxo(next_utxo_id),
            Opcode::PreloadMethod => CoroutineId::Utxo(next_utxo_id.saturating_sub(1)),
            Opcode::GetStorage | Opcode::SkipConsumed => CoroutineId::Utxo(tx.output_cursor),
            Opcode::ReadAbi => CoroutineId::Utxo(tx.output_cursor.saturating_sub(1)),
            _ => CoroutineId::Coord(0),
        };
        let curr_before = curr;
        let event_owner = if opcode == Opcode::FinalizeCoordinator {
            CoroutineId::Coord(1)
        } else if matches!(
            opcode,
            Opcode::SetStorage | Opcode::GetStorage | Opcode::SkipConsumed
        ) {
            boundary_utxo
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
            Step::ReadAbi { method } => {
                method_hash = Some(encode_method_hash(*method));
                method_index = method_indices[method];
                abi_generation_address = boundary_utxo;
                abi_generation_before = abi_generations.get(&boundary_utxo).copied().unwrap_or(0);
                enabled_method_log_utxo = boundary_utxo;
                enabled_method_log_generation = abi_generation_before;
                enabled_method_log_address = log_address_by_entry
                    .get(&(
                        boundary_utxo,
                        method_index,
                        abi_generation_before,
                        tx.abi_read_ordinal,
                    ))
                    .copied()
                    .unwrap_or(enabled_method_log_len_before);
                tx.abi_read_ordinal = tx.abi_read_ordinal.wrapping_add(1);
                tx.abi_read_remaining = tx.abi_read_remaining.wrapping_sub(1);
            }
            Step::SetStorage { storage, resource } => {
                expected_arguments = Some(storage.words32().map(|x| F::new(u64::from(x))).to_vec());
                next_utxo_id = next_utxo_id
                    .checked_add(1)
                    .expect("UTXO allocator fits u32");
                resolver_address = (curr_before, resource.0);
                resolver_value = boundary_utxo;
                resolver_write = true;
                resource_resolver.insert(resolver_address, resolver_value);
                tx.last_input_has_abi = false;
            }
            Step::FinishTransaction => tx.phase = TxPhase::Finished,
            Step::GetStorage { storage } => {
                tx.abi_read_remaining = abi_method_counts.get(&boundary_utxo).copied().unwrap_or(0);
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
            Step::FinalizeCoordinator => tx.coordinator_finalized = true,
            Step::NewUtxo {
                arguments,
                resource,
            } => {
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
                callers.push(curr_before);
                pending_ctor_key = Some((curr_before, resource.0));
                curr_after = target;
                call_target = target;
                resolver_address = (curr_before, resource.0);
            }
            Step::EnterConstructor { arguments } => {
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
                    boundary_utxo
                } else {
                    curr_before
                };
                method_hash.replace(encode_method_hash(*method));
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
                expected_arguments.replace(
                    arguments
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );
                method_hash.replace(encode_method_hash(*method));
                expected_result.replace(
                    result
                        .0
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );
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
                enabled_method_log_address = last_log_membership
                    .get(&(target, method_index, abi_generation_before))
                    .copied()
                    .unwrap_or(enabled_method_log_len_before);
            }
            Step::EnterMethod { method, arguments } => {
                expected_arguments.replace(
                    arguments
                        .words32()
                        .iter()
                        .map(|&x| F::new(x as u64))
                        .collect(),
                );
                method_hash.replace(encode_method_hash(*method));
            }
        }

        curr = curr_after;

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
            abi_method_count_before = abi_method_counts.get(&boundary_utxo).copied().unwrap_or(0);
        }
        let mut row = Wit {
            io_before: io_chain,
            io_after: io_chain,
            abi_method_count_before,
            abi_method_count_after,
            tx_before,
            tx_after: tx,
            boundary_utxo,
            event_owner,
            commitment_before,
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

pub(crate) struct Wit {
    pub(crate) io_before: [F; 4],
    pub(crate) io_after: [F; 4],
    pub(crate) tx_before: TransactionState,
    pub(crate) tx_after: TransactionState,
    pub(crate) boundary_utxo: CoroutineId,
    pub(crate) event_owner: CoroutineId,
    pub(crate) commitment_before: [F; 4],
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
    pub(crate) coordinator_finalized: bool,
    pub(crate) abi_read_remaining: u32,
    pub(crate) abi_read_ordinal: u32,
}

impl Wit {
    /// A circuit-only fixed point of the carried state, with inactive buses
    /// assigned zero. It goes through the same column assigner as execution.
    pub(crate) fn padding_after(&self) -> Self {
        Self {
            tx_before: self.tx_after,
            io_before: self.io_after,
            io_after: self.io_after,
            tx_after: self.tx_after,
            boundary_utxo: CoroutineId::Coord(0),
            event_owner: CoroutineId::Coord(0),
            commitment_before: [F::ZERO; 4],
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
            abi_method_count_before: 0,
            abi_method_count_after: 0,
            abi_generation_after: 0,
            enabled_method_log_address: 0,
            enabled_method_log_utxo: CoroutineId::Coord(0),
            enabled_method_log_generation: 0,
        }
    }
}
