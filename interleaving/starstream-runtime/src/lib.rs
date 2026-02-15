use sha2::{Digest, Sha256};
use starstream_interleaving_proof::commit;
use starstream_interleaving_spec::{
    CoroutineState, Hash, InterfaceId, InterleavingInstance, InterleavingWitness,
    LedgerEffectsCommitment, NewOutput, OutputRef, ProcessId, ProvenTransaction, Ref, UtxoId,
    Value, WasmModule, WitEffectOutput, WitLedgerEffect, builder::TransactionBuilder,
};
use std::collections::{HashMap, HashSet};
use wasmi::{
    Caller, Config, Engine, Linker, Memory, Store, TypedResumableCall, TypedResumableCallHostTrap,
    Val, errors::HostError,
};

mod trace_mermaid;
pub use trace_mermaid::{
    register_mermaid_decoder, register_mermaid_default_decoder, register_mermaid_process_labels,
};

#[doc(hidden)]
pub mod test_support;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("invalid proof: {0}")]
    InvalidProof(String),
    #[error("runtime error: {0}")]
    RuntimeError(String),
    #[error("wasmi error: {0}")]
    Wasmi(#[from] wasmi::Error),
}

pub type WasmProgram = Vec<u8>;

#[derive(Debug)]
struct Interrupt {}

impl std::fmt::Display for Interrupt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl HostError for Interrupt {}

fn args_to_hash(a: u64, b: u64, c: u64, d: u64) -> [u8; 32] {
    let mut buffer = [0u8; 32];
    buffer[0..8].copy_from_slice(&a.to_le_bytes());
    buffer[8..16].copy_from_slice(&b.to_le_bytes());
    buffer[16..24].copy_from_slice(&c.to_le_bytes());
    buffer[24..32].copy_from_slice(&d.to_le_bytes());
    buffer
}

fn snapshot_globals(
    store: &Store<RuntimeState>,
    globals: &[wasmi::Global],
) -> Result<Vec<Value>, Error> {
    let mut values = Vec::with_capacity(globals.len());
    for global in globals {
        let val = global.get(store);
        let value = match val {
            Val::I64(v) => Value(v as u64),
            Val::I32(v) => Value(v as u32 as u64),
            _ => {
                return Err(Error::RuntimeError(
                    "unsupported global type (only i32/i64 supported)".into(),
                ));
            }
        };
        values.push(value);
    }
    Ok(values)
}

fn restore_globals(
    store: &mut Store<RuntimeState>,
    globals: &[wasmi::Global],
    values: &[Value],
) -> Result<(), Error> {
    if globals.len() != values.len() {
        return Err(Error::RuntimeError(format!(
            "global count mismatch: expected {}, got {}",
            globals.len(),
            values.len()
        )));
    }

    for (global, value) in globals.iter().zip(values.iter()) {
        if global.ty(&mut *store).mutability().is_const() {
            continue;
        }
        let val = match global.ty(&mut *store).content() {
            wasmi::ValType::I64 => Val::I64(value.0 as i64),
            wasmi::ValType::I32 => Val::I32(value.0 as i32),
            _ => {
                return Err(Error::RuntimeError(
                    "unsupported global type (only i32/i64 supported)".into(),
                ));
            }
        };
        global
            .set(&mut *store, val)
            .map_err(|e| Error::RuntimeError(e.to_string()))?;
    }

    Ok(())
}

fn suspend_with_effect<T>(
    caller: &mut Caller<'_, RuntimeState>,
    effect: WitLedgerEffect,
) -> Result<T, wasmi::Error> {
    let current_pid = caller.data().current_process;
    caller
        .data_mut()
        .traces
        .entry(current_pid)
        .or_default()
        .push(effect);
    Err(wasmi::Error::host(Interrupt {}))
}

fn effect_result_arity(effect: &WitLedgerEffect) -> usize {
    match effect {
        WitLedgerEffect::Resume { .. }
        | WitLedgerEffect::Activation { .. }
        | WitLedgerEffect::Init { .. } => 2,
        WitLedgerEffect::ProgramHash { .. } => 4,
        WitLedgerEffect::NewUtxo { .. }
        | WitLedgerEffect::NewCoord { .. }
        | WitLedgerEffect::GetHandlerFor { .. }
        | WitLedgerEffect::NewRef { .. } => 1,
        WitLedgerEffect::RefGet { .. } => 4,
        WitLedgerEffect::InstallHandler { .. }
        | WitLedgerEffect::UninstallHandler { .. }
        | WitLedgerEffect::Burn { .. }
        | WitLedgerEffect::Yield { .. }
        | WitLedgerEffect::Bind { .. }
        | WitLedgerEffect::Unbind { .. }
        | WitLedgerEffect::RefPush { .. }
        | WitLedgerEffect::RefWrite { .. } => 0,
    }
}

pub struct UnprovenTransaction {
    pub inputs: Vec<UtxoId>,
    pub input_states: Vec<CoroutineState>,
    pub input_ownership: Vec<Option<ProcessId>>,
    pub programs: Vec<WasmProgram>,
    pub is_utxo: Vec<bool>,
    pub entrypoint: usize,
}

pub struct RuntimeState {
    pub traces: HashMap<ProcessId, Vec<WitLedgerEffect>>,
    pub interleaving: Vec<(ProcessId, WitLedgerEffect)>,
    pub current_process: ProcessId,
    pub memories: HashMap<ProcessId, Memory>,

    pub handler_stack: HashMap<InterfaceId, Vec<ProcessId>>,
    pub ref_store: HashMap<Ref, Vec<Value>>,
    pub ref_sizes: HashMap<Ref, usize>,
    pub ref_state: HashMap<ProcessId, (Ref, usize, usize)>, // (ref, elem_offset, size_words)
    pub next_ref: u64,

    pub pending_activation: HashMap<ProcessId, (Ref, ProcessId)>,
    pub pending_init: HashMap<ProcessId, (Ref, ProcessId)>,
    pub globals: HashMap<ProcessId, Vec<Value>>,

    pub ownership: HashMap<ProcessId, Option<ProcessId>>,
    pub process_hashes: HashMap<ProcessId, Hash<WasmModule>>,
    pub is_utxo: HashMap<ProcessId, bool>,
    pub allocated_processes: HashSet<ProcessId>,
    pub yield_to: HashMap<ProcessId, ProcessId>,
    pub on_yield: HashMap<ProcessId, bool>,

    pub must_burn: HashSet<ProcessId>,
    pub n_new: usize,
    pub n_coord: usize,
}

pub struct Runtime {
    pub engine: Engine,
    pub linker: Linker<RuntimeState>,
    pub store: Store<RuntimeState>,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        let config = Config::default();
        let engine = Engine::new(&config);
        let mut linker = Linker::new(&engine);

        let state = RuntimeState {
            traces: HashMap::new(),
            interleaving: Vec::new(),
            current_process: ProcessId(0),
            memories: HashMap::new(),
            handler_stack: HashMap::new(),
            ref_store: HashMap::new(),
            ref_sizes: HashMap::new(),
            ref_state: HashMap::new(),
            next_ref: 0,
            pending_activation: HashMap::new(),
            pending_init: HashMap::new(),
            globals: HashMap::new(),
            ownership: HashMap::new(),
            process_hashes: HashMap::new(),
            is_utxo: HashMap::new(),
            allocated_processes: HashSet::new(),
            yield_to: HashMap::new(),
            on_yield: HashMap::new(),
            must_burn: HashSet::new(),
            n_new: 0,
            n_coord: 1,
        };

        let store = Store::new(&engine, state);

        linker
            .func_wrap(
                "env",
                "starstream_resume",
                |mut caller: Caller<'_, RuntimeState>,
                 target: u64,
                 val: u64|
                 -> Result<(u64, u64), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let target = ProcessId(target as usize);
                    let val = Ref(val);
                    let ret = WitEffectOutput::Thunk;

                    caller
                        .data_mut()
                        .pending_activation
                        .insert(target, (val, current_pid));

                    let was_on_yield = caller.data().on_yield.get(&target).copied().unwrap_or(true);
                    if was_on_yield {
                        caller.data_mut().yield_to.insert(target, current_pid);
                        caller.data_mut().on_yield.insert(target, false);
                    }

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::Resume {
                            target,
                            val,
                            ret,
                            caller: WitEffectOutput::Thunk,
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_yield",
                |mut caller: Caller<'_, RuntimeState>, val: u64| -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    caller.data_mut().on_yield.insert(current_pid, true);
                    suspend_with_effect(&mut caller, WitLedgerEffect::Yield { val: Ref(val) })
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_new_utxo",
                |mut caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64,
                 val: u64|
                 -> Result<u64, wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let h = Hash(args_to_hash(h0, h1, h2, h3), std::marker::PhantomData);
                    let val = Ref(val);

                    let mut found_id = None;
                    let limit = caller.data().process_hashes.len();
                    for i in 0..limit {
                        let pid = ProcessId(i);
                        if !caller.data().allocated_processes.contains(&pid)
                            && let Some(ph) = caller.data().process_hashes.get(&pid)
                            && *ph == h
                            && let Some(&is_u) = caller.data().is_utxo.get(&pid)
                            && is_u
                        {
                            found_id = Some(pid);
                            break;
                        }
                    }
                    let id = found_id.ok_or(wasmi::Error::new("no matching utxo process found"))?;
                    caller.data_mut().allocated_processes.insert(id);

                    caller
                        .data_mut()
                        .pending_init
                        .insert(id, (val, current_pid));
                    caller.data_mut().n_new += 1;

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::NewUtxo {
                            program_hash: h,
                            val,
                            id: WitEffectOutput::Resolved(id),
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_new_coord",
                |mut caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64,
                 val: u64|
                 -> Result<u64, wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let h = Hash(args_to_hash(h0, h1, h2, h3), std::marker::PhantomData);
                    let val = Ref(val);

                    let mut found_id = None;
                    let limit = caller.data().process_hashes.len();
                    for i in 0..limit {
                        let pid = ProcessId(i);
                        if !caller.data().allocated_processes.contains(&pid)
                            && let Some(ph) = caller.data().process_hashes.get(&pid)
                            && *ph == h
                            && let Some(&is_u) = caller.data().is_utxo.get(&pid)
                            && !is_u
                        {
                            found_id = Some(pid);
                            break;
                        }
                    }
                    let id =
                        found_id.ok_or(wasmi::Error::new("no matching coord process found"))?;
                    caller.data_mut().allocated_processes.insert(id);

                    caller
                        .data_mut()
                        .pending_init
                        .insert(id, (val, current_pid));
                    caller.data_mut().n_coord += 1;

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::NewCoord {
                            program_hash: h,
                            val,
                            id: WitEffectOutput::Resolved(id),
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_install_handler",
                |mut caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64|
                 -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let interface_id = Hash(args_to_hash(h0, h1, h2, h3), std::marker::PhantomData);
                    caller
                        .data_mut()
                        .handler_stack
                        .entry(interface_id)
                        .or_default()
                        .push(current_pid);
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::InstallHandler { interface_id },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_uninstall_handler",
                |mut caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64|
                 -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let interface_id = Hash(args_to_hash(h0, h1, h2, h3), std::marker::PhantomData);
                    let stack = caller
                        .data_mut()
                        .handler_stack
                        .get_mut(&interface_id)
                        .ok_or(wasmi::Error::new("handler stack not found"))?;
                    if stack.pop() != Some(current_pid) {
                        return Err(wasmi::Error::new("uninstall handler mismatch"));
                    }
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::UninstallHandler { interface_id },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_get_handler_for",
                |mut caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64|
                 -> Result<u64, wasmi::Error> {
                    let interface_id = Hash(args_to_hash(h0, h1, h2, h3), std::marker::PhantomData);
                    let handler_id = {
                        let stack = caller
                            .data()
                            .handler_stack
                            .get(&interface_id)
                            .ok_or(wasmi::Error::new("handler stack not found"))?;
                        *stack
                            .last()
                            .ok_or(wasmi::Error::new("handler stack empty"))?
                    };
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::GetHandlerFor {
                            interface_id,
                            handler_id: WitEffectOutput::Resolved(handler_id),
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_activation",
                |mut caller: Caller<'_, RuntimeState>| -> Result<(u64, u64), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let (val, caller_id) = {
                        let (val, caller_id) = caller
                            .data()
                            .pending_activation
                            .get(&current_pid)
                            .ok_or(wasmi::Error::new("no pending activation"))?;
                        (*val, *caller_id)
                    };
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::Activation {
                            val: WitEffectOutput::Resolved(val),
                            caller: WitEffectOutput::Resolved(caller_id),
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_init",
                |mut caller: Caller<'_, RuntimeState>| -> Result<(u64, u64), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let (val, caller_id) = {
                        let (val, caller_id) = caller
                            .data()
                            .pending_init
                            .get(&current_pid)
                            .ok_or(wasmi::Error::new("no pending init"))?;
                        (*val, *caller_id)
                    };
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::Init {
                            val: WitEffectOutput::Resolved(val),
                            caller: WitEffectOutput::Resolved(caller_id),
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_new_ref",
                |mut caller: Caller<'_, RuntimeState>, size: u64| -> Result<u64, wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let size_words = size as usize;
                    let size_elems = size_words
                        .checked_mul(starstream_interleaving_spec::REF_PUSH_WIDTH)
                        .ok_or(wasmi::Error::new("ref size overflow"))?;
                    let ref_id = Ref(caller.data().next_ref);
                    caller.data_mut().next_ref += size_elems as u64;

                    caller
                        .data_mut()
                        .ref_store
                        .insert(ref_id, vec![Value(0); size_elems]);
                    caller.data_mut().ref_sizes.insert(ref_id, size_words);
                    caller
                        .data_mut()
                        .ref_state
                        .insert(current_pid, (ref_id, 0, size_words));

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::NewRef {
                            size: size_words,
                            ret: WitEffectOutput::Resolved(ref_id),
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_ref_push",
                |mut caller: Caller<'_, RuntimeState>,
                 val_0: u64,
                 val_1: u64,
                 val_2: u64,
                 val_3: u64|
                 -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let vals = [Value(val_0), Value(val_1), Value(val_2), Value(val_3)];
                    let (ref_id, offset, size_words) = *caller
                        .data()
                        .ref_state
                        .get(&current_pid)
                        .ok_or(wasmi::Error::new("no ref state"))?;

                    let store = caller
                        .data_mut()
                        .ref_store
                        .get_mut(&ref_id)
                        .ok_or(wasmi::Error::new("ref not found"))?;

                    let elem_offset = offset;
                    for (i, val) in vals.iter().enumerate() {
                        if let Some(pos) = store.get_mut(elem_offset + i) {
                            *pos = *val;
                        }
                    }

                    caller.data_mut().ref_state.insert(
                        current_pid,
                        (
                            ref_id,
                            elem_offset + starstream_interleaving_spec::REF_PUSH_WIDTH,
                            size_words,
                        ),
                    );

                    suspend_with_effect(&mut caller, WitLedgerEffect::RefPush { vals })
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_ref_write",
                |mut caller: Caller<'_, RuntimeState>,
                 reff: u64,
                 offset: u64,
                 val_0: u64,
                 val_1: u64,
                 val_2: u64,
                 val_3: u64|
                 -> Result<(), wasmi::Error> {
                    let ref_id = Ref(reff);
                    let offset_words = offset as usize;

                    let size = *caller
                        .data()
                        .ref_sizes
                        .get(&ref_id)
                        .ok_or(wasmi::Error::new("ref size not found"))?;

                    if offset_words >= size {
                        return Err(wasmi::Error::new("ref write overflow"));
                    }

                    let vals = [Value(val_0), Value(val_1), Value(val_2), Value(val_3)];
                    let store = caller
                        .data_mut()
                        .ref_store
                        .get_mut(&ref_id)
                        .ok_or(wasmi::Error::new("ref not found"))?;

                    let elem_offset = offset_words * starstream_interleaving_spec::REF_WRITE_WIDTH;
                    for (i, val) in vals.iter().enumerate() {
                        store[elem_offset + i] = *val;
                    }

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::RefWrite {
                            reff: ref_id,
                            offset: offset_words,
                            vals,
                        },
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_ref_get",
                |mut caller: Caller<'_, RuntimeState>,
                 reff: u64,
                 offset: u64|
                 -> Result<(i64, i64, i64, i64), wasmi::Error> {
                    let ref_id = Ref(reff);
                    let offset_words = offset as usize;
                    let store = caller
                        .data()
                        .ref_store
                        .get(&ref_id)
                        .ok_or(wasmi::Error::new("ref not found"))?;
                    let size = *caller
                        .data()
                        .ref_sizes
                        .get(&ref_id)
                        .ok_or(wasmi::Error::new("ref size not found"))?;
                    if offset_words >= size {
                        return Err(wasmi::Error::new("ref get overflow"));
                    }
                    let mut ret = [Value::nil(); starstream_interleaving_spec::REF_GET_WIDTH];
                    for (i, slot) in ret.iter_mut().enumerate() {
                        let idx = (offset_words * starstream_interleaving_spec::REF_GET_WIDTH) + i;
                        if idx < size * starstream_interleaving_spec::REF_GET_WIDTH {
                            *slot = store[idx];
                        }
                    }
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::RefGet {
                            reff: ref_id,
                            offset: offset_words,
                            ret: WitEffectOutput::Resolved(ret),
                        },
                    )?;
                    Ok((
                        ret[0].0 as i64,
                        ret[1].0 as i64,
                        ret[2].0 as i64,
                        ret[3].0 as i64,
                    ))
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_bind",
                |mut caller: Caller<'_, RuntimeState>, owner_id: u64| -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let owner_id = ProcessId(owner_id as usize);
                    caller
                        .data_mut()
                        .ownership
                        .insert(current_pid, Some(owner_id));
                    suspend_with_effect(&mut caller, WitLedgerEffect::Bind { owner_id })
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_unbind",
                |mut caller: Caller<'_, RuntimeState>, token_id: u64| -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    let token_id = ProcessId(token_id as usize);
                    if caller.data().ownership.get(&token_id) != Some(&Some(current_pid)) {
                        eprintln!(
                            "unbind called by non-owner: token_id={}, current_pid={}",
                            token_id.0, current_pid.0
                        );
                    }
                    caller.data_mut().ownership.insert(token_id, None);
                    suspend_with_effect(&mut caller, WitLedgerEffect::Unbind { token_id })
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_burn",
                |mut caller: Caller<'_, RuntimeState>, ret: u64| -> Result<(), wasmi::Error> {
                    let current_pid = caller.data().current_process;
                    caller.data_mut().must_burn.insert(current_pid);
                    suspend_with_effect(&mut caller, WitLedgerEffect::Burn { ret: Ref(ret) })
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_get_program_hash",
                |mut caller: Caller<'_, RuntimeState>,
                 target_pid: u64|
                 -> Result<(u64, u64, u64, u64), wasmi::Error> {
                    let target = ProcessId(target_pid as usize);
                    let program_hash = *caller
                        .data()
                        .process_hashes
                        .get(&target)
                        .ok_or(wasmi::Error::new("process hash not found"))?;

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::ProgramHash {
                            target,
                            program_hash: WitEffectOutput::Resolved(program_hash),
                        },
                    )
                },
            )
            .unwrap();

        Self {
            engine,
            linker,
            store,
        }
    }
}

impl UnprovenTransaction {
    fn get_globals(&self, pid: usize, state: &RuntimeState) -> Result<Vec<Value>, Error> {
        state
            .globals
            .get(&ProcessId(pid))
            .cloned()
            .ok_or_else(|| Error::RuntimeError(format!("No globals for pid {}", pid)))
    }

    pub fn prove(self) -> Result<ProvenTransaction, Error> {
        let (instance, state, witness) = self.execute()?;

        let proof = starstream_interleaving_proof::prove(instance.clone(), witness.clone())
            .map_err(|e| Error::RuntimeError(e.to_string()))?;

        trace_mermaid::emit_trace_mermaid(&instance, &state);

        let mut builder = TransactionBuilder::new();
        builder = builder.with_entrypoint(self.entrypoint);

        let n_inputs = instance.n_inputs;
        if self.inputs.len() != n_inputs {
            return Err(Error::RuntimeError(format!(
                "Input count mismatch: expected {}, got {}",
                n_inputs,
                self.inputs.len()
            )));
        }

        let traces = &witness.traces;

        // Inputs
        for (i, trace) in traces.iter().enumerate().take(n_inputs) {
            let trace = trace.clone();
            let utxo_id = self.inputs[i].clone();
            let host_calls_root = instance.host_calls_roots[i].clone();

            let continuation = if instance.must_burn[i] {
                None
            } else {
                let globals = self.get_globals(i, &state)?;
                Some(CoroutineState { pc: 0, globals })
            };

            builder = builder.with_input_and_trace_commitment(
                utxo_id,
                continuation,
                trace,
                host_calls_root,
            );
        }

        // New Outputs
        for (i, trace) in traces
            .iter()
            .enumerate()
            .skip(n_inputs)
            .take(instance.n_new)
        {
            let trace = trace.clone();
            let globals = self.get_globals(i, &state)?;
            let contract_hash = state.process_hashes[&ProcessId(i)];
            let host_calls_root = instance.host_calls_roots[i].clone();

            builder = builder.with_fresh_output_and_trace_commitment(
                NewOutput {
                    state: CoroutineState { pc: 0, globals },
                    contract_hash,
                },
                trace,
                host_calls_root,
            );
        }

        // Coords
        for (i, trace) in traces
            .iter()
            .enumerate()
            .skip(n_inputs + instance.n_new)
            .take(instance.n_coords)
        {
            let trace = trace.clone();
            let contract_hash = state.process_hashes[&ProcessId(i)];
            let host_calls_root = instance.host_calls_roots[i].clone();
            builder = builder.with_coord_script_and_trace_commitment(
                contract_hash,
                trace,
                host_calls_root,
            );
        }

        // Ownership
        for (token, owner_opt) in state.ownership {
            if let Some(owner) = owner_opt {
                builder =
                    builder.with_ownership(OutputRef::from(token.0), OutputRef::from(owner.0));
            }
        }

        Ok(builder.build(proof))
    }

    pub fn to_instance(&self) -> InterleavingInstance {
        self.execute().unwrap().0
    }

    pub fn execute(
        &self,
    ) -> Result<(InterleavingInstance, RuntimeState, InterleavingWitness), Error> {
        let mut runtime = Runtime::new();

        let n_inputs = self.inputs.len();
        if !self.input_states.is_empty() && self.input_states.len() != n_inputs {
            return Err(Error::RuntimeError(format!(
                "Input state count mismatch: expected {}, got {}",
                n_inputs,
                self.input_states.len()
            )));
        }
        if !self.input_ownership.is_empty() && self.input_ownership.len() != n_inputs {
            return Err(Error::RuntimeError(format!(
                "Input ownership count mismatch: expected {}, got {}",
                n_inputs,
                self.input_ownership.len()
            )));
        }

        let mut instances = Vec::new();
        let mut process_table = Vec::new();
        let mut globals_by_pid: Vec<Vec<wasmi::Global>> = Vec::new();

        for (pid, program_bytes) in self.programs.iter().enumerate() {
            let mut hasher = Sha256::new();
            hasher.update(program_bytes);
            let result = hasher.finalize();
            let mut h = [0u8; 32];
            h.copy_from_slice(&result);
            let hash = Hash(h, std::marker::PhantomData);

            runtime
                .store
                .data_mut()
                .process_hashes
                .insert(ProcessId(pid), hash);

            // Populate is_utxo map
            runtime
                .store
                .data_mut()
                .is_utxo
                .insert(ProcessId(pid), self.is_utxo[pid]);

            process_table.push(hash);

            let module = wasmi::Module::new(&runtime.engine, program_bytes)?;
            let instance = runtime
                .linker
                .instantiate_and_start(&mut runtime.store, &module)?;

            // Store memory in RuntimeState for hash reading
            if let Some(extern_) = instance.get_export(&runtime.store, "memory")
                && let Some(memory) = extern_.into_memory()
            {
                runtime
                    .store
                    .data_mut()
                    .memories
                    .insert(ProcessId(pid), memory);
            }

            let mut globals = Vec::new();
            for export in instance.exports(&runtime.store) {
                let name = export.name().to_string();
                if let Some(global) = export.into_global() {
                    globals.push((name, global));
                }
            }
            globals.sort_by(|a, b| a.0.cmp(&b.0));
            let globals: Vec<wasmi::Global> = globals.into_iter().map(|(_, g)| g).collect();

            if pid < n_inputs && !self.input_states.is_empty() {
                let values = &self.input_states[pid].globals;
                restore_globals(&mut runtime.store, &globals, values)?;
            }

            globals_by_pid.push(globals);
            instances.push(instance);
        }

        if !self.input_ownership.is_empty() {
            for (pid, owner_opt) in self.input_ownership.iter().enumerate().take(n_inputs) {
                runtime
                    .store
                    .data_mut()
                    .ownership
                    .insert(ProcessId(pid), *owner_opt);
            }
        }

        // Map of suspended processes
        let mut resumables: HashMap<ProcessId, TypedResumableCallHostTrap<()>> = HashMap::new();

        // Start entrypoint
        let mut current_pid = ProcessId(self.entrypoint);
        runtime
            .store
            .data_mut()
            .allocated_processes
            .insert(current_pid);

        runtime.store.data_mut().current_process = current_pid;

        // Initial argument? 0?
        let mut next_args = [0u64; 5];

        loop {
            runtime.store.data_mut().current_process = current_pid;

            let result = if let Some(continuation) = resumables.remove(&current_pid) {
                let n_results = {
                    let traces = &runtime.store.data().traces;
                    let trace = traces.get(&current_pid).expect("trace exists");
                    let last = trace.last().expect("trace not empty");
                    effect_result_arity(last)
                };

                // Update previous effect with return value
                let traces = &mut runtime.store.data_mut().traces;
                if let Some(trace) = traces.get_mut(&current_pid)
                    && let Some(last) = trace.last_mut()
                {
                    match last {
                        WitLedgerEffect::Resume { ret, caller, .. } => {
                            *ret = WitEffectOutput::Resolved(Ref(next_args[0]));
                            *caller =
                                WitEffectOutput::Resolved(Some(ProcessId(next_args[1] as usize)));
                        }
                        _ => {}
                    }
                }

                let vals = [
                    Val::I64(next_args[0] as i64),
                    Val::I64(next_args[1] as i64),
                    Val::I64(next_args[2] as i64),
                    Val::I64(next_args[3] as i64),
                    Val::I64(next_args[4] as i64),
                ];

                continuation.resume(&mut runtime.store, &vals[..n_results])?
            } else {
                let instance = instances[current_pid.0];
                // Start with _start, 0 args, 0 results
                let func = instance.get_typed_func::<(), ()>(&runtime.store, "_start")?;
                func.call_resumable(&mut runtime.store, ()).unwrap()
            };

            match result {
                TypedResumableCall::Finished(_) => {
                    // Process finished naturally.
                    break;
                }
                TypedResumableCall::HostTrap(invocation) => {
                    // It suspended.

                    // Inspect the last effect
                    let last_effect = {
                        let traces = &runtime.store.data().traces;
                        let trace = traces
                            .get(&current_pid)
                            .expect("trace exists after suspend");
                        trace.last().expect("trace not empty after suspend").clone()
                    };

                    runtime
                        .store
                        .data_mut()
                        .interleaving
                        .push((current_pid, last_effect.clone()));

                    resumables.insert(current_pid, invocation);

                    match last_effect {
                        WitLedgerEffect::Resume { target, val, .. } => {
                            next_args = [val.0, current_pid.0 as u64, 0, 0, 0];
                            current_pid = target;
                        }
                        WitLedgerEffect::Yield { val, .. } => {
                            let caller = *runtime
                                .store
                                .data()
                                .yield_to
                                .get(&current_pid)
                                .expect("yield on missing yield_to");
                            next_args = [val.0, current_pid.0 as u64, 0, 0, 0];
                            current_pid = caller;
                        }
                        WitLedgerEffect::Burn { .. } => {
                            let caller = *runtime
                                .store
                                .data()
                                .yield_to
                                .get(&current_pid)
                                .expect("burn on missing yield_to");
                            next_args = [0; 5];
                            current_pid = caller;
                        }
                        WitLedgerEffect::NewUtxo { id, .. } => {
                            next_args = [id.unwrap().0 as u64, 0, 0, 0, 0];
                        }
                        WitLedgerEffect::NewCoord { id, .. } => {
                            next_args = [id.unwrap().0 as u64, 0, 0, 0, 0];
                        }
                        WitLedgerEffect::GetHandlerFor { handler_id, .. } => {
                            next_args = [handler_id.unwrap().0 as u64, 0, 0, 0, 0];
                        }
                        WitLedgerEffect::Activation { val, caller } => {
                            next_args = [val.unwrap().0, caller.unwrap().0 as u64, 0, 0, 0];
                        }
                        WitLedgerEffect::Init { val, caller } => {
                            next_args = [val.unwrap().0, caller.unwrap().0 as u64, 0, 0, 0];
                        }
                        WitLedgerEffect::NewRef { ret, .. } => {
                            next_args = [ret.unwrap().0, 0, 0, 0, 0];
                        }
                        WitLedgerEffect::RefGet { ret, .. } => {
                            let ret = ret.unwrap();
                            next_args = [ret[0].0, ret[1].0, ret[2].0, ret[3].0, 0];
                        }
                        WitLedgerEffect::RefWrite { .. } => {
                            next_args = [0; 5];
                        }
                        WitLedgerEffect::ProgramHash { program_hash, .. } => {
                            let limbs = program_hash.unwrap().0;
                            next_args = [
                                u64::from_le_bytes(limbs[0..8].try_into().unwrap()),
                                u64::from_le_bytes(limbs[8..16].try_into().unwrap()),
                                u64::from_le_bytes(limbs[16..24].try_into().unwrap()),
                                u64::from_le_bytes(limbs[24..32].try_into().unwrap()),
                                0,
                            ];
                        }
                        _ => {
                            next_args = [0; 5];
                        }
                    }
                }
                TypedResumableCall::OutOfFuel(_) => {
                    todo!();
                }
            }
        }

        let mut host_calls_roots = Vec::new();
        let mut must_burn = Vec::new();
        let mut ownership_in = Vec::new();
        let mut ownership_out = Vec::new();
        let mut traces = Vec::new();

        let is_utxo = self.is_utxo.clone();
        let n_coords = runtime.store.data().n_coord;
        let n_new = runtime.store.data().n_new;
        let n_inputs = process_table.len() - n_new - n_coords;

        for pid in 0..self.programs.len() {
            let data = runtime.store.data();
            let trace = data
                .traces
                .get(&ProcessId(pid))
                .cloned()
                .unwrap_or_default();
            let mut commitment = LedgerEffectsCommitment::iv();
            for op in &trace {
                commitment = commit(commitment, op.clone());
            }
            host_calls_roots.push(commitment);
            traces.push(trace);

            if pid < n_inputs {
                must_burn.push(data.must_burn.contains(&ProcessId(pid)));
            }
        }

        let utxo_count = n_inputs + n_new;
        ownership_in.resize(utxo_count, None);
        ownership_out.resize(utxo_count, None);
        for pid in 0..utxo_count {
            let proc_id = ProcessId(pid);
            if pid < n_inputs && !self.input_ownership.is_empty() {
                ownership_in[pid] = self.input_ownership[pid];
            }
            ownership_out[pid] = runtime
                .store
                .data()
                .ownership
                .get(&proc_id)
                .copied()
                .flatten();
        }

        for (pid, globals) in globals_by_pid.iter().enumerate() {
            let globals = snapshot_globals(&runtime.store, globals)?;
            runtime
                .store
                .data_mut()
                .globals
                .insert(ProcessId(pid), globals);
        }

        let instance = InterleavingInstance {
            host_calls_roots,
            process_table,
            is_utxo,
            must_burn,
            n_inputs,
            n_new,
            n_coords,
            ownership_in,
            ownership_out,
            entrypoint: ProcessId(self.entrypoint),
            input_states: self.input_states.clone(),
        };

        let witness = starstream_interleaving_spec::InterleavingWitness { traces };

        Ok((instance, runtime.store.into_data(), witness))
    }
}
