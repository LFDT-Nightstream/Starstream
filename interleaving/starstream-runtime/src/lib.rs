use ark_ff::PrimeField;
use starstream_interleaving_proof::commit;
use starstream_interleaving_spec::{
    ArgName, CoroutineState, EffectDiscriminant, Hash, InterfaceId, InterleavingInstance,
    InterleavingWitness, LedgerEffectsCommitment, NewOutput, OutputRef, ProcessId,
    ProvenTransaction, Ref, UtxoId, Value, WasmModule, WitEffectOutput, WitLedgerEffect,
    builder::TransactionBuilder,
};
use std::collections::{HashMap, HashSet};
use wasmtime::{Caller, Config, Engine, Linker, Memory, Store, Val};

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
    #[error("wasmtime error: {0}")]
    Wasmtime(#[from] wasmtime::Error),
}

pub type WasmProgram = Vec<u8>;

fn pack_bytes_to_safe_limbs(bytes: &[u8]) -> Vec<ark_poseidon2::F> {
    let mut out = Vec::with_capacity(bytes.len().div_ceil(7));

    for chunk in bytes.chunks(7) {
        let mut limb = [0u8; 8];
        limb[..chunk.len()].copy_from_slice(chunk);
        out.push(ark_poseidon2::F::from(u64::from_le_bytes(limb)));
    }

    out
}

pub fn poseidon_program_hash(program_bytes: &[u8]) -> [u64; 4] {
    let mut msg = pack_bytes_to_safe_limbs("starstream/program_hash/v1/poseidon2".as_bytes());
    msg.push(ark_poseidon2::F::from(program_bytes.len() as u64));
    msg.extend(pack_bytes_to_safe_limbs(program_bytes));

    let hash = ark_poseidon2::sponge_12_trace(&msg).unwrap();

    let mut out = [0; 4];

    out[0] = hash[0].into_bigint().0[0];
    out[1] = hash[0].into_bigint().0[0];
    out[2] = hash[0].into_bigint().0[0];
    out[3] = hash[0].into_bigint().0[0];

    out
}

fn snapshot_globals(
    store: &mut Store<RuntimeState>,
    globals: &[wasmtime::Global],
) -> Result<Vec<Value>, Error> {
    let mut values = Vec::with_capacity(globals.len());
    for global in globals {
        let val = global.get(&mut *store);
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
    globals: &[wasmtime::Global],
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
            wasmtime::ValType::I64 => Val::I64(value.0 as i64),
            wasmtime::ValType::I32 => Val::I32(value.0 as i32),
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
    ret: T,
) -> Result<T, wasmtime::Error> {
    caller.data_mut().pending_host_effect = Some(effect);
    Ok(ret)
}

fn trap(msg: impl Into<String>) -> wasmtime::Error {
    wasmtime::Error::msg(msg.into())
}

fn write_u64_to_memory(
    caller: &mut Caller<'_, RuntimeState>,
    out_ptr: i32,
    lane: usize,
    value: u64,
) -> Result<(), wasmtime::Error> {
    let out_ptr = usize::try_from(out_ptr).map_err(|_| trap("negative out pointer"))?;
    let offset = out_ptr
        .checked_add(lane.saturating_mul(8))
        .ok_or_else(|| trap("out pointer overflow"))?;
    let memory = caller
        .get_export("memory")
        .and_then(|e| e.into_memory())
        .ok_or_else(|| trap("missing guest memory export"))?;
    memory
        .write(&mut *caller, offset, &value.to_le_bytes())
        .map_err(|e| trap(e.to_string()))
}

fn hash4<T>(a0: u64, a1: u64, a2: u64, a3: u64) -> Hash<T> {
    Hash([a0, a1, a2, a3], std::marker::PhantomData)
}

fn decode_optional_pid(encoded: u64) -> Option<ProcessId> {
    if encoded == 0 {
        None
    } else {
        Some(ProcessId((encoded - 1) as usize))
    }
}

fn decode_effect_from_commit_abi(
    discriminant: u64,
    args: [u64; 7],
) -> Result<WitLedgerEffect, String> {
    let disc = EffectDiscriminant::try_from(discriminant)
        .map_err(|e| format!("invalid starstream_trace discriminant: {}", e))?;
    let arg = |name: ArgName| args[name.idx()];

    let effect = match disc {
        EffectDiscriminant::Resume => WitLedgerEffect::Resume {
            target: ProcessId(arg(ArgName::Target) as usize),
            val: Ref(arg(ArgName::Val)),
            ret: WitEffectOutput::Resolved(Ref(arg(ArgName::Ret))),
            caller: WitEffectOutput::Resolved(decode_optional_pid(arg(ArgName::Caller))),
        },
        EffectDiscriminant::Yield => WitLedgerEffect::Yield {
            val: Ref(arg(ArgName::Val)),
        },
        EffectDiscriminant::NewUtxo => WitLedgerEffect::NewUtxo {
            program_hash: hash4(
                arg(ArgName::ProgramHash0),
                arg(ArgName::ProgramHash1),
                arg(ArgName::ProgramHash2),
                arg(ArgName::ProgramHash3),
            ),
            val: Ref(arg(ArgName::Val)),
            id: WitEffectOutput::Resolved(ProcessId(arg(ArgName::Target) as usize)),
        },
        EffectDiscriminant::NewCoord => WitLedgerEffect::NewCoord {
            program_hash: hash4(
                arg(ArgName::ProgramHash0),
                arg(ArgName::ProgramHash1),
                arg(ArgName::ProgramHash2),
                arg(ArgName::ProgramHash3),
            ),
            val: Ref(arg(ArgName::Val)),
            id: WitEffectOutput::Resolved(ProcessId(arg(ArgName::Target) as usize)),
        },
        EffectDiscriminant::InstallHandler => WitLedgerEffect::InstallHandler {
            interface_id: hash4(
                arg(ArgName::InterfaceId0),
                arg(ArgName::InterfaceId1),
                arg(ArgName::InterfaceId2),
                arg(ArgName::InterfaceId3),
            ),
        },
        EffectDiscriminant::UninstallHandler => WitLedgerEffect::UninstallHandler {
            interface_id: hash4(
                arg(ArgName::InterfaceId0),
                arg(ArgName::InterfaceId1),
                arg(ArgName::InterfaceId2),
                arg(ArgName::InterfaceId3),
            ),
        },
        EffectDiscriminant::GetHandlerFor => WitLedgerEffect::GetHandlerFor {
            interface_id: hash4(
                arg(ArgName::InterfaceId0),
                arg(ArgName::InterfaceId1),
                arg(ArgName::InterfaceId2),
                arg(ArgName::InterfaceId3),
            ),
            handler_id: WitEffectOutput::Resolved(ProcessId(arg(ArgName::Target) as usize)),
        },
        EffectDiscriminant::Burn => WitLedgerEffect::Burn {
            ret: Ref(arg(ArgName::Ret)),
        },
        EffectDiscriminant::Activation => WitLedgerEffect::Activation {
            val: WitEffectOutput::Resolved(Ref(arg(ArgName::Val))),
            caller: WitEffectOutput::Resolved(ProcessId(arg(ArgName::ActivationCaller) as usize)),
        },
        EffectDiscriminant::Init => WitLedgerEffect::Init {
            val: WitEffectOutput::Resolved(Ref(arg(ArgName::Val))),
            caller: WitEffectOutput::Resolved(ProcessId(arg(ArgName::ActivationCaller) as usize)),
        },
        EffectDiscriminant::NewRef => WitLedgerEffect::NewRef {
            size: arg(ArgName::Size) as usize,
            ret: WitEffectOutput::Resolved(Ref(arg(ArgName::Ret))),
        },
        EffectDiscriminant::RefPush => WitLedgerEffect::RefPush {
            vals: [
                Value(arg(ArgName::PackedRef0)),
                Value(arg(ArgName::PackedRef1)),
                Value(arg(ArgName::PackedRef2)),
                Value(arg(ArgName::PackedRef3)),
            ],
        },
        EffectDiscriminant::RefGet => WitLedgerEffect::RefGet {
            reff: Ref(arg(ArgName::Val)),
            offset: arg(ArgName::Offset) as usize,
            ret: WitEffectOutput::Resolved([
                Value(arg(ArgName::PackedRef0)),
                Value(arg(ArgName::PackedRef2)),
                Value(arg(ArgName::PackedRef4)),
                Value(arg(ArgName::PackedRef5)),
            ]),
        },
        EffectDiscriminant::Bind => WitLedgerEffect::Bind {
            owner_id: ProcessId(arg(ArgName::OwnerId) as usize),
        },
        EffectDiscriminant::Unbind => WitLedgerEffect::Unbind {
            token_id: ProcessId(arg(ArgName::TokenId) as usize),
        },
        EffectDiscriminant::ProgramHash => WitLedgerEffect::ProgramHash {
            target: ProcessId(arg(ArgName::Target) as usize),
            program_hash: WitEffectOutput::Resolved(hash4(
                arg(ArgName::ProgramHash0),
                arg(ArgName::ProgramHash1),
                arg(ArgName::ProgramHash2),
                arg(ArgName::ProgramHash3),
            )),
        },
        EffectDiscriminant::RefWrite => WitLedgerEffect::RefWrite {
            reff: Ref(arg(ArgName::Val)),
            offset: arg(ArgName::Offset) as usize,
            vals: [
                Value(arg(ArgName::PackedRef0)),
                Value(arg(ArgName::PackedRef2)),
                Value(arg(ArgName::PackedRef4)),
                Value(arg(ArgName::PackedRef5)),
            ],
        },
        EffectDiscriminant::Return => WitLedgerEffect::Return {},
        EffectDiscriminant::CallEffectHandler => WitLedgerEffect::CallEffectHandler {
            interface_id: hash4(
                arg(ArgName::InterfaceId0),
                arg(ArgName::InterfaceId1),
                arg(ArgName::InterfaceId2),
                arg(ArgName::InterfaceId3),
            ),
            val: Ref(arg(ArgName::Val)),
            ret: WitEffectOutput::Resolved(Ref(arg(ArgName::Ret))),
        },
    };
    Ok(effect)
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
    pub effect_traces: HashMap<ProcessId, Vec<WitLedgerEffect>>,
    pub suspended_effect: HashMap<ProcessId, WitLedgerEffect>,
    pub pending_host_effect: Option<WitLedgerEffect>,
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
        let engine = Engine::new(&config).expect("failed to create wasmtime engine");
        let mut linker = Linker::new(&engine);

        let state = RuntimeState {
            effect_traces: HashMap::new(),
            suspended_effect: HashMap::new(),
            pending_host_effect: None,
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
                "starstream_trace",
                |mut caller: Caller<'_, RuntimeState>,
                 discriminant: u64,
                 a0: u64,
                 a1: u64,
                 a2: u64,
                 a3: u64,
                 a4: u64,
                 a5: u64,
                 a6: u64|
                 -> Result<(), wasmtime::Error> {
                    let effect =
                        decode_effect_from_commit_abi(discriminant, [a0, a1, a2, a3, a4, a5, a6])
                            .map_err(|e| trap(e.to_string()))?;
                    let current_pid = caller.data().current_process;
                    caller
                        .data_mut()
                        .effect_traces
                        .entry(current_pid)
                        .or_default()
                        .push(effect);
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_resume",
                |mut caller: Caller<'_, RuntimeState>,
                 target: u64,
                 val: u64|
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let target = ProcessId(target as usize);
                    let val = Ref(val);
                    let ret = WitEffectOutput::Resolved(Ref(0));

                    caller
                        .data_mut()
                        .pending_activation
                        .insert(target, (val, current_pid));

                    let curr_is_utxo = caller
                        .data()
                        .is_utxo
                        .get(&current_pid)
                        .copied()
                        .unwrap_or(false);
                    let was_on_yield = caller.data().on_yield.get(&target).copied().unwrap_or(true);
                    if !curr_is_utxo && was_on_yield {
                        caller.data_mut().yield_to.insert(target, current_pid);
                        caller.data_mut().on_yield.insert(target, false);
                    }

                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::Resume {
                            target,
                            val,
                            ret,
                            caller: WitEffectOutput::Resolved(None),
                        },
                        (),
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_yield",
                |mut caller: Caller<'_, RuntimeState>, val: u64| -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    caller.data_mut().on_yield.insert(current_pid, true);
                    suspend_with_effect(&mut caller, WitLedgerEffect::Yield { val: Ref(val) }, ())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_return",
                |mut caller: Caller<'_, RuntimeState>| -> Result<(), wasmtime::Error> {
                    suspend_with_effect(&mut caller, WitLedgerEffect::Return {}, ())
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
                 -> Result<u64, wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let h = Hash([h0, h1, h2, h3], std::marker::PhantomData);
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
                    let id = found_id.ok_or(trap("no matching utxo process found"))?;
                    caller.data_mut().allocated_processes.insert(id);

                    caller
                        .data_mut()
                        .pending_init
                        .insert(id, (val, current_pid));
                    caller.data_mut().n_new += 1;

                    Ok(id.0 as u64)
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
                 -> Result<u64, wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let h = Hash([h0, h1, h2, h3], std::marker::PhantomData);
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
                    let id = found_id.ok_or(trap("no matching coord process found"))?;
                    caller.data_mut().allocated_processes.insert(id);

                    caller
                        .data_mut()
                        .pending_init
                        .insert(id, (val, current_pid));
                    caller.data_mut().n_coord += 1;

                    Ok(id.0 as u64)
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
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let interface_id = Hash([h0, h1, h2, h3], std::marker::PhantomData);
                    caller
                        .data_mut()
                        .handler_stack
                        .entry(interface_id)
                        .or_default()
                        .push(current_pid);
                    Ok(())
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
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let interface_id = Hash([h0, h1, h2, h3], std::marker::PhantomData);
                    let stack = caller
                        .data_mut()
                        .handler_stack
                        .get_mut(&interface_id)
                        .ok_or(trap("handler stack not found"))?;
                    if stack.pop() != Some(current_pid) {
                        return Err(trap("uninstall handler mismatch"));
                    }
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_get_handler_for",
                |caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64|
                 -> Result<u64, wasmtime::Error> {
                    let interface_id = Hash([h0, h1, h2, h3], std::marker::PhantomData);
                    let handler_id = {
                        let stack = caller
                            .data()
                            .handler_stack
                            .get(&interface_id)
                            .ok_or(trap("handler stack not found"))?;
                        *stack.last().ok_or(trap("handler stack empty"))?
                    };
                    Ok(handler_id.0 as u64)
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_call_effect_handler",
                |mut caller: Caller<'_, RuntimeState>,
                 h0: u64,
                 h1: u64,
                 h2: u64,
                 h3: u64,
                 val: u64|
                 -> Result<(), wasmtime::Error> {
                    let interface_id = Hash([h0, h1, h2, h3], std::marker::PhantomData);
                    suspend_with_effect(
                        &mut caller,
                        WitLedgerEffect::CallEffectHandler {
                            interface_id,
                            val: Ref(val),
                            ret: WitEffectOutput::Resolved(Ref(0)),
                        },
                        (),
                    )
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_activation",
                |mut caller: Caller<'_, RuntimeState>,
                 out_ptr: i32|
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let (val, caller_id) = caller
                        .data_mut()
                        .pending_activation
                        .remove(&current_pid)
                        .ok_or(trap("no pending activation"))?;
                    write_u64_to_memory(&mut caller, out_ptr, 0, val.0)?;
                    write_u64_to_memory(&mut caller, out_ptr, 1, caller_id.0 as u64)?;
                    Ok(())
                },
            )
            .unwrap();
        linker
            .func_wrap(
                "env",
                "starstream_untraced_activation",
                |mut caller: Caller<'_, RuntimeState>,
                 out_ptr: i32|
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let (val, caller_id) = caller
                        .data()
                        .pending_activation
                        .get(&current_pid)
                        .copied()
                        .ok_or(trap("no pending activation"))?;
                    write_u64_to_memory(&mut caller, out_ptr, 0, val.0)?;
                    write_u64_to_memory(&mut caller, out_ptr, 1, caller_id.0 as u64)?;
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_init",
                |mut caller: Caller<'_, RuntimeState>,
                 out_ptr: i32|
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let (val, caller_id) = {
                        let (val, caller_id) = caller
                            .data()
                            .pending_init
                            .get(&current_pid)
                            .ok_or(trap("no pending init"))?;
                        (*val, *caller_id)
                    };
                    write_u64_to_memory(&mut caller, out_ptr, 0, val.0)?;
                    write_u64_to_memory(&mut caller, out_ptr, 1, caller_id.0 as u64)?;
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_new_ref",
                |mut caller: Caller<'_, RuntimeState>, size: u64| -> Result<u64, wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let size_words = size as usize;
                    let size_elems = size_words
                        .checked_mul(starstream_interleaving_spec::REF_PUSH_WIDTH)
                        .ok_or(trap("ref size overflow"))?;
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

                    Ok(ref_id.0)
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
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let vals = [Value(val_0), Value(val_1), Value(val_2), Value(val_3)];
                    let (ref_id, offset, size_words) = *caller
                        .data()
                        .ref_state
                        .get(&current_pid)
                        .ok_or(trap("no ref state"))?;

                    let store = caller
                        .data_mut()
                        .ref_store
                        .get_mut(&ref_id)
                        .ok_or(trap("ref not found"))?;

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

                    Ok(())
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
                 -> Result<(), wasmtime::Error> {
                    let ref_id = Ref(reff);
                    let offset_words = offset as usize;

                    let size = *caller
                        .data()
                        .ref_sizes
                        .get(&ref_id)
                        .ok_or(trap("ref size not found"))?;

                    if offset_words >= size {
                        return Err(trap("ref write overflow"));
                    }

                    let vals = [Value(val_0), Value(val_1), Value(val_2), Value(val_3)];
                    let store = caller
                        .data_mut()
                        .ref_store
                        .get_mut(&ref_id)
                        .ok_or(trap("ref not found"))?;

                    let elem_offset = offset_words * starstream_interleaving_spec::REF_WRITE_WIDTH;
                    for (i, val) in vals.iter().enumerate() {
                        store[elem_offset + i] = *val;
                    }

                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_ref_get",
                |mut caller: Caller<'_, RuntimeState>,
                 reff: u64,
                 offset: u64,
                 out_ptr: i32|
                 -> Result<(), wasmtime::Error> {
                    let ref_id = Ref(reff);
                    let offset_words = offset as usize;
                    let store = caller
                        .data()
                        .ref_store
                        .get(&ref_id)
                        .ok_or(trap("ref not found"))?;
                    let size = *caller
                        .data()
                        .ref_sizes
                        .get(&ref_id)
                        .ok_or(trap("ref size not found"))?;
                    if offset_words >= size {
                        return Err(trap("ref get overflow"));
                    }
                    let mut ret = [Value::nil(); starstream_interleaving_spec::REF_GET_WIDTH];
                    for (i, slot) in ret.iter_mut().enumerate() {
                        let idx = (offset_words * starstream_interleaving_spec::REF_GET_WIDTH) + i;
                        if idx < size * starstream_interleaving_spec::REF_GET_WIDTH {
                            *slot = store[idx];
                        }
                    }
                    write_u64_to_memory(&mut caller, out_ptr, 0, ret[0].0)?;
                    write_u64_to_memory(&mut caller, out_ptr, 1, ret[1].0)?;
                    write_u64_to_memory(&mut caller, out_ptr, 2, ret[2].0)?;
                    write_u64_to_memory(&mut caller, out_ptr, 3, ret[3].0)?;
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_bind",
                |mut caller: Caller<'_, RuntimeState>,
                 owner_id: u64|
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let owner_id = ProcessId(owner_id as usize);
                    caller
                        .data_mut()
                        .ownership
                        .insert(current_pid, Some(owner_id));
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_unbind",
                |mut caller: Caller<'_, RuntimeState>,
                 token_id: u64|
                 -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    let token_id = ProcessId(token_id as usize);
                    if caller.data().ownership.get(&token_id) != Some(&Some(current_pid)) {
                        eprintln!(
                            "unbind called by non-owner: token_id={}, current_pid={}",
                            token_id.0, current_pid.0
                        );
                    }
                    caller.data_mut().ownership.insert(token_id, None);
                    Ok(())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_burn",
                |mut caller: Caller<'_, RuntimeState>, ret: u64| -> Result<(), wasmtime::Error> {
                    let current_pid = caller.data().current_process;
                    caller.data_mut().must_burn.insert(current_pid);
                    suspend_with_effect(&mut caller, WitLedgerEffect::Burn { ret: Ref(ret) }, ())
                },
            )
            .unwrap();

        linker
            .func_wrap(
                "env",
                "starstream_get_program_hash",
                |mut caller: Caller<'_, RuntimeState>,
                 target_pid: u64,
                 out_ptr: i32|
                 -> Result<(), wasmtime::Error> {
                    let target = ProcessId(target_pid as usize);
                    let program_hash = *caller
                        .data()
                        .process_hashes
                        .get(&target)
                        .ok_or(trap("process hash not found"))?;
                    write_u64_to_memory(&mut caller, out_ptr, 0, program_hash.0[0])?;
                    write_u64_to_memory(&mut caller, out_ptr, 1, program_hash.0[1])?;
                    write_u64_to_memory(&mut caller, out_ptr, 2, program_hash.0[2])?;
                    write_u64_to_memory(&mut caller, out_ptr, 3, program_hash.0[3])?;
                    Ok(())
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
        let mut globals_by_pid: Vec<Vec<wasmtime::Global>> = Vec::new();

        for (pid, program_bytes) in self.programs.iter().enumerate() {
            let hash = Hash(
                poseidon_program_hash(program_bytes),
                std::marker::PhantomData,
            );

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

            let module = wasmtime::Module::new(&runtime.engine, program_bytes)?;
            let instance = runtime.linker.instantiate(&mut runtime.store, &module)?;

            // Store memory in RuntimeState for hash reading
            if let Some(extern_) = instance.get_export(&mut runtime.store, "memory")
                && let Some(memory) = extern_.into_memory()
            {
                runtime
                    .store
                    .data_mut()
                    .memories
                    .insert(ProcessId(pid), memory);
            }

            let mut globals = Vec::new();
            for export in instance.exports(&mut runtime.store) {
                let name = export.name().to_string();
                if let Some(global) = export.into_global() {
                    globals.push((name, global));
                }
            }
            globals.sort_by(|a, b| a.0.cmp(&b.0));
            let globals: Vec<wasmtime::Global> = globals.into_iter().map(|(_, g)| g).collect();

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

        // Start entrypoint
        let mut current_pid = ProcessId(self.entrypoint);
        runtime
            .store
            .data_mut()
            .allocated_processes
            .insert(current_pid);

        runtime.store.data_mut().current_process = current_pid;

        loop {
            runtime.store.data_mut().current_process = current_pid;
            runtime.store.data_mut().pending_host_effect = None;
            let instance = instances[current_pid.0];
            let func = instance.get_typed_func::<(), ()>(&mut runtime.store, "_start")?;
            func.call(&mut runtime.store, ())?;

            let Some(last_effect) = runtime.store.data_mut().pending_host_effect.take() else {
                if current_pid == ProcessId(self.entrypoint) {
                    break;
                }
                return Err(Error::RuntimeError(format!(
                    "process {current_pid:?} returned without control effect"
                )));
            };

            runtime
                .store
                .data_mut()
                .interleaving
                .push((current_pid, last_effect.clone()));

            match last_effect {
                WitLedgerEffect::Resume { target, val, .. } => {
                    runtime
                        .store
                        .data_mut()
                        .pending_activation
                        .insert(target, (val, current_pid));
                    current_pid = target;
                }
                WitLedgerEffect::CallEffectHandler {
                    interface_id, val, ..
                } => {
                    let target = {
                        let stack = runtime
                            .store
                            .data()
                            .handler_stack
                            .get(&interface_id)
                            .expect(
                                "handler stack not found while dispatching call_effect_handler",
                            );
                        *stack
                            .last()
                            .expect("handler stack empty while dispatching call_effect_handler")
                    };
                    runtime
                        .store
                        .data_mut()
                        .pending_activation
                        .insert(target, (val, current_pid));
                    current_pid = target;
                }
                WitLedgerEffect::Yield { val, .. } => {
                    let caller = *runtime
                        .store
                        .data()
                        .yield_to
                        .get(&current_pid)
                        .expect("yield on missing yield_to");
                    runtime
                        .store
                        .data_mut()
                        .pending_activation
                        .insert(caller, (val, current_pid));
                    current_pid = caller;
                }
                WitLedgerEffect::Return { .. } => {
                    if let Some(caller) = runtime.store.data().yield_to.get(&current_pid).copied() {
                        runtime
                            .store
                            .data_mut()
                            .pending_activation
                            .insert(caller, (Ref(0), current_pid));
                        current_pid = caller;
                    } else if current_pid == ProcessId(self.entrypoint) {
                        break;
                    } else {
                        return Err(Error::RuntimeError(
                            "return on missing yield_to for non-entrypoint".into(),
                        ));
                    }
                }
                WitLedgerEffect::Burn { .. } => {
                    let caller = *runtime
                        .store
                        .data()
                        .yield_to
                        .get(&current_pid)
                        .expect("burn on missing yield_to");
                    runtime
                        .store
                        .data_mut()
                        .pending_activation
                        .insert(caller, (Ref(0), current_pid));
                    current_pid = caller;
                }
                _ => {
                    return Err(Error::RuntimeError(format!(
                        "process {current_pid:?} ended with non-control effect {:?}",
                        last_effect
                    )));
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
            let effect_trace = data
                .effect_traces
                .get(&ProcessId(pid))
                .cloned()
                .unwrap_or_default();
            let trace = effect_trace;
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
            let globals = snapshot_globals(&mut runtime.store, globals)?;
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
