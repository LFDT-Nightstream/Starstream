//! A *mock* interpreter for the interleaving / transaction semantics.
//!
//! This is meant for tests/examples: you can “mock” traces as Vec<HostCall> per process.
//!
//! It also doesn't use commitments to the tables, it just has access to the
//! actual traces (where actual the circuit would, but as witnesses, it won't
//! happen *in* the ledger).
//!
//! It's mainly a direct translation of the algorithm in the README

use std::collections::HashMap;
use thiserror;

use crate::{Hash, InterleavingInstance, Value, WasmModule};

// ---------------------------- basic types ----------------------------

pub type ProcessId = usize;
pub type InterfaceId = u64;

/// One entry in the per-process host-call trace.
/// This corresponds to "t[c] == <Opcode, ...>" checks in the semantics.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostCall {
    Resume {
        target: ProcessId,
        val: Value,
        ret: Value,
        id_prev: Option<ProcessId>,
    },
    Yield {
        val: Value,
        ret: Option<Value>,
        id_prev: Option<ProcessId>,
    },
    ProgramHash {
        target: ProcessId,
        program_hash: Hash<WasmModule>,
    },
    NewUtxo {
        program_hash: Hash<WasmModule>,
        val: Value,
        id: ProcessId,
    },
    NewCoord {
        program_hash: Hash<WasmModule>,
        val: Value,
        id: ProcessId,
    },
    InstallHandler {
        interface_id: InterfaceId,
    },
    UninstallHandler {
        interface_id: InterfaceId,
    },
    GetHandlerFor {
        interface_id: InterfaceId,
        handler_id: ProcessId,
    },

    // UTXO-only
    Burn {
        ret: Value,
    },

    Input {
        val: Value,
        caller: ProcessId,
    },

    // Tokens
    Bind {
        owner_id: ProcessId,
    },
    Unbind {
        token_id: ProcessId,
    },
}

/// A “proof input” for tests: provide per-process traces directly.
#[derive(Clone, Debug)]
pub struct InterleavingWitness {
    /// One trace per process in canonical order: inputs ++ new_outputs ++ coord scripts
    pub traces: Vec<Vec<HostCall>>,
}

#[derive(thiserror::Error, Debug)]
pub enum InterleavingError {
    #[error("instance shape mismatch: {0}")]
    Shape(&'static str),

    #[error("invalid process id {0}")]
    BadPid(ProcessId),

    #[error("host call index out of bounds: pid={pid} counter={counter} len={len}")]
    HostCallOob {
        pid: ProcessId,
        counter: usize,
        len: usize,
    },

    #[error(
        "host call mismatch at pid={pid} counter={counter}: expected {expected:?}, got {got:?}"
    )]
    HostCallMismatch {
        pid: ProcessId,
        counter: usize,
        expected: HostCall,
        got: HostCall,
    },

    #[error("resume self-resume forbidden (pid={0})")]
    SelfResume(ProcessId),

    #[error("utxo cannot resume utxo: caller={caller} target={target}")]
    UtxoResumesUtxo {
        caller: ProcessId,
        target: ProcessId,
    },

    #[error("resume target not initialized: target={0}")]
    TargetNotInitialized(ProcessId),

    #[error("resume claim mismatch: target={target} expected={expected:?} got={got:?}")]
    ResumeClaimMismatch {
        target: ProcessId,
        expected: Value,
        got: Value,
    },

    #[error("yield claim mismatch: id_prev={id_prev:?} expected={expected:?} got={got:?}")]
    YieldClaimMismatch {
        id_prev: Option<ProcessId>,
        expected: Value,
        got: Value,
    },

    #[error("program hash mismatch: target={target} expected={expected:?} got={got:?}")]
    ProgramHashMismatch {
        target: ProcessId,
        expected: Hash<WasmModule>,
        got: Hash<WasmModule>,
    },

    #[error("coord-only op used by utxo (pid={0})")]
    CoordOnly(ProcessId),

    #[error("utxo-only op used by coord (pid={0})")]
    UtxoOnly(ProcessId),

    #[error("uninstall handler not top: interface={interface_id} top={top:?} pid={pid}")]
    HandlerNotTop {
        interface_id: InterfaceId,
        top: Option<ProcessId>,
        pid: ProcessId,
    },

    #[error(
        "get_handler_for returned wrong handler: interface={interface_id} expected={expected:?} got={got}"
    )]
    HandlerGetMismatch {
        interface_id: InterfaceId,
        expected: Option<ProcessId>,
        got: ProcessId,
    },

    #[error("bind: token already owned (token={token} owner={owner:?})")]
    TokenAlreadyOwned {
        token: ProcessId,
        owner: Option<ProcessId>,
    },

    #[error("bind: owner is not utxo (owner={0})")]
    OwnerNotUtxo(ProcessId),

    #[error("unbind: not current owner (token={token} owner={owner:?} caller={caller})")]
    UnbindNotOwner {
        token: ProcessId,
        owner: Option<ProcessId>,
        caller: ProcessId,
    },

    #[error("verification: counters mismatch for pid={pid}: counter={counter} len={len}")]
    CounterLenMismatch {
        pid: ProcessId,
        counter: usize,
        len: usize,
    },

    #[error("verification: utxo not finalized (safe_to_ledger=false) pid={0}")]
    UtxoNotFinalized(ProcessId),

    #[error("verification: finished in utxo (id_curr={0})")]
    FinishedInUtxo(ProcessId),

    #[error("verification: burned input did not Burn (pid={pid})")]
    BurnedInputNoBurn { pid: ProcessId },

    #[error(
        "ownership_out does not match computed end state (pid={pid} expected={expected:?} got={got:?})"
    )]
    OwnershipOutMismatch {
        pid: ProcessId,
        expected: Option<ProcessId>,
        got: Option<ProcessId>,
    },
    #[error("a process was not initialized {pid}")]
    ProcessNotInitialized { pid: usize },
}

// ---------------------------- verifier ----------------------------

#[derive(Clone, Debug)]
pub struct InterleavingState {
    id_curr: ProcessId,
    id_prev: Option<ProcessId>,

    /// Claims memory: M[pid] = expected argument to next Resume into pid.
    expected_input: Vec<Value>,

    arg: Vec<Option<(Value, ProcessId)>>,

    process_table: Vec<Hash<WasmModule>>,
    traces: Vec<Vec<HostCall>>,
    counters: Vec<usize>,
    safe_to_ledger: Vec<bool>,
    is_utxo: Vec<bool>,
    initialized: Vec<bool>,

    handler_stack: HashMap<InterfaceId, Vec<ProcessId>>,

    /// token -> owner (both ProcessId). None => unowned.
    ownership: Vec<Option<ProcessId>>,

    burned: Vec<bool>,
}

pub fn verify_interleaving_semantics(
    inst: &InterleavingInstance,
    wit: &InterleavingWitness,
    input_states: &[crate::CoroutineState],
) -> Result<(), InterleavingError> {
    // ---------- shape checks ----------
    // TODO: a few of these may be redundant
    //
    // the data layout is still a bit weird
    let n = inst.process_table.len();
    if inst.is_utxo.len() != n {
        return Err(InterleavingError::Shape("is_utxo len != process_table len"));
    }
    if inst.ownership_in.len() != n || inst.ownership_out.len() != n {
        return Err(InterleavingError::Shape(
            "ownership_* len != process_table len",
        ));
    }
    if wit.traces.len() != n {
        return Err(InterleavingError::Shape(
            "witness traces len != process_table len",
        ));
    }
    if inst.entrypoint >= n {
        return Err(InterleavingError::BadPid(inst.entrypoint));
    }

    if inst.burned.len() != inst.n_inputs {
        return Err(InterleavingError::Shape("burned len != n_inputs"));
    }

    // a utxo that did yield at the end of a previous transaction, gets
    // initialized with the same data.
    let mut claims_memory = vec![Value::nil(); n];
    for i in 0..inst.n_inputs {
        claims_memory[i] = input_states[i].last_yield.clone();
    }

    let mut state = InterleavingState {
        id_curr: inst.entrypoint,
        id_prev: None,
        expected_input: claims_memory,
        arg: vec![None; n],
        process_table: inst.process_table.clone(),
        traces: wit.traces.clone(),
        counters: vec![0; n],
        safe_to_ledger: vec![false; n],
        is_utxo: inst.is_utxo.clone(),
        initialized: vec![false; n],
        handler_stack: HashMap::new(),
        ownership: inst.ownership_in.clone(),
        burned: inst.burned.clone(),
    };

    // Inputs exist already (on-ledger) so they start initialized.
    // The entrypoint coordination script is the currently executing VM, so it
    // starts initialized.
    // Everything else must be explicitly constructed/spawned via NewUtxo/NewCoord.
    for pid in 0..n {
        state.initialized[pid] = pid < inst.n_inputs;
    }

    state.initialized[inst.entrypoint] = true;

    // ---------- run until current trace ends ----------
    // This is deterministic: at each step, read the next host call of id_curr at counters[id_curr].
    loop {
        let pid = state.id_curr;
        let c = state.counters[pid];

        let trace = &state.traces[pid];
        if c >= trace.len() {
            break;
        }

        let op = trace[c].clone();
        step(&mut state, op)?;
    }

    // ---------- final verification conditions ----------
    // 1) counters match per-process host call lengths
    //
    // (so we didn't just prove a prefix)
    for pid in 0..n {
        let counter = state.counters[pid];
        let len = state.traces[pid].len();
        if counter != len {
            return Err(InterleavingError::CounterLenMismatch { pid, counter, len });
        }
    }

    // 2) process called burn but it has a continuation output in the tx
    for i in 0..inst.n_inputs {
        if state.burned[i] {
            let has_burn = state.traces[i]
                .iter()
                .any(|hc| matches!(hc, HostCall::Burn { ret: _ }));
            if !has_burn {
                return Err(InterleavingError::BurnedInputNoBurn { pid: i });
            }
        }
    }

    // 3) all utxos finalize (safe_to_ledger true)
    for pid in 0..n {
        if state.is_utxo[pid] {
            if !state.safe_to_ledger[pid] {
                return Err(InterleavingError::UtxoNotFinalized(pid));
            }
        }
    }

    // 4) finish in a coordination script
    if state.is_utxo[state.id_curr] {
        return Err(InterleavingError::FinishedInUtxo(state.id_curr));
    }

    // 5) ownership_out matches computed end state
    for pid in 0..n {
        let expected = inst.ownership_out[pid];
        let got = state.ownership[pid];
        if expected != got {
            return Err(InterleavingError::OwnershipOutMismatch { pid, expected, got });
        }
    }

    // every object had a constructor called by a coordination script.
    for pid in 0..n {
        if !state.initialized[pid] && !state.burned[pid] {
            return Err(InterleavingError::ProcessNotInitialized { pid });
        }
    }

    Ok(())
}

pub fn step(state: &mut InterleavingState, op: HostCall) -> Result<(), InterleavingError> {
    let id_curr = state.id_curr;
    let c = state.counters[id_curr];
    let trace = &state.traces[id_curr];

    // For every rule, enforce "host call lookup condition" by checking op ==
    // t[c].
    //
    // Here op is always t[c] anyway because there is no commitment, since this
    // doesn't do any zk, it's just trace, but in the circuit this would be a
    // lookup constraint into the right table.
    if c >= trace.len() {
        return Err(InterleavingError::HostCallOob {
            pid: id_curr,
            counter: c,
            len: trace.len(),
        });
    }
    let got = trace[c].clone();
    if got != op {
        return Err(InterleavingError::HostCallMismatch {
            pid: id_curr,
            counter: c,
            expected: op,
            got,
        });
    }

    state.counters[id_curr] += 1;

    match op {
        HostCall::Resume {
            target,
            val,
            ret,
            id_prev,
        } => {
            if id_curr == target {
                return Err(InterleavingError::SelfResume(id_curr));
            }

            if state.is_utxo[id_curr] && state.is_utxo[target] {
                return Err(InterleavingError::UtxoResumesUtxo {
                    caller: id_curr,
                    target,
                });
            }

            if !state.initialized[target] {
                return Err(InterleavingError::TargetNotInitialized(target));
            }

            if state.arg[target].is_some() {
                return Err(InterleavingError::Shape(
                    "target already has arg (re-entrancy)",
                ));
            }

            state.arg[id_curr] = None;

            let expected = state.expected_input[target].clone();
            if expected != val {
                return Err(InterleavingError::ResumeClaimMismatch {
                    target,
                    expected,
                    got: val,
                });
            }

            state.arg[target] = Some((val.clone(), id_curr));

            state.expected_input[id_curr] = ret;

            if id_prev != state.id_prev {
                return Err(InterleavingError::HostCallMismatch {
                    pid: id_curr,
                    counter: c,
                    expected: HostCall::Resume {
                        target,
                        val: expected, // not perfect, but avoids adding another type
                        ret: state.expected_input[id_curr].clone(),
                        id_prev: state.id_prev,
                    },
                    got: HostCall::Resume {
                        target,
                        val: state.expected_input[target].clone(),
                        ret: state.expected_input[id_curr].clone(),
                        id_prev,
                    },
                });
            }

            state.id_prev = Some(id_curr);

            state.id_curr = target;

            state.safe_to_ledger[target] = false;
        }

        HostCall::Yield { val, ret, id_prev } => {
            if id_prev != state.id_prev {
                return Err(InterleavingError::HostCallMismatch {
                    pid: id_curr,
                    counter: c,
                    expected: HostCall::Yield {
                        val: val.clone(),
                        ret: ret.clone(),
                        id_prev: state.id_prev,
                    },
                    got: HostCall::Yield { val, ret, id_prev },
                });
            }

            let parent = state
                .id_prev
                .expect("end-tx yield should have parent or sentinel");

            match ret {
                Some(retv) => {
                    state.expected_input[id_curr] = retv;

                    state.id_prev = Some(id_curr);

                    state.safe_to_ledger[id_curr] = false;

                    if let Some(prev) = state.id_prev {
                        let expected = state.expected_input[prev].clone();
                        if expected != val {
                            return Err(InterleavingError::YieldClaimMismatch {
                                id_prev: state.id_prev,
                                expected,
                                got: val,
                            });
                        }
                    }
                }
                None => {
                    state.id_prev = Some(id_curr);

                    state.safe_to_ledger[id_curr] = true;
                }
            }

            state.arg[id_curr] = None;
            state.id_curr = parent;
        }

        HostCall::ProgramHash {
            target,
            program_hash,
        } => {
            // check lookup against process_table
            let expected = state.process_table[target].clone();
            if expected != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target,
                    expected,
                    got: program_hash,
                });
            }
        }

        HostCall::NewUtxo {
            program_hash,
            val,
            id,
        } => {
            if state.is_utxo[id_curr] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            if !state.is_utxo[id] {
                // in your rule NewUtxo requires is_utxo[id]
                return Err(InterleavingError::Shape("NewUtxo id must be utxo"));
            }
            if state.process_table[id] != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target: id,
                    expected: state.process_table[id].clone(),
                    got: program_hash,
                });
            }
            if state.counters[id] != 0 {
                return Err(InterleavingError::Shape("NewUtxo requires counters[id]==0"));
            }
            if state.initialized[id] {
                return Err(InterleavingError::Shape(
                    "NewUtxo requires initialized[id]==false",
                ));
            }
            state.initialized[id] = true;
            state.expected_input[id] = val;

            state.arg[id] = None;
        }

        HostCall::NewCoord {
            program_hash,
            val,
            id,
        } => {
            if state.is_utxo[id_curr] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            if state.is_utxo[id] {
                return Err(InterleavingError::Shape("NewCoord id must be coord"));
            }
            if state.process_table[id] != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target: id,
                    expected: state.process_table[id].clone(),
                    got: program_hash,
                });
            }
            if state.counters[id] != 0 {
                return Err(InterleavingError::Shape(
                    "NewCoord requires counters[id]==0",
                ));
            }
            if state.initialized[id] {
                return Err(InterleavingError::Shape(
                    "NewCoord requires initialized[id]==false",
                ));
            }

            state.initialized[id] = true;
            state.expected_input[id] = val;
        }

        HostCall::InstallHandler { interface_id } => {
            if state.is_utxo[id_curr] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            state
                .handler_stack
                .entry(interface_id)
                .or_default()
                .push(id_curr);
        }

        HostCall::UninstallHandler { interface_id } => {
            if state.is_utxo[id_curr] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            let stack = state.handler_stack.entry(interface_id).or_default();
            let top = stack.last().copied();
            if top != Some(id_curr) {
                return Err(InterleavingError::HandlerNotTop {
                    interface_id,
                    top,
                    pid: id_curr,
                });
            }
            stack.pop();
        }

        HostCall::GetHandlerFor {
            interface_id,
            handler_id,
        } => {
            let stack = state.handler_stack.entry(interface_id).or_default();
            let expected = stack.last().copied();
            if expected != Some(handler_id) {
                return Err(InterleavingError::HandlerGetMismatch {
                    interface_id,
                    expected,
                    got: handler_id,
                });
            }
        }

        HostCall::Input { val, caller } => {
            let curr = state.id_curr;

            let Some((v, c)) = &state.arg[curr] else {
                return Err(InterleavingError::Shape("Input called with no arg set"));
            };

            if v != &val || c != &caller {
                return Err(InterleavingError::Shape("Input result mismatch"));
            }
        }

        HostCall::Burn { ret } => {
            if !state.is_utxo[id_curr] {
                return Err(InterleavingError::UtxoOnly(id_curr));
            }

            let prev = state.id_prev.unwrap();
            let expected = state.expected_input[prev].clone();
            if expected != ret {
                // Burn is the final return of the coroutine
                return Err(InterleavingError::YieldClaimMismatch {
                    id_prev: state.id_prev,
                    expected,
                    got: ret,
                });
            }

            state.arg[id_curr] = None;
            state.safe_to_ledger[id_curr] = true;
            state.initialized[id_curr] = false;
            state.expected_input[id_curr] = ret;
            let parent = state.id_prev.unwrap();
            state.id_prev = Some(id_curr);
            state.id_curr = parent;
        }

        HostCall::Bind { owner_id } => {
            let token_id = id_curr;

            if !state.is_utxo[token_id] {
                return Err(InterleavingError::Shape("Bind: token_id must be utxo"));
            }
            if !state.is_utxo[owner_id] {
                return Err(InterleavingError::OwnerNotUtxo(owner_id));
            }
            if !state.initialized[token_id] || !state.initialized[owner_id] {
                return Err(InterleavingError::Shape("Bind: both must be initialized"));
            }
            if state.ownership[token_id].is_some() {
                return Err(InterleavingError::TokenAlreadyOwned {
                    token: token_id,
                    owner: state.ownership[token_id],
                });
            }

            state.ownership[token_id] = Some(owner_id);
        }

        HostCall::Unbind { token_id } => {
            let owner_id = id_curr;

            if !state.is_utxo[owner_id] {
                return Err(InterleavingError::Shape("Unbind: caller must be utxo"));
            }
            if !state.is_utxo[token_id] || !state.initialized[token_id] {
                return Err(InterleavingError::Shape(
                    "Unbind: token must exist and be utxo",
                ));
            }
            let cur_owner = state.ownership[token_id];
            if cur_owner != Some(owner_id) {
                return Err(InterleavingError::UnbindNotOwner {
                    token: token_id,
                    owner: cur_owner,
                    caller: owner_id,
                });
            }

            state.ownership[token_id] = None;
        }
    }

    Ok(())
}
