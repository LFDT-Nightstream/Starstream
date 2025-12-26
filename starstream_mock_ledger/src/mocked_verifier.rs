//! A *mock* interpreter for the interleaving / transaction semantics.
//!
//! This is meant for tests/examples: you can “mock” traces as Vec<HostCall> per process.
//!
//! It also doesn't use commitments to the tables, it just has access to the
//! actual traces (where actual the circuit would, but as witnesses, it won't
//! happen *in* the ledger).
//!
//! It's mainly a direct translation of the algorithm in the README

use crate::{
    Hash, InterleavingInstance, Value, WasmModule,
    transaction_effects::{InterfaceId, ProcessId, witness::WitLedgerEffect},
};
use std::collections::HashMap;
use thiserror;

#[derive(Clone, PartialEq, Eq)]
pub struct MockedLookupTableCommitment {
    // obviously the actual commitment shouldn't have this
    // but this is used for the mocked circuit
    pub(crate) trace: Vec<WitLedgerEffect>,
}

/// A “proof input” for tests: provide per-process traces directly.
#[derive(Clone, Debug)]
pub struct InterleavingWitness {
    /// One trace per process in canonical order: inputs ++ new_outputs ++ coord scripts
    pub traces: Vec<Vec<WitLedgerEffect>>,
}

#[derive(thiserror::Error, Debug)]
pub enum InterleavingError {
    #[error("instance shape mismatch: {0}")]
    Shape(&'static str),

    #[error("unknown process id {0}")]
    BadPid(ProcessId),

    #[error("host call index out of bounds: pid={pid} counter={counter} len={len}")]
    CounterOutOfBounds {
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
        expected: WitLedgerEffect,
        got: WitLedgerEffect,
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

    #[error("uninstall handler not top: interface={interface_id:?} pid={pid}")]
    HandlerNotFound {
        interface_id: InterfaceId,
        pid: ProcessId,
    },

    #[error(
        "get_handler_for returned wrong handler: interface={interface_id:?} expected={expected:?} got={got}"
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

    #[error("yield called without a parent process (pid={pid})")]
    YieldWithNoParent { pid: ProcessId },

    #[error("burn called without a parent process (pid={pid})")]
    BurnWithNoParent { pid: ProcessId },

    #[error("verification: counters mismatch for pid={pid}: counter={counter} len={len}")]
    CounterLenMismatch {
        pid: ProcessId,
        counter: usize,
        len: usize,
    },

    #[error("verification: utxo not finalized (finalized=false) pid={0}")]
    UtxoNotFinalized(ProcessId),

    #[error("verification: utxo does not have a continuation but it did not call Burn pid={0}")]
    UtxoShouldBurn(ProcessId),

    #[error("verification: utxo does have a continuation but it did call Burn pid={0}")]
    UtxoShouldNotBurn(ProcessId),

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
    ProcessNotInitialized { pid: ProcessId },

    #[error("resume target already has arg (re-entrancy): target={0}")]
    ReentrantResume(ProcessId),
}

// ---------------------------- verifier ----------------------------

pub struct ROM {
    process_table: Vec<Hash<WasmModule>>,
    must_burn: Vec<bool>,
    is_utxo: Vec<bool>,

    // mocked, this should be only a commitment
    traces: Vec<Vec<WitLedgerEffect>>,
}

#[derive(Clone, Debug)]
pub struct InterleavingState {
    id_curr: ProcessId,
    id_prev: Option<ProcessId>,

    /// Claims memory: M[pid] = expected argument to next Resume into pid.
    expected_input: Vec<Value>,

    arg: Vec<Option<(Value, ProcessId)>>,

    counters: Vec<usize>,

    /// If a new output or coordination script is created, it must be through a
    /// spawn from a coordinator script
    initialized: Vec<bool>,
    /// Whether the last instruction in that utxo was Yield or Burn
    finalized: Vec<bool>,
    /// Keep track of whether Burn is called
    did_burn: Vec<bool>,

    /// token -> owner (both ProcessId). None => unowned.
    ownership: Vec<Option<ProcessId>>,

    /// A stack per possible interface
    handler_stack: HashMap<InterfaceId, Vec<ProcessId>>,
}

pub fn verify_interleaving_semantics(
    inst: &InterleavingInstance,
    wit: &InterleavingWitness,
    input_states: &[crate::CoroutineState],
) -> Result<(), InterleavingError> {
    inst.check_shape()?;

    let n = inst.n_inputs + inst.n_new + inst.n_coords;

    if wit.traces.len() != n {
        return Err(InterleavingError::Shape(
            "witness traces len != process_table len",
        ));
    }

    // a utxo that did yield at the end of a previous transaction, gets
    // initialized with the same data.
    //
    // TODO: maybe we also need to assert/prove that it starts with a Yield
    let mut claims_memory = vec![Value::nil(); n];
    for i in 0..inst.n_inputs {
        claims_memory[i] = input_states[i].last_yield.clone();
    }

    let rom = ROM {
        process_table: inst.process_table.clone(),
        must_burn: inst.must_burn.clone(),
        is_utxo: inst.is_utxo.clone(),
        traces: wit.traces.clone(),
    };

    let mut state = InterleavingState {
        id_curr: ProcessId(inst.entrypoint.into()),
        id_prev: None,
        expected_input: claims_memory,
        arg: vec![None; n],
        counters: vec![0; n],
        handler_stack: HashMap::new(),
        ownership: inst.ownership_in.clone(),
        initialized: vec![false; n],
        finalized: vec![false; n],
        did_burn: vec![false; n],
    };

    // Inputs exist already (on-ledger) so they start initialized.
    // The entrypoint coordination script is the currently executing VM, so it
    // starts initialized.
    // Everything else must be explicitly constructed/spawned via NewUtxo/NewCoord.
    for pid in 0..n {
        state.initialized[pid] = pid < inst.n_inputs;
    }

    state.initialized[inst.entrypoint.0] = true;

    // ---------- run until current trace ends ----------
    // This is deterministic: at each step, read the next host call of id_curr at counters[id_curr].
    let mut current_state = state;
    loop {
        let pid = current_state.id_curr;
        let c = current_state.counters[pid.0];

        let trace = &rom.traces[pid.0];
        if c >= trace.len() {
            break;
        }

        let op = trace[c].clone();
        current_state = state_transition(current_state, &rom, op)?;
    }
    let state = current_state;

    // ---------- final verification conditions ----------
    // 1) counters match per-process host call lengths
    //
    // (so we didn't just prove a prefix)
    for pid in 0..n {
        let counter = state.counters[pid];
        let len = rom.traces[pid].len();
        if counter != len {
            return Err(InterleavingError::CounterLenMismatch {
                pid: ProcessId(pid),
                counter,
                len,
            });
        }
    }

    // 2) process called burn but it has a continuation output in the tx
    for i in 0..inst.n_inputs {
        if rom.must_burn[i] {
            let has_burn = rom.traces[i]
                .iter()
                .any(|hc| matches!(hc, WitLedgerEffect::Burn { ret: _ }));
            if !has_burn {
                return Err(InterleavingError::BurnedInputNoBurn { pid: ProcessId(i) });
            }
        }
    }

    // 3) all utxos finalize
    for pid in 0..(inst.n_inputs + inst.n_new) {
        if !state.finalized[pid] {
            return Err(InterleavingError::UtxoNotFinalized(ProcessId(pid)));
        }
    }

    // 4) all utxos without continuation did call Burn
    for pid in 0..inst.n_inputs {
        if rom.must_burn[pid] && !state.did_burn[pid] {
            return Err(InterleavingError::UtxoShouldBurn(ProcessId(pid)));
        }
    }

    // 5) finish in a coordination script
    if rom.is_utxo[state.id_curr.0] {
        return Err(InterleavingError::FinishedInUtxo(state.id_curr));
    }

    // 6) ownership_out matches computed end state
    for pid in 0..(inst.n_inputs + inst.n_new) {
        let expected = inst.ownership_out[pid];
        let got = state.ownership[pid];
        if expected != got {
            return Err(InterleavingError::OwnershipOutMismatch {
                pid: ProcessId(pid),
                expected,
                got,
            });
        }
    }

    // every object had a constructor called by a coordination script.
    //
    // TODO: this may be redundant, since resume should not work on unitialized
    // programs, and without resuming then the trace counter will catch this
    for pid in 0..n {
        if !state.initialized[pid] {
            return Err(InterleavingError::ProcessNotInitialized {
                pid: ProcessId(pid),
            });
        }
    }

    Ok(())
}

pub fn state_transition(
    mut state: InterleavingState,
    rom: &ROM,
    op: WitLedgerEffect,
) -> Result<InterleavingState, InterleavingError> {
    let id_curr = state.id_curr;
    let c = state.counters[id_curr.0];
    let trace = &rom.traces[id_curr.0];

    // For every rule, enforce "host call lookup condition" by checking op ==
    // t[c].
    //
    // Here op is always t[c] anyway because there is no commitment, since this
    // doesn't do any zk, it's just trace, but in the circuit this would be a
    // lookup constraint into the right table.
    if c >= trace.len() {
        return Err(InterleavingError::CounterOutOfBounds {
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

    state.counters[id_curr.0] += 1;

    match op {
        WitLedgerEffect::Resume {
            target,
            val,
            ret,
            id_prev,
        } => {
            if id_curr == target {
                return Err(InterleavingError::SelfResume(id_curr));
            }

            if rom.is_utxo[id_curr.0] && rom.is_utxo[target.0] {
                return Err(InterleavingError::UtxoResumesUtxo {
                    caller: id_curr,
                    target,
                });
            }

            if !state.initialized[target.0] {
                return Err(InterleavingError::TargetNotInitialized(target));
            }

            if state.arg[target.0].is_some() {
                return Err(InterleavingError::ReentrantResume(target));
            }

            state.arg[id_curr.0] = None;

            if state.expected_input[target.0].clone() != val {
                return Err(InterleavingError::ResumeClaimMismatch {
                    target,
                    expected: state.expected_input[target.0].clone(),
                    got: val,
                });
            }

            state.arg[target.0] = Some((val.clone(), id_curr));

            state.expected_input[id_curr.0] = ret;

            if id_prev != state.id_prev {
                return Err(InterleavingError::HostCallMismatch {
                    pid: id_curr,
                    counter: c,
                    expected: WitLedgerEffect::Resume {
                        target,
                        val: state.expected_input[target.0].clone(),
                        ret: state.expected_input[id_curr.0].clone(),
                        id_prev: state.id_prev,
                    },
                    got: WitLedgerEffect::Resume {
                        target,
                        val: state.expected_input[target.0].clone(),
                        ret: state.expected_input[id_curr.0].clone(),
                        id_prev,
                    },
                });
            }

            state.id_prev = Some(id_curr);

            state.id_curr = target;

            state.finalized[target.0] = false;
        }

        WitLedgerEffect::Yield { val, ret, id_prev } => {
            if id_prev != state.id_prev {
                return Err(InterleavingError::HostCallMismatch {
                    pid: id_curr,
                    counter: c,
                    expected: WitLedgerEffect::Yield {
                        val: val.clone(),
                        ret: ret.clone(),
                        id_prev: state.id_prev,
                    },
                    got: WitLedgerEffect::Yield { val, ret, id_prev },
                });
            }

            let parent = state
                .id_prev
                .ok_or(InterleavingError::YieldWithNoParent { pid: id_curr })?;

            match ret {
                Some(retv) => {
                    state.expected_input[id_curr.0] = retv;

                    state.id_prev = Some(id_curr);

                    state.finalized[id_curr.0] = false;

                    if let Some(prev) = state.id_prev {
                        let expected = state.expected_input[prev.0].clone();
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

                    state.finalized[id_curr.0] = true;
                }
            }

            state.arg[id_curr.0] = None;
            state.id_curr = parent;
        }

        WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        } => {
            // check lookup against process_table
            let expected = rom.process_table[target.0].clone();
            if expected != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target,
                    expected,
                    got: program_hash,
                });
            }
        }

        WitLedgerEffect::NewUtxo {
            program_hash,
            val,
            id,
        } => {
            if rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            if !rom.is_utxo[id.0] {
                return Err(InterleavingError::Shape("NewUtxo id must be utxo"));
            }
            if rom.process_table[id.0] != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target: id,
                    expected: rom.process_table[id.0].clone(),
                    got: program_hash,
                });
            }
            if state.counters[id.0] != 0 {
                return Err(InterleavingError::Shape("NewUtxo requires counters[id]==0"));
            }
            if state.initialized[id.0] {
                return Err(InterleavingError::Shape(
                    "NewUtxo requires initialized[id]==false",
                ));
            }
            state.initialized[id.0] = true;
            state.expected_input[id.0] = val;

            state.arg[id.0] = None;
        }

        WitLedgerEffect::NewCoord {
            program_hash,
            val,
            id,
        } => {
            if rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            if rom.is_utxo[id.0] {
                return Err(InterleavingError::Shape("NewCoord id must be coord"));
            }
            if rom.process_table[id.0] != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target: id,
                    expected: rom.process_table[id.0].clone(),
                    got: program_hash,
                });
            }
            if state.counters[id.0] != 0 {
                return Err(InterleavingError::Shape(
                    "NewCoord requires counters[id]==0",
                ));
            }
            if state.initialized[id.0] {
                return Err(InterleavingError::Shape(
                    "NewCoord requires initialized[id]==false",
                ));
            }

            state.initialized[id.0] = true;
            state.expected_input[id.0] = val;
        }

        WitLedgerEffect::InstallHandler { interface_id } => {
            if rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            state
                .handler_stack
                .entry(interface_id)
                .or_default()
                .push(id_curr);
        }

        WitLedgerEffect::UninstallHandler { interface_id } => {
            if rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            let stack = state.handler_stack.entry(interface_id.clone()).or_default();
            let Some(_) = stack.pop() else {
                return Err(InterleavingError::HandlerNotFound {
                    interface_id,
                    pid: id_curr,
                });
            };
        }

        WitLedgerEffect::GetHandlerFor {
            interface_id,
            handler_id,
        } => {
            let stack = state.handler_stack.entry(interface_id.clone()).or_default();
            let expected = stack.last().copied();
            if expected != Some(handler_id) {
                return Err(InterleavingError::HandlerGetMismatch {
                    interface_id,
                    expected,
                    got: handler_id,
                });
            }
        }

        WitLedgerEffect::Input { val, caller } => {
            let curr = state.id_curr;

            let Some((v, c)) = &state.arg[curr.0] else {
                return Err(InterleavingError::Shape("Input called with no arg set"));
            };

            if v != &val || c != &caller {
                return Err(InterleavingError::Shape("Input result mismatch"));
            }
        }

        WitLedgerEffect::Burn { ret } => {
            if !rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::UtxoOnly(id_curr));
            }

            if !rom.must_burn[id_curr.0] {
                return Err(InterleavingError::UtxoShouldNotBurn(id_curr));
            }

            let parent = state
                .id_prev
                .ok_or(InterleavingError::BurnWithNoParent { pid: id_curr })?;

            if state.expected_input[parent.0].clone() != ret {
                // Burn is the final return of the coroutine
                return Err(InterleavingError::YieldClaimMismatch {
                    id_prev: state.id_prev,
                    expected: state.expected_input[parent.0].clone(),
                    got: ret,
                });
            }

            state.arg[id_curr.0] = None;
            state.finalized[id_curr.0] = true;
            state.did_burn[id_curr.0] = true;
            state.expected_input[id_curr.0] = ret;
            state.id_prev = Some(id_curr);
            state.id_curr = parent;
        }

        WitLedgerEffect::Bind { owner_id } => {
            let token_id = id_curr;

            if !rom.is_utxo[token_id.0] {
                return Err(InterleavingError::Shape("Bind: token_id must be utxo"));
            }
            if !rom.is_utxo[owner_id.0] {
                return Err(InterleavingError::OwnerNotUtxo(owner_id));
            }
            if !state.initialized[token_id.0] || !state.initialized[owner_id.0] {
                return Err(InterleavingError::Shape("Bind: both must be initialized"));
            }
            if state.ownership[token_id.0].is_some() {
                return Err(InterleavingError::TokenAlreadyOwned {
                    token: token_id,
                    owner: state.ownership[token_id.0],
                });
            }

            state.ownership[token_id.0] = Some(owner_id);
        }

        WitLedgerEffect::Unbind { token_id } => {
            let owner_id = id_curr;

            if !rom.is_utxo[owner_id.0] {
                return Err(InterleavingError::Shape("Unbind: caller must be utxo"));
            }
            if !rom.is_utxo[token_id.0] || !state.initialized[token_id.0] {
                return Err(InterleavingError::Shape(
                    "Unbind: token must exist and be utxo",
                ));
            }
            let cur_owner = state.ownership[token_id.0];
            if cur_owner != Some(owner_id) {
                return Err(InterleavingError::UnbindNotOwner {
                    token: token_id,
                    owner: cur_owner,
                    caller: owner_id,
                });
            }

            state.ownership[token_id.0] = None;
        }
    }

    Ok(state)
}
