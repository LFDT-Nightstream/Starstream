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
    Hash, InterleavingInstance, REF_GET_WIDTH, REF_PUSH_WIDTH, Ref, Value, WasmModule,
    transaction_effects::{
        InterfaceId, ProcessId,
        witness::{REF_WRITE_WIDTH, WitLedgerEffect},
    },
};
use ark_ff::Zero;
use ark_goldilocks::FpGoldilocks;
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LedgerEffectsCommitment(pub [FpGoldilocks; 4]);

impl Default for LedgerEffectsCommitment {
    fn default() -> Self {
        Self::iv()
    }
}

impl LedgerEffectsCommitment {
    pub fn iv() -> Self {
        static TRACE_IV: OnceLock<[FpGoldilocks; 4]> = OnceLock::new();
        let iv = TRACE_IV.get_or_init(|| {
            let domain = encode_domain_rate8("starstream/trace_ic/v1/poseidon2");
            ark_poseidon2::sponge_8_trace(&domain).expect("trace iv sponge should succeed")
        });
        Self(*iv)
    }
}

fn encode_domain_rate8(domain: &str) -> [FpGoldilocks; 8] {
    let bytes = domain.as_bytes();
    assert!(
        bytes.len() <= 49,
        "domain tag too long for safe 7-byte/limb encoding: {} bytes",
        bytes.len()
    );

    let mut out = [FpGoldilocks::zero(); 8];
    // Goldilocks field elements cannot safely encode arbitrary 8-byte u64 values
    // without modular wraparound. We pack 7 bytes per limb and store the string
    // length in limb 0 to avoid ambiguity from trailing zero padding.
    out[0] = FpGoldilocks::from(bytes.len() as u64);
    for (i, chunk) in bytes.chunks(7).enumerate() {
        let mut limb = [0u8; 8];
        limb[..chunk.len()].copy_from_slice(chunk);
        out[i + 1] = FpGoldilocks::from(u64::from_le_bytes(limb));
    }
    out
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
        expected: Ref,
        got: Ref,
    },

    #[error("yield claim mismatch: id_prev={id_prev:?} expected={expected:?} got={got:?}")]
    YieldClaimMismatch {
        id_prev: Option<ProcessId>,
        expected: Vec<Value>,
        got: Vec<Value>,
    },

    #[error("resumer mismatch: target={target} expected={expected} got={got}")]
    ResumerMismatch {
        target: ProcessId,
        expected: ProcessId,
        got: ProcessId,
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

    #[error(
        "uninstall handler: only the installing process can uninstall itself (top != current): interface={interface_id:?} pid={pid}"
    )]
    InstalledHandlerIsNotCurrent {
        interface_id: InterfaceId,
        pid: ProcessId,
    },

    #[error("uninstall handler not found: interface={interface_id:?} pid={pid}")]
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

    #[error("ref not found: {0:?}")]
    RefNotFound(Ref),

    #[error("RefPush called but not building ref (pid={0})")]
    RefPushNotBuilding(ProcessId),

    #[error("building ref but called other op (pid={0})")]
    BuildingRefButCalledOther(ProcessId),

    #[error("RefPush called but full (pid={pid} size={size})")]
    RefPushOutOfBounds { pid: ProcessId, size: usize },

    #[error("RefGet offset out of bounds: ref={0:?} offset={1} len={2}")]
    RefGetOutOfBounds(Ref, usize, usize),

    #[error("NewRef result mismatch. Got: {0:?}. Expected: {0:?}")]
    RefInitializationMismatch(Ref, Ref),
}

// ---------------------------- verifier ----------------------------

pub struct Rom {
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
    expected_input: Vec<Option<Ref>>,
    expected_resumer: Vec<Option<ProcessId>>,
    on_yield: Vec<bool>,
    yield_to: Vec<Option<ProcessId>>,

    activation: Vec<Option<(Ref, ProcessId)>>,
    init: Vec<Option<(Ref, ProcessId)>>,

    counters: Vec<usize>,
    ref_counter: u64,
    ref_store: HashMap<Ref, Vec<Value>>,
    ref_sizes: HashMap<Ref, usize>,
    ref_building: HashMap<ProcessId, (Ref, usize, usize)>,

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

#[allow(clippy::result_large_err)]
pub fn verify_interleaving_semantics(
    inst: &InterleavingInstance,
    wit: &InterleavingWitness,
) -> Result<(), InterleavingError> {
    inst.check_shape()?;

    let n = inst.n_inputs + inst.n_new + inst.n_coords;

    if wit.traces.len() != n {
        return Err(InterleavingError::Shape(
            "witness traces len != process_table len",
        ));
    }

    // Inputs do not start with an expected resume argument unless we track it
    // explicitly in the instance.
    let claims_memory = vec![None; n];

    let rom = Rom {
        process_table: inst.process_table.clone(),
        must_burn: inst.must_burn.clone(),
        is_utxo: inst.is_utxo.clone(),
        traces: wit.traces.clone(),
    };

    let mut state = InterleavingState {
        id_curr: ProcessId(inst.entrypoint.into()),
        id_prev: None,
        expected_input: claims_memory,
        expected_resumer: vec![None; n],
        on_yield: vec![true; n],
        yield_to: vec![None; n],
        activation: vec![None; n],
        init: vec![None; n],
        counters: vec![0; n],
        ref_counter: 0,
        ref_store: HashMap::new(),
        ref_sizes: HashMap::new(),
        ref_building: HashMap::new(),
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

#[allow(clippy::result_large_err)]
pub fn state_transition(
    mut state: InterleavingState,
    rom: &Rom,
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

    if state.ref_building.contains_key(&id_curr) && !matches!(op, WitLedgerEffect::RefPush { .. }) {
        return Err(InterleavingError::BuildingRefButCalledOther(id_curr));
    }

    state.counters[id_curr.0] += 1;

    match op {
        WitLedgerEffect::Resume {
            target,
            val,
            ret,
            caller,
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

            if state.activation[target.0].is_some() {
                return Err(InterleavingError::ReentrantResume(target));
            }

            state.activation[id_curr.0] = None;

            if let Some(expected) = state.expected_input[target.0]
                && expected != val
            {
                return Err(InterleavingError::ResumeClaimMismatch {
                    target,
                    expected,
                    got: val,
                });
            }

            if let Some(expected) = state.expected_resumer[target.0]
                && expected != id_curr
            {
                return Err(InterleavingError::ResumerMismatch {
                    target,
                    expected,
                    got: id_curr,
                });
            }

            // Expectations are consumed by the resume.
            state.expected_input[target.0] = None;
            state.expected_resumer[target.0] = None;

            state.activation[target.0] = Some((val, id_curr));

            state.expected_input[id_curr.0] = ret.to_option();
            state.expected_resumer[id_curr.0] = caller.to_option().flatten();

            if state.on_yield[target.0] {
                state.yield_to[target.0] = Some(id_curr);
                state.on_yield[target.0] = false;
            }

            state.id_prev = Some(id_curr);

            state.id_curr = target;

            state.finalized[target.0] = false;
        }

        WitLedgerEffect::Yield { val } => {
            let parent = state
                .yield_to
                .get(id_curr.0)
                .and_then(|p| *p)
                .ok_or(InterleavingError::YieldWithNoParent { pid: id_curr })?;

            let val = state
                .ref_store
                .get(&val)
                .ok_or(InterleavingError::RefNotFound(val))?;

            if let Some(expected) = state.expected_resumer[parent.0]
                && expected != id_curr
            {
                return Err(InterleavingError::ResumerMismatch {
                    target: parent,
                    expected,
                    got: id_curr,
                });
            }

            state.finalized[id_curr.0] = true;

            if let Some(expected_ref) = state.expected_input[parent.0] {
                let expected = state
                    .ref_store
                    .get(&expected_ref)
                    .ok_or(InterleavingError::RefNotFound(expected_ref))?;
                if expected != val {
                    return Err(InterleavingError::YieldClaimMismatch {
                        id_prev: state.id_prev,
                        expected: expected.clone(),
                        got: val.clone(),
                    });
                }
            }

            state.on_yield[id_curr.0] = true;
            state.id_prev = Some(id_curr);
            state.activation[id_curr.0] = None;
            state.id_curr = parent;
        }

        WitLedgerEffect::ProgramHash {
            target,
            program_hash,
        } => {
            // check lookup against process_table
            let expected = rom.process_table[target.0];
            if expected != program_hash.unwrap() {
                return Err(InterleavingError::ProgramHashMismatch {
                    target,
                    expected,
                    got: program_hash.unwrap(),
                });
            }
        }

        WitLedgerEffect::NewUtxo {
            program_hash,
            val,
            id,
        } => {
            let id = id.unwrap();
            if rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            if !rom.is_utxo[id.0] {
                return Err(InterleavingError::Shape("NewUtxo id must be utxo"));
            }
            if rom.process_table[id.0] != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target: id,
                    expected: rom.process_table[id.0],
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
            state.init[id.0] = Some((val, id_curr));
            state.expected_input[id.0] = None;
            state.expected_resumer[id.0] = None;
            state.on_yield[id.0] = true;
            state.yield_to[id.0] = None;
        }

        WitLedgerEffect::NewCoord {
            program_hash,
            val,
            id,
        } => {
            let id = id.unwrap();
            if rom.is_utxo[id_curr.0] {
                return Err(InterleavingError::CoordOnly(id_curr));
            }
            if rom.is_utxo[id.0] {
                return Err(InterleavingError::Shape("NewCoord id must be coord"));
            }
            if rom.process_table[id.0] != program_hash {
                return Err(InterleavingError::ProgramHashMismatch {
                    target: id,
                    expected: rom.process_table[id.0],
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
            state.init[id.0] = Some((val, id_curr));
            state.expected_input[id.0] = None;
            state.expected_resumer[id.0] = None;
            state.on_yield[id.0] = true;
            state.yield_to[id.0] = None;
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
            let stack = state.handler_stack.entry(interface_id).or_default();
            let Some(top) = stack.last().copied() else {
                return Err(InterleavingError::HandlerNotFound {
                    interface_id,
                    pid: id_curr,
                });
            };
            if top != id_curr {
                return Err(InterleavingError::InstalledHandlerIsNotCurrent {
                    interface_id,
                    pid: id_curr,
                });
            }
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
            let stack = state.handler_stack.entry(interface_id).or_default();
            let expected = stack.last().copied();
            if expected != Some(handler_id.unwrap()) {
                return Err(InterleavingError::HandlerGetMismatch {
                    interface_id,
                    expected,
                    got: handler_id.unwrap(),
                });
            }
        }

        WitLedgerEffect::Activation { val, caller } => {
            let curr = state.id_curr;

            let Some((v, c)) = &state.activation[curr.0] else {
                return Err(InterleavingError::Shape(
                    "Activation called with no arg set",
                ));
            };

            if v != &val.unwrap() || c != &caller.unwrap() {
                return Err(InterleavingError::Shape("Activation result mismatch"));
            }
        }

        WitLedgerEffect::Init { val, caller } => {
            let curr = state.id_curr;

            let Some((v, c)) = state.init[curr.0].take() else {
                return Err(InterleavingError::Shape("Init called with no arg set"));
            };

            if v != val.unwrap() || c != caller.unwrap() {
                return Err(InterleavingError::Shape("Init result mismatch"));
            }
        }

        WitLedgerEffect::NewRef { size, ret } => {
            if state.ref_building.contains_key(&id_curr) {
                return Err(InterleavingError::BuildingRefButCalledOther(id_curr));
            }
            let new_ref = Ref(state.ref_counter);
            let size_words = size;
            let size_elems = size_words * REF_PUSH_WIDTH;
            state.ref_counter += size_elems as u64;
            if new_ref != ret.unwrap() {
                return Err(InterleavingError::RefInitializationMismatch(
                    ret.unwrap(),
                    new_ref,
                ));
            }
            state.ref_store.insert(new_ref, vec![Value(0); size_elems]);
            state.ref_sizes.insert(new_ref, size_words);
            state.ref_building.insert(id_curr, (new_ref, 0, size_words));
        }

        WitLedgerEffect::RefPush { vals } => {
            let (reff, offset_words, size_words) = state
                .ref_building
                .remove(&id_curr)
                .ok_or(InterleavingError::RefPushNotBuilding(id_curr))?;

            let new_offset = offset_words + 1;

            let vec = state
                .ref_store
                .get_mut(&reff)
                .ok_or(InterleavingError::RefNotFound(reff))?;

            for (i, val) in vals.iter().enumerate() {
                let pos = (offset_words * REF_PUSH_WIDTH) + i;
                if pos >= size_words * REF_PUSH_WIDTH && *val != Value::nil() {
                    return Err(InterleavingError::RefPushOutOfBounds {
                        pid: id_curr,
                        size: size_words,
                    });
                }

                if let Some(pos) = vec.get_mut(pos) {
                    *pos = *val;
                }
            }

            if new_offset < size_words {
                state
                    .ref_building
                    .insert(id_curr, (reff, new_offset, size_words));
            }
        }

        WitLedgerEffect::RefGet { reff, offset, ret } => {
            let vec = state
                .ref_store
                .get(&reff)
                .ok_or(InterleavingError::RefNotFound(reff))?;
            let size_words = state
                .ref_sizes
                .get(&reff)
                .copied()
                .ok_or(InterleavingError::RefNotFound(reff))?;
            if offset >= size_words {
                return Err(InterleavingError::Shape("RefGet out of bounds"));
            }
            let mut val = [Value::nil(); REF_GET_WIDTH];
            for (i, slot) in val.iter_mut().enumerate() {
                let idx = (offset * REF_GET_WIDTH) + i;
                if idx < size_words * REF_GET_WIDTH {
                    *slot = vec[idx];
                }
            }
            if val != ret.unwrap() {
                return Err(InterleavingError::Shape("RefGet result mismatch"));
            }
        }

        WitLedgerEffect::RefWrite { reff, offset, vals } => {
            let vec = state
                .ref_store
                .get_mut(&reff)
                .ok_or(InterleavingError::RefNotFound(reff))?;
            let size_words = state
                .ref_sizes
                .get(&reff)
                .copied()
                .ok_or(InterleavingError::RefNotFound(reff))?;

            if offset >= size_words {
                return Err(InterleavingError::Shape("RefWrite out of bounds"));
            }

            for (i, val) in vals.iter().enumerate() {
                let idx = (offset * REF_WRITE_WIDTH) + i;
                vec[idx] = *val;
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

            let ret_val = state
                .ref_store
                .get(&ret)
                .ok_or(InterleavingError::RefNotFound(ret))?;

            if let Some(expected_ref) = state.expected_input[parent.0] {
                let expected_val = state
                    .ref_store
                    .get(&expected_ref)
                    .ok_or(InterleavingError::RefNotFound(expected_ref))?;

                if expected_val != ret_val {
                    // Burn is the final return of the coroutine
                    return Err(InterleavingError::YieldClaimMismatch {
                        id_prev: state.id_prev,
                        expected: expected_val.clone(),
                        got: ret_val.clone(),
                    });
                }
            }

            state.activation[id_curr.0] = None;
            state.finalized[id_curr.0] = true;
            state.did_burn[id_curr.0] = true;
            state.expected_input[id_curr.0] = Some(ret);
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
