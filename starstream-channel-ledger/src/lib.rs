use std::{
    cell::RefCell,
    collections::BTreeMap,
    mem::replace,
    sync::{Arc, Mutex},
};

use bincode::Encode;
use k256::schnorr::{Signature, VerifyingKey};
use replace_with::replace_with_or_abort;
use wasmi::{Engine, Extern, Func, FuncType, Instance, Module, Store};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Encode)]
pub struct Hash([u8; 32]);

fn hash(input: &[u8]) -> Hash {
    let h = blake3::hash(input);
    Hash(*h.as_bytes())
}

#[derive(Clone, Copy, PartialEq, Eq, Encode)]
pub struct TxHash(pub Hash);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Encode)]
pub struct UTXORef(pub Hash);

#[derive(Clone, Copy, PartialEq, Eq, Encode)]
pub struct Init(pub Hash);

#[derive(Clone, Copy, PartialEq, Eq, Encode, PartialOrd, Ord)]
pub struct ProgramHash(pub Hash);

pub struct Annex {
    program_id_list: Vec<ProgramId>,
}

#[derive(Clone, Copy, PartialEq, Eq, Encode, PartialOrd, Ord)]
pub struct AnnexHash(pub Hash);

#[derive(Clone, Copy, PartialEq, Eq, Encode)]
pub struct ProgramId {
    program_hash: ProgramHash,
    annex_hash: AnnexHash,
    init: Init,
}

#[derive(Clone, Copy, PartialEq, Eq, Encode)]
pub struct User(pub Hash);

#[derive(Clone, Copy, PartialEq, Eq, Encode)]
pub struct Channel(pub Hash);

type Channels = BTreeMap<u32, Channel>;

#[derive(Encode)]
pub enum SomeProgramId {
    Any,
    Own,
    Some(ProgramId),
}

#[derive(Encode)]
pub enum ProgramOp {
    Connect { at: u32 },
    Sync { at: u32, n: u32 },
    Swap { at: u32, this: u32 },
    Handshake { at: u32, program_id: SomeProgramId },
    Ping { at: u32 },
    Pong { at: u32 },
    ConnectOwn { at: (u32, u32) },
    SwapOwn { this: (u32, u32) },
    Close { at: u32 },
    Release { this: u32 },
}

#[derive(Encode)]
pub enum Action {
    Pair(u32, u32),
    Connect { program: u32, at: u32 },
    Swap { program: u32, at: u32, this: u32 },
    Sync { program: u32, at: u32 },
    HandleOwn { program: u32 },
    Capture { program: u32, into: u32 },
    ConnectOwn { at: (u32, u32) },
}

#[derive(Clone)]
pub struct ProgramState(pub Vec<u8>);

struct TxState {
    operationss: Vec<Vec<ProgramOp>>,
    channelss: Vec<Channels>,
    free_channels: Channels,
    new_channel_counter: u32,
}

#[derive(Encode)]
pub enum Output {
    Program(u32),
    User { user: User, channels: Vec<u32> },
}

#[derive(Encode)]
pub struct Tx {
    // for every input, the number of times (- 1) it's entered/executed is included
    existing_inputs: Vec<(UTXORef, u32)>,
    new_program_inputs: Vec<(ProgramId, u32)>,
    new_init_inputs: Vec<(ProgramHash, AnnexHash, u32)>,
    actions: Vec<Action>,
    outputs: Vec<Output>,
    witness: Vec<u32>,
}

pub enum UTXO {
    Program {
        program_id: ProgramId,
        program_state: ProgramState,
        channels: Channels,
    },
    User {
        user: User,
        channels: Vec<Channel>,
    },
}

pub struct Ledger(BTreeMap<UTXORef, UTXO>);

fn hash_tx(tx: &Tx) -> TxHash {
    // TODO: inefficient, we could hash as we serialize
    let v = bincode::encode_to_vec(tx, bincode::config::standard()).expect("impossible");
    TxHash(hash(v.as_slice()))
}

fn init_salt() -> Hash {
    hash("starstream init".as_bytes())
}

fn create_init(tx_hash: TxHash, i: u32) -> Init {
    Init(hash(
        init_salt()
            .0
            .into_iter()
            .chain(tx_hash.0.0)
            .chain(i.to_le_bytes())
            .collect::<Vec<_>>()
            .as_slice(),
    ))
}

fn utxo_ref_salt() -> Hash {
    hash("starstream utxo ref salt".as_bytes())
}

fn create_utxo_ref(tx_hash: TxHash, i: u32) -> UTXORef {
    UTXORef(hash(
        utxo_ref_salt()
            .0
            .into_iter()
            .chain(tx_hash.0.0)
            .chain(i.to_le_bytes())
            .collect::<Vec<_>>()
            .as_slice(),
    ))
}

fn channel_salt() -> Hash {
    hash("starstream channel".as_bytes())
}

fn create_channel(tx_hash: TxHash, idx: u32) -> Channel {
    Channel(hash(
        channel_salt()
            .0
            .into_iter()
            .chain(tx_hash.0.0)
            .chain(idx.to_le_bytes())
            .collect::<Vec<_>>()
            .as_slice(),
    ))
}

fn remove_channel(channels: &mut Channels, idx: u32) -> Channel {
    channels.remove(&idx).expect("invalid channel")
}

fn set_channel(channels: &mut Channels, idx: u32, c: Channel) {
    let _ = channels.insert(idx, c);
}

fn match_channels((channels_x, at_x): (&Channels, u32), (channels_y, at_y): (&Channels, u32)) {
    if channels_x.get(&at_x).expect("invalid channel")
        != channels_y.get(&at_y).expect("invalid channel")
    {
        panic!("channels don't match");
    }
}

fn match_program_id(left: ProgramId, right: SomeProgramId, own: ProgramId) {
    match right {
        SomeProgramId::Any => {}
        SomeProgramId::Own => {
            if left != own {
                panic!("program id mismatch")
            }
        }
        SomeProgramId::Some(right) => {
            if left != right {
                panic!("program id mismatch")
            }
        }
    }
}

// WASM
type Program = Vec<u8>;

type ProgramStore = BTreeMap<ProgramHash, Program>;

fn step(
    tx_hash: TxHash,
    program_ids: &[ProgramId],
    TxState {
        mut operationss,
        mut channelss,
        mut free_channels,
        new_channel_counter,
    }: TxState,
    action: Action,
) -> TxState {
    match action {
        Action::Pair(x, y) => {
            let program_id_x = program_ids[x as usize];
            let program_id_y = program_ids[y as usize];
            let Ok([operations_x, operations_y]) =
                operationss.get_disjoint_mut([x as usize, y as usize])
            else {
                panic!()
            };
            let Ok([channels_x, channels_y]) = channelss.get_disjoint_mut([x as usize, y as usize])
            else {
                panic!()
            };
            let mut new_channel_counter = new_channel_counter;
            let operation_x = operations_x.pop().expect("nothing to pair");
            let operation_y = operations_y.pop().expect("nothing to pair");
            match (operation_x, operation_y) {
                (ProgramOp::Connect { at: at_x }, ProgramOp::Connect { at: at_y }) => {
                    let channel = create_channel(tx_hash, new_channel_counter);
                    set_channel(channels_x, at_x, channel);
                    set_channel(channels_y, at_y, channel);
                    *(&mut new_channel_counter) += 1;
                }
                (ProgramOp::Sync { at: at_x, n: n_x }, ProgramOp::Sync { at: at_y, n: n_y }) => {
                    match_channels((channels_x, at_x), (channels_y, at_y));
                    if n_x != n_y {
                        panic!("invalid match")
                    }
                }
                (
                    ProgramOp::Swap {
                        at: at_x,
                        this: this_x,
                    },
                    ProgramOp::Swap {
                        at: at_y,
                        this: this_y,
                    },
                ) => {
                    match_channels((channels_x, at_x), (channels_y, at_y));
                    let channel_x = remove_channel(channels_x, this_x);
                    let channel_y = remove_channel(channels_y, this_y);
                    set_channel(channels_x, this_x, channel_y);
                    set_channel(channels_y, this_y, channel_x);
                }
                (
                    ProgramOp::Handshake {
                        at: at_x,
                        program_id: program_id_y_,
                    },
                    ProgramOp::Handshake {
                        at: at_y,
                        program_id: program_id_x_,
                    },
                ) => {
                    match_channels((channels_x, at_x), (channels_y, at_y));
                    match_program_id(program_id_x, program_id_x_, program_id_y);
                    match_program_id(program_id_y, program_id_y_, program_id_x);
                }
                (ProgramOp::Pong { at: at_x }, ProgramOp::Ping { at: at_y })
                | (ProgramOp::Ping { at: at_x }, ProgramOp::Pong { at: at_y }) => {
                    match_channels((channels_x, at_x), (channels_y, at_y));
                }
                _ => panic!(),
            }
            TxState {
                channelss,
                operationss,
                free_channels,
                new_channel_counter,
            }
        }
        Action::HandleOwn { program } => {
            let operations = &mut operationss[program as usize];
            let channels = &mut channelss[program as usize];
            let mut new_channel_counter = new_channel_counter;
            let op = operations.pop().expect("nothing to handle");
            match op {
                ProgramOp::ConnectOwn { at: (at_x, at_y) } => {
                    let channel = create_channel(tx_hash, new_channel_counter);
                    set_channel(channels, at_x, channel);
                    set_channel(channels, at_y, channel);
                    new_channel_counter += 1;
                }
                ProgramOp::SwapOwn {
                    this: (this_x, this_y),
                } => {
                    let channel_x = remove_channel(channels, this_x);
                    let channel_y = remove_channel(channels, this_y);
                    set_channel(channels, this_x, channel_y);
                    set_channel(channels, this_y, channel_x);
                }
                _ => panic!(),
            }
            TxState {
                operationss,
                channelss,
                free_channels,
                new_channel_counter,
            }
        }
        Action::Capture { program, into } => {
            let operations = &mut operationss[program as usize];
            let channels = &mut channelss[program as usize];
            let op = operations.pop().expect("nothing to capture");
            match op {
                ProgramOp::Release { this } => {
                    let channel = remove_channel(channels, this);
                    let _ = free_channels.insert(into, channel);
                }
                _ => panic!(),
            }
            TxState {
                channelss,
                operationss,
                free_channels,
                new_channel_counter,
            }
        }
        Action::Connect { program, at } => {
            let operations = &mut operationss[program as usize];
            let channels = &mut channelss[program as usize];
            let mut new_channel_counter = new_channel_counter;
            let op = operations.pop().expect("nothing to connect");
            match op {
                ProgramOp::Connect { at: at_program } => {
                    let channel = create_channel(tx_hash, new_channel_counter);
                    set_channel(&mut free_channels, at, channel);
                    set_channel(channels, at_program, channel);
                    new_channel_counter += 1;
                }
                _ => panic!(),
            }
            TxState {
                channelss,
                operationss,
                free_channels,
                new_channel_counter,
            }
        }
        Action::Swap { program, at, this } => {
            let operations = &mut operationss[program as usize];
            let channels = &mut channelss[program as usize];
            let op = operations.pop().expect("nothing to swap");
            match op {
                ProgramOp::Swap {
                    at: at_program,
                    this: this_program,
                } => {
                    match_channels((&free_channels, at), (channels, at_program));
                    let channel_free = remove_channel(&mut free_channels, this);
                    let channel_program = remove_channel(channels, this_program);
                    set_channel(&mut free_channels, at, channel_program);
                    set_channel(channels, at_program, channel_free);
                }
                _ => panic!(),
            }
            TxState {
                channelss,
                operationss,
                free_channels,
                new_channel_counter,
            }
        }
        Action::Sync { program, at } => {
            let operations = &mut operationss[program as usize];
            let channels = &mut channelss[program as usize];
            let op = operations.pop().expect("nothing to swap");
            match op {
                ProgramOp::Sync {
                    at: at_program,
                    n: _,
                }
                | ProgramOp::Handshake {
                    at: at_program,
                    program_id: SomeProgramId::Any,
                }
                | ProgramOp::Ping { at: at_program }
                | ProgramOp::Pong { at: at_program } => {
                    match_channels((&free_channels, at), (channels, at_program));
                }
                _ => panic!(),
            }
            TxState {
                channelss,
                operationss,
                free_channels,
                new_channel_counter,
            }
        }
        Action::ConnectOwn { at: (at_x, at_y) } => {
            let channel = create_channel(tx_hash, new_channel_counter);
            set_channel(&mut free_channels, at_x, channel);
            set_channel(&mut free_channels, at_y, channel);
            TxState {
                channelss,
                operationss,
                free_channels,
                new_channel_counter: new_channel_counter + 1,
            }
        }
    }
}

fn verify_signed(signatures: &[(Signature, VerifyingKey)], user: User, tx: TxHash) {
    for (sig, vk) in signatures {
        let user_ = hash(vk.to_bytes().as_slice());
        if user_ == user.0 && vk.verify_raw(tx.0.0.as_slice(), sig).is_ok() {
            return;
        }
    }
    panic!("unsigned")
}

fn run_script(
    wasm: &[u8],
    program_ids: &[ProgramId],
    ProgramState(input): ProgramState,
    is_initial: bool,
    witness: impl FnMut() -> u32 + Send + Sync + 'static,
    out: impl FnMut(ProgramOp) + Send + Sync + 'static,
) -> ProgramState {
    let engine = Engine::new(&Default::default());
    let mut store = Store::new(&engine, ());
    let module = Module::new(&engine, wasm).expect("invalid WASM module?");
    struct State<Witness, Out> {
        input: Vec<u8>,
        output: Vec<u8>,
        output_len: u32,
        out: Out,
        witness: Witness,
        program_ids: Vec<ProgramId>,
    }
    let state = State {
        input,
        output: Vec::new(),
        output_len: 0,
        out,
        witness,
        program_ids: Vec::from(program_ids),
    };
    let state: Arc<Mutex<Option<State<_, _>>>> = Arc::new(Mutex::new(Some(state)));
    let state_read_input = state.clone();
    let read_input = move |index: u32| {
        let guard = state_read_input.lock().expect("should be impossible");
        let state = guard.as_ref().expect("should be impossible");
        state.input.get(index as usize).expect("input not that big");
    };
    let state_write_output = state.clone();
    let write_output = move |index: u32, value: u32| {
        if value > 255 {
            panic!("must be byte")
        }
        let mut guard = state_write_output.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        if state.output.len() <= index as usize {
            state.output.resize(index as usize + 1, 0);
        }
        state.output[index as usize] = value as u8;
    };
    let state_length_input = state.clone();
    let length_input = move || {
        let guard = state_length_input.lock().expect("should be impossible");
        let state = guard.as_ref().expect("should be impossible");
        state.input.len() as u32
    };
    let state_set_length_output = state.clone();
    let set_length_output = move |length: u32| {
        let mut guard = state_set_length_output
            .lock()
            .expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        state.output_len = length;
    };
    let is_initial = move || if is_initial { 1 } else { 0 };
    let state_connect = state.clone();
    let connect = move |at| {
        let mut guard = state_connect.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Connect { at })
    };
    let state_sync = state.clone();
    let sync = move |at, n| {
        let mut guard = state_sync.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Sync { at, n })
    };
    let state_swap = state.clone();
    let swap = move |at, this| {
        let mut guard = state_swap.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Swap { at, this })
    };
    let state_handshake_any = state.clone();
    let handshake_any = move |at| {
        let mut guard = state_handshake_any.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Handshake {
            at,
            program_id: SomeProgramId::Any,
        })
    };
    let state_handshake_self = state.clone();
    let handshake_self = move |at| {
        let mut guard = state_handshake_self.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Handshake {
            at,
            program_id: SomeProgramId::Own,
        })
    };
    let state_handshake = state.clone();
    let handshake = move |at, program_id_index: u32| {
        let mut guard = state_handshake.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Handshake {
            at,
            program_id: SomeProgramId::Some(state.program_ids[program_id_index as usize]),
        })
    };
    let state_ping = state.clone();
    let ping = move |at| {
        let mut guard = state_ping.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Ping { at })
    };
    let state_pong = state.clone();
    let pong = move |at| {
        let mut guard = state_pong.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Ping { at })
    };
    let state_connect_own = state.clone();
    let connect_own = move |first: u32, second: u32| {
        let mut guard = state_connect_own.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::ConnectOwn {
            at: (first, second),
        })
    };
    let state_swap_own = state.clone();
    let swap_own = move |first: u32, second: u32| {
        let mut guard = state_swap_own.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::SwapOwn {
            this: (first, second),
        })
    };
    let state_close = state.clone();
    let close = move |at: u32| {
        let mut guard = state_close.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Close { at })
    };
    let state_release = state.clone();
    let release = move |this: u32| {
        let mut guard = state_release.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.out)(ProgramOp::Release { this })
    };
    let state_witness = state.clone();
    let witness = move || -> u32 {
        let mut guard = state_witness.lock().expect("should be impossible");
        let state = guard.as_mut().expect("should be impossible");
        (state.witness)()
    };
    let imports = [
        Extern::Func(Func::wrap(&mut store, read_input)),
        Extern::Func(Func::wrap(&mut store, write_output)),
        Extern::Func(Func::wrap(&mut store, length_input)),
        Extern::Func(Func::wrap(&mut store, set_length_output)),
        Extern::Func(Func::wrap(&mut store, is_initial)),
        Extern::Func(Func::new(&mut store, FuncType::new([], []), |_, _, _| {
            Err(wasmi::Error::i32_exit(0))
        })),
        Extern::Func(Func::wrap(&mut store, connect)),
        Extern::Func(Func::wrap(&mut store, sync)),
        Extern::Func(Func::wrap(&mut store, swap)),
        Extern::Func(Func::wrap(&mut store, handshake_any)),
        Extern::Func(Func::wrap(&mut store, handshake_self)),
        Extern::Func(Func::wrap(&mut store, handshake)),
        Extern::Func(Func::wrap(&mut store, ping)),
        Extern::Func(Func::wrap(&mut store, pong)),
        Extern::Func(Func::wrap(&mut store, connect_own)),
        Extern::Func(Func::wrap(&mut store, swap_own)),
        Extern::Func(Func::wrap(&mut store, close)),
        Extern::Func(Func::wrap(&mut store, release)),
        Extern::Func(Func::new(&mut store, FuncType::new([], []), |_, _, _| {
            Err(wasmi::Error::new("failed"))
        })),
        Extern::Func(Func::wrap(&mut store, witness)),
    ];
    let instance =
        Instance::new(&mut store, &module, &imports).expect("couldn't instantiate WASM module?");
    let main = instance
        .get_func(&mut store, "main")
        .expect("must export main");
    let mut outputs = [];
    match main.call(&mut store, &[], &mut outputs) {
        Ok(()) => {}
        Err(e) => match e.kind() {
            wasmi::errors::ErrorKind::I32ExitStatus(0) => {}
            _ => panic!("failed {e}"),
        },
    }
    let State {
        input: _,
        output,
        output_len,
        out: _,
        witness: _,
        program_ids: _,
    } = state
        .lock()
        .expect("should be impossible")
        .take()
        .expect("should be impossible");
    if output.len() != output_len as usize {
        panic!("bad output length");
    }
    ProgramState(output)
}

pub fn apply_tx(
    program_store: &ProgramStore,
    annex_store: &BTreeMap<AnnexHash, Annex>,
    Ledger(mut ledger): Ledger,
    tx: Tx,
    signatures: Vec<(Signature, VerifyingKey)>,
) -> Ledger {
    let tx_hash = hash_tx(&tx);
    let Tx {
        existing_inputs,
        new_program_inputs,
        new_init_inputs,
        actions,
        outputs,
        mut witness,
    }: Tx = tx;

    witness.reverse();

    let mut channelss = Vec::new();
    let mut operationss: Vec<Vec<ProgramOp>> = Vec::new();
    let mut program_ids = Vec::new();
    let mut states: Vec<ProgramState> = Vec::new();
    // let mut states_old: Vec<ProgramState> = Vec::new();

    let mut free_channels: Channels = Channels::new();
    let mut free_channel_counter = 0;
    for (existing_input, count) in existing_inputs {
        match ledger.remove(&existing_input).expect("missing input") {
            UTXO::Program {
                program_id,
                program_state,
                channels,
            } => {
                let wasm = program_store
                    .get(&program_id.program_hash)
                    .expect("no such program");
                let annex = annex_store
                    .get(&program_id.annex_hash)
                    .expect("no such annex");
                channelss.push(channels);
                program_ids.push(program_id);
                // states_old.push(program_state.clone());
                let operations = Arc::new(Mutex::new(Vec::new()));
                let state = RefCell::new(Some(program_state));
                for _ in 0..=count {
                    let operations = operations.clone();
                    replace_with_or_abort(&mut witness, |w| {
                        let w = Arc::new(Mutex::new(Some(w)));
                        let w_ = w.clone();
                        state.replace_with(|state| {
                            Some(run_script(
                                &wasm,
                                &annex.program_id_list,
                                state.take().expect("should be impossible"),
                                false,
                                move || {
                                    w.lock()
                                        .expect("should be impossible")
                                        .as_mut()
                                        .expect("should be impossible")
                                        .pop()
                                        .expect("not enough witnesses")
                                },
                                move |op| operations.lock().expect("should be impossible").push(op),
                            ))
                        });
                        w_.lock()
                            .expect("should be impossible")
                            .take()
                            .expect("should be impossible")
                    });
                }
                let mut guard = operations.lock().expect("should be impossible");
                let mut operations = replace(&mut *guard, Vec::new());
                operations.reverse();
                operationss.push(operations);
                states.push(state.take().expect("should be impossible"));
            }
            UTXO::User { user, channels } => {
                verify_signed(&signatures, user, tx_hash);
                for channel in channels {
                    free_channels.insert(free_channel_counter, channel);
                    free_channel_counter += 1;
                }
            }
        }
    }
    for (program_id, count) in new_program_inputs {
        let wasm = program_store
            .get(&program_id.program_hash)
            .expect("no such program");
        let annex = annex_store
            .get(&program_id.annex_hash)
            .expect("no such annex");
        channelss.push(Channels::default());
        program_ids.push(program_id);
        // states_old.push(program_state.clone());
        let operations = Arc::new(Mutex::new(Vec::new()));
        let state = RefCell::new(Some(ProgramState(Vec::new())));
        for _ in 0..=count {
            let operations = operations.clone();
            replace_with_or_abort(&mut witness, |w| {
                let w = Arc::new(Mutex::new(Some(w)));
                let w_ = w.clone();
                state.replace_with(|state| {
                    Some(run_script(
                        &wasm,
                        &annex.program_id_list,
                        state.take().expect("should be impossible"),
                        false,
                        move || {
                            w.lock()
                                .expect("should be impossible")
                                .as_mut()
                                .expect("should be impossible")
                                .pop()
                                .expect("not enough witnesses")
                        },
                        move |op| operations.lock().expect("should be impossible").push(op),
                    ))
                });
                w_.lock()
                    .expect("should be impossible")
                    .take()
                    .expect("should be impossible")
            });
        }
        let mut guard = operations.lock().expect("should be impossible");
        let mut operations = replace(&mut *guard, Vec::new());
        operations.reverse();
        operationss.push(operations);
        states.push(state.take().expect("should be impossible"));
    }
    for (i, (program_hash, annex_hash, count)) in new_init_inputs.into_iter().enumerate() {
        let wasm = program_store.get(&program_hash).expect("no such program");
        let annex = annex_store.get(&annex_hash).expect("no such annex");
        channelss.push(Channels::default());
        program_ids.push(ProgramId {
            program_hash,
            annex_hash,
            init: create_init(tx_hash, i as u32),
        });
        // states_old.push(program_state.clone());
        let operations = Arc::new(Mutex::new(Vec::new()));
        let state = RefCell::new(Some(ProgramState(Vec::new())));
        for _ in 0..=count {
            let operations = operations.clone();
            replace_with_or_abort(&mut witness, |w| {
                let w = Arc::new(Mutex::new(Some(w)));
                let w_ = w.clone();
                state.replace_with(|state| {
                    Some(run_script(
                        &wasm,
                        &annex.program_id_list,
                        state.take().expect("should be impossible"),
                        true,
                        move || {
                            w.lock()
                                .expect("should be impossible")
                                .as_mut()
                                .expect("should be impossible")
                                .pop()
                                .expect("not enough witnesses")
                        },
                        move |op| operations.lock().expect("should be impossible").push(op),
                    ))
                });
                w_.lock()
                    .expect("should be impossible")
                    .take()
                    .expect("should be impossible")
            });
        }
        let mut guard = operations.lock().expect("should be impossible");
        let mut operations = replace(&mut *guard, Vec::new());
        operations.reverse();
        operationss.push(operations);
        states.push(state.take().expect("should be impossible"));
    }

    let mut state = TxState {
        operationss,
        channelss,
        free_channels,
        new_channel_counter: 0,
    };
    for action in actions {
        state = step(tx_hash, &program_ids, state, action);
    }
    let TxState {
        operationss,
        mut channelss,
        mut free_channels,
        new_channel_counter: _,
    } = state;

    for operations in operationss {
        if !operations.is_empty() {
            panic!("unprocessed operations left")
        }
    }

    for (output_counter, output) in outputs.into_iter().enumerate() {
        let utxo_ref = create_utxo_ref(tx_hash, output_counter as u32);
        let utxo = match output {
            Output::Program(idx) => {
                let program_id = program_ids.remove(idx as usize);
                let channels = channelss.remove(idx as usize);
                let program_state = states.remove(idx as usize);
                UTXO::Program {
                    program_id,
                    program_state,
                    channels,
                }
            }
            Output::User { user, channels } => {
                let mut channels_ = Vec::new();
                for at in channels {
                    channels_.push(remove_channel(&mut free_channels, at));
                }
                UTXO::User {
                    user,
                    channels: channels_,
                }
            }
        };
        ledger.insert(utxo_ref, utxo);
    }

    for ProgramState(state) in states {
        if !state.is_empty() {
            panic!("can't delete UTXO with non-empty state");
        }
    }

    Ledger(ledger)
}
