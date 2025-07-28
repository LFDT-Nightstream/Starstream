//! Starstream VM as a library.
#![allow(dead_code)] // We're WIP enough that some dead code is to be expected.

use std::{collections::HashMap, sync::Arc};

use byteorder::{LittleEndian, ReadBytesExt};
pub use code::ContractCode;
use code::{CodeCache, CodeHash};
use log::{debug, info, trace};
use rand::RngCore;
use sha2::{Sha256, digest::DynDigest};
use tiny_keccak::Hasher;
use util::DisplayHex;
use wasmi::{
    AsContext, AsContextMut, Caller, Config, Engine, ExternRef, ExternType, ImportType, Instance,
    Linker, ResumableCall, Store, StoreContext, StoreContextMut, Value, core::HostError,
};

mod code;
mod mermaid;
mod nebula;
mod util;

fn memory<'a, T>(caller: &'a mut Caller<T>) -> (&'a mut [u8], &'a mut T) {
    caller
        .get_export("memory")
        .unwrap()
        .into_memory()
        .unwrap()
        .data_and_store_mut(caller.as_context_mut())
}

// ----------------------------------------------------------------------------
// Asyncify

/*
enum AsyncifyState {
    Normal = 0,
    Unwind = 1,
    Rewind = 2,
}

/// Where the unwind/rewind data structure will live.
const STACK_START: u32 = 16;
const STACK_END: u32 = 1024;

fn asyncify(blob: &[u8]) -> Vec<u8> {
    let mut module = binaryen::Module::read(blob).unwrap();
    module
        .run_optimization_passes(["asyncify"], &binaryen::CodegenConfig::default())
        .unwrap();
    module.write()
}
*/

// ----------------------------------------------------------------------------

fn fake_import<T>(linker: &mut Linker<T>, import: &ImportType, message: &str) {
    if let ExternType::Func(func) = import.ty() {
        let message = format!("{}:{}: {}", import.module(), import.name(), message);

        let r = linker.func_new(
            import.module(),
            import.name(),
            func.clone(),
            move |_caller, _inputs, _outputs| {
                panic!("{}", message);
            },
        );
        if !matches!(
            r,
            Err(wasmi::errors::LinkerError::DuplicateDefinition { .. })
        ) {
            r.unwrap();
        }
    }
}

// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Interrupt {
    // Common
    CoordinationCode {
        return_addr: u32,
    },
    RegisterEffectHandler {
        name: String,
        handler_addr: u32,
    },
    UnRegisterEffectHandler {
        name: String,
    },
    GetRaisedEffectData {
        name: String,
        output_ptr_data: u32,
        not_null: u32,
    },
    ResumeThrowingProgram {
        name: String,
        input_ptr_data: u32,
    },
    // Coordination -> UTXO
    UtxoNew {
        code: CodeHash,
        entry_point: String,
        inputs: Vec<Value>,
    },
    UtxoResume {
        utxo_id: UtxoId,
        inputs: Vec<Value>,
    },
    UtxoQuery {
        utxo_id: UtxoId,
        method: String,
        inputs: Vec<Value>,
    },
    UtxoMutate {
        utxo_id: UtxoId,
        method: String,
        inputs: Vec<Value>,
    },
    UtxoConsume {
        utxo_id: UtxoId,
        method: String,
        inputs: Vec<Value>,
    },
    // Coordination <- UTXO
    Yield {
        name: String,
        data: u32,
        resume_arg: u32,
        resume_arg_len: u32,
    },
    Raise {
        name: String,
        data: u32,
        data_len: u32,
        resume_arg: u32,
        resume_arg_len: u32,
    },
    // UTXO -> Token
    TokenBind {
        code: CodeHash,
        entry_point: String,
        inputs: Vec<Value>,
    },
    TokenUnbind {
        token_id: TokenId,
        // Sanity checking - must match that of the token.
        //code: CodeHash,
        //unbind_fn: String,
    },
    GetTokens {
        data: u32,
        data_len: u32,
        skip: u32,
    },
}

type WasmiError = wasmi::core::Trap;

#[inline]
fn host(i: Interrupt) -> Result<(), WasmiError> {
    Err(wasmi::core::Trap::from(i))
}

impl std::fmt::Display for Interrupt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl HostError for Interrupt {}

fn starstream_eprint<T>(mut caller: Caller<T>, ptr: u32, len: u32) {
    let (memory, _) = memory(&mut caller);
    let slice = &memory[ptr as usize..(ptr + len) as usize];
    info!(target: "program", "{}", String::from_utf8_lossy(slice));
}

/// Fulfiller of imports from `env`.
#[allow(clippy::unused_unit)] // False positive. `clippy --fix` breaks the code.
fn starstream_env<T>(linker: &mut Linker<T>, module: &str, this_code: &ContractCode) {
    let this_code = this_code.hash();

    linker
        .func_wrap(module, "abort", || -> () {
            panic!("contract called abort()");
        })
        .unwrap();
    linker
        .func_wrap(
            module,
            "eprint",
            |caller: Caller<T>, ptr: u32, len: u32| -> () {
                starstream_eprint(caller, ptr, len);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_coordination_code",
            move |return_addr: u32| -> Result<(), WasmiError> {
                trace!("starstream_coordination_code({return_addr:#x})");
                host(Interrupt::CoordinationCode { return_addr })
            },
        )
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_this_code",
            move |mut caller: Caller<T>, return_addr: u32| {
                trace!("starstream_this_code({return_addr:#x})");
                let (memory, _) = memory(&mut caller);
                let hash = this_code.raw();
                memory[return_addr as usize..return_addr as usize + hash.len()]
                    .copy_from_slice(&hash);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_keccak256",
            |mut caller: Caller<T>, ptr: u32, len: u32, return_addr: u32| {
                let mut hasher = tiny_keccak::Keccak::v256();

                let (memory, _) = memory(&mut caller);
                let slice = &memory[ptr as usize..(ptr + len) as usize];

                hasher.update(slice);

                hasher.finalize(&mut memory[return_addr as usize..return_addr as usize + 32]);
            },
        )
        .unwrap();

    linker
        .func_wrap(
            module,
            "starstream_register_effect_handler",
            move |mut caller: Caller<T>, ptr: u32, len: u32, handler_addr: i32| {
                let (memory, _) = memory(&mut caller);

                let name_slice = &memory[ptr as usize..(ptr + len) as usize];

                host(Interrupt::RegisterEffectHandler {
                    name: String::from_utf8_lossy(name_slice).into_owned(),
                    handler_addr: handler_addr as u32,
                })
            },
        )
        .unwrap();

    linker
        .func_wrap(
            module,
            "starstream_unregister_effect_handler",
            move |mut caller: Caller<T>, ptr: u32, len: u32| {
                let (memory, _) = memory(&mut caller);

                let slice = &memory[ptr as usize..(ptr + len) as usize];
                host(Interrupt::UnRegisterEffectHandler {
                    name: String::from_utf8_lossy(slice).into_owned(),
                })
            },
        )
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_get_raised_effect_data",
            move |mut caller: Caller<T>,
                  ptr: u32,
                  len: u32,
                  output_ptr_data: u32,
                  not_null: u32| {
                let (memory, _) = memory(&mut caller);

                let slice = &memory[ptr as usize..(ptr + len) as usize];
                host(Interrupt::GetRaisedEffectData {
                    name: String::from_utf8_lossy(slice).into_owned(),
                    output_ptr_data,
                    not_null,
                })
            },
        )
        .unwrap();

    linker
        .func_wrap(
            module,
            "starstream_resume_throwing_program",
            move |mut caller: Caller<T>, ptr: u32, len: u32, input_ptr_data: u32| {
                let (memory, _) = memory(&mut caller);

                let slice = &memory[ptr as usize..(ptr + len) as usize];
                host(Interrupt::ResumeThrowingProgram {
                    name: String::from_utf8_lossy(slice).into_owned(),
                    input_ptr_data,
                })
            },
        )
        .unwrap();
}

/// Fulfiller of imports from `starstream_utxo_env`.
fn starstream_utxo_env<T>(linker: &mut Linker<T>, module: &str) {
    linker
        .func_wrap(
            module,
            "starstream_yield",
            |mut caller: Caller<T>,
             name: u32,
             name_len: u32,
             data: u32,
             _data_len: u32,
             resume_arg: u32,
             resume_arg_len: u32|
             -> Result<(), WasmiError> {
                trace!("starstream_yield()");
                host(Interrupt::Yield {
                    name: std::str::from_utf8(
                        &memory(&mut caller).0[name as usize..(name + name_len) as usize],
                    )
                    .unwrap()
                    .to_owned(),
                    data,
                    resume_arg,
                    resume_arg_len,
                })
            },
        )
        .unwrap();

    linker
        .func_wrap(
            module,
            "starstream_raise",
            |mut caller: Caller<T>,
             name: u32,
             name_len: u32,
             data: u32,
             data_len: u32,
             resume_arg: u32,
             resume_arg_len: u32|
             -> Result<(), WasmiError> {
                trace!("starstream_raise()");
                host(Interrupt::Raise {
                    name: std::str::from_utf8(
                        &memory(&mut caller).0[name as usize..(name + name_len) as usize],
                    )
                    .unwrap()
                    .to_owned(),
                    data,
                    data_len,
                    resume_arg,
                    resume_arg_len,
                })
            },
        )
        .unwrap();

    linker
        .func_wrap(
            module,
            "starstream_get_tokens",
            |_caller: Caller<T>, data: u32, data_len: u32, skip: u32| -> Result<u32, WasmiError> {
                host(Interrupt::GetTokens {
                    data,
                    data_len,
                    skip,
                })
                .map(|_| unreachable!())
            },
        )
        .unwrap();
}

// ----------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct TokenId {
    bytes: [u8; 16],
}

impl TokenId {
    fn random() -> TokenId {
        let mut bytes = [0; 16];
        rand::thread_rng().fill_bytes(&mut bytes);
        TokenId { bytes }
    }

    fn to_wasm_i64(self, mut store: StoreContextMut<TransactionInner>) -> Value {
        let scrambled = rand::thread_rng().next_u64();
        store.data_mut().temporary_token_ids.insert(scrambled, self);
        Value::I64(scrambled as i64)
    }

    fn to_wasm_externref(self, store: StoreContextMut<TransactionInner>) -> Value {
        Value::ExternRef(ExternRef::new::<TokenId>(store, Some(self)))
    }

    fn from_wasm(value: &Value, store: StoreContext<TransactionInner>) -> Option<TokenId> {
        match value {
            Value::I64(scrambled) => store
                .data()
                .temporary_token_ids
                .get(&(*scrambled as u64))
                .copied(),
            Value::ExternRef(handle) => handle.data(store)?.downcast_ref::<TokenId>().copied(),
            _ => None,
        }
    }
}

impl std::fmt::Debug for TokenId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenId({})", DisplayHex(&self.bytes[..]))
    }
}

/*
struct UtxoInstance {
    coordination_code: Arc<ContractCode>,
    code_cache: Arc<CodeCache>,

    tokens: Vec<Token>,
    temporary_token_ids: HashMap<u32, TokenId>,
}
*/

fn utxo_linker(
    engine: &Engine,
    code_cache: &Arc<CodeCache>,
    utxo_code: &ContractCode,
) -> Linker<TransactionInner> {
    let mut linker = Linker::<TransactionInner>::new(engine);

    starstream_env(&mut linker, "env", utxo_code);

    starstream_utxo_env(&mut linker, "starstream_utxo_env");

    let current_code_hash = utxo_code.hash();

    for import in utxo_code.module(engine).imports() {
        if let ExternType::Func(func_ty) = import.ty() {
            if let Some(rest) = import.module().strip_prefix("starstream_token:") {
                if import.name().starts_with("starstream_bind_") {
                    let name = import.name().to_owned();
                    let rest = rest.to_owned();
                    let code_cache = code_cache.clone();
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |_caller, inputs, _outputs| {
                                trace!("{rest}::{name}{inputs:?}");

                                let code = if rest == "this" {
                                    current_code_hash
                                } else {
                                    code_cache.load_debug(&rest).hash()
                                };

                                host(Interrupt::TokenBind {
                                    code,
                                    entry_point: name.clone(),
                                    inputs: inputs.to_vec(),
                                })
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_unbind_") {
                    let name = import.name().to_owned();
                    let rest = rest.to_owned();
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |caller, inputs, _outputs| {
                                trace!("{rest}::{name}{inputs:?}");
                                let token_id =
                                    TokenId::from_wasm(&inputs[0], caller.as_context()).unwrap();
                                host(Interrupt::TokenUnbind {
                                    token_id,
                                    //hash,
                                    //unbind_fn: name.clone(),
                                })
                            },
                        )
                        .unwrap();
                }
            } else {
                fake_import(&mut linker, &import, "not available in UTXO context");
            }
        }
    }

    linker
}

// ----------------------------------------------------------------------------

#[derive(Debug)]
pub struct Utxo {
    program: ProgramIdx,
    tokens: HashMap<TokenId, Token>,
}

// ----------------------------------------------------------------------------

fn token_linker(engine: &Engine, token_code: &Arc<ContractCode>) -> Linker<TransactionInner> {
    let mut linker = Linker::new(engine);

    starstream_env(&mut linker, "env", token_code);

    starstream_utxo_env(&mut linker, "starstream_utxo_env");

    for import in token_code.module(engine).imports() {
        if import.module() != "starstream_utxo_env" {
            fake_import(&mut linker, &import, "Not available in token context");
        }
    }

    linker
}

// ----------------------------------------------------------------------------

#[derive(Debug)]
struct Token {
    bind_program: ProgramIdx,
    id: u64,
    amount: u64,
}

/*
impl Token {
    fn mint(token_code: Arc<ContractCode>, mint_fn: &str, inputs: &[Value]) -> Token {
        let burn_fn = mint_fn.replace("starstream_mint_", "starstream_burn_");
        assert_ne!(mint_fn, burn_fn);

        // Prepend struct return slot to inputs
        let return_addr: usize = 16;
        let inputs = std::iter::once(Value::I32(return_addr as i32))
            .chain(inputs.iter().cloned())
            .collect::<Vec<_>>();

        let engine = Engine::default();
        let mut store = Store::new(&engine, TokenInstance {});
        let linker = token_linker(&engine, &token_code);
        let instance = linker
            .instantiate(&mut store, &token_code.module(&engine))
            .unwrap()
            .ensure_no_start(&mut store)
            .unwrap();
        let mint = instance.get_func(&mut store, &mint_fn).unwrap();
        mint.call(&mut store, &inputs[..], &mut []).unwrap();

        // Read id and amount
        let memory = instance
            .get_export(&store, "memory")
            .unwrap()
            .into_memory()
            .unwrap()
            .data(&store);
        let mut cursor = &memory[return_addr..];
        let id = cursor.read_u64::<LittleEndian>().unwrap();
        let amount = cursor.read_u64::<LittleEndian>().unwrap();
        Token {
            code: token_code,

            burn_fn,
            id,
            amount,
        }
    }

    fn burn(self, burn_fn: &str) {
        assert_eq!(self.burn_fn, burn_fn);

        let engine = Engine::default();
        let mut store = Store::new(&engine, TokenInstance {});
        let linker = token_linker(&engine, &self.code);
        let instance = linker
            .instantiate(&mut store, &self.code.module(&engine))
            .unwrap()
            .ensure_no_start(&mut store)
            .unwrap();
        let burn = instance.get_func(&mut store, burn_fn).unwrap();
        burn.call(
            &mut store,
            &[Value::I64(self.id as i64), Value::I64(self.amount as i64)],
            &mut [],
        )
        .unwrap();
    }
}
*/

// ----------------------------------------------------------------------------

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct UtxoId {
    bytes: [u8; 16],
}

impl UtxoId {
    fn random() -> UtxoId {
        let mut bytes = [0; 16];
        rand::thread_rng().fill_bytes(&mut bytes);
        UtxoId { bytes }
    }

    fn to_wasm_i64(self, mut store: StoreContextMut<TransactionInner>) -> Value {
        let scrambled = rand::thread_rng().next_u64();
        store.data_mut().temporary_utxo_ids.insert(scrambled, self);
        Value::I64(scrambled as i64)
    }

    fn to_wasm_externref(self, store: StoreContextMut<TransactionInner>) -> Value {
        Value::ExternRef(ExternRef::new::<UtxoId>(store, Some(self)))
    }

    fn from_wasm_i64(value: &Value, store: StoreContext<TransactionInner>) -> Option<UtxoId> {
        match value {
            Value::I64(scrambled) => store
                .data()
                .temporary_utxo_ids
                .get(&(*scrambled as u64))
                .copied(),
            _ => None,
        }
    }

    fn from_wasm_externref(value: &Value, store: StoreContext<TransactionInner>) -> Option<UtxoId> {
        match value {
            Value::ExternRef(handle) => handle.data(store)?.downcast_ref::<UtxoId>().copied(),
            _ => None,
        }
    }
}

impl std::fmt::Debug for UtxoId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UtxoId({})", DisplayHex(&self.bytes[..]))
    }
}

fn coordination_script_linker(
    engine: &Engine,
    code_cache: &Arc<CodeCache>,
    coordination_code: Arc<ContractCode>,
) -> Linker<TransactionInner> {
    let mut linker = Linker::<TransactionInner>::new(engine);

    starstream_env(&mut linker, "env", &coordination_code);

    linker
        .func_wrap(
            "starstream_utxo",
            "starstream_status",
            |caller: Caller<TransactionInner>, utxo_id: u64| -> Result<u32, WasmiError> {
                trace!("starstream_status()");
                let utxo_id =
                    UtxoId::from_wasm_i64(&Value::I64(utxo_id as i64), caller.as_context())
                        .expect("invalid utxo");
                let to_program = caller.data().utxos[&utxo_id].program;
                let n = if caller.data().programs[to_program.0].interrupt().is_some() {
                    1
                } else {
                    0
                };
                Ok(n)
            },
        )
        .unwrap();

    let current_code_hash = coordination_code.hash();
    for import in coordination_code.module(engine).imports() {
        if import.module() == "env" {
            // already handled by code above
        } else if let Some(rest) = import.module().strip_prefix("starstream_utxo:") {
            let rest = rest.to_owned();
            if let ExternType::Func(func_ty) = import.ty() {
                let name = import.name().to_owned();
                if import.name().starts_with("starstream_new_") {
                    let code_cache = code_cache.clone();
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |_caller, inputs: &[Value], _outputs| -> Result<(), WasmiError> {
                                trace!("{rest}::{name}{inputs:?}");
                                let code = if rest == "this" {
                                    current_code_hash
                                } else {
                                    code_cache.load_debug(&rest).hash()
                                };
                                host(Interrupt::UtxoNew {
                                    code,
                                    entry_point: name.clone(),
                                    inputs: inputs.to_vec(),
                                })
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_resume_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |caller, inputs, _outputs| {
                                trace!("{name}{inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm_i64(&inputs[0], caller.as_context()).unwrap();
                                host(Interrupt::UtxoResume {
                                    utxo_id,
                                    inputs: inputs.to_vec(),
                                })
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_query_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |caller, inputs, _outputs| {
                                trace!("{rest}::{name}{inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm_i64(&inputs[0], caller.as_context()).unwrap();
                                host(Interrupt::UtxoQuery {
                                    utxo_id,
                                    method: name.clone(),
                                    inputs: inputs[1..].to_vec(),
                                })
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_mutate_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |caller, inputs, _outputs| {
                                trace!("{rest}::{name}{inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm_i64(&inputs[0], caller.as_context()).unwrap();
                                host(Interrupt::UtxoMutate {
                                    utxo_id,
                                    method: name.clone(),
                                    inputs: inputs[1..].to_vec(),
                                })
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_consume_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |caller, inputs, _outputs| {
                                trace!("{rest}::{name}{inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm_i64(&inputs[0], caller.as_context()).unwrap();
                                host(Interrupt::UtxoConsume {
                                    utxo_id,
                                    method: name.clone(),
                                    inputs: inputs[1..].to_vec(),
                                })
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_event_") {
                    fake_import(&mut linker, &import, "TODO starstream_event_");
                } else if import.name().starts_with("starstream_handle_") {
                    fake_import(&mut linker, &import, "TODO starstream_handle_");
                } else {
                    panic!("bad import {import:?}");
                }
            } else {
                panic!("bad import {import:?}");
            }
        } else {
            // Permit out-of-scope imports so a single .wasm module can be used as multiple things.
            fake_import(
                &mut linker,
                &import,
                "not available in Coordination context",
            );
        }
    }

    linker
}

// ----------------------------------------------------------------------------

/// Index into the list of programs loaded by a transaction.
#[derive(PartialEq, Eq, Clone, Copy)]
struct ProgramIdx(usize);

#[allow(non_upper_case_globals)]
impl ProgramIdx {
    const Root: ProgramIdx = ProgramIdx(usize::MAX);
}

impl std::fmt::Debug for ProgramIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            ProgramIdx::Root => f.write_str("Root"),
            ProgramIdx(other) => write!(f, "{}", other),
        }
    }
}

struct TxProgram {
    return_to: ProgramIdx,
    return_is_token: bool,
    yield_to: Option<ProgramIdx>,
    yield_to_constructor: Option<Value>,

    code: CodeHash,
    entry_point: String,
    // Num outputs of root fn of `resumable`. wasmi knows this but doesn't expose it.
    num_outputs: usize,

    // Memory is in here
    instance: Instance,
    // None if just started, Finished if finished, Resumable if yielded
    resumable: ResumableCall,

    utxo: Option<UtxoId>,
}

impl TxProgram {
    fn interrupt(&self) -> Option<&Interrupt> {
        match &self.resumable {
            ResumableCall::Resumable(f) => f.host_error().downcast_ref::<Interrupt>(),
            _ => None,
        }
    }

    fn hash(&self, store: &Store<TransactionInner>) -> MemoryHash {
        // Currently this is just sha256 of the whole WASM file. There might
        // be stuff in the WASM file that we don't want to count or that isn't
        // reproducible and should exclude here, but that seems tricky.
        let mut hash = [0; 32];
        let mut hasher = Sha256::default();

        // Hash linear memory.
        hasher.update(
            self.instance
                .get_export(store, "memory")
                .unwrap()
                .into_memory()
                .unwrap()
                .data(store),
        );

        // TODO: include other things than just linear memory in the hash.
        // For stackful UTXOs, we'd need to hash in `self.resumable`.

        hasher.finalize_into(&mut hash[..]).unwrap();
        MemoryHash(hash)
    }
}

impl std::fmt::Debug for TxProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TxProgram")
            .field("return_to", &self.return_to)
            .field("code", &self.code)
            .field("entry_point", &self.entry_point)
            .field("num_outputs", &self.num_outputs)
            .field("utxo", &self.utxo)
            .field(
                "interrupt",
                &match &self.resumable {
                    ResumableCall::Resumable(resumable) => {
                        resumable.host_error().downcast_ref::<Interrupt>()
                    }
                    ResumableCall::Finished => None::<&Interrupt>,
                },
            )
            .finish()
    }
}

struct MemorySegment {
    address: u32,
    data: Vec<u8>,
}

impl std::fmt::Debug for MemorySegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:#x}, {})", self.address, DisplayHex(&self.data))
    }
}

const MAX_FUEL: u64 = u64::MAX;

#[derive(Debug)]
struct TxWitness {
    reply_to_witness: usize,
    /// Total fuel spent by the transaction as of the time of this witness.
    fuel: u64,
    is_create: bool,
    is_destroy: bool,

    from_program: ProgramIdx,
    from_state_after: MemoryHash,
    /// Memory segments read from `from_program` by this witness.
    read_from_memory: Vec<MemorySegment>,
    values: Vec<Value>,

    to_program: ProgramIdx,
    to_state_before: MemoryHash,
    /// Memory segments written to `to_program` by this witness.
    write_to_memory: Vec<MemorySegment>,
}

/// A row in the continuation table describing UTXO evolution.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ContinuationEntry {
    program: usize,

    /// Hash of the contract the UTXO belongs to.
    code: CodeHash,
    /// Hash of the UTXO's memory and attached state before the call.
    /// All zeroes means a freshly-created UTXO.
    state_before: MemoryHash,
    /// The entry point function, such as X_init or X_resume.
    entry_point: String,
    /// The arguments passed to the UTXO.
    #[serde(with = "serde_value_vec")]
    input: Vec<Value>,
    /// Hash of the UTXO's memory and attached state after the call.
    /// All zeroes means an ended UTXO.
    state_after: MemoryHash,
}

mod serde_value_vec {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};
    use wasmi::{
        Value,
        core::{F32, F64},
    };

    // TODO: actually serialize here. The hard parts are FuncRef (we don't use it)
    // and ExternRef (we'd *like* to use it for UTXOs, but maybe this isn't practical).

    #[derive(serde::Serialize, serde::Deserialize)]
    enum Value2 {
        I32(i32),
        I64(i64),
        F32(u32),
        F64(u64),
    }

    pub fn serialize<S: Serializer>(value: &Vec<Value>, ser: S) -> Result<S::Ok, S::Error> {
        let vec2 = value
            .iter()
            .map(|v| match v {
                Value::I32(i) => Value2::I32(*i),
                Value::I64(i) => Value2::I64(*i),
                Value::F32(i) => Value2::F32(i.to_bits()),
                Value::F64(i) => Value2::F64(i.to_bits()),
                _ => todo!(),
            })
            .collect::<Vec<_>>();
        vec2.serialize(ser)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(de: D) -> Result<Vec<Value>, D::Error> {
        let vec2 = Vec::<Value2>::deserialize(de)?;
        Ok(vec2
            .into_iter()
            .map(|v| match v {
                Value2::I32(i) => Value::I32(i),
                Value2::I64(i) => Value::I64(i),
                Value2::F32(i) => Value::F32(F32::from_bits(i)),
                Value2::F64(i) => Value::F64(F64::from_bits(i)),
            })
            .collect())
    }
}

// NOTE: TxWitness and ContinuationEntry are currently partially redundant.
// Maybe they could be combined somehow in the future.

/// State inside a transaction. The Transaction itself keeps the wasm Store.
#[derive(Default)]
struct TransactionInner {
    utxos: HashMap<UtxoId, Utxo>,
    temporary_utxo_ids: HashMap<u64, UtxoId>,
    temporary_token_ids: HashMap<u64, TokenId>,

    /// Programs this transaction has started or resumed.
    programs: Vec<TxProgram>,
    /// Call and return values between programs, logged for future ZK use.
    witnesses: Vec<TxWitness>,

    registered_effect_handler: HashMap<String, Vec<(ProgramIdx, u32)>>,
    raised_effects: HashMap<String, ProgramIdx>,
}

/// An in-progress transaction and its traces. Contains all related WASM execution.
pub struct Transaction {
    store: Store<TransactionInner>,
    code_cache: Arc<CodeCache>,
}

impl Default for Transaction {
    fn default() -> Self {
        Self::new()
    }
}

impl Transaction {
    /// Begin a new transaction with no dependencies.
    pub fn new() -> Transaction {
        let engine = Engine::new(Config::default().consume_fuel(true));
        let mut store = Store::new(&engine, TransactionInner::default());
        store.add_fuel(MAX_FUEL).unwrap();
        Transaction {
            store,
            code_cache: Default::default(),
        }
    }

    pub fn code_cache(&self) -> &Arc<CodeCache> {
        &self.code_cache
    }

    pub fn utxos(&mut self) -> Vec<(Value, String)> {
        let data = self.store.data();

        let mut res = vec![];
        let iter = data
            .utxos
            .iter()
            .filter_map(|(utxo_id, utxo)| {
                let tx_program = &data.programs[utxo.program.0];

                if tx_program.interrupt().is_some() {
                    Some((*utxo_id, tx_program.entry_point.clone()))
                } else {
                    None
                }
            })
            // TODO: can probably avoid this, but just do this for simplicity
            .collect::<Vec<_>>();

        for (utxo_id, entry_point) in iter {
            res.push((
                utxo_id.to_wasm_externref(self.store.as_context_mut()),
                entry_point,
            ));
        }

        res
    }

    pub fn run_coordination_script(
        &mut self,
        coordination_code: &Arc<ContractCode>,
        entry_point: &str,
        mut inputs: Vec<Value>,
    ) -> Value {
        debug!("run_coordination_script({entry_point:?}, {inputs:?})");

        let linker = coordination_script_linker(
            &self.store.engine().clone(),
            &self.code_cache,
            coordination_code.clone(),
        );

        // Turn ExternRefs into numeric UTXO refs
        for value in &mut inputs {
            if let Some(utxo_id) = UtxoId::from_wasm_externref(value, self.store.as_context()) {
                *value = utxo_id.to_wasm_i64(self.store.as_context_mut());
            }
        }

        let (mut from_program, mut result) = self.start_program(
            ProgramIdx::Root,
            &linker,
            coordination_code,
            entry_point,
            inputs,
        );
        // Main effect scheduler loop.
        loop {
            (from_program, result) = match result {
                // ------------------------------------------------------------
                // Entry point returned
                Ok(mut values) => {
                    // Program returned.
                    let to_program = self.store.data_mut().programs[from_program.0].return_to;
                    if to_program == ProgramIdx::Root {
                        debug!("{from_program:?} -> {to_program:?}: {values:?}");
                        // Transform WASM-side values to UTXO IDs if needed.
                        let result = if !values.is_empty() {
                            if let Some(utxo) =
                                UtxoId::from_wasm_i64(&values[0], self.store.as_context())
                            {
                                // TODO: collisions still technically possible here.
                                // Should consider examining static types.
                                utxo.to_wasm_externref(self.store.as_context_mut())
                            } else {
                                values[0].clone()
                            }
                        } else {
                            Value::I32(0)
                        };

                        // Push final witness
                        let fuel = self.store.fuel_consumed().unwrap();
                        let from_state_after = self.hash_program(from_program);
                        let to_state_before = self.hash_program(to_program);
                        self.store.data_mut().witnesses.push(TxWitness {
                            fuel,
                            from_program,
                            from_state_after,
                            to_program: ProgramIdx::Root,
                            to_state_before,
                            reply_to_witness: 0,
                            values,
                            read_from_memory: Default::default(),
                            write_to_memory: Default::default(),
                            is_create: false,
                            is_destroy: true,
                        });

                        return result;
                    }

                    let mut read_from_memory = vec![];
                    if self.store.data().programs[from_program.0].return_is_token {
                        // Transform id & amount in memory into [TokenId]. Kind of awkward?
                        let instance = self.store.data().programs[from_program.0].instance;
                        let memory = instance
                            .get_export(&self.store, "memory")
                            .unwrap()
                            .into_memory()
                            .unwrap()
                            .data(&self.store);

                        let segment = MemorySegment {
                            address: 16,
                            data: memory[16..32].to_vec(),
                        };
                        let mut cursor = &segment.data[..];
                        let id = cursor.read_u64::<LittleEndian>().unwrap();
                        let amount = cursor.read_u64::<LittleEndian>().unwrap();
                        read_from_memory.push(segment);

                        let token_id = TokenId::random();
                        let token = Token {
                            // code and unbind_fn can be determined by the bind() program
                            bind_program: from_program,
                            id,
                            amount,
                        };
                        let utxo_id = self.store.data_mut().programs[to_program.0].utxo.unwrap();
                        self.store
                            .data_mut()
                            .utxos
                            .get_mut(&utxo_id)
                            .unwrap()
                            .tokens
                            .insert(token_id, token);
                        values = vec![token_id.to_wasm_i64(self.store.as_context_mut())];
                    }

                    self.resume(from_program, to_program, values, read_from_memory, vec![])
                }

                // ------------------------------------------------------------
                // Common
                Err(Interrupt::CoordinationCode { return_addr }) => {
                    let to_program = from_program;
                    self.resume(
                        from_program,
                        to_program,
                        vec![],
                        vec![],
                        vec![MemorySegment {
                            address: return_addr,
                            data: coordination_code.hash().raw().to_vec(),
                        }],
                    )
                }
                Err(Interrupt::RegisterEffectHandler { name, handler_addr }) => {
                    let to_program = from_program;

                    self.store
                        .data_mut()
                        .registered_effect_handler
                        .entry(name)
                        .or_default()
                        .push((from_program, handler_addr));

                    self.resume(from_program, to_program, vec![], vec![], vec![])
                }
                Err(Interrupt::UnRegisterEffectHandler { name }) => {
                    let to_program = from_program;

                    let effect_handlers = &mut self
                        .store
                        .data_mut()
                        .registered_effect_handler
                        .get_mut(&name)
                        .unwrap();

                    let (index, _) = effect_handlers
                        .iter()
                        .enumerate()
                        .find(|(_index, (program, _))| *program == from_program)
                        .unwrap();

                    effect_handlers.remove(index);

                    self.resume(from_program, to_program, vec![], vec![], vec![])
                }
                Err(Interrupt::GetRaisedEffectData {
                    name,
                    output_ptr_data,
                    not_null,
                }) => {
                    let to_program = from_program;

                    let throwing_program = self.store.data().raised_effects.get(&name);

                    let mut write_to_memory = vec![];

                    if let Some(throwing_program) = throwing_program {
                        let (data, data_len) =
                            match self.store.data().programs[throwing_program.0].interrupt() {
                                Some(Interrupt::Raise { data, data_len, .. }) => (*data, *data_len),
                                other => panic!("program didn't throw {other:?}"),
                            };

                        let throwed_data = self.store.data().programs[throwing_program.0]
                            .instance
                            .get_export(&self.store, "memory")
                            .unwrap()
                            .into_memory()
                            .unwrap()
                            .data(&self.store)
                            [data as usize..data as usize + data_len as usize]
                            .to_vec();

                        write_to_memory.push(MemorySegment {
                            address: not_null,
                            data: vec![1u8],
                        });

                        write_to_memory.push(MemorySegment {
                            address: output_ptr_data,
                            data: throwed_data,
                        });
                    } else {
                        write_to_memory.push(MemorySegment {
                            address: not_null,
                            data: vec![0u8],
                        });
                    }

                    self.resume(from_program, to_program, vec![], vec![], write_to_memory)
                }
                Err(Interrupt::ResumeThrowingProgram {
                    name,
                    input_ptr_data,
                }) => {
                    let throwing_program =
                        self.store.data_mut().raised_effects.remove(&name).unwrap();
                    let to_program = throwing_program;

                    let (output_ptr_data, data_len) =
                        match self.store.data().programs[throwing_program.0].interrupt() {
                            Some(Interrupt::Raise {
                                resume_arg,
                                resume_arg_len,
                                ..
                            }) => (*resume_arg, *resume_arg_len),
                            other => panic!("program didn't throw {other:?}"),
                        };

                    let caller_memory = self.store.data().programs[from_program.0]
                        .instance
                        .get_export(&self.store, "memory")
                        .unwrap()
                        .into_memory()
                        .unwrap()
                        .data(&self.store)
                        [input_ptr_data as usize..input_ptr_data as usize + data_len as usize]
                        // TODO: needed to avoid double borrow on the store
                        // can we avoid this?
                        .to_vec();

                    let resumed_program_memory = self.store.data().programs[to_program.0]
                        .instance
                        .get_export(&self.store, "memory")
                        .unwrap()
                        .into_memory()
                        .unwrap()
                        .data_mut(&mut self.store);

                    resumed_program_memory
                        [output_ptr_data as usize..output_ptr_data as usize + data_len as usize]
                        .copy_from_slice(&caller_memory);

                    self.resume(from_program, to_program, vec![], vec![], vec![])
                }
                // ------------------------------------------------------------
                // Coordination scripts can call into UTXOs
                Err(Interrupt::UtxoNew {
                    code: code_hash,
                    entry_point,
                    inputs,
                }) => {
                    let code = self.code_cache.get(code_hash);
                    let linker = utxo_linker(self.store.engine(), &self.code_cache, &code);
                    let id = UtxoId::random();

                    let (to_program, result) =
                        self.start_program(from_program, &linker, &code, &entry_point, inputs);
                    self.store.data_mut().programs[to_program.0].yield_to = Some(from_program);

                    self.store.data_mut().programs[to_program.0].yield_to_constructor =
                        Some(id.to_wasm_i64(self.store.as_context_mut()));

                    self.store.data_mut().programs[to_program.0].utxo = Some(id);
                    self.store.data_mut().utxos.insert(
                        id,
                        Utxo {
                            program: to_program,
                            tokens: Default::default(),
                        },
                    );
                    (to_program, result)
                }
                Err(Interrupt::UtxoResume { utxo_id, inputs }) => {
                    let to_program = self.store.data().utxos[&utxo_id].program;

                    // TODO: I think this is correct if the utxo is resumed
                    // from a coordination script, because there is a chance the
                    // current value of return_to points to an already finished
                    // coordination script.
                    //
                    // But this wouldn't work with utxos. That said, that can't
                    // happen now anyway.
                    self.store.data_mut().programs[to_program.0].return_to = from_program;
                    self.store.data_mut().programs[to_program.0].yield_to = Some(from_program);

                    let (resume_arg, resume_len) =
                        match self.store.data().programs[to_program.0].interrupt() {
                            Some(Interrupt::Yield {
                                resume_arg,
                                resume_arg_len,
                                ..
                            }) => (*resume_arg, *resume_arg_len),
                            other => panic!("cannot query a UTXO in state {other:?}"),
                        };

                    let copy_from = match inputs[1] {
                        Value::I32(n) => n as usize,
                        Value::I64(n) => n as usize,
                        _ => panic!("Expected pointer as the first argument in UtxoResume"),
                    };

                    let caller_memory_data = self.store.data().programs[from_program.0]
                        .instance
                        .get_export(&self.store, "memory")
                        .unwrap()
                        .into_memory()
                        .unwrap()
                        .data(&self.store)[copy_from..copy_from + resume_len as usize]
                        .to_vec();

                    let write_to_memory = vec![MemorySegment {
                        address: resume_arg,
                        data: caller_memory_data,
                    }];

                    self.resume(from_program, to_program, vec![], vec![], write_to_memory)
                }
                Err(Interrupt::UtxoQuery {
                    utxo_id,
                    method,
                    mut inputs,
                }) => {
                    let to_program = self.store.data().utxos[&utxo_id].program;

                    // Insert address of yielded object.
                    let address = match self.store.data().programs[to_program.0].interrupt() {
                        Some(Interrupt::Yield { data, .. }) => *data,
                        other => panic!("cannot query a UTXO in state {other:?}"),
                    };
                    inputs.insert(0, Value::I32(address as i32));
                    self.call_method(from_program, to_program, method, inputs)
                    // TODO: either enforce non-mutation or drop the query/mutate split
                }
                Err(Interrupt::UtxoMutate {
                    utxo_id,
                    method,
                    mut inputs,
                }) => {
                    let to_program = self.store.data().utxos[&utxo_id].program;

                    // Insert address of yielded object.
                    let address = match self.store.data().programs[to_program.0].interrupt() {
                        Some(Interrupt::Yield { data, .. }) => *data,
                        other => panic!("cannot mutate a UTXO in state {other:?}"),
                    };
                    inputs.insert(0, Value::I32(address as i32));
                    self.call_method(from_program, to_program, method, inputs)
                }
                Err(Interrupt::UtxoConsume {
                    utxo_id,
                    method,
                    mut inputs,
                }) => {
                    let to_program = self.store.data().utxos[&utxo_id].program;

                    // Insert address of yielded object.
                    let address = match self.store.data().programs[to_program.0].interrupt() {
                        Some(Interrupt::Yield { data, .. }) => *data,
                        other => panic!("cannot consume a UTXO in state {other:?}"),
                    };
                    inputs.insert(0, Value::I32(address as i32));
                    // Now throw away that object
                    self.store.data_mut().programs[to_program.0].resumable =
                        ResumableCall::Finished;
                    self.call_method(from_program, to_program, method, inputs)
                }

                // ------------------------------------------------------------
                // UTXOs can yield and call into tokens
                Err(Interrupt::Yield { .. }) => {
                    let utxo_scrambled_id = self.store.data_mut().programs[from_program.0]
                        .yield_to_constructor
                        .take();

                    let to_program = self.store.data_mut().programs[from_program.0]
                        .yield_to
                        .unwrap();

                    let mut inputs = vec![];

                    if let Some(id) = utxo_scrambled_id {
                        inputs.push(id);
                    }

                    self.resume(from_program, to_program, inputs, vec![], vec![])
                }
                Err(Interrupt::Raise { name, .. }) => {
                    let (to_program, handler_address) =
                        *self.store.data_mut().registered_effect_handler[&name]
                            .last()
                            .unwrap();

                    let method = format!("{}_handle", name);

                    self.store
                        .data_mut()
                        .raised_effects
                        .insert(name, from_program);

                    self.call_method(
                        from_program,
                        to_program,
                        method,
                        vec![Value::I32(handler_address as i32)],
                    )
                }
                Err(Interrupt::TokenBind {
                    code,
                    entry_point,
                    mut inputs,
                }) => {
                    let code = self.code_cache.get(code);
                    let linker = token_linker(self.store.engine(), &code);
                    //let id = TokenId::random();

                    // Prepend TokenStorage struct return address to inputs.
                    // HACK: The 16 here is a low but nonzero memory address
                    // that we're crossing our fingers and hoping that the WASM
                    // doesn't actually use.
                    // BETTER: Extend the WASM memory with a new page that we
                    // know won't collide because it didn't exist before,
                    // and return there (downside: uses more memory).
                    // BEST: Use WASM multivalues (Rust -Ctarget-feature=+multivalue)
                    // instead of struct return addresses in the first place.
                    // TODO: Memory trace this or fix the hack described above.
                    let return_addr: usize = 16;
                    inputs.insert(0, Value::I32(return_addr as i32));

                    let (to_program, result) =
                        self.start_program(from_program, &linker, &code, &entry_point, inputs);
                    self.store.data_mut().programs[to_program.0].return_is_token = true;

                    (to_program, result)
                }
                Err(Interrupt::TokenUnbind { token_id }) => {
                    // assume that only the utxo that owns the token can unbind it?
                    let utxo_id = self.store.data_mut().programs[from_program.0].utxo.unwrap();

                    let token = self
                        .store
                        .data_mut()
                        .utxos
                        .get_mut(&utxo_id)
                        .unwrap()
                        .tokens
                        .remove(&token_id)
                        .unwrap();

                    let code = self.store.data().programs[token.bind_program.0].code;
                    let code = self.code_cache.get(code);

                    let entry_point = self.store.data().programs[token.bind_program.0]
                        .entry_point
                        .replace("bind", "unbind");

                    let linker = token_linker(self.store.engine(), &code);

                    let inputs = vec![Value::I64(token.id as i64), Value::I64(token.amount as i64)];

                    let (to_program, result) =
                        self.start_program(from_program, &linker, &code, &entry_point, inputs);

                    (to_program, result)
                }
                Err(Interrupt::GetTokens {
                    data,
                    data_len,
                    skip,
                }) => {
                    let to_program = from_program;

                    let utxo_id = self.store.data().programs[from_program.0].utxo.unwrap();

                    let tokens = {
                        let utxo = &self.store.data().utxos[&utxo_id];

                        let mut tokens_sorted = utxo.tokens.keys().copied().collect::<Vec<_>>();

                        tokens_sorted.sort();

                        tokens_sorted
                    };

                    let mut count = 0;
                    let mut raw = vec![];

                    for token_id in tokens.iter().skip(skip as usize).take(data_len as usize) {
                        let i = token_id.to_wasm_i64(self.store.as_context_mut());

                        let i = match i {
                            Value::I64(i) => i as u64,
                            _ => unreachable!(),
                        };

                        raw.extend_from_slice(&i.to_le_bytes());
                        count += 1;
                    }

                    let writes = vec![MemorySegment {
                        address: data,
                        data: raw,
                    }];

                    self.resume(
                        from_program,
                        to_program,
                        vec![Value::I32(count)],
                        vec![],
                        writes,
                    )
                }
            }
        }
    }

    fn hash_program(&self, id: ProgramIdx) -> MemoryHash {
        if id == ProgramIdx::Root {
            MemoryHash::NOTHING
        } else {
            self.store.data().programs[id.0].hash(&self.store)
        }
    }

    /// Instantiate a new contract instance.
    fn start_program(
        &mut self,
        from_program: ProgramIdx,
        linker: &Linker<TransactionInner>,
        code: &ContractCode,
        entry_point: &str,
        inputs: Vec<Value>,
    ) -> (ProgramIdx, Result<Vec<Value>, Interrupt>) {
        let module = &code.module(self.store.engine());
        let instance = linker
            .instantiate(&mut self.store, module)
            .unwrap()
            .ensure_no_start(&mut self.store)
            .unwrap();

        let id = ProgramIdx(self.store.data_mut().programs.len());
        debug!("start: {from_program:?} -> {id:?} = {entry_point}{inputs:?}");

        let fuel = self.store.fuel_consumed().unwrap();
        let main = instance
            .get_func(&mut self.store, entry_point)
            .expect(&entry_point);
        let num_outputs = main.ty(&mut self.store).results().len();
        let mut outputs = [Value::from(ExternRef::null())];
        let resumable = main
            .call_resumable(&mut self.store, &inputs, &mut outputs[..num_outputs])
            .unwrap();
        assert_eq!(
            id.0,
            self.store.data_mut().programs.len(),
            "unexpected re-entrancy in start_program"
        );
        let result = match &resumable {
            ResumableCall::Finished => Ok(outputs[..num_outputs].to_vec()),
            ResumableCall::Resumable(invocation) => Err(invocation
                .host_error()
                .downcast_ref::<Interrupt>()
                .unwrap()
                .clone()),
        };
        debug!("= {result:?}");
        self.store.data_mut().programs.push(TxProgram {
            return_to: from_program,
            return_is_token: false,
            yield_to: None,
            yield_to_constructor: None,
            code: code.hash(),
            entry_point: entry_point.to_owned(),
            instance,
            num_outputs,
            resumable,
            utxo: None,
        });
        let from_state_after = self.hash_program(from_program);
        let to_state_before = self.hash_program(id);
        self.store.data_mut().witnesses.push(TxWitness {
            fuel,
            from_program,
            from_state_after,
            to_program: id,
            to_state_before,
            reply_to_witness: usize::MAX,
            values: inputs,
            read_from_memory: Default::default(),
            write_to_memory: Default::default(),
            is_create: true,
            is_destroy: false,
        });
        (id, result)
    }

    /// Resume a suspended call stack of a WASM instance.
    fn resume(
        &mut self,
        from_program: ProgramIdx,
        to_program: ProgramIdx,
        //reply_to_witness: usize,
        inputs: Vec<Value>, // The inputs of this function are the outputs of the yield.
        read_from_memory: Vec<MemorySegment>,
        write_to_memory: Vec<MemorySegment>,
    ) -> (ProgramIdx, Result<Vec<Value>, Interrupt>) {
        match std::mem::replace(
            &mut self.store.data_mut().programs[to_program.0].resumable,
            ResumableCall::Finished,
        ) {
            ResumableCall::Finished => panic!("attempt to resume finished program"),
            ResumableCall::Resumable(invocation) => {
                debug!("resume: {from_program:?} -> {to_program:?} {inputs:?}");

                if !write_to_memory.is_empty() {
                    // Commit memory writes.
                    let instance = self.store.data_mut().programs[to_program.0].instance;
                    let (memory, _) = instance
                        .get_export(&mut self.store, "memory")
                        .unwrap()
                        .into_memory()
                        .unwrap()
                        .data_and_store_mut(&mut self.store);
                    for &MemorySegment { address, ref data } in &write_to_memory {
                        memory[address as usize..address as usize + data.len()]
                            .copy_from_slice(data);
                        debug!("  {:#x}: {}", address, DisplayHex(data));
                    }
                }

                let fuel = self.store.fuel_consumed().unwrap();
                let from_state_after = self.hash_program(from_program);
                let to_state_before = self.hash_program(to_program);
                let num_outputs = self.store.data_mut().programs[to_program.0].num_outputs;
                let mut outputs = [Value::from(ExternRef::null())];
                let resumable = invocation
                    .resume(&mut self.store, &inputs[..], &mut outputs[..num_outputs])
                    .unwrap();
                let result = match &resumable {
                    ResumableCall::Finished => Ok(outputs[..num_outputs].to_vec()),
                    ResumableCall::Resumable(invocation) => Err(invocation
                        .host_error()
                        .downcast_ref::<Interrupt>()
                        .unwrap()
                        .clone()),
                };
                debug!("= {result:?}");
                self.store.data_mut().programs[to_program.0].resumable = resumable;
                self.store.data_mut().witnesses.push(TxWitness {
                    fuel,
                    from_program,
                    from_state_after,
                    to_program,
                    to_state_before,
                    reply_to_witness: usize::MAX,
                    values: inputs,
                    read_from_memory,
                    write_to_memory,
                    is_create: false,
                    is_destroy: false,
                });
                (to_program, result)
            }
        }
    }

    /// Spawn an additional function call in an existing WASM instance.
    fn call_method(
        &mut self,
        from_program: ProgramIdx,
        to_program: ProgramIdx,
        method: String,
        inputs: Vec<Value>,
    ) -> (ProgramIdx, Result<Vec<Value>, Interrupt>) {
        let code = self.store.data().programs[to_program.0].code;
        let instance = self.store.data().programs[to_program.0].instance;

        let id = ProgramIdx(self.store.data_mut().programs.len());
        debug!("call: {from_program:?} -> {to_program:?} -> {id:?} = {method}{inputs:?}");

        let main = instance
            .get_func(&mut self.store, &method)
            .expect("no such method");
        let num_outputs = main.ty(&mut self.store).results().len();
        let mut outputs = [Value::from(ExternRef::null())];
        let fuel = self.store.fuel_consumed().unwrap();
        let from_state_after = self.hash_program(from_program);
        let to_state_before = self.hash_program(to_program);
        let resumable = main
            .call_resumable(&mut self.store, &inputs, &mut outputs[..num_outputs])
            .unwrap();
        assert_eq!(
            id.0,
            self.store.data_mut().programs.len(),
            "unexpected re-entrancy in Transaction::call_method"
        );
        let result = match &resumable {
            ResumableCall::Finished => Ok(outputs[..num_outputs].to_vec()),
            ResumableCall::Resumable(invocation) => Err(invocation
                .host_error()
                .downcast_ref::<Interrupt>()
                .unwrap()
                .clone()),
        };
        debug!("= {result:?}");
        let utxo = self.store.data().programs[to_program.0].utxo;
        self.store.data_mut().programs.push(TxProgram {
            return_to: from_program,
            return_is_token: false,
            yield_to: None,
            yield_to_constructor: None,
            code,
            entry_point: method.to_owned(),
            num_outputs,
            instance,
            resumable,
            utxo,
        });
        self.store.data_mut().witnesses.push(TxWitness {
            fuel,
            from_program,
            from_state_after,
            to_program: id,
            to_state_before,
            reply_to_witness: usize::MAX,
            values: inputs,
            read_from_memory: Default::default(),
            write_to_memory: Default::default(),
            is_create: true,
            is_destroy: false,
        });
        (id, result)
    }

    pub fn map_continuations(&self) -> Vec<ContinuationEntry> {
        let mut result = Vec::new();
        let mut iter = self.store.data().witnesses.iter();
        let Some(first) = iter.next() else {
            return result;
        };

        let first_program = &self.store.data().programs[first.to_program.0];
        result.push(ContinuationEntry {
            program: first.to_program.0,
            code: first_program.code,
            state_before: first.to_state_before,
            entry_point: first_program.entry_point.clone(),
            input: first.values.clone(),
            state_after: MemoryHash::UNFINISHED,
        });
        for each in iter {
            result.last_mut().unwrap().state_after = each.from_state_after;
            if each.to_program != ProgramIdx::Root {
                let each_program = &self.store.data().programs[each.to_program.0];
                result.push(ContinuationEntry {
                    program: each.to_program.0,
                    code: each_program.code,
                    state_before: each.to_state_before,
                    entry_point: each_program.entry_point.clone(),
                    input: each.values.clone(),
                    state_after: MemoryHash::UNFINISHED,
                });
            }
        }

        result
    }

    pub fn prove(&self) -> TransactionProof {
        self.do_nebula_stuff()
    }
}

impl std::fmt::Debug for Transaction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = self.store.data();
        f.debug_struct("Transaction")
            .field("utxos", &inner.utxos)
            .field("programs", &inner.programs)
            .field("witnesses", &inner.witnesses)
            .finish()
    }
}

/// A proof of a transaction that can be passed around and verified later.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct TransactionProof {
    pub continuations: Vec<ContinuationEntry>,
    pub program_proofs: Vec<nebula::ProgramProof>,
    pub table_proof: nebula::TableProof,
}

impl TransactionProof {
    /// Verify the proof.
    pub fn verify(&self) -> Result<(), String> {
        // TODO: actually verify continuations.
        self.table_proof.verify();
        for pp in self.program_proofs.iter() {
            pp.verify(); // TODO: currently panics, should probably return a useful error.
        }
        Ok(())
    }
}

// TODO: Universe or World type which can spawn transactions (loading a subset
// of UTXOs into WASM memories) and commit them (verify, flush WASM instances).
// In the long term it should be possible to commit ZK proofs of transactions.

#[derive(Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct MemoryHash([u8; 32]);

impl MemoryHash {
    /// Represents "before" a UTXO new or "after" a UTXO end.
    pub const NOTHING: MemoryHash = MemoryHash([0; 32]);
    /// Represents a "null" in the table where a value hasn't yet been filled.
    pub const UNFINISHED: MemoryHash = MemoryHash([0xff; 32]);

    pub fn as_u64s(&self) -> [u64; 4] {
        [
            u64::from_le_bytes(self.0[0..8].try_into().unwrap()),
            u64::from_le_bytes(self.0[8..16].try_into().unwrap()),
            u64::from_le_bytes(self.0[16..24].try_into().unwrap()),
            u64::from_le_bytes(self.0[24..32].try_into().unwrap()),
        ]
    }
}

impl std::fmt::Debug for MemoryHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MemoryHash({})", DisplayHex(&self.0[..]))
    }
}
