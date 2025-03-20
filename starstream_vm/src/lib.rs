use std::{
    cell::RefCell,
    collections::HashMap,
    sync::{Arc, Mutex, RwLock},
    usize,
};

use byteorder::{LittleEndian, ReadBytesExt};
use rand::RngCore;
use tiny_keccak::Hasher;
use wasmi::{
    AsContext, AsContextMut, Caller, Engine, ExternRef, ExternType, Func, ImportType, Instance,
    Linker, Module, ResumableCall, Store, StoreContext, StoreContextMut, Value,
    core::{HostError, Trap, ValueType},
};

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

fn fake_import<T>(linker: &mut Linker<T>, import: &ImportType, message: &'static str) {
    if let ExternType::Func(func) = import.ty() {
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

type ContractCodeId = String;

type CodeHash = [u8; 32];

fn hash_code(code: &[u8]) -> CodeHash {
    [0; 32] // TODO
}

// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Interrupt {
    Yield { name: String, data: u32 },
}

impl Interrupt {
    // TODO: these should probably be dropped in favor of global memory or something.
    fn yield_data(&self) -> u32 {
        match self {
            Interrupt::Yield { data, .. } => *data,
        }
    }
    fn yield_name(&self) -> &str {
        match self {
            Interrupt::Yield { name, .. } => name,
        }
    }
}

impl std::fmt::Display for Interrupt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl HostError for Interrupt {}

/// Fulfiller of imports from `env`.
fn starstream_env<T>(
    linker: &mut Linker<T>,
    module: &str,
    this_code: &ContractCode,
    coordination_code: impl Fn(&T) -> &ContractCode + Send + Sync + 'static,
) {
    let this_code = this_code.hash;

    linker
        .func_wrap(module, "abort", || -> () {
            panic!("abort() called");
        })
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_log",
            |mut caller: Caller<T>, ptr: u32, len: u32| -> () {
                let (memory, _) = memory(&mut caller);
                let slice = &memory[ptr as usize..(ptr + len) as usize];
                eprintln!("starstream_log: {slice:x?}");
            },
        )
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_coordination_code",
            move |mut caller: Caller<T>, return_addr: u32| {
                let (memory, env) = memory(&mut caller);
                let hash = &coordination_code(env).hash;
                memory[return_addr as usize..return_addr as usize + hash.len()]
                    .copy_from_slice(hash);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            module,
            "starstream_this_code",
            move |mut caller: Caller<T>, return_addr: u32| {
                let (memory, _) = memory(&mut caller);
                memory[return_addr as usize..return_addr as usize + this_code.len()]
                    .copy_from_slice(&this_code);
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
}

/// Fulfiller of imports from `starstream_utxo_env`.
fn starstream_utxo_env(linker: &mut Linker<UtxoInstance>, module: &str) {
    linker
        .func_wrap(
            module,
            "starstream_yield",
            |mut caller: Caller<UtxoInstance>,
             name: u32,
             name_len: u32,
             data: u32,
             data_len: u32,
             resume_arg: u32,
             resume_arg_len: u32|
             -> Result<(), Trap> {
                eprintln!("YIELD");
                Err(Trap::from(Interrupt::Yield {
                    name: std::str::from_utf8(
                        &memory(&mut caller).0[name as usize..(name + name_len) as usize],
                    )
                    .unwrap()
                    .to_owned(),
                    data,
                }))
            },
        )
        .unwrap();
}

// ----------------------------------------------------------------------------

pub struct ContractCode {
    wasm: Vec<u8>,
    hash: CodeHash,
}

impl ContractCode {
    fn load(wasm: Vec<u8>) -> ContractCode {
        ContractCode {
            hash: hash_code(&wasm),
            wasm,
        }
    }

    fn module(&self, engine: &Engine) -> Module {
        Module::new(engine, &self.wasm[..]).unwrap()
    }

    pub fn hash(&self) -> CodeHash {
        self.hash
    }
}

impl std::fmt::Debug for ContractCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContractCode")
            .field("hash", &self.hash)
            .finish()
    }
}

// ----------------------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
struct TokenId {
    id: usize,
}

impl TokenId {
    fn to_wasm_u32(self, mut store: StoreContextMut<UtxoInstance>) -> Value {
        let scrambled = rand::rng().next_u32();
        store.data_mut().temporary_token_ids.insert(scrambled, self);
        Value::I32(scrambled as i32)
    }

    fn to_wasm_externref(self, store: StoreContextMut<UtxoInstance>) -> Value {
        Value::ExternRef(ExternRef::new::<TokenId>(store, Some(self)))
    }

    fn from_wasm(value: &Value, store: StoreContext<UtxoInstance>) -> Option<TokenId> {
        match value {
            Value::I32(scrambled) => store
                .data()
                .temporary_token_ids
                .get(&(*scrambled as u32))
                .copied(),
            Value::ExternRef(handle) => handle.data(store)?.downcast_ref::<TokenId>().copied(),
            _ => None,
        }
    }
}

struct UtxoInstance {
    coordination_code: Arc<ContractCode>,
    code_cache: Arc<CodeCache>,

    tokens: Vec<Token>,
    temporary_token_ids: HashMap<u32, TokenId>,
}

fn utxo_linker(
    engine: &Engine,
    utxo_code: &ContractCode,
    coordination_code: &Arc<ContractCode>,
) -> Linker<UtxoInstance> {
    let mut linker = Linker::new(engine);

    starstream_env(&mut linker, "env", utxo_code, |instance: &UtxoInstance| {
        &instance.coordination_code
    });

    starstream_utxo_env(&mut linker, "starstream_utxo_env");

    for import in utxo_code.module(engine).imports() {
        if let ExternType::Func(func_ty) = import.ty() {
            if let Some(rest) = import.module().strip_prefix("starstream_token:") {
                if import.name().starts_with("starstream_mint_") {
                    let name = import.name().to_owned();
                    let rest = rest.to_owned();
                    let coordination_code = coordination_code.clone();
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |mut caller, inputs, outputs| {
                                eprintln!("MINT {name:?} {inputs:?}");
                                let mut store = caller.as_context_mut();
                                let data = store.data_mut();
                                let utxo = Token::mint(
                                    coordination_code.clone(),
                                    data.code_cache.load_debug(&rest),
                                    &name,
                                    inputs,
                                );
                                let id = data.tokens.len();
                                data.tokens.push(utxo);
                                outputs[0] = TokenId { id }.to_wasm_u32(caller.as_context_mut());
                                Ok(())
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_burn_") {
                    linker
                        .func_wrap(import.module(), import.name(), |handle: u32| -> () {
                            todo!()
                        })
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

struct Utxo {
    code: Arc<ContractCode>,
    entry_point: String,
    store: RefCell<Store<UtxoInstance>>,
    instance: Instance,
    status: ResumableCall,
}

impl Utxo {
    fn start(
        coordination_code: Arc<ContractCode>,
        utxo_code: Arc<ContractCode>,
        code_cache: &Arc<CodeCache>,
        entry_point: String,
        inputs: &[Value],
    ) -> Utxo {
        let engine = Engine::default();
        let mut store = Store::new(
            &engine,
            UtxoInstance {
                coordination_code: coordination_code.clone(),
                code_cache: code_cache.clone(),
                tokens: Default::default(),
                temporary_token_ids: Default::default(),
            },
        );
        let linker = utxo_linker(&engine, &utxo_code, &coordination_code);
        let instance = linker
            .instantiate(&mut store, &utxo_code.module(&engine))
            .unwrap()
            .ensure_no_start(&mut store)
            .unwrap();
        let main = instance.get_func(&mut store, &entry_point).unwrap();
        // TODO: call_resumable is naturally what we want here, but it's not
        // serializable to disk yet. We could patch wasmi to make it so, or go
        // back to binaryen-asyncify.
        let status = main.call_resumable(&mut store, inputs, &mut []).unwrap();
        Utxo {
            code: utxo_code,
            entry_point,
            store: RefCell::new(store),
            instance,
            status,
        }
    }

    fn is_alive(&self) -> bool {
        matches!(self.status, ResumableCall::Resumable(_))
    }

    fn resume(&mut self) {
        let ResumableCall::Resumable(resumable) =
            std::mem::replace(&mut self.status, ResumableCall::Finished)
        else {
            panic!("Cannot resume() after exit")
        };
        self.status = resumable
            .resume(self.store.borrow_mut().as_context_mut(), &[], &mut [])
            .unwrap();
    }

    fn query(&self, method: &str, inputs: &[Value], outputs: &mut [Value]) {
        eprintln!("query {method:?} {inputs:?} {}", outputs.len());
        let ResumableCall::Resumable(resumable) = &self.status else {
            panic!("Cannot query() after exit");
        };
        let inputs = std::iter::once(Value::I32(
            resumable
                .host_error()
                .downcast_ref::<Interrupt>()
                .unwrap()
                .yield_data() as i32,
        ))
        .chain(inputs.iter().cloned())
        .collect::<Vec<_>>();

        let func = self
            .instance
            .get_func(self.store.borrow().as_context(), method)
            .unwrap();
        func.call(self.store.borrow_mut().as_context_mut(), &inputs, outputs)
            .unwrap()
    }

    fn mutate(&mut self, method: &str, inputs: &[Value], outputs: &mut [Value]) {
        eprintln!("mutate {method:?} {inputs:?} {}", outputs.len());
        let ResumableCall::Resumable(resumable) = &self.status else {
            panic!("Cannot query() after exit");
        };
        let inputs: Vec<Value> = std::iter::once(Value::I32(
            resumable
                .host_error()
                .downcast_ref::<Interrupt>()
                .unwrap()
                .yield_data() as i32,
        ))
        .chain(inputs.iter().cloned())
        .collect::<Vec<_>>();

        let func = self
            .instance
            .get_func(self.store.borrow().as_context(), method)
            .unwrap();
        func.call(self.store.borrow_mut().as_context_mut(), &inputs, outputs)
            .unwrap()
    }

    fn consume(&mut self, method: &str, inputs: &[Value], outputs: &mut [Value]) {
        eprintln!("consume {method:?} {inputs:?} {}", outputs.len());
        let ResumableCall::Resumable(resumable) = &self.status else {
            panic!("Cannot query() after exit");
        };
        let inputs: Vec<Value> = std::iter::once(Value::I32(
            resumable
                .host_error()
                .downcast_ref::<Interrupt>()
                .unwrap()
                .yield_data() as i32,
        ))
        .chain(inputs.iter().cloned())
        .collect::<Vec<_>>();

        let func = self
            .instance
            .get_func(self.store.borrow().as_context(), method)
            .unwrap();
        let r = func
            .call(self.store.borrow_mut().as_context_mut(), &inputs, outputs)
            .unwrap();
        self.status = ResumableCall::Finished;
        r
    }
}

impl std::fmt::Debug for Utxo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("Utxo");
        s.field("entry_point", &self.entry_point);
        match &self.status {
            ResumableCall::Finished => {
                s.field("finished", &true);
            }
            ResumableCall::Resumable(resumable) => {
                let pause = resumable.host_error().downcast_ref::<Interrupt>().unwrap();
                let inputs = [Value::I32(pause.yield_data() as i32)];
                let name = pause.yield_name();
                s.field("type", &name);
                let last_part = name.split("::").last().unwrap();

                let mut store = self.store.borrow_mut();
                let funcs = self
                    .instance
                    .exports(store.as_context())
                    .filter_map(|e| {
                        let n = e.name().to_owned();
                        e.into_func().map(|x| (n, x))
                    })
                    .collect::<Vec<_>>();
                for (name, func) in funcs {
                    let mut outputs = [Value::ExternRef(ExternRef::null())];
                    if name.starts_with("starstream_query_")
                        && name["starstream_query_".len()..].starts_with(&last_part)
                        && func.ty(store.as_context()).params() == &[ValueType::I32]
                        && func.ty(store.as_context()).results().len() == 1
                    {
                        func.call(store.as_context_mut(), &inputs, &mut outputs)
                            .unwrap();
                        s.field(&name, &outputs[0]);
                    }
                }
            }
        }
        s.finish()
    }
}

// ----------------------------------------------------------------------------

struct TokenInstance {
    coordination_code: Arc<ContractCode>,
}

fn token_linker(engine: &Engine, token_code: &Arc<ContractCode>) -> Linker<TokenInstance> {
    let mut linker = Linker::new(engine);

    starstream_env(
        &mut linker,
        "env",
        token_code,
        |instance: &TokenInstance| &instance.coordination_code,
    );

    for import in token_code.module(engine).imports() {
        fake_import(&mut linker, &import, "Not available in UTXO context");
    }

    linker
}

// ----------------------------------------------------------------------------

struct Token {
    code: Arc<ContractCode>,
    // Note: doesn't save Store or Instance, instead recreates it from scratch
    // on burn() call therefore not needing to persist aribtrary memory for
    // tokens.
    burn_fn: String,
    id: u64,
    amount: u64,
}

impl Token {
    fn mint(
        coordination_code: Arc<ContractCode>,
        token_code: Arc<ContractCode>,
        mint_fn: &str,
        inputs: &[Value],
    ) -> Token {
        let burn_fn = mint_fn.replace("starstream_mint_", "starstream_burn_");
        assert_ne!(mint_fn, burn_fn);

        // Prepend struct return slot to inputs
        let return_addr: usize = 16;
        let inputs = std::iter::once(Value::I32(return_addr as i32))
            .chain(inputs.iter().cloned())
            .collect::<Vec<_>>();

        let engine = Engine::default();
        let mut store = Store::new(&engine, TokenInstance { coordination_code });
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

    fn burn(self, burn_fn: &str, coordination_code: Arc<ContractCode>) {
        assert_eq!(self.burn_fn, burn_fn);

        let engine = Engine::default();
        let mut store = Store::new(&engine, TokenInstance { coordination_code });
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

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Token")
            .field("burn_fn", &self.burn_fn)
            .field("id", &self.id)
            .field("amount", &self.amount)
            .finish()
    }
}

// ----------------------------------------------------------------------------

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct UtxoId {
    bytes: [u8; 16],
}

impl UtxoId {
    fn random() -> UtxoId {
        let mut bytes = [0; 16];
        rand::rng().fill_bytes(&mut bytes);
        UtxoId { bytes }
    }

    fn to_wasm_u32(self, mut store: StoreContextMut<CoordinationScriptInstance>) -> Value {
        let scrambled = rand::rng().next_u32();
        store.data_mut().temporary_utxo_ids.insert(scrambled, self);
        Value::I32(scrambled as i32)
    }

    fn to_wasm_externref(self, store: StoreContextMut<CoordinationScriptInstance>) -> Value {
        Value::ExternRef(ExternRef::new::<UtxoId>(store, Some(self)))
    }

    fn from_wasm(value: &Value, store: StoreContext<CoordinationScriptInstance>) -> Option<UtxoId> {
        match value {
            Value::I32(scrambled) => store
                .data()
                .temporary_utxo_ids
                .get(&(*scrambled as u32))
                .copied(),
            Value::ExternRef(handle) => handle.data(store)?.downcast_ref::<UtxoId>().copied(),
            _ => None,
        }
    }
}

impl std::fmt::Debug for UtxoId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UtxoId({:02x?})", &self.bytes[..])
    }
}

struct CoordinationScriptInstance<'tx> {
    coordination_code: &'tx ContractCode,
    tx: &'tx mut Transaction,
    temporary_utxo_ids: HashMap<u32, UtxoId>,
}

fn coordination_script_linker<'tx>(
    engine: &Engine,
    code_cache: &Arc<CodeCache>,
    coordination_code: Arc<ContractCode>,
) -> Linker<CoordinationScriptInstance<'tx>> {
    let mut linker = Linker::new(engine);

    starstream_env(
        &mut linker,
        "env",
        &coordination_code,
        |env: &CoordinationScriptInstance| &env.coordination_code,
    );

    for import in coordination_code.module(&engine).imports() {
        if import.module() == "env" {
            // handled by starstream_env above
        } else if let Some(rest) = import.module().strip_prefix("starstream_utxo:") {
            if let ExternType::Func(func_ty) = import.ty() {
                let name = import.name().to_owned();
                if import.name().starts_with("starstream_status_") {
                } else if import.name().starts_with("starstream_resume_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |mut caller, inputs, outputs| {
                                let utxo_id =
                                    UtxoId::from_wasm(&inputs[0], caller.as_context()).unwrap();
                                caller.as_context_mut().data_mut().tx.utxos[&utxo_id].query(
                                    &name,
                                    &inputs[1..],
                                    outputs,
                                );
                                Ok(())
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_new_") {
                    let code_cache = code_cache.clone();
                    let coordination_code = coordination_code.clone();
                    let rest = rest.to_owned();
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |mut caller, inputs, outputs| {
                                eprintln!("NEW {name:?} {inputs:?}");
                                let utxo_code = code_cache.load_debug(&rest);
                                let utxo = Utxo::start(
                                    coordination_code.clone(),
                                    utxo_code.clone(),
                                    &code_cache,
                                    name.clone(),
                                    inputs,
                                );
                                let mut store = caller.as_context_mut();
                                let local_utxos = &mut store.data_mut().tx.utxos;
                                let id = UtxoId::random();
                                local_utxos.insert(id, utxo);
                                outputs[0] = id.to_wasm_u32(caller.as_context_mut());
                                Ok(())
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_query_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |mut caller, inputs, outputs| {
                                //eprintln!("inputs are {inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm(&inputs[0], caller.as_context()).unwrap();
                                caller.as_context_mut().data_mut().tx.utxos[&utxo_id].query(
                                    &name,
                                    &inputs[1..],
                                    outputs,
                                );
                                Ok(())
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_mutate_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |mut caller, inputs, outputs| {
                                //eprintln!("inputs are {inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm(&inputs[0], caller.as_context()).unwrap();
                                caller
                                    .as_context_mut()
                                    .data_mut()
                                    .tx
                                    .utxos
                                    .get_mut(&utxo_id)
                                    .unwrap()
                                    .mutate(&name, &inputs[1..], outputs);
                                Ok(())
                            },
                        )
                        .unwrap();
                } else if import.name().starts_with("starstream_consume_") {
                    linker
                        .func_new(
                            import.module(),
                            import.name(),
                            func_ty.clone(),
                            move |mut caller, inputs, outputs| {
                                eprintln!("inputs are {inputs:?}");
                                let utxo_id =
                                    UtxoId::from_wasm(&inputs[0], caller.as_context()).unwrap();
                                // NB: not remove() because we want a record of the dead UTXO for later
                                caller
                                    .as_context_mut()
                                    .data_mut()
                                    .tx
                                    .utxos
                                    .get_mut(&utxo_id)
                                    .unwrap()
                                    .consume(&name, &inputs[1..], outputs);
                                Ok(())
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

#[derive(Debug, Clone)]
pub enum ValueOrUtxo {
    Value(Value),
    Utxo(UtxoId),
}

impl From<Value> for ValueOrUtxo {
    fn from(value: Value) -> Self {
        ValueOrUtxo::Value(value)
    }
}

impl From<UtxoId> for ValueOrUtxo {
    fn from(value: UtxoId) -> Self {
        ValueOrUtxo::Utxo(value)
    }
}

#[derive(Default)]
pub struct CodeCache {
    contract_code: RwLock<HashMap<ContractCodeId, Arc<ContractCode>>>,
}

impl CodeCache {
    pub fn load_debug(&self, name: &str) -> Arc<ContractCode> {
        if let Some(code) = self.contract_code.read().unwrap().get(name) {
            code.clone()
        } else {
            let path = format!("target/wasm32-unknown-unknown/debug/{name}.wasm");
            let result = Arc::new(ContractCode::load(std::fs::read(path).unwrap()));
            self.contract_code
                .write()
                .unwrap()
                .insert(name.to_owned(), result.clone());
            result
        }
    }
}

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

#[derive(Debug)]
struct TxProgram {
    return_to: ProgramIdx,

    code: CodeHash,
    entry_point: String,

    // Memory is in here
    instance: Instance,
    // None if just started, Finished if finished, Resumable if yielded
    resumable: ResumableCall,
    // Num outputs of root fn of `resumable`. wasmi knows this but doesn't expose it.
    num_outputs: usize,
}

impl TxProgram {
    fn interrupt(&self) -> Option<&Interrupt> {
        match &self.resumable {
            ResumableCall::Resumable(f) => f.host_error().downcast_ref::<Interrupt>(),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct TxWitness {
    from_program: ProgramIdx,
    to_program: ProgramIdx,
    values: Vec<Value>,
}

pub struct Transaction {
    pub code_cache: Arc<CodeCache>,
    utxos: HashMap<UtxoId, Utxo>,
    /// Programs this transaction has started or resumed.
    programs: Vec<TxProgram>,
    /// Call and return values between programs, logged for future ZK use.
    witnesses: Vec<TxWitness>,
}

impl Transaction {
    pub fn isolated() -> Transaction {
        Transaction {
            code_cache: Default::default(),
            utxos: Default::default(),
            programs: Default::default(),
            witnesses: Default::default(),
        }
    }

    fn start_program<'a>(
        store: &mut Store<CoordinationScriptInstance<'a>>,
        linker: &Linker<CoordinationScriptInstance<'a>>,
        code: &Arc<ContractCode>,
        entry_point: &str,
        from_program: ProgramIdx,
        values: Vec<Value>,
    ) -> (ProgramIdx, Result<Vec<Value>, Interrupt>) {
        let module = &code.module(store.engine());
        let instance = linker
            .instantiate(&mut *store, module)
            .unwrap()
            .ensure_no_start(&mut *store)
            .unwrap();

        let id = ProgramIdx(store.data_mut().tx.programs.len());
        eprintln!("{from_program:?} -> {id:?} = {entry_point}{values:?}");

        let main = instance.get_func(&mut *store, entry_point).unwrap();
        let num_outputs = main.ty(&mut *store).results().len();
        let mut outputs = [Value::from(ExternRef::null())];
        let resumable = main
            .call_resumable(&mut *store, &values[..], &mut outputs[..num_outputs])
            .unwrap();
        assert_eq!(
            id.0,
            store.data_mut().tx.programs.len(),
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
        eprintln!("  = {result:?}");
        store.data_mut().tx.programs.push(TxProgram {
            return_to: from_program,
            code: code.hash(),
            entry_point: entry_point.to_owned(),
            instance,
            num_outputs,
            resumable,
        });
        store.data_mut().tx.witnesses.push(TxWitness {
            from_program,
            to_program: id,
            values,
        });
        (id, result)
    }

    fn resume(
        store: &mut Store<CoordinationScriptInstance>,
        from_program: ProgramIdx,
        to_program: ProgramIdx,
        values: Vec<Value>,
    ) -> Result<Vec<Value>, Interrupt> {
        match std::mem::replace(
            &mut store.data_mut().tx.programs[to_program.0].resumable,
            ResumableCall::Finished,
        ) {
            ResumableCall::Finished => panic!("attempt to resume finished program"),
            ResumableCall::Resumable(invocation) => {
                let num_outputs = store.data_mut().tx.programs[to_program.0].num_outputs;
                let mut outputs = [Value::from(ExternRef::null())];
                let resumable = invocation
                    .resume(&mut *store, &values[..], &mut outputs[..num_outputs])
                    .unwrap();
                let result = match &resumable {
                    ResumableCall::Finished => Ok(outputs[..num_outputs].to_vec()),
                    ResumableCall::Resumable(invocation) => Err(invocation
                        .host_error()
                        .downcast_ref::<Interrupt>()
                        .unwrap()
                        .clone()),
                };
                store.data_mut().tx.programs[to_program.0].resumable = resumable;
                store.data_mut().tx.witnesses.push(TxWitness {
                    from_program,
                    to_program,
                    values,
                });
                result
            }
        }
    }

    pub fn run_coordination_script(
        &mut self,
        coordination_code: &Arc<ContractCode>,
        entry_point: &str,
        inputs: &[ValueOrUtxo],
    ) -> ValueOrUtxo {
        eprintln!("run_transaction({entry_point:?}, {inputs:?})");

        let engine = Engine::default();

        let linker = coordination_script_linker(
            &engine.clone(),
            &self.code_cache,
            coordination_code.clone(),
        );

        let mut store = Store::new(
            &engine,
            CoordinationScriptInstance {
                coordination_code: &coordination_code,
                tx: self,
                temporary_utxo_ids: Default::default(),
            },
        );

        // Turn ExternRefs into u32 UTXO refs
        let mut inputs2 = Vec::with_capacity(inputs.len());
        for value in inputs {
            inputs2.push(match value {
                ValueOrUtxo::Value(v) => v.clone(),
                ValueOrUtxo::Utxo(u) => u.to_wasm_u32(store.as_context_mut()),
            });
        }

        let (mut program, mut result) = Transaction::start_program(
            &mut store,
            &linker,
            coordination_code,
            entry_point,
            ProgramIdx::Root,
            inputs2,
        );
        loop {
            match result {
                Ok(values) => {
                    // Program returned.
                    let return_to = store.data_mut().tx.programs[program.0].return_to;
                    eprintln!("{program:?} -> {return_to:?}: {values:?}");
                    if return_to == ProgramIdx::Root {
                        let result = if values.len() > 0 {
                            if let Some(utxo) = UtxoId::from_wasm(&values[0], store.as_context()) {
                                // TODO: collisions still technically possible here.
                                // Should consider examining static types.
                                ValueOrUtxo::Utxo(utxo)
                            } else {
                                ValueOrUtxo::Value(values[0].clone())
                            }
                        } else {
                            ValueOrUtxo::Value(Value::I32(0))
                        };

                        store.data_mut().tx.witnesses.push(TxWitness {
                            from_program: program,
                            to_program: ProgramIdx::Root,
                            values,
                        });

                        return result;
                    }
                    (program, result) = (
                        return_to,
                        Transaction::resume(&mut store, program, return_to, values),
                    );
                }
                Err(Interrupt::Yield { .. }) => {
                    todo!();
                }
            }
        }
    }
}

impl std::fmt::Debug for Transaction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Transaction")
            .field("utxos", &self.utxos)
            //.field("programs", &self.programs)
            .field("witnesses", &self.witnesses)
            .finish()
    }
}

// TODO: Universe or World type which can spawn transactions (loading a subset
// of UTXOs into WASM memories) and commit them (verify, flush WASM instances).
// In the long term it should be possible to commit ZK proofs of transactions.
