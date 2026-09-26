//! WebAssembly bindings for the Starstream compiler and contract runtime,
//! driven over a small C ABI by two Web Workers (`website/src/*.worker.ts`): a
//! compile worker ([`run`]) and a run worker ([`deploy`] + [`construct`],
//! [`call`], …).
//!
//! The contract runtime is `starstream-runtime-next`, driven through its
//! `*_async` APIs on JSPI-backed fibers (see [`fiber`]); values cross the JS
//! boundary as JSON, lowered to/from [`wasmtime::component::Val`] against each
//! function's declared type.
mod fiber;
mod platform;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::panic;
use std::pin::Pin;
use std::sync::{Arc, Mutex, MutexGuard};

use fiber::block_on;
use log::error;
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};
use starstream_runtime_next::{Contract, ContractLookup, Host, Token, Utxo, UtxoExport, bindings};
use wasmtime::component::{Component, Resource, ResourceTable, Type, Val, types};
use wasmtime::error::Context as _;
use wasmtime::{StoreContextMut, bail, format_err};
use wit_component::DecodedWasm;

// Imports to manipulate the UI contents, provided by the JS page.
#[link(wasm_import_module = "env")]
unsafe extern "C" {
    unsafe fn read_input(ptr: *mut u8, len: usize);

    unsafe fn sandbox_log(
        level: u32,
        target: *const u8,
        target_len: usize,
        body: *const u8,
        body_len: usize,
    );
    unsafe fn set_wat(ptr: *const u8, len: usize);
    unsafe fn set_core_wasm(ptr: *const u8, len: usize);
    unsafe fn set_wit(ptr: *const u8, len: usize);
    unsafe fn set_component_wasm(ptr: *const u8, len: usize);

    // JSON sinks back to the page (see the function producing each).
    unsafe fn set_describe(ptr: *const u8, len: usize);
    unsafe fn set_call_result(ptr: *const u8, len: usize);
    unsafe fn set_storage(ptr: *const u8, len: usize);
    unsafe fn set_implemented(ptr: *const u8, len: usize);
    unsafe fn set_events(ptr: *const u8, len: usize);
}

/// A 256-bit `implements-method` hash, as four `u64` words from the guest.
type MethodHash = (u64, u64, u64, u64);

/// Cardano context observable via `starstream:std/cardano`, set from the page.
#[derive(Clone, Copy, Default)]
struct CardanoCtx {
    block_height: i64,
    current_slot: i64,
}

/// Host-side state of one UTXO: the methods it declared via
/// `implements-method`, cleared again on `resume`. [`call`] gates invocations
/// on it.
#[derive(Default)]
struct UtxoCtx {
    methods: Vec<MethodHash>,
}

fn lock(cx: &Mutex<UtxoCtx>) -> wasmtime::Result<MutexGuard<'_, UtxoCtx>> {
    cx.lock().map_err(|err| format_err!("{err}"))
}

/// Per-store data. Every live UTXO owns one store, so the events its
/// instance emits during an operation collect here and are flushed by
/// [`flush_events`] afterwards.
#[derive(Default)]
struct Ctx {
    table: ResourceTable,
    cardano: CardanoCtx,
    events: Vec<Value>,
}

impl bindings::starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        Ok(self.cardano.block_height)
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        Ok(self.cardano.current_slot)
    }
}

impl Host for Ctx {
    type UtxoContext = Arc<Mutex<UtxoCtx>>;

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn call_utxo_main(
        store: StoreContextMut<'_, Self>,
        _instance_name: Arc<str>,
        _external_id: Option<Arc<str>>,
        _export: UtxoExport,
        f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        f(store, Self::UtxoContext::default()).await
    }

    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: MethodHash,
    ) -> wasmtime::Result<bool> {
        let utxo = store.data().table.get(&utxo)?;
        let cx = lock(utxo.context())?;
        Ok(cx.methods.contains(&hash))
    }

    fn drop_utxo(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        store.data_mut().table.delete(utxo)?;
        Ok(())
    }

    fn implements_method(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: MethodHash,
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get(&cx)?;
        lock(cx)?.methods.push(hash);
        Ok(())
    }

    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get(&cx)?;
        lock(cx)?.methods.clear();
        Ok(())
    }

    fn drop_utxo_context(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        store.data_mut().table.delete(cx)?;
        Ok(())
    }

    fn drop_token(
        mut store: StoreContextMut<Self>,
        token: Resource<Token>,
    ) -> wasmtime::Result<()> {
        store.data_mut().table.delete(token)?;
        Ok(())
    }

    fn emit_event(
        mut store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        let params: Vec<Value> = params
            .iter()
            .map(|v| val_to_json(v).unwrap_or_else(Value::String))
            .collect();
        store.data_mut().events.push(json!({
            "instance": &**abi_name,
            "name": &**name,
            "params": params,
        }));
        Ok(())
    }
}

/// Contracts are deployed in isolation: imports of other contracts cannot be
/// satisfied.
struct NoContracts;

impl ContractLookup<Ctx> for NoContracts {
    fn get_contract(&self, external_id: &str) -> wasmtime::Result<Contract<Ctx>> {
        bail!("contract `{external_id}` is not deployed")
    }
}

/// A live `utxo` handle: the UTXO, the store its instance lives in, and the
/// export it came from.
struct Handle {
    export: UtxoExport,
    store: wasmtime::Store<Ctx>,
    utxo: Utxo<Arc<Mutex<UtxoCtx>>>,
}

/// A deployed contract and its table of live UTXO handles.
struct Deployment {
    engine: wasmtime::Engine,
    contract: Contract<Ctx>,
    handles: BTreeMap<u32, Handle>,
    next_id: u32,
    cardano: CardanoCtx,
}

thread_local! {
    /// Deployed contracts keyed by digest; the sandbox is single-threaded.
    static CONTRACTS: RefCell<BTreeMap<u32, Deployment>> = const { RefCell::new(BTreeMap::new()) };
}

#[derive(serde::Deserialize)]
struct Input<'a> {
    code: &'a str,
}

/// Set up output and panic context. Idempotent: the one-time global setup
/// (logger, panic hook, Cranelift profiler) runs only on the first call, so
/// every exported entrypoint can call it cheaply.
fn init() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(|| {
        /// Cranelift profiler that does nothing: the default one calls
        /// `Instant::now()`, which panics on wasm32-unknown-unknown.
        struct CraneliftProfiler;
        impl cranelift_codegen::timing::Profiler for CraneliftProfiler {
            fn start_pass(&self, _pass: cranelift_codegen::timing::Pass) -> Box<dyn std::any::Any> {
                Box::new(())
            }
        }
        _ = cranelift_codegen::timing::set_thread_profiler(Box::new(CraneliftProfiler));

        _ = log::set_logger(&LOGGER);
        log::set_max_level(log::LevelFilter::Trace);
        panic::set_hook(Box::new(|info| {
            if let Some(s) = info.payload().downcast_ref::<&str>() {
                error!("panic! {s}");
            } else if let Some(s) = info.payload().downcast_ref::<String>() {
                error!("panic! {s}");
            } else {
                error!("panic!");
            }
            if let Some(loc) = info.location() {
                error!("at {}:{}:{}", loc.file(), loc.line(), loc.column());
            }
        }));
    });
}

/// Compile the CBOR [`Input`] from `read_input` to core/WIT/component.
///
/// # Safety
///
/// `input_len` must equal the number of bytes the host staged for `read_input`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(input_len: usize) {
    init();

    let mut input = vec![0; input_len];
    unsafe { read_input(input.as_mut_ptr(), input.len()) };
    let input: Input = serde_cbor::from_slice(&input).unwrap();
    let code = input.code;

    // Parse to AST.
    let (program, errors) = starstream_compiler::parse_program(code).into_output_errors();
    for error in errors {
        write_report(&error.into());
    }
    let Some(program) = program else {
        return;
    };

    // Typecheck.
    let typed = match starstream_compiler::typecheck_program(&program, Default::default()) {
        Ok(mut program) => {
            for warning in program.warnings.drain(..) {
                write_report(&warning.into());
            }
            program
        }
        Err(failure) => {
            for warning in failure.warnings {
                write_report(&warning.into());
            }
            for error in failure.errors {
                write_report(&error.into());
            }
            return;
        }
    };

    // Compile to Wasm.
    let compile_result = starstream_to_wasm::compile(&typed.program);
    for error in &compile_result.errors {
        write_report(&error.clone().into());
    }
    let Some(wasm) = &compile_result.wasm else {
        return;
    };

    unsafe { set_core_wasm(wasm.as_ptr(), wasm.len()) };

    // WITify core version if we can.
    match print_wit(wasm, true) {
        Ok(wit) => unsafe { set_wit(wit.as_ptr(), wit.len()) },
        Err(error) => error!("print_wit(core): {error}"),
    }

    // Componentize.
    let component;
    let wasm = match compile_result.to_component() {
        Ok(wasm) => {
            unsafe { set_component_wasm(wasm.as_ptr(), wasm.len()) };

            // WITify component version (it'll be more compact).
            match print_wit(&wasm, false) {
                Ok(wit) => unsafe { set_wit(wit.as_ptr(), wit.len()) },
                Err(error) => error!("print_wit(component): {error}"),
            }

            component = wasm;
            &component[..]
        }
        Err(error) => {
            error!("componentize: {error}");
            wasm
        }
    };

    // Format to WAT.
    let mut wat = Vec::new();
    match wasmprinter::Config::new().fold_instructions(true).print(
        wasm,
        &mut wasmprinter::PrintTermcolor(termcolor::Ansi::new(&mut wat)),
    ) {
        Ok(()) => {
            unsafe { set_wat(wat.as_ptr(), wat.len()) };
        }
        Err(wat_err) => {
            let wat_err = wat_err.to_string();
            unsafe { set_wat(wat_err.as_ptr(), wat_err.len()) };
        }
    }
}

// ----------------------------------------------------------------------------
// Run tab: deploy and drive a compiled contract.

fn read_input_bytes(input_len: usize) -> Vec<u8> {
    let mut input = vec![0; input_len];
    unsafe { read_input(input.as_mut_ptr(), input.len()) };
    input
}

/// Flush and report the ABI events buffered in `store` during an operation.
fn flush_events(store: &mut wasmtime::Store<Ctx>) {
    let drained = std::mem::take(&mut store.data_mut().events);
    if drained.is_empty() {
        return;
    }
    let json = Value::Array(drained).to_string();
    unsafe { set_events(json.as_ptr(), json.len()) };
}

#[derive(serde::Deserialize)]
struct DeployInput<'a> {
    /// Digest of `wasm`, identifying the contract.
    digest: u32,
    /// The component Wasm.
    wasm: &'a [u8],
}

fn handle_deploy(input_len: usize) -> wasmtime::Result<()> {
    let input = read_input_bytes(input_len);
    let DeployInput { digest, wasm } = serde_cbor::from_slice(&input)?;

    let mut config = wasmtime::Config::new();
    config.wasm_component_model_implements(true);
    // Pulley bytecode in malloc-backed memory: no signals, guards or CoW images
    // (see `platform`).
    config.target("pulley32")?;
    config.signals_based_traps(false);
    config.memory_guard_size(0);
    config.memory_reservation(0);
    config.memory_reservation_for_growth(1 << 20);
    config.memory_init_cow(false);
    let engine = wasmtime::Engine::new(&config)?;
    let component = Component::from_binary(&engine, wasm).context("failed to compile component")?;
    let contract =
        Contract::new(&component, None, NoContracts).context("failed to link contract")?;
    let describe = describe(&contract)?;

    CONTRACTS.with_borrow_mut(|contracts| {
        contracts.insert(
            digest,
            Deployment {
                engine,
                contract,
                handles: BTreeMap::new(),
                next_id: 0,
                cardano: CardanoCtx::default(),
            },
        );
    });
    unsafe { set_describe(describe.as_ptr(), describe.len()) };
    Ok(())
}

/// Deploy a contract, reporting its instances via `set_describe`. Returns 0, or
/// -1 on failure.
///
/// # Safety
///
/// `input_len` must equal the number of bytes the host staged for `read_input`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn deploy(input_len: usize) -> i32 {
    init();
    match handle_deploy(input_len) {
        Ok(()) => 0,
        Err(error) => {
            error!("deploy: {error:#}");
            -1
        }
    }
}

#[derive(serde::Deserialize)]
struct ConstructInput {
    /// Digest of the contract, as passed to `deploy`.
    digest: u32,
    /// Export name of the UTXO-owning instance.
    instance: String,
    /// Export name of the `[static]` `main fn` to call.
    constructor: String,
    /// One JSON value per `main fn` parameter (excluding the `utxo-context`).
    args: Vec<Value>,
}

fn handle_construct(input_len: usize) -> wasmtime::Result<u32> {
    let input = read_input_bytes(input_len);
    let ConstructInput {
        digest,
        instance,
        constructor,
        args,
    } = serde_json::from_slice(&input)?;

    CONTRACTS.with_borrow_mut(|contracts| {
        let dep = contracts.get_mut(&digest).context("contract not found")?;
        let export = dep.contract.get_utxo(&instance)?;
        let main = dep.contract.get_utxo_main(&export, &constructor)?;
        let params = convert_args(main.ty().params(), 1, &args)?;

        let mut store = wasmtime::Store::new(
            &dep.engine,
            Ctx {
                cardano: dep.cardano,
                ..Ctx::default()
            },
        );
        let cx = Arc::new(Mutex::new(UtxoCtx::default()));
        let cx_res = store.data_mut().table.push(Arc::clone(&cx))?;
        let cx_res = cx_res.try_into_resource_any(&mut store)?;
        let mut full = Vec::with_capacity(params.len() + 1);
        full.push(Val::Resource(cx_res));
        full.extend(params);

        let instance = block_on(dep.contract.instantiate(&mut store))?;
        let utxo = block_on(instance.call_utxo_main(&mut store, &export, &main, cx, &full))?;
        flush_events(&mut store);

        let id = dep.next_id;
        dep.next_id += 1;
        dep.handles.insert(
            id,
            Handle {
                export,
                store,
                utxo,
            },
        );
        Ok(id)
    })
}

/// Mint a UTXO via a `[static]` `main fn`. Returns the new handle id, or -1 on
/// failure.
///
/// # Safety
///
/// `input_len` must equal the number of bytes the host staged for `read_input`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn construct(input_len: usize) -> i32 {
    init();
    match handle_construct(input_len) {
        Ok(id) => id as i32,
        Err(error) => {
            error!("construct: {error:#}");
            -1
        }
    }
}

#[derive(serde::Deserialize)]
struct CallInput {
    /// Digest of the contract, as passed to `deploy`.
    digest: u32,
    /// Number of the UTXO handle to call, as returned by `construct`. The handle
    /// already carries its instance export, so no instance name is needed.
    handle: u32,
    /// Export name of the `[method]` to invoke.
    method: String,
    /// One JSON value per method parameter (excluding the `self` receiver).
    args: Vec<Value>,
}

fn handle_call(input_len: usize) -> wasmtime::Result<()> {
    let input = read_input_bytes(input_len);
    let CallInput {
        digest,
        handle,
        method,
        args,
    } = serde_json::from_slice(&input)?;

    CONTRACTS.with_borrow_mut(|contracts| {
        let dep = contracts.get_mut(&digest).context("contract not found")?;
        let handle = dep.handles.get_mut(&handle).context("handle not found")?;
        let export = dep.contract.get_utxo_method(&handle.export, &method)?;

        // Only callable if the UTXO declared this method via `implements-method`.
        let hash = method_hash(&method);
        if !lock(handle.utxo.context())?.methods.contains(&hash) {
            bail!(
                "method `{method}` is not callable: this UTXO did not declare it via `implements-method`"
            );
        }

        let params = convert_args(export.ty().params(), 1, &args)?;
        let mut full = Vec::with_capacity(params.len() + 1);
        full.push(Val::Resource(handle.utxo.resource()));
        full.extend(params);
        let mut results = vec![Val::Bool(false); export.ty().results().len()];

        block_on(
            handle
                .utxo
                .call_method(&mut handle.store, &export, &full, &mut results),
        )?;
        let results = results
            .iter()
            .map(val_to_json)
            .collect::<Result<Vec<Value>, String>>()
            .map_err(|err| format_err!("{err}"))?;
        let json = serde_json::to_string(&results)?;
        unsafe { set_call_result(json.as_ptr(), json.len()) };
        flush_events(&mut handle.store);
        Ok(())
    })
}

/// Invoke a `[method]` on a live UTXO handle, reporting results via
/// `set_call_result`. Returns 0, or -1 on failure.
///
/// # Safety
///
/// `input_len` must equal the number of bytes the host staged for `read_input`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn call(input_len: usize) -> i32 {
    init();
    match handle_call(input_len) {
        Ok(()) => 0,
        Err(error) => {
            error!("call: {error:#}");
            -1
        }
    }
}

fn handle_storage_get(digest: u32, handle: u32) -> wasmtime::Result<()> {
    CONTRACTS.with_borrow_mut(|contracts| {
        let dep = contracts.get_mut(&digest).context("contract not found")?;
        let handle = dep.handles.get_mut(&handle).context("handle not found")?;
        let storage = handle
            .export
            .storage()
            .context("this resource has no storage")?;
        let fields = block_on(handle.utxo.storage(storage).call_get(&mut handle.store))?;
        let obj = fields
            .iter()
            .map(|(name, val)| Ok((name.clone(), val_to_json(val)?)))
            .collect::<Result<Map<String, Value>, String>>()
            .map_err(|err| format_err!("{err}"))?;
        let json = Value::Object(obj).to_string();
        unsafe { set_storage(json.as_ptr(), json.len()) };
        Ok(())
    })
}

/// Reads a live UTXO handle's `storage` record as a JSON object, reported via
/// `set_storage`. Returns 0, or -1 on failure.
#[unsafe(no_mangle)]
pub extern "C" fn storage_get(digest: u32, handle: u32) -> i32 {
    init();
    match handle_storage_get(digest, handle) {
        Ok(()) => 0,
        Err(error) => {
            error!("storage_get: {error:#}");
            -1
        }
    }
}

fn handle_implemented_methods(digest: u32, handle: u32) -> wasmtime::Result<()> {
    CONTRACTS.with_borrow(|contracts| {
        let dep = contracts.get(&digest).context("contract not found")?;
        let handle = dep.handles.get(&handle).context("handle not found")?;
        let declared = lock(handle.utxo.context())?.methods.clone();
        let names: Vec<&str> = dep
            .contract
            .utxo_methods(&handle.export)
            .filter_map(|(name, method)| method.ok().map(|_| name))
            .filter(|name| declared.contains(&method_hash(name)))
            .collect();
        let json = serde_json::to_string(&names)?;
        unsafe { set_implemented(json.as_ptr(), json.len()) };
        Ok(())
    })
}

/// Lists the method export names a live UTXO handle has declared callable via
/// `implements-method`, reported as a JSON array via `set_implemented`. Returns
/// 0, or -1 on failure.
#[unsafe(no_mangle)]
pub extern "C" fn implemented_methods(digest: u32, handle: u32) -> i32 {
    init();
    match handle_implemented_methods(digest, handle) {
        Ok(()) => 0,
        Err(error) => {
            error!("implemented_methods: {error:#}");
            -1
        }
    }
}

fn handle_drop_resource(digest: u32, handle: u32) -> wasmtime::Result<()> {
    CONTRACTS.with_borrow_mut(|contracts| {
        let dep = contracts.get_mut(&digest).context("contract not found")?;
        let Handle {
            mut store, utxo, ..
        } = dep.handles.remove(&handle).context("handle not found")?;
        block_on(utxo.drop(&mut store))?;
        flush_events(&mut store);
        Ok(())
    })
}

/// Drops a live UTXO handle, running the guest resource's destructor. Returns 0,
/// or -1 on failure.
#[unsafe(no_mangle)]
pub extern "C" fn drop_resource(digest: u32, handle: u32) -> i32 {
    init();
    match handle_drop_resource(digest, handle) {
        Ok(()) => 0,
        Err(error) => {
            error!("drop_resource: {error:#}");
            -1
        }
    }
}

/// Sets the Cardano context (`cardano#block-height` / `cardano#current-slot`)
/// reported to guests of the deployment. Applies to UTXOs minted *after* this
/// call. Returns 0, or -1 if the deployment is unknown.
#[unsafe(no_mangle)]
pub extern "C" fn set_cardano(digest: u32, block_height: i64, current_slot: i64) -> i32 {
    init();
    CONTRACTS.with_borrow_mut(|contracts| match contracts.get_mut(&digest) {
        Some(dep) => {
            dep.cardano = CardanoCtx {
                block_height,
                current_slot,
            };
            0
        }
        None => {
            error!("set_cardano: contract not found");
            -1
        }
    })
}

// ----------------------------------------------------------------------------
// JSON <-> component-model value plumbing.

/// JSON description of the contract's instances for the page.
///
/// Shape: `{ instances: [{ name, resource, constructors: [func], methods:
/// [func], storage: [{name, kind}] | null }] }`, where `func` is
/// `{ export, label, params: [{name, kind}] }`.
fn describe(contract: &Contract<Ctx>) -> wasmtime::Result<String> {
    // Collect owned pairs first to release the `utxos()` borrow before re-borrowing.
    let utxos: Vec<(String, UtxoExport)> = contract
        .utxos()
        .filter_map(|(name, utxo)| utxo.ok().map(|utxo| (name.to_string(), utxo)))
        .collect();

    let mut instances = Vec::with_capacity(utxos.len());
    for (name, utxo) in &utxos {
        let constructors: Vec<Value> = contract
            .utxo_mains(utxo)
            // Skip the leading `utxo-context`; the host mints it per UTXO.
            .filter_map(|(export, main)| main.ok().map(|main| func_json(export, main.ty(), 1)))
            .collect();
        let methods: Vec<Value> = contract
            .utxo_methods(utxo)
            // Skip the leading `self` (`borrow<utxo>`); it comes from the handle.
            .filter_map(|(export, method)| {
                method.ok().map(|method| func_json(export, method.ty(), 1))
            })
            .collect();
        let storage = utxo.storage().map(|storage| {
            storage
                .ty()
                .fields()
                .map(|field| json!({ "name": field.name, "kind": kind_str(&field.ty) }))
                .collect::<Vec<_>>()
        });
        instances.push(json!({
            "name": name,
            "resource": "utxo",
            "constructors": constructors,
            "methods": methods,
            "storage": storage,
        }));
    }
    Ok(serde_json::to_string(&json!({ "instances": instances }))?)
}

/// Render a function as a describe entry, dropping the first `skip` params.
fn func_json(export: &str, ty: &types::ComponentFunc, skip: usize) -> Value {
    let params: Vec<Value> = ty
        .params()
        .skip(skip)
        .map(|(name, ty)| json!({ "name": name, "kind": kind_str(&ty) }))
        .collect();
    // Export names look like `[static]utxo.new` / `[method]utxo.plus-chips`.
    let label = export.rsplit('.').next().unwrap_or(export);
    json!({ "export": export, "label": label, "params": params })
}

/// The `implements-method` hash for `export`: `sha256` of the method's
/// `snake_case` source name as four little-endian `u64` words. WIT exports it
/// `kebab-case` (`[method]utxo.plus-chips`), so undo the mangling (`-` → `_`).
fn method_hash(export: &str) -> MethodHash {
    let name = export
        .rsplit('.')
        .next()
        .unwrap_or(export)
        .replace('-', "_");
    let digest = Sha256::digest(name.as_bytes());
    let word = |i: usize| {
        u64::from_le_bytes(
            digest[i * 8..i * 8 + 8]
                .try_into()
                .expect("a sha256 digest is 32 bytes"),
        )
    };
    (word(0), word(1), word(2), word(3))
}

/// A JS-friendly tag for a type; non-scalars map to `"json"` (raw JSON box).
fn kind_str(ty: &Type) -> &'static str {
    match ty {
        Type::Bool => "bool",
        Type::S8 => "s8",
        Type::U8 => "u8",
        Type::S16 => "s16",
        Type::U16 => "u16",
        Type::S32 => "s32",
        Type::U32 => "u32",
        Type::S64 => "s64",
        Type::U64 => "u64",
        _ => "json",
    }
}

/// Convert positional JSON arguments to typed [`Val`]s against a function's
/// parameter list, skipping the first `skip` parameters.
fn convert_args<'a>(
    params: impl Iterator<Item = (&'a str, Type)>,
    skip: usize,
    args: &[Value],
) -> wasmtime::Result<Vec<Val>> {
    let tys: Vec<Type> = params.skip(skip).map(|(_, ty)| ty).collect();
    if tys.len() != args.len() {
        bail!("expected {} argument(s), got {}", tys.len(), args.len());
    }
    tys.iter()
        .zip(args)
        .map(|(ty, arg)| json_to_val(ty, arg))
        .collect::<Result<Vec<_>, String>>()
        .map_err(|err| format_err!("{err}"))
}

/// Lower JSON into a [`Val`]. Integers and `bool` plus `record`/`tuple`/
/// `option`; other types are rejected. Integer narrowing is intentionally lossy.
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn json_to_val(ty: &Type, v: &Value) -> Result<Val, String> {
    let as_i = |v: &Value| {
        v.as_i64()
            .or_else(|| v.as_f64().map(|f| f as i64))
            .ok_or_else(|| format!("expected an integer, got `{v}`"))
    };
    let as_u = |v: &Value| {
        v.as_u64()
            .or_else(|| v.as_f64().map(|f| f as u64))
            .ok_or_else(|| format!("expected an unsigned integer, got `{v}`"))
    };
    match ty {
        Type::Bool => v
            .as_bool()
            .map(Val::Bool)
            .ok_or_else(|| format!("expected a boolean, got `{v}`")),
        Type::S8 => Ok(Val::S8(as_i(v)? as i8)),
        Type::U8 => Ok(Val::U8(as_u(v)? as u8)),
        Type::S16 => Ok(Val::S16(as_i(v)? as i16)),
        Type::U16 => Ok(Val::U16(as_u(v)? as u16)),
        Type::S32 => Ok(Val::S32(as_i(v)? as i32)),
        Type::U32 => Ok(Val::U32(as_u(v)? as u32)),
        Type::S64 => Ok(Val::S64(as_i(v)?)),
        Type::U64 => Ok(Val::U64(as_u(v)?)),
        Type::Record(record) => {
            let obj = v
                .as_object()
                .ok_or_else(|| format!("expected an object for record, got `{v}`"))?;
            let mut fields = Vec::new();
            for field in record.fields() {
                let value = obj
                    .get(field.name)
                    .ok_or_else(|| format!("missing record field `{}`", field.name))?;
                fields.push((field.name.to_string(), json_to_val(&field.ty, value)?));
            }
            Ok(Val::Record(fields))
        }
        Type::Tuple(tuple) => {
            let arr = v
                .as_array()
                .ok_or_else(|| format!("expected an array for tuple, got `{v}`"))?;
            let tys: Vec<Type> = tuple.types().collect();
            if tys.len() != arr.len() {
                return Err(format!(
                    "expected a {}-tuple, got {} elements",
                    tys.len(),
                    arr.len()
                ));
            }
            tys.iter()
                .zip(arr)
                .map(|(ty, x)| json_to_val(ty, x))
                .collect::<Result<Vec<_>, _>>()
                .map(Val::Tuple)
        }
        Type::Option(opt) => {
            if v.is_null() {
                Ok(Val::Option(None))
            } else {
                json_to_val(&opt.ty(), v).map(|val| Val::Option(Some(Box::new(val))))
            }
        }
        _ => Err("unsupported parameter type".into()),
    }
}

/// Lift a [`Val`] into JSON for display. Mirrors [`json_to_val`]: integers,
/// `bool`, and `record`/`tuple`/`option`; other types are rejected.
fn val_to_json(v: &Val) -> Result<Value, String> {
    Ok(match v {
        Val::Bool(b) => json!(b),
        Val::S8(n) => json!(n),
        Val::U8(n) => json!(n),
        Val::S16(n) => json!(n),
        Val::U16(n) => json!(n),
        Val::S32(n) => json!(n),
        Val::U32(n) => json!(n),
        Val::S64(n) => json!(n),
        Val::U64(n) => json!(n),
        Val::Tuple(xs) => Value::Array(xs.iter().map(val_to_json).collect::<Result<_, _>>()?),
        Val::Record(fields) => Value::Object(
            fields
                .iter()
                .map(|(k, v)| Ok((k.clone(), val_to_json(v)?)))
                .collect::<Result<_, String>>()?,
        ),
        Val::Option(o) => match o.as_deref() {
            Some(v) => val_to_json(v)?,
            None => Value::Null,
        },
        other => return Err(format!("unsupported result type `{other:?}`")),
    })
}

// ----------------------------------------------------------------------------

fn print_wit(wasm: &[u8], is_core: bool) -> Result<String, Box<dyn std::error::Error>> {
    let decoded = if is_core {
        let (_, bindgen) = wit_component::metadata::decode(wasm)?;
        DecodedWasm::Component(bindgen.resolve, bindgen.world)
    } else {
        wit_component::decode(wasm)?
    };

    let mut printer = wit_component::WitPrinter::default();
    printer.print(decoded.resolve(), decoded.package(), &[])?;
    Ok(printer.output.to_string())
}

// ----------------------------------------------------------------------------

fn write_report(report: &miette::Report) {
    error!("{report}");
}

// ----------------------------------------------------------------------------

static LOGGER: Logger = Logger;

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        let body = record.args().to_string();
        unsafe {
            sandbox_log(
                record.level() as u32,
                record.target().as_ptr(),
                record.target().len(),
                body.as_ptr(),
                body.len(),
            );
        };
    }

    fn flush(&self) {}
}
