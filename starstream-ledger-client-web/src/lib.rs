//! Starstream ledger client as a Wasm module, which executes coordination
//! scripts locally and signs envelopes, sending its requests to a ledger HTTP
//! API through the embedder.
//!
//! `client_start` sets the base URL of the ledger HTTP API. Each operation runs
//! to completion, passes its outcome to the `client_result` import and returns
//! 0, or returns -1 after passing a message to the `client_error` import. The
//! requests of an operation go to the `client_fetch` import, which the
//! embedder answers by calling `client_fetch_response` or `client_fetch_error`.
//! An operation started while another is in progress, or after one trapped,
//! fails. Operations must be entered through `WebAssembly.promising`, see
//! [`starstream_runtime_web`]. The other exports do not suspend, so the
//! embedder may call them while an operation is suspended.

#![cfg(target_family = "wasm")]

use core::cell::RefCell;
use core::iter::zip;
use core::ptr;
use core::sync::atomic::{AtomicBool, Ordering};

use std::collections::HashMap;
use std::panic;
use std::rc::Rc;
use std::sync::{Once, OnceLock};

use anyhow::{Context as _, anyhow, bail, ensure};
use bytes::{Bytes, BytesMut};
use ed25519_dalek::{SigningKey, VerifyingKey};
use futures::channel::oneshot;
use futures::try_join;
use http_body_util::{BodyExt as _, Full};
use serde_json::{Value, json};
use starstream_ledger::client::http::{Client, ClientBuilder, Transport, TransportService};
use starstream_ledger::client::runtime::{
    self, CompiledContract, Contract, Ctx, call_coordination_script, compile_contract,
    resolve_utxo_results,
};
use starstream_ledger::{
    CardanoCtx, Transaction, TransactionInput, TransactionNonce, encode_digest, parse_digest,
};
use starstream_runtime_web::{block_on, configure_wasmtime, disable_cranelift_profiler};
use tokio_util::codec::{Decoder as _, Encoder as _};
use wasmtime::Store;
use wasmtime::component::types::ComponentItem;
use wasmtime::component::{Component, Type, Val};
use wrpc::Receive as _;
use wrpc::wasmtime::{ValDecoder, ValEncoder};

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    fn client_error(ptr: *const u8, len: usize);
    fn client_result(value: *const u8, value_len: usize, results: *const u8, results_len: usize);
    /// Sends the request `head`, formatted like the `head` of `ledger_handle`
    /// in `starstream-ledger-web` but with an absolute URI, with `body`, then
    /// passes its response to `client_fetch_response(reply, ..)` or fails it
    /// with `client_fetch_error(reply, ..)`.
    fn client_fetch(
        head: *const u8,
        head_len: usize,
        body: *const u8,
        body_len: usize,
        reply: *mut u8,
    );
}

type Reply = oneshot::Sender<Result<(u16, Vec<u8>), String>>;

/// Published contract compiled with the contracts it imports.
struct Compiled {
    wasm: Bytes,
    contract: CompiledContract,
    imports: HashMap<[u8; 32], Contract>,
}

thread_local! {
    static CONTRACTS: RefCell<HashMap<[u8; 32], Rc<Compiled>>> = RefCell::default();
}
static CLIENT: OnceLock<Client<TransportService<FetchTransport>, FetchTransport>> = OnceLock::new();
static ENTERED: AtomicBool = AtomicBool::new(false);

#[derive(Default)]
struct Outcome {
    value: Vec<u8>,
    results: Bytes,
}

#[derive(Clone)]
struct FetchTransport;

impl Transport for FetchTransport {
    async fn request(
        &self,
        req: http::Request<Full<Bytes>>,
    ) -> anyhow::Result<(http::response::Parts, Bytes)> {
        let (parts, body) = req.into_parts();
        let Ok(body) = body.collect().await;
        let body = body.to_bytes();
        let mut head = format!("{} {}\n", parts.method, parts.uri).into_bytes();
        for (name, value) in &parts.headers {
            head.extend_from_slice(name.as_str().as_bytes());
            head.extend_from_slice(b": ");
            head.extend_from_slice(value.as_bytes());
            head.push(b'\n');
        }
        let (tx, rx): (Reply, _) = oneshot::channel();
        let reply = Box::into_raw(Box::new(tx)).cast();
        unsafe { client_fetch(head.as_ptr(), head.len(), body.as_ptr(), body.len(), reply) };
        let res = rx.await.context("request dropped")?;
        let (status, body) = res
            .map_err(anyhow::Error::msg)
            .with_context(|| format!("failed to send request to `{}`", parts.uri))?;
        let res = http::Response::builder()
            .status(status)
            .body(())
            .context("failed to build response")?;
        let (parts, ()) = res.into_parts();
        Ok((parts, Bytes::from(body)))
    }
}

/// Decode a value of type `ty` from the front of `buf`.
fn decode(buf: &mut BytesMut, ty: Type) -> anyhow::Result<Val> {
    let mut dec = ValDecoder::new(ty).with_max_len(u32::MAX);
    let Some(v) = dec.decode(buf)? else {
        bail!("value incomplete");
    };
    ensure!(dec.is_complete(), "value contains pending values");
    Val::try_from(v).map_err(anyhow::Error::from)
}

fn report_error(err: &str) {
    unsafe { client_error(err.as_ptr(), err.len()) }
}

fn init() {
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        disable_cranelift_profiler();
        panic::set_hook(Box::new(|info| report_error(&info.to_string())));
    });
}

fn report(res: anyhow::Result<Outcome>) -> i32 {
    match res {
        Ok(Outcome { value, results }) => {
            unsafe { client_result(value.as_ptr(), value.len(), results.as_ptr(), results.len()) };
            0
        }
        Err(err) => {
            report_error(&format!("{err:#}"));
            -1
        }
    }
}

fn run<T: Future<Output = anyhow::Result<Outcome>>>(
    op: impl FnOnce(&'static Client<TransportService<FetchTransport>, FetchTransport>) -> T,
) -> i32 {
    init();
    if ENTERED.swap(true, Ordering::Relaxed) {
        report_error("client is busy or trapped");
        return -1;
    }
    let res = match CLIENT.get() {
        Some(client) => block_on(op(client)),
        None => Err(anyhow!("client not started")),
    };
    ENTERED.store(false, Ordering::Relaxed);
    report(res)
}

unsafe fn take(ptr: *mut u8, len: usize) -> Vec<u8> {
    let buf = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, len)) };
    buf.into_vec()
}

fn string(buf: Vec<u8>, what: &str) -> anyhow::Result<String> {
    String::from_utf8(buf).with_context(|| format!("{what} is not UTF-8"))
}

fn network(buf: Vec<u8>) -> anyhow::Result<Option<String>> {
    let network = string(buf, "network")?;
    Ok((!network.is_empty()).then_some(network))
}

fn signing_key(key: &[u8]) -> anyhow::Result<SigningKey> {
    SigningKey::try_from(key).context("signing key is not valid")
}

fn start(url: Vec<u8>) -> anyhow::Result<Outcome> {
    let url = string(url, "URL")?;
    let url = url.parse::<http::Uri>().context("URL is not valid")?;
    let mut config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut config);
    configure_wasmtime(&mut config)?;
    let engine = wasmtime::Engine::new(&config)?;
    let client = ClientBuilder::new(
        TransportService(FetchTransport),
        FetchTransport,
        url,
        engine,
    )
    .build();
    if CLIENT.set(client).is_err() {
        bail!("client already started");
    }
    Ok(Outcome::default())
}

/// Get the published contract with `contract` digest compiled, from the
/// contracts compiled before or by fetching and compiling it.
async fn compiled(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    contract: &str,
) -> anyhow::Result<Rc<Compiled>> {
    let digest = parse_digest(contract)
        .with_context(|| format!("failed to parse `{contract}` as multibase multihash"))?;
    if let Some(compiled) = CONTRACTS.with_borrow(|contracts| contracts.get(&digest).cloned()) {
        return Ok(compiled);
    }
    let wasm = runtime::Client::get_contract(client, digest).await?;
    let mut imports = HashMap::default();
    let contract = compile_contract(
        client,
        client.engine(),
        client.wizer(),
        &wasm,
        None,
        &mut imports,
    )
    .await?;
    let compiled = Rc::new(Compiled {
        wasm,
        contract,
        imports,
    });
    CONTRACTS.with_borrow_mut(|contracts| contracts.insert(digest, Rc::clone(&compiled)));
    Ok(compiled)
}

fn describe(ty: &Type) -> anyhow::Result<Value> {
    let kind = |kind: &str| json!({ "kind": kind });
    let value = match ty {
        Type::Bool => kind("bool"),
        Type::S8 => kind("s8"),
        Type::U8 => kind("u8"),
        Type::S16 => kind("s16"),
        Type::U16 => kind("u16"),
        Type::S32 => kind("s32"),
        Type::U32 => kind("u32"),
        Type::S64 => kind("s64"),
        Type::U64 => kind("u64"),
        Type::Float32 => kind("f32"),
        Type::Float64 => kind("f64"),
        Type::Char => kind("char"),
        Type::String => kind("string"),
        Type::List(ty) => {
            let element = describe(&ty.ty())?;
            json!({ "kind": "list", "element": element })
        }
        Type::Record(ty) => {
            let fields = ty
                .fields()
                .map(|field| {
                    let ty = describe(&field.ty)?;
                    Ok(json!({ "name": field.name, "ty": ty }))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            json!({ "kind": "record", "fields": fields })
        }
        Type::Tuple(ty) => {
            let elements = ty
                .types()
                .map(|ty| describe(&ty))
                .collect::<anyhow::Result<Vec<_>>>()?;
            json!({ "kind": "tuple", "elements": elements })
        }
        Type::Variant(ty) => {
            let cases = ty
                .cases()
                .map(|case| {
                    let ty = case.ty.as_ref().map(describe).transpose()?;
                    Ok(json!({ "name": case.name, "ty": ty }))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            json!({ "kind": "variant", "cases": cases })
        }
        Type::Enum(ty) => json!({ "kind": "enum", "names": ty.names().collect::<Vec<_>>() }),
        Type::Option(ty) => {
            let some = describe(&ty.ty())?;
            json!({ "kind": "option", "some": some })
        }
        Type::Result(ty) => {
            let ok = ty.ok().as_ref().map(describe).transpose()?;
            let err = ty.err().as_ref().map(describe).transpose()?;
            json!({ "kind": "result", "ok": ok, "err": err })
        }
        Type::Flags(ty) => json!({ "kind": "flags", "names": ty.names().collect::<Vec<_>>() }),
        ty => bail!("type `{ty:?}` is not supported"),
    };
    Ok(value)
}

fn describe_script(ty: &Type) -> anyhow::Result<Value> {
    match ty {
        Type::Own(..) | Type::Borrow(..) => Ok(json!({ "kind": "utxo" })),
        ty => describe(ty),
    }
}

fn signature<'a>(
    name: &str,
    params: impl IntoIterator<Item = (&'a str, Type)>,
    mut results: impl Iterator<Item = Type>,
    describe: fn(&Type) -> anyhow::Result<Value>,
) -> anyhow::Result<Value> {
    let params = params
        .into_iter()
        .map(|(name, ty)| {
            let ty = describe(&ty)?;
            Ok(json!({ "name": name, "ty": ty }))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let result = results.next().as_ref().map(describe).transpose()?;
    ensure!(
        results.next().is_none(),
        "multiple results are not supported"
    );
    Ok(json!({ "name": name, "params": params, "result": result }))
}

async fn script_type(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    contract: Vec<u8>,
    script: Vec<u8>,
) -> anyhow::Result<Outcome> {
    let contract = string(contract, "contract digest")?;
    let name = string(script, "script name")?;
    let compiled = compiled(client, &contract).await?;
    let script = compiled.contract.get_coordination_script(&name)?;
    let ty = script.ty();
    let signature = signature(&name, ty.params(), ty.results(), describe_script)?;
    let value = serde_json::to_vec(&signature)?;
    Ok(Outcome {
        value,
        ..Outcome::default()
    })
}

async fn scripts(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    contract: Vec<u8>,
) -> anyhow::Result<Outcome> {
    let contract = string(contract, "contract digest")?;
    let compiled = compiled(client, &contract).await?;
    let scripts = compiled
        .contract
        .coordination_scripts()
        .map(|(name, script)| {
            let script = script?;
            let ty = script.ty();
            signature(name, ty.params(), ty.results(), describe_script)
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let value = serde_json::to_vec(&scripts)?;
    Ok(Outcome {
        value,
        ..Outcome::default()
    })
}

fn contract_scripts(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    wasm: Vec<u8>,
) -> anyhow::Result<Outcome> {
    let engine = client.engine();
    let component = Component::new(engine, &wasm)
        .map_err(anyhow::Error::from)
        .context("failed to compile contract")?;
    let scripts = component
        .component_type()
        .exports(engine)
        .filter_map(|(name, export)| match export.ty {
            ComponentItem::ComponentFunc(ty) => {
                Some(signature(name, ty.params(), ty.results(), describe_script))
            }
            _ => None,
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let value = serde_json::to_vec(&scripts)?;
    Ok(Outcome {
        value,
        ..Outcome::default()
    })
}

async fn utxo_methods(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    contract: Vec<u8>,
    instance: Vec<u8>,
) -> anyhow::Result<Outcome> {
    let contract = string(contract, "contract digest")?;
    let instance = string(instance, "UTXO instance")?;
    let compiled = compiled(client, &contract).await?;
    let utxo = compiled
        .contract
        .get_utxo(&instance)
        .map_err(anyhow::Error::from)
        .with_context(|| format!("failed to get UTXO instance `{instance}`"))?;
    let methods = compiled
        .contract
        .utxo_methods(&utxo)
        .map(|(name, method)| {
            let method = method?;
            let name = name.strip_prefix("[method]utxo.").unwrap_or(name);
            let ty = method.ty();
            signature(name, ty.params().skip(1), ty.results(), describe)
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    let value = serde_json::to_vec(&methods)?;
    Ok(Outcome {
        value,
        ..Outcome::default()
    })
}

async fn call(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    contract: Vec<u8>,
    script: Vec<u8>,
    args: Vec<u8>,
    cardano: CardanoCtx,
) -> anyhow::Result<Outcome> {
    let contract = string(contract, "contract digest")?;
    let script = string(script, "script name")?;
    let compiled = compiled(client, &contract).await?;
    let mut contracts = compiled.imports.clone();
    let script = compiled.contract.get_coordination_script(&script)?;
    let ty = script.ty();
    let mut params = Vec::with_capacity(ty.params().len());
    let mut args = BytesMut::from(&args[..]);
    for (name, ty) in ty.params() {
        match ty {
            Type::Own(..) | Type::Borrow(..) => {
                let transaction = decode(&mut args, Type::String)
                    .with_context(|| format!("failed to decode input of parameter `{name}`"))?;
                let index = decode(&mut args, Type::U32)
                    .with_context(|| format!("failed to decode input of parameter `{name}`"))?;
                let (Val::String(transaction), Val::U32(index)) = (transaction, index) else {
                    unreachable!("decoded value type mismatch");
                };
                let input = if transaction.is_empty() {
                    TransactionInput::genesis(index)
                } else {
                    let transaction = parse_digest(&transaction).with_context(|| {
                        format!("failed to parse transaction digest of parameter `{name}`")
                    })?;
                    TransactionInput::new(transaction, index)
                };
                params.push(input.into());
            }
            ty => {
                let v = decode(&mut args, ty)
                    .with_context(|| format!("failed to decode parameter `{name}`"))?;
                params.push(v.into());
            }
        }
    }
    ensure!(args.is_empty(), "trailing arguments");

    let mut results = vec![Val::Bool(false); ty.results().len()];
    let ctx = Ctx {
        cardano,
        ..Ctx::default()
    };
    let mut store = Store::new(client.engine(), ctx);
    let mut utxos = Vec::default();
    let (tx, ..) = call_coordination_script(
        &mut store,
        client,
        client.wizer(),
        &compiled.contract,
        &compiled.wasm,
        &script,
        &mut contracts,
        params,
        &mut results,
        &mut utxos,
        None,
    )
    .await?;

    resolve_utxo_results(&mut store, &utxos, &mut results)?;
    let tx = minicbor::to_vec(&tx).context("failed to encode transaction")?;
    let mut buf = BytesMut::default();
    for (result, ty) in zip(&results, ty.results()) {
        let ty = match ty {
            Type::Own(..) | Type::Borrow(..) => Type::U32,
            ty => ty,
        };
        ValEncoder::new(ty)
            .encode(result, &mut buf)
            .map_err(anyhow::Error::from)
            .context("failed to encode result")?;
    }
    Ok(Outcome {
        value: tx,
        results: buf.freeze(),
    })
}

async fn fund(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    key: Vec<u8>,
    account: Vec<u8>,
    amount: u64,
    network: Vec<u8>,
    nonce: Option<u64>,
) -> anyhow::Result<Outcome> {
    let key = signing_key(&key)?;
    let account = VerifyingKey::try_from(&account[..]).context("account key is not valid")?;
    let network = self::network(network)?;
    let (network, nonce) = try_join!(client.resolve_network(network), async {
        match nonce {
            Some(nonce) => Ok(nonce),
            None => client.next_admin_nonce().await,
        }
    })?;
    client.fund(key, &network, nonce, &account, amount).await?;
    Ok(Outcome::default())
}

async fn publish(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    key: Vec<u8>,
    wasm: Vec<u8>,
    network: Vec<u8>,
    nonce: Option<u64>,
) -> anyhow::Result<Outcome> {
    let key = signing_key(&key)?;
    let network = self::network(network)?;
    let (network, nonce) = try_join!(client.resolve_network(network), async {
        match nonce {
            Some(nonce) => Ok(nonce),
            None => client.next_account_nonce(&key.verifying_key()).await,
        }
    })?;
    let digest = client.publish_contract(key, &network, nonce, wasm).await?;
    Ok(Outcome {
        value: encode_digest(&digest).into(),
        ..Outcome::default()
    })
}

async fn transact(
    client: &Client<TransportService<FetchTransport>, FetchTransport>,
    key: Vec<u8>,
    tx: Vec<u8>,
    network: Vec<u8>,
    nonce: Option<u64>,
) -> anyhow::Result<Outcome> {
    let mut tx: Transaction = minicbor::decode(&tx).context("failed to decode transaction")?;
    let key = signing_key(&key)?;
    let account = key.verifying_key();
    let network = self::network(network)?;
    let (network, nonce) = try_join!(client.resolve_network(network), async {
        match nonce {
            Some(nonce) => Ok(nonce),
            None => {
                let nonce = client.get_account_nonce(&account).await?;
                let nonce = nonce.unwrap_or_default();
                nonce.checked_add(1).context("account nonce exhausted")
            }
        }
    })?;
    tx.nonces = vec![TransactionNonce {
        key: account.to_bytes(),
        nonce,
    }];
    let digest = client.transact(key, &network, tx).await?;
    Ok(Outcome {
        value: encode_digest(&digest).into(),
        ..Outcome::default()
    })
}

/// Allocate `len` uninitialized bytes to be written in full and passed to
/// another export, which takes ownership.
#[unsafe(no_mangle)]
pub extern "C" fn client_alloc(len: usize) -> *mut u8 {
    init();
    Box::into_raw(Box::<[u8]>::new_uninit_slice(len)).cast()
}

/// Send requests to the ledger HTTP API at the base `url`.
///
/// # Safety
///
/// `url` must be the result of `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_start(url: *mut u8, len: usize) -> i32 {
    let url = unsafe { take(url, len) };
    init();
    report(start(url))
}

/// Get the type of the coordination script `script` of the contract with
/// `contract` digest, as the value of the outcome, JSON-encoded as
/// `{"name", "params": [{"name", "ty"}...], "result": ty | null}`, each type an object
/// with a `kind`: a primitive (`bool`, `s8` to `u64`, `f32`, `f64`, `char`,
/// `string`), `list` with an `element`, `record` with `fields`, `tuple` with
/// `elements`, `variant` with `cases`, `enum` or `flags` with `names`,
/// `option` with `some`, `result` with `ok` and `err`, or `utxo` for UTXOs.
/// The client keeps the contracts it compiles.
///
/// # Safety
///
/// `contract` and `script` must be results of `client_alloc`, written in
/// full, with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_script_type(
    contract: *mut u8,
    contract_len: usize,
    script: *mut u8,
    script_len: usize,
) -> i32 {
    let contract = unsafe { take(contract, contract_len) };
    let script = unsafe { take(script, script_len) };
    run(|client| script_type(client, contract, script))
}

/// Get the coordination scripts of the contract with `contract` digest, as the
/// value of the outcome, a JSON-encoded array of
/// `{"name", "params", "result"}`, described like by `client_script_type`.
///
/// # Safety
///
/// `contract` must be the result of `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_scripts(contract: *mut u8, len: usize) -> i32 {
    let contract = unsafe { take(contract, len) };
    run(|client| scripts(client, contract))
}

/// Get the coordination scripts of the contract `wasm`, which need not be
/// published, described like by `client_scripts`.
///
/// # Safety
///
/// `wasm` must be the result of `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_contract_scripts(wasm: *mut u8, len: usize) -> i32 {
    let wasm = unsafe { take(wasm, len) };
    run(|client| async { contract_scripts(client, wasm) })
}

/// Get the methods of the UTXOs of the instance `instance` of the contract with
/// `contract` digest, as the value of the outcome, a JSON-encoded array of
/// `{"name", "params", "result"}` without the UTXO itself, described like by
/// `client_script_type`.
///
/// # Safety
///
/// `contract` and `instance` must be results of `client_alloc`, written in
/// full, with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_utxo_methods(
    contract: *mut u8,
    contract_len: usize,
    instance: *mut u8,
    instance_len: usize,
) -> i32 {
    let contract = unsafe { take(contract, contract_len) };
    let instance = unsafe { take(instance, instance_len) };
    run(|client| utxo_methods(client, contract, instance))
}

/// Call the coordination script `script` of the contract with `contract`
/// digest with the Cardano context `cardano_block_height` and
/// `cardano_current_slot` and the wRPC-encoded arguments `args`, each UTXO
/// argument encoded as the transaction digest, empty for genesis, followed by
/// the output index. The outcome carries the CBOR-encoded transaction the call
/// produces, without signing or submitting it, as its value, and the
/// wRPC-encoded results, each UTXO result encoded as the index of its
/// transaction output, as its results.
///
/// # Safety
///
/// `contract`, `script` and `args` must be results of `client_alloc`, written
/// in full, with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_call(
    contract: *mut u8,
    contract_len: usize,
    script: *mut u8,
    script_len: usize,
    args: *mut u8,
    args_len: usize,
    cardano_block_height: i64,
    cardano_current_slot: i64,
) -> i32 {
    let contract = unsafe { take(contract, contract_len) };
    let script = unsafe { take(script, script_len) };
    let args = unsafe { take(args, args_len) };
    let cardano = CardanoCtx {
        block_height: cardano_block_height,
        current_slot: cardano_current_slot,
    };
    run(|client| call(client, contract, script, args, cardano))
}

/// Credit the account with key `account` with `amount`, signed by the admin
/// signing key `key`, for `network`, defaulting to the ledger's if empty, with
/// `nonce` if `has_nonce` is not 0, defaulting to the one following the last
/// admin nonce.
///
/// # Safety
///
/// `key`, `account` and `network` must be results of `client_alloc`, written
/// in full, with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_fund(
    key: *mut u8,
    key_len: usize,
    account: *mut u8,
    account_len: usize,
    amount: u64,
    network: *mut u8,
    network_len: usize,
    nonce: u64,
    has_nonce: u32,
) -> i32 {
    let key = unsafe { take(key, key_len) };
    let account = unsafe { take(account, account_len) };
    let network = unsafe { take(network, network_len) };
    let nonce = (has_nonce != 0).then_some(nonce);
    run(|client| fund(client, key, account, amount, network, nonce))
}

/// Publish the contract `wasm`, signed by the signing key `key`, for
/// `network`, defaulting to the ledger's if empty, with `nonce` if
/// `has_nonce` is not 0, defaulting to the one following the last nonce of
/// the key. The outcome carries the digest of the contract as its value.
///
/// # Safety
///
/// `key`, `wasm` and `network` must be results of `client_alloc`, written in
/// full, with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_publish(
    key: *mut u8,
    key_len: usize,
    wasm: *mut u8,
    wasm_len: usize,
    network: *mut u8,
    network_len: usize,
    nonce: u64,
    has_nonce: u32,
) -> i32 {
    let key = unsafe { take(key, key_len) };
    let wasm = unsafe { take(wasm, wasm_len) };
    let network = unsafe { take(network, network_len) };
    let nonce = (has_nonce != 0).then_some(nonce);
    run(|client| publish(client, key, wasm, network, nonce))
}

/// Submit the CBOR-encoded transaction `tx`, signed by the signing key `key`,
/// for `network`, defaulting to the ledger's if empty, with `nonce` if
/// `has_nonce` is not 0 and the one following the last one used by the key
/// otherwise as its only nonce. The outcome carries the digest of the
/// transaction as its value.
///
/// # Safety
///
/// `key`, `tx` and `network` must be results of `client_alloc`, written in
/// full, with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_transact(
    key: *mut u8,
    key_len: usize,
    tx: *mut u8,
    tx_len: usize,
    network: *mut u8,
    network_len: usize,
    nonce: u64,
    has_nonce: u32,
) -> i32 {
    let key = unsafe { take(key, key_len) };
    let tx = unsafe { take(tx, tx_len) };
    let network = unsafe { take(network, network_len) };
    let nonce = (has_nonce != 0).then_some(nonce);
    run(|client| transact(client, key, tx, network, nonce))
}

/// Pass the Ed25519 public key of the secret key `key` to `client_result` as
/// its value. Unlike the operations, it does not suspend.
///
/// # Safety
///
/// `key` must be the result of `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_public_key(key: *mut u8, len: usize) -> i32 {
    let key = unsafe { take(key, len) };
    init();
    report(signing_key(&key).map(|key| Outcome {
        value: key.verifying_key().to_bytes().into(),
        ..Outcome::default()
    }))
}

/// Pass the response with `status` and `body` to `reply`.
///
/// # Safety
///
/// `reply` must have been passed to `client_fetch` and not answered yet, and
/// `body` must be the result of `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_fetch_response(
    reply: *mut u8,
    status: u16,
    body: *mut u8,
    len: usize,
) {
    let reply = unsafe { Box::from_raw(reply.cast::<Reply>()) };
    let body = unsafe { take(body, len) };
    _ = reply.send(Ok((status, body)));
}

/// Fail `reply` with the UTF-8 message `err`.
///
/// # Safety
///
/// `reply` must have been passed to `client_fetch` and not answered yet, and
/// `err` must be the result of `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_fetch_error(reply: *mut u8, err: *mut u8, len: usize) {
    let reply = unsafe { Box::from_raw(reply.cast::<Reply>()) };
    let err = unsafe { take(err, len) };
    _ = reply.send(Err(String::from_utf8_lossy(&err).into_owned()));
}
