//! Starstream ledger client as a Wasm module.
//!
//! `client_call` executes a coordination script of a published contract and
//! passes the resulting transaction to the `client_result` import, without
//! signing or submitting it. Contracts and the outputs spent by the
//! transaction are requested from the embedder through the
//! `client_get_contract` and `client_get_output` imports, which it answers by
//! calling `client_reply_ok` or `client_reply_error`.
//! A call to `client_call` made while another is in progress, or after one
//! trapped, fails. `client_call` must be entered through
//! `WebAssembly.promising`, see [`starstream_runtime_web`]. The other exports
//! do not suspend, so the embedder may call them while it is suspended.

#![cfg(target_family = "wasm")]

use core::iter::zip;
use core::ptr;
use core::sync::atomic::{AtomicBool, Ordering};

use std::collections::HashMap;
use std::panic;
use std::sync::{Arc, Once, OnceLock};

use anyhow::{Context as _, bail, ensure};
use bytes::{Bytes, BytesMut};
use futures::channel::oneshot;
use starstream_ledger::client::runtime::{
    Client, Ctx, UtxoCtx, call_coordination_script, compile_contract,
};
use starstream_ledger::wrpc::bindings::starstream::ledger::types;
use starstream_ledger::wrpc::codec::{ValEncoder, read_value};
use starstream_ledger::{CardanoCtx, TransactionInput, TransactionOutput, encode_digest};
use starstream_runtime::Utxo;
use starstream_runtime_web::{block_on, disable_cranelift_profiler};
use tokio_util::codec::Encoder as _;
use wasmtime::Store;
use wasmtime::component::{Type, Val};
use wasmtime_wizer::Wizer;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    fn client_error(ptr: *const u8, len: usize);
    fn client_result(tx: *const u8, tx_len: usize, results: *const u8, results_len: usize);
    /// Fetches the Wasm of the contract with `digest`, then passes it to
    /// `client_reply_ok(reply, ..)` or fails `reply`.
    fn client_get_contract(digest: *const u8, digest_len: usize, reply: *mut u8);
    /// Fetches the output `index` of `transaction`, empty for genesis, then
    /// passes it wRPC-encoded to `client_reply_ok(reply, ..)` or fails `reply`.
    fn client_get_output(
        transaction: *const u8,
        transaction_len: usize,
        index: u32,
        reply: *mut u8,
    );
}

static ENTERED: AtomicBool = AtomicBool::new(false);
static ENGINE: OnceLock<wasmtime::Engine> = OnceLock::new();

type Reply = oneshot::Sender<Result<Bytes, String>>;

struct EmbedderClient;

async fn request(f: impl FnOnce(*mut u8)) -> anyhow::Result<Bytes> {
    let (tx, rx) = oneshot::channel::<Result<Bytes, String>>();
    f(Box::into_raw(Box::new(tx)).cast());
    let res = rx.await.context("request dropped")?;
    res.map_err(anyhow::Error::msg)
}

impl Client for EmbedderClient {
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        let digest = encode_digest(&digest);
        request(|reply| unsafe { client_get_contract(digest.as_ptr(), digest.len(), reply) })
            .await
            .with_context(|| format!("failed to get contract `{digest}`"))
    }

    async fn get_input_utxo(&self, input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        let TransactionInput { transaction, index } = input;
        let buf = request(|reply| unsafe {
            client_get_output(transaction.as_ptr(), transaction.len(), *index, reply)
        })
        .await
        .with_context(|| format!("failed to get output `{transaction}:{index}`"))?;
        let output = wrpc_transport::unpack::<types::TransactionOutput>(&mut BytesMut::from(buf))
            .context("failed to decode output")?;
        Ok(output.into())
    }
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

fn report(res: impl FnOnce() -> anyhow::Result<()>) -> i32 {
    init();
    if ENTERED.swap(true, Ordering::Relaxed) {
        report_error("client is busy or trapped");
        return -1;
    }
    let res = res();
    ENTERED.store(false, Ordering::Relaxed);
    match res {
        Ok(()) => 0,
        Err(err) => {
            report_error(&format!("{err:#}"));
            -1
        }
    }
}

unsafe fn take(ptr: *mut u8, len: usize) -> Vec<u8> {
    let buf = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, len)) };
    buf.into_vec()
}

fn engine() -> anyhow::Result<&'static wasmtime::Engine> {
    if let Some(engine) = ENGINE.get() {
        return Ok(engine);
    }
    let mut config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut config);
    starstream_runtime_web::configure_wasmtime(&mut config)?;
    let engine = wasmtime::Engine::new(&config)?;
    Ok(ENGINE.get_or_init(|| engine))
}

async fn call(
    contract: &str,
    script: &str,
    mut args: &[u8],
    cardano: CardanoCtx,
) -> anyhow::Result<()> {
    let engine = engine()?;
    let wizer = Wizer::new();

    let digest = starstream_ledger::parse_digest(contract)
        .with_context(|| format!("failed to parse `{contract}` as multibase multihash"))?;
    let wasm = EmbedderClient.get_contract(digest).await?;
    let mut contracts = HashMap::default();
    let contract =
        compile_contract(&EmbedderClient, engine, &wizer, &wasm, None, &mut contracts).await?;
    let script = contract.get_coordination_script(script)?;

    let ty = script.ty();
    let mut params = Vec::with_capacity(ty.params().len());
    for (name, ty) in ty.params() {
        let mut v = Val::Bool(false);
        match ty {
            Type::Own(..) | Type::Borrow(..) => {
                let mut index = Val::Bool(false);
                read_value(&mut args, &mut v, &Type::String)
                    .await
                    .with_context(|| format!("failed to decode input of parameter `{name}`"))?;
                read_value(&mut args, &mut index, &Type::U32)
                    .await
                    .with_context(|| format!("failed to decode input of parameter `{name}`"))?;
                let (Val::String(transaction), Val::U32(index)) = (v, index) else {
                    unreachable!("decoded value type mismatch");
                };
                params.push(
                    TransactionInput {
                        transaction: transaction.into(),
                        index,
                    }
                    .into(),
                );
            }
            ty => {
                read_value(&mut args, &mut v, &ty)
                    .await
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
    let mut store = Store::new(engine, ctx);
    let mut utxos = Vec::default();
    let (tx, ..) = call_coordination_script(
        &mut store,
        &EmbedderClient,
        &wizer,
        &contract,
        &wasm,
        &script,
        &mut contracts,
        params,
        &mut results,
        &mut utxos,
    )
    .await?;

    let mut buf = BytesMut::default();
    'outer: for (result, ty) in zip(results, ty.results()) {
        if let Val::Resource(utxo) = result {
            let utxo = utxo
                .try_into_resource(&mut store)
                .map_err(anyhow::Error::from)
                .context("result resource is not a UTXO")?;
            let utxo: &Utxo<Arc<std::sync::Mutex<UtxoCtx>>> = store
                .data()
                .table
                .get(&utxo)
                .context("result UTXO not found")?;
            for (i, out) in zip(0u32.., &utxos) {
                if utxo.resource() == out.resource() {
                    ValEncoder::new(&Type::U32)
                        .encode(&Val::U32(i), &mut buf)
                        .map_err(anyhow::Error::from)
                        .context("failed to encode result")?;
                    continue 'outer;
                }
            }
            bail!("failed to identify result resource");
        }
        ValEncoder::new(&ty)
            .encode(&result, &mut buf)
            .map_err(anyhow::Error::from)
            .context("failed to encode result")?;
    }
    let mut tx_buf = BytesMut::default();
    wrpc_transport::pack_ready(types::Transaction::from(tx), &mut tx_buf)
        .context("failed to encode transaction")?;
    unsafe { client_result(tx_buf.as_ptr(), tx_buf.len(), buf.as_ptr(), buf.len()) };
    Ok(())
}

/// Allocate `len` uninitialized bytes to be written in full and passed to
/// another export, which takes ownership.
#[unsafe(no_mangle)]
pub extern "C" fn client_alloc(len: usize) -> *mut u8 {
    init();
    Box::into_raw(Box::<[u8]>::new_uninit_slice(len)).cast()
}

/// Call the coordination script `script` of the contract with `contract`
/// digest with wRPC-encoded `args`, each UTXO argument encoded as the
/// transaction digest, empty for genesis, followed by the output index, and
/// pass the wRPC-encoded transaction and results to the `client_result`
/// import, each UTXO result encoded as the index of its transaction output.
///
/// # Safety
///
/// `contract`, `script` and `args` must each be the result of
/// `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
#[allow(clippy::too_many_arguments)]
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
    report(|| {
        let contract = str::from_utf8(&contract).context("contract digest is not UTF-8")?;
        let script = str::from_utf8(&script).context("script name is not UTF-8")?;
        let cardano = CardanoCtx {
            block_height: cardano_block_height,
            current_slot: cardano_current_slot,
        };
        block_on(call(contract, script, &args, cardano))
    })
}

unsafe fn send(reply: *mut u8, res: Result<Bytes, String>) {
    let reply = unsafe { Box::from_raw(reply.cast::<Reply>()) };
    _ = reply.send(res);
}

/// Answer the request passed `reply` with `data`.
///
/// # Safety
///
/// `reply` must be passed to a `client_get_*` import and to exactly one
/// `client_reply_*` export, and `data` must be the result of
/// `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_reply_ok(reply: *mut u8, data: *mut u8, len: usize) {
    let data = unsafe { take(data, len) };
    unsafe { send(reply, Ok(Bytes::from(data))) }
}

/// Fail the request passed `reply` with the UTF-8 message `err`.
///
/// # Safety
///
/// `reply` must be passed to a `client_get_*` import and to exactly one
/// `client_reply_*` export, and `err` must be the result of
/// `client_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn client_reply_error(reply: *mut u8, err: *mut u8, len: usize) {
    let err = unsafe { take(err, len) };
    unsafe { send(reply, Err(String::from_utf8_lossy(&err).into_owned())) }
}
