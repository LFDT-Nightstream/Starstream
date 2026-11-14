//! Starstream ledger as a Wasm module.
//!
//! The module runs a single ledger, which handles requests concurrently:
//! `ledger_handle` queues a request, and the `ledger_wake` import asks the
//! embedder to call `ledger_poll`, which makes progress on the queued requests,
//! until they are answered or `ledger_abort` drops them.
//! A call to `ledger_import`, `ledger_start` or `ledger_poll` made while
//! another of them is in progress, or after one trapped, fails. Those that may
//! run guest code, `ledger_start` and `ledger_poll`, must be entered through
//! `WebAssembly.promising`, see [`starstream_runtime_web`]. The other exports
//! do not suspend, so the embedder may call them while those are suspended.

#![cfg(target_family = "wasm")]

use core::cell::RefCell;
use core::ptr;
use core::sync::atomic::{AtomicBool, Ordering};
use core::time::Duration;

use std::collections::HashMap;
use std::panic;
use std::sync::{Once, OnceLock};

use anyhow::{Context as _, bail};
use bytes::Bytes;
use ed25519_dalek::VerifyingKey;
use http::header::CONTENT_LENGTH;
use http::{HeaderMap, StatusCode};
use http_body_util::BodyExt as _;
use serde::Deserialize;
use sha2::{Digest as _, Sha256};
use starstream_ledger::CardanoCtx;
use starstream_ledger::genesis::Genesis;
use starstream_ledger::server::{
    DEFAULT_FUEL, DEFAULT_IO_TIMEOUT, DEFAULT_MAX_REQUESTS, Ledger, build_text_response,
};
use starstream_runtime_web::{block_on, configure_wasmtime, disable_cranelift_profiler, sleep};

mod body;
use body::*;

mod task;
use task::*;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    fn ledger_error(ptr: *const u8, len: usize);
    fn ledger_response(
        id: u32,
        status: u32,
        head: *const u8,
        head_len: usize,
        body: *const u8,
        body_len: usize,
    );
}

thread_local! {
    static IMPORTS: RefCell<HashMap<[u8; 32], Bytes>> = RefCell::default();
}
static NODE: OnceLock<Node> = OnceLock::new();
static ENTERED: AtomicBool = AtomicBool::new(false);

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Config {
    network: Box<str>,
    #[serde(deserialize_with = "hex::serde::deserialize")]
    admin: [u8; 32],
    #[serde(default)]
    genesis: Genesis,
    max_requests: Option<u32>,
    fuel: Option<u64>,
    io_timeout: Option<u64>,
    #[serde(default)]
    cardano_block_height: i64,
    #[serde(default)]
    cardano_current_slot: i64,
}

struct Node {
    ledger: Ledger,
    io_timeout: Duration,
}

fn report_error(err: &str) {
    unsafe { ledger_error(err.as_ptr(), err.len()) }
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
        report_error("ledger is busy or trapped");
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

fn start(config: &[u8]) -> anyhow::Result<()> {
    if NODE.get().is_some() {
        bail!("ledger already started");
    }
    let Config {
        network,
        admin,
        genesis,
        max_requests,
        fuel,
        io_timeout,
        cardano_block_height,
        cardano_current_slot,
    } = serde_json::from_slice(config).context("failed to parse ledger config")?;
    let admin = VerifyingKey::from_bytes(&admin).context("invalid admin key")?;
    let cardano = CardanoCtx {
        block_height: cardano_block_height,
        current_slot: cardano_current_slot,
    };
    let mut engine_config = wasmtime::Config::default();
    configure_wasmtime(&mut engine_config)?;
    let ledger = IMPORTS.with_borrow(|imports| {
        block_on(Ledger::with_genesis(
            engine_config,
            max_requests.unwrap_or(u32::from(DEFAULT_MAX_REQUESTS)),
            fuel.unwrap_or(DEFAULT_FUEL),
            network,
            admin,
            genesis,
            imports,
            cardano,
        ))
    })?;
    let io_timeout = io_timeout.map_or(DEFAULT_IO_TIMEOUT, Duration::from_secs);
    if NODE.set(Node { ledger, io_timeout }).is_err() {
        unreachable!("ledger started concurrently");
    }
    IMPORTS.take();
    Ok(())
}

fn parse_request(id: u32, head: &[u8]) -> anyhow::Result<http::Request<RequestBody>> {
    let mut lines = head
        .split(|b| *b == b'\n')
        .map(<[u8]>::trim_ascii)
        .filter(|line| !line.is_empty());
    let line = lines.next().context("missing request line")?;
    let mut words = line
        .split(u8::is_ascii_whitespace)
        .filter(|word| !word.is_empty());
    let (Some(method), Some(uri), None) = (words.next(), words.next(), words.next()) else {
        bail!("invalid request line");
    };
    let mut req = http::Request::builder().method(method).uri(uri);
    for line in lines {
        let colon = line
            .iter()
            .position(|b| *b == b':')
            .context("invalid request header")?;
        req = req.header(line[..colon].trim_ascii(), line[colon + 1..].trim_ascii());
    }
    let req = req.body(()).context("invalid request")?;
    let len = match req.headers().get(CONTENT_LENGTH) {
        Some(len) => {
            let len = len.to_str().context("invalid content length")?;
            let len = len.parse().context("invalid content length")?;
            Some(len)
        }
        None => None,
    };
    Ok(req.map(|()| RequestBody::new(id, len)))
}

fn respond(id: u32, status: StatusCode, headers: &HeaderMap, body: &[u8]) {
    let mut head = Vec::default();
    for (name, value) in headers {
        head.extend_from_slice(name.as_str().as_bytes());
        head.extend_from_slice(b": ");
        head.extend_from_slice(value.as_bytes());
        head.push(b'\n');
    }
    unsafe {
        ledger_response(
            id,
            status.as_u16().into(),
            head.as_ptr(),
            head.len(),
            body.as_ptr(),
            body.len(),
        );
    }
}

async fn handle(id: u32, head: Vec<u8>) {
    let res = match NODE.get() {
        Some(node) => match parse_request(id, &head) {
            Ok(req) => {
                node.ledger
                    .handle_http(req, || sleep(node.io_timeout))
                    .await
            }
            Err(err) => build_text_response(StatusCode::BAD_REQUEST, format!("{err:#}")),
        },
        None => build_text_response(StatusCode::SERVICE_UNAVAILABLE, "ledger not started"),
    };
    let res =
        res.or_else(|err| build_text_response(StatusCode::INTERNAL_SERVER_ERROR, err.to_string()));
    let (parts, body) = match res {
        Ok(res) => res.into_parts(),
        Err(..) => {
            return respond(
                id,
                StatusCode::INTERNAL_SERVER_ERROR,
                &HeaderMap::default(),
                &[],
            );
        }
    };
    let Ok(body) = body.collect().await;
    respond(id, parts.status, &parts.headers, &body.to_bytes());
}

/// Allocate `len` uninitialized bytes to be written in full and passed to
/// another export, which takes ownership.
#[unsafe(no_mangle)]
pub extern "C" fn ledger_alloc(len: usize) -> *mut u8 {
    init();
    Box::into_raw(Box::<[u8]>::new_uninit_slice(len)).cast()
}

/// Make the contract `wasm` available to the genesis of the ledger, which
/// must not be started yet.
///
/// # Safety
///
/// `wasm` must be the result of `ledger_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ledger_import(wasm: *mut u8, len: usize) -> i32 {
    let wasm = unsafe { take(wasm, len) };
    report(|| {
        if NODE.get().is_some() {
            bail!("ledger already started");
        }
        let digest = Sha256::digest(&wasm).into();
        IMPORTS.with_borrow_mut(|imports| imports.insert(digest, Bytes::from(wasm)));
        Ok(())
    })
}

/// Start the ledger with a JSON-encoded config: the `network`, the
/// hex-encoded `admin` key and optionally the `genesis`, `max_requests`,
/// `fuel`, `io_timeout` in seconds, `cardano_block_height` and
/// `cardano_current_slot`, defaulting to those of `starstream-ledger-node`.
///
/// # Safety
///
/// `config` must be the result of `ledger_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ledger_start(config: *mut u8, len: usize) -> i32 {
    let config = unsafe { take(config, len) };
    report(|| start(&config))
}

/// Queue request `id`, reading its body through the `ledger_body_read` import
/// and passing its response to the `ledger_response` import.
///
/// `head` is the request method and target, separated by whitespace on the
/// first line, followed by a `name: value` line per header. Blank lines are
/// ignored. A malformed `head` is answered with 400.
///
/// # Safety
///
/// `head` must be the result of `ledger_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ledger_handle(id: u32, head: *mut u8, len: usize) {
    let head = unsafe { take(head, len) };
    spawn(id, handle(id, head));
}
