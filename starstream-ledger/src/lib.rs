//! A standalone Starstream ledger server built on `starstream-runtime-next`.
//!
//! [`Ledger::handle_http`] serves the HTTP API specified in
//! `docs/ledger.md`:
//!
//! - `PUT`/`GET /contracts/<digest>` — publish and fetch contracts,
//!   content-addressed by the SHA-256 digest of the component Wasm.
//!   Publishing is a signed `COSE_Sign1` envelope over the CBOR
//!   transaction `[context, network, nonce, wasm]` (see
//!   [`PUBLISH_CONTEXT`]), charged to an [`Account`] identified by the
//!   signer's Ed25519 public key.
//! - `GET`/`POST /contracts/<digest>/rpc` — fetch a contract's
//!   coordination-script ABI as WIT and invoke a coordination script over
//!   wRPC framing, with UTXO imports resolved via [`X_STARSTREAM_UTXO`]
//!   headers; UTXOs the script constructs are snapshotted and persisted
//!   as a transaction.
//! - `GET`/`POST /transactions/<tx>/utxos/<utxo>/rpc` — fetch a persisted
//!   UTXO's ABI as WIT and invoke its methods over wRPC framing.
//!
//! Running contracts observe the ledger's [`CardanoCtx`] through the
//! `starstream:std/cardano` host interface.

use core::fmt::Write as _;
use core::iter::zip;
use core::mem;
use core::net::SocketAddr;
use core::ops::{Deref, DerefMut};

use std::collections::{HashMap, HashSet, hash_map};
use std::sync::Arc;

use anyhow::Context as _;
use bytes::{Buf, Bytes, BytesMut};
use coset::{CborSerializable as _, CoseSign1, TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signature, VerifyingKey};
use futures::{StreamExt as _, TryStreamExt as _};
use headers_accept::Accept;
use headers_core::Header as _;
use hex::ToHex as _;
use http::HeaderValue;
use http::header::{ACCEPT, CONTENT_TYPE};
use http_body_util::BodyExt as _;
use http_body_util::combinators::BoxBody;
use hyper::service::service_fn;
use hyper_util::rt::{TokioExecutor, TokioIo};
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use starstream_runtime_next::{CoordinationScriptExport, UtxoExport};
use tokio::net::TcpSocket;
use tokio::sync::{RwLock, Semaphore, TryAcquireError};
use tokio::task::JoinSet;
use tokio_util::codec::{Encoder as _, FramedRead};
use tokio_util::io::StreamReader;
use tracing::{Instrument as _, debug, error, info, instrument, warn};
use wasmtime::component::{ResourceTable, Type, types, wasm_wave};
use wasmtime_wizer::{WasmtimeWizerComponent, Wizer};

use crate::codec::{ValEncoder, read_value};

pub mod codec;
mod host;

/// The domain-separation context every publish transaction must carry as its
/// first element, binding the signature to this protocol.
pub const PUBLISH_CONTEXT: &str = "starstream:publish";

/// The header mapping a UTXO import instance to the digest of the contract
/// providing it on coordination-script invocation requests
/// (`<instance>=<contract-digest>`, repeatable), and naming the instance of
/// each persisted UTXO — in output order — on the response.
pub const X_STARSTREAM_UTXO: &str = "X-Starstream-Utxo";

const APPLICATION_OCTET_STREAM: MediaType = MediaType::new(
    mediatype::names::APPLICATION,
    mediatype::names::OCTET_STREAM,
);
const APPLICATION_COSE: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::COSE);
const APPLICATION_WASM: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::WASM);
const TEXT_PLAIN_UTF_8: MediaType = MediaType::from_parts(
    mediatype::names::TEXT,
    mediatype::names::PLAIN,
    None,
    &[(mediatype::names::CHARSET, mediatype::values::UTF_8)],
);

/// Representations a published contract can be served as, in order of
/// server preference.
const CONTRACT_MEDIA_TYPES: &[MediaType] = &[APPLICATION_COSE, APPLICATION_WASM];

/// Representations served WIT can take, in order of server preference.
const WIT_MEDIA_TYPES: &[MediaType] = &[TEXT_PLAIN_UTF_8, APPLICATION_WASM];

/// The WIT package the served UTXO ABI worlds are defined in.
const UTXO_WIT_PACKAGE: &str = "starstream:utxo";

/// The WIT package the served coordination-script ABI world is defined in.
const CONTRACT_WIT_PACKAGE: &str = "starstream:contract";

/// The WIT world name the coordination-script ABI of a contract is served
/// under. The scripts themselves are invoked on the root (empty) wRPC
/// instance.
const CONTRACT_WIT_WORLD: &str = "contract";

pub enum Action {
    ContractUpload(Bytes),
    FundAccount { key: Box<str>, amount: u64 },
    Transaction(Arc<Transaction>),
}

pub struct Block {
    pub height: usize,
    pub actions: Box<[Action]>,
    // TODO: Add proofs
}

fn bind_tcp(address: SocketAddr) -> anyhow::Result<TcpSocket> {
    debug!("binding TCP socket");
    let sock = match address {
        SocketAddr::V4(..) => TcpSocket::new_v4(),
        SocketAddr::V6(..) => TcpSocket::new_v6(),
    }
    .context("failed to create HTTP TCP socket")?;
    // Conditionally enable `SO_REUSEADDR` depending on the current
    // platform. On Unix we want this to be able to rebind an address in
    // the `TIME_WAIT` state which can happen then a server is killed with
    // active TCP connections and then restarted. On Windows though if
    // `SO_REUSEADDR` is specified then it enables multiple applications to
    // bind the port at the same time which is not something we want. Hence
    // this is conditionally set based on the platform (and deviates from
    // Tokio's default from always-on).
    sock.set_reuseaddr(!cfg!(windows))?;
    sock.bind(address)
        .with_context(|| format!("failed to bind on `{address}`"))?;
    Ok(sock)
}

fn build_http_response<T, E>(
    code: http::StatusCode,
    body: impl Into<T>,
) -> http::Result<http::Response<BoxBody<T, E>>>
where
    T: Buf + Sync + Send + 'static,
{
    http::Response::builder().status(code).body(
        http_body_util::Full::new(body.into())
            .map_err(|_| unreachable!())
            .boxed(),
    )
}

/// The hash a contract declares for `export` via `implements-method`.
///
/// The Starstream compiler identifies each method by `sha256` of its source
/// name (`snake_case`), split into four little-endian `u64` words. An exported
/// method is named `[method]utxo.plus-chips` in WIT (`kebab-case`); take the
/// trailing segment and undo the `kebab-case` mangling (`-` → `_`) to recover
/// the name the compiler hashed.
fn method_hash(export: &str) -> (u64, u64, u64, u64) {
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

/// Render a component-model [`Type`] as WIT. `wasm_wave`'s `DisplayType` covers
/// the structural types but renders resource handles as `<<UNSUPPORTED>>`; the
/// `utxo` resource is the only one in play here, so spell its handles by name.
fn wit_type(ty: &Type) -> String {
    match ty {
        Type::Own(..) => "utxo".into(),
        Type::Borrow(..) => "borrow<utxo>".into(),
        ty => wasm_wave::wasm::DisplayType(ty).to_string(),
    }
}

/// Render an export as a WIT function declaration: the trailing name segment
/// (the `<name>` of a `[method]utxo.<name>` export), its params — with the
/// leading implicit `self: borrow<utxo>` receiver dropped when `receiver` is
/// set — and results.
fn wit_func(export: &str, ty: &types::ComponentFunc, receiver: bool) -> String {
    let name = export.rsplit('.').next().unwrap_or(export);
    let params = ty
        .params()
        .skip(receiver.into())
        .map(|(n, t)| format!("{n}: {}", wit_type(&t)))
        .collect::<Vec<_>>()
        .join(", ");
    let results: Vec<_> = ty.results().collect();
    let ret = match results.as_slice() {
        [] => String::new(),
        [t] => format!(" -> {}", wit_type(t)),
        types => format!(
            " -> ({})",
            types.iter().map(wit_type).collect::<Vec<_>>().join(", "),
        ),
    };
    format!("{name}: func({params}){ret};")
}

/// The WIT world name the ABI of a UTXO exported as `instance` is served
/// under: the trailing name segment, package and version stripped. The
/// methods themselves are invoked on the root (empty) wRPC instance.
fn utxo_world(instance: &str) -> &str {
    let world = instance
        .split_once('/')
        .map_or(instance, |(_, world)| world);
    world.split_once('@').map_or(world, |(world, ..)| world)
}

/// The Cardano context a contract can observe via the `starstream:std/cardano`
/// host functions.
#[derive(Clone, Copy, Debug, Default)]
pub struct CardanoCtx {
    /// Block height reported to the guest via `cardano#block-height`.
    pub block_height: i64,
    /// Current slot reported to the guest via `cardano#current-slot`.
    pub current_slot: i64,
}

#[derive(Clone, Debug)]
struct UtxoOutput {
    utxo: starstream_runtime_next::Utxo,
    instance: Arc<str>,
    implemented: HashSet<(u64, u64, u64, u64)>,
}

#[derive(Default)]
struct Ctx {
    table: ResourceTable,
    cardano: CardanoCtx,
    implemented: HashSet<(u64, u64, u64, u64)>,
    imports: HashMap<Box<str>, UtxoImport>,
    outputs: Vec<UtxoOutput>,
}

struct Contract {
    contract: starstream_runtime_next::Contract<Ctx>,
    scripts: HashMap<Box<str>, CoordinationScriptExport>,
    wasm: Bytes,
    envelope: Bytes,
}

impl Deref for Contract {
    type Target = starstream_runtime_next::Contract<Ctx>;

    fn deref(&self) -> &Self::Target {
        &self.contract
    }
}

impl DerefMut for Contract {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.contract
    }
}

#[derive(Clone, Debug, Default)]
pub struct Utxo {
    pub contract_digest: Arc<str>,
    pub instance: Arc<str>,
    pub wasm: Bytes,
    pub implemented: HashSet<(u64, u64, u64, u64)>,
    pub storage: Vec<(String, wasmtime::component::Val)>,
}

#[derive(Clone)]
struct UtxoImport {
    contract: starstream_runtime_next::Contract<Ctx>,
    export: UtxoExport,
}

/// The ledger-side state of a publishing account, identified by the
/// lowercase-hex encoding of its Ed25519 public key.
///
/// Publishing charges the balance one unit per byte of Wasm and must carry a
/// nonce strictly greater than `last_nonce`.
#[derive(Clone, Debug, Default)]
pub struct Account {
    /// Account balance.
    pub balance: u64,
    /// Last nonce used by the account.
    pub last_nonce: u64,
}

pub struct Transaction {
    pub coordination_script_digest: Box<str>,
    pub inputs: Box<[(usize, usize)]>,
    pub outputs: Box<[Arc<Utxo>]>,
    // TODO: Add proof
}

/// A ledger node: published contracts, persisted transactions, and the
/// accounts paying for publishes, served over HTTP by [`handle_http`].
///
/// [`handle_http`]: Ledger::handle_http
pub struct Ledger {
    engine: wasmtime::Engine,
    wizer: Wizer,
    blocks: Arc<RwLock<Vec<Block>>>,
    contracts: Arc<RwLock<HashMap<Arc<str>, Contract>>>,
    transactions: Arc<RwLock<Vec<Arc<Transaction>>>>,
    accounts: Arc<RwLock<HashMap<Box<str>, Account>>>,
    cardano: CardanoCtx,
    network: Arc<str>,
    max_requests: u32,
    permits: Arc<Semaphore>,
}

impl Ledger {
    /// Create an empty ledger on `network` with the given genesis `accounts`,
    /// serving at most `max_requests` concurrent HTTP requests and reporting
    /// `cardano` to running contracts.
    pub fn new(
        engine: wasmtime::Engine,
        max_requests: u32,
        cardano: CardanoCtx,
        network: impl Into<Arc<str>>,
        accounts: HashMap<Box<str>, Account>,
    ) -> Self {
        let max_requests = usize::try_from(max_requests)
            .unwrap_or(Semaphore::MAX_PERMITS)
            .min(Semaphore::MAX_PERMITS);
        Self {
            engine,
            wizer: Wizer::new(),
            blocks: Arc::default(),
            contracts: Arc::default(),
            transactions: Arc::default(),
            accounts: Arc::new(RwLock::new(accounts)),
            cardano,
            network: network.into(),
            max_requests: max_requests as _,
            permits: Arc::new(Semaphore::new(max_requests)),
        }
    }

    /// Bind `address` and return the future serving the ledger HTTP API on
    /// it; the future runs the accept loop until dropped.
    ///
    /// The served endpoints are specified in `docs/ledger.md`.
    #[instrument(skip_all)]
    pub async fn handle_http(
        &self,
        address: SocketAddr,
    ) -> anyhow::Result<impl Future<Output = ()> + use<>> {
        let exe = TokioExecutor::new();

        let sock = bind_tcp(address)?;
        let sock = sock
            .listen(self.max_requests)
            .context("failed to listen on TCP socket")?;

        let blocks = Arc::clone(&self.blocks);
        let contracts = Arc::clone(&self.contracts);
        let transactions = Arc::clone(&self.transactions);
        let accounts = Arc::clone(&self.accounts);
        let permits = Arc::clone(&self.permits);
        let engine = self.engine.clone();
        let cardano = self.cardano;
        let network = Arc::clone(&self.network);
        let wizer = self.wizer.clone();
        let svc = service_fn(move |mut req: http::Request<hyper::body::Incoming>| {
            let blocks = Arc::clone(&blocks);
            let contracts = Arc::clone(&contracts);
            let transactions = Arc::clone(&transactions);
            let accounts = Arc::clone(&accounts);
            let permits = Arc::clone(&permits);
            let engine = engine.clone();
            let network = Arc::clone(&network);
            let wizer = wizer.clone();
            async move {
                let _permit = match permits.try_acquire() {
                    Ok(permit) => permit,
                    Err(TryAcquireError::NoPermits) => {
                        return build_http_response(
                            http::StatusCode::SERVICE_UNAVAILABLE,
                            "maximum concurrent request count reached",
                        );
                    }
                    Err(TryAcquireError::Closed) => {
                        return build_http_response(
                            http::StatusCode::INTERNAL_SERVER_ERROR,
                            "semaphore closed",
                        );
                    }
                };
                let Some(pq) = req.uri().path_and_query() else {
                    return build_http_response(http::StatusCode::OK, "");
                };
                let mut path = pq.path().split('/');
                let Some("") = path.next() else {
                    return build_http_response(http::StatusCode::OK, "");
                };
                match (
                    req.method().as_str(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                ) {
                    ("GET", Some("contracts"), Some(digest), None, ..) => {
                        let contracts = contracts.read().await;
                        let Some(Contract { wasm, envelope, .. }) = contracts.get(digest) else {
                            return build_http_response(
                                http::StatusCode::NOT_FOUND,
                                format!("contract `{digest}` not found"),
                            );
                        };
                        if !req.headers().contains_key(ACCEPT) {
                            return http::Response::builder()
                                .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
                                .body(
                                    http_body_util::Full::new(envelope.clone())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                );
                        }
                        let accept = match Accept::decode(&mut req.headers().get_all(ACCEPT).iter())
                        {
                            Ok(accept) => accept,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    err.to_string(),
                                );
                            }
                        };
                        let Some(accept) = accept.negotiate(CONTRACT_MEDIA_TYPES) else {
                            return build_http_response(
                                http::StatusCode::NOT_ACCEPTABLE,
                                format!(
                                    "no acceptable media type, available: `{APPLICATION_COSE}`, `{APPLICATION_WASM}`"
                                ),
                            );
                        };
                        if *accept == APPLICATION_WASM {
                            http::Response::builder()
                                .header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                                .body(
                                    http_body_util::Full::new(wasm.clone())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                )
                        } else {
                            http::Response::builder()
                                .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
                                .body(
                                    http_body_util::Full::new(envelope.clone())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                )
                        }
                    }
                    ("PUT", Some("contracts"), Some(digest), None, ..) => {
                        let mut buf = [0u8; 32];
                        if let Err(err) = hex::decode_to_slice(digest, &mut buf) {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                format!("digest is not valid hex-encoded sha256 digest: {err}"),
                            );
                        };
                        let digest = buf;

                        match req.headers().get(CONTENT_TYPE).map(HeaderValue::to_str) {
                            None => {}
                            Some(Ok(ct)) => match MediaType::parse(ct) {
                                Ok(ct) if ct.essence() == APPLICATION_COSE => {}
                                Ok(ct) => {
                                    return build_http_response(
                                        http::StatusCode::UNSUPPORTED_MEDIA_TYPE,
                                        format!(
                                            "expected `{APPLICATION_COSE}` content-type, got `{ct}`"
                                        ),
                                    );
                                }
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::BAD_REQUEST,
                                        err.to_string(),
                                    );
                                }
                            },
                            Some(Err(err)) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    err.to_string(),
                                );
                            }
                        }
                        // TODO: Check content-length, enforce limit

                        let envelope = match req.into_body().collect().await {
                            Ok(envelope) => envelope.to_bytes(),
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    err.to_string(),
                                );
                            }
                        };
                        let sign1 = match CoseSign1::from_tagged_slice(&envelope)
                            .or_else(|_| CoseSign1::from_slice(&envelope))
                        {
                            Ok(sign1) => sign1,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("body is not a valid COSE_Sign1: {err}"),
                                );
                            }
                        };
                        if sign1.protected.header.alg
                            != Some(coset::Algorithm::Assigned(iana::Algorithm::EdDSA))
                        {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                "protected `alg` header must be EdDSA",
                            );
                        }
                        let Ok(key) =
                            <[u8; 32]>::try_from(sign1.protected.header.key_id.as_slice())
                        else {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                "protected `kid` header must be a raw 32-byte Ed25519 public key",
                            );
                        };
                        let key = match VerifyingKey::from_bytes(&key) {
                            Ok(key) => key,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("`kid` is not a valid Ed25519 public key: {err}"),
                                );
                            }
                        };
                        if let Err(err) = sign1.verify_signature(b"", |signature, data| {
                            Signature::from_slice(signature)
                                .and_then(|signature| key.verify_strict(data, &signature))
                        }) {
                            return build_http_response(
                                http::StatusCode::UNAUTHORIZED,
                                format!("signature verification failed: {err}"),
                            );
                        }
                        let Some(payload) = sign1.payload.as_deref() else {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                "COSE_Sign1 payload missing",
                            );
                        };
                        let tx = match ciborium::from_reader(payload) {
                            Ok(ciborium::Value::Array(tx)) => tx,
                            Ok(..) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    "publish transaction must be a `[context, network, nonce, wasm]` array",
                                );
                            }
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("COSE_Sign1 payload is not valid CBOR: {err}"),
                                );
                            }
                        };
                        let (context, tx_network, nonce, wasm) =
                            match <[ciborium::Value; 4]>::try_from(tx) {
                                Ok(
                                    [
                                        ciborium::Value::Text(context),
                                        ciborium::Value::Text(network),
                                        ciborium::Value::Integer(nonce),
                                        ciborium::Value::Bytes(wasm),
                                    ],
                                ) => (context, network, nonce, wasm),
                                _ => {
                                    return build_http_response(
                                        http::StatusCode::BAD_REQUEST,
                                        "publish transaction must be a `[context, network, nonce, wasm]` array",
                                    );
                                }
                            };
                        if context != PUBLISH_CONTEXT {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                format!(
                                    "unexpected context `{context}`, expected `{PUBLISH_CONTEXT}`"
                                ),
                            );
                        }
                        if tx_network != *network {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                format!("unexpected network `{tx_network}`, expected `{network}`"),
                            );
                        }
                        let Ok(nonce) = u64::try_from(nonce) else {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                "publish transaction nonce does not fit in u64",
                            );
                        };
                        let wasm_digest = Sha256::digest(&wasm);
                        if wasm_digest != digest.into() {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                format!("digest mismatch, got: `{}`", hex::encode(wasm_digest)),
                            );
                        }

                        let account_id = hex::encode(&sign1.protected.header.key_id);
                        let mut accounts = accounts.write().await;
                        let Some(account) = accounts.get_mut(account_id.as_str()) else {
                            return build_http_response(
                                http::StatusCode::FORBIDDEN,
                                format!("account ID `{account_id}` not found"),
                            );
                        };
                        if nonce <= account.last_nonce {
                            return build_http_response(
                                http::StatusCode::CONFLICT,
                                format!(
                                    "nonce must be higher than {}, got {nonce}",
                                    account.last_nonce
                                ),
                            );
                        }
                        let Some(balance) = account.balance.checked_sub(wasm.len() as _) else {
                            return build_http_response(
                                http::StatusCode::PAYMENT_REQUIRED,
                                format!(
                                    "balance insufficient, required at least {}, available {}",
                                    wasm.len(),
                                    account.balance,
                                ),
                            );
                        };

                        let mut contracts = contracts.write().await;
                        let digest: Box<str> = digest.encode_hex();
                        let hash_map::Entry::Vacant(entry) = contracts.entry(digest.into()) else {
                            return build_http_response(http::StatusCode::OK, "");
                        };

                        // TODO: Split component
                        let contract = match starstream_runtime_next::Contract::new(&engine, &wasm)
                        {
                            Ok(contract) => contract,
                            Err(err) => {
                                // TODO: Handle error type and set status accordingly
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        };
                        let mut scripts = HashMap::default();
                        for (name, export) in contract.coordination_scripts() {
                            match export {
                                Ok(export) => scripts.insert(name.into(), export),
                                Err(err) => {
                                    // TODO: Handle error type and set status accordingly
                                    return build_http_response(
                                        http::StatusCode::BAD_REQUEST,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                        }
                        entry.insert(Contract {
                            contract,
                            wasm: wasm.into(),
                            scripts,
                            envelope: envelope.clone(),
                        });
                        {
                            let mut blocks = blocks.write().await;
                            let height = blocks.len().saturating_add(1);
                            blocks.push(Block {
                                actions: [Action::ContractUpload(envelope)].into(),
                                height,
                            });
                        }
                        account.balance = balance;
                        account.last_nonce = nonce;
                        build_http_response(http::StatusCode::OK, "")
                    }
                    (method, Some("contracts"), Some(..), None, ..) => build_http_response(
                        http::StatusCode::METHOD_NOT_ALLOWED,
                        format!("method `{method}` not allowed for path `{}`", pq.path()),
                    ),

                    ("GET", Some("contracts"), Some(digest), Some("rpc"), None, ..) => {
                        let accept = if req.headers().contains_key(ACCEPT) {
                            let accept =
                                match Accept::decode(&mut req.headers().get_all(ACCEPT).iter()) {
                                    Ok(accept) => accept,
                                    Err(err) => {
                                        return build_http_response(
                                            http::StatusCode::BAD_REQUEST,
                                            err.to_string(),
                                        );
                                    }
                                };
                            let Some(accept) = accept.negotiate(WIT_MEDIA_TYPES) else {
                                return build_http_response(
                                    http::StatusCode::NOT_ACCEPTABLE,
                                    format!(
                                        "no acceptable media type, available: `{TEXT_PLAIN_UTF_8}`, `{APPLICATION_WASM}`"
                                    ),
                                );
                            };
                            accept
                        } else {
                            &TEXT_PLAIN_UTF_8
                        };

                        let contracts = contracts.read().await;
                        let Some(Contract { contract, .. }) = contracts.get(digest) else {
                            return build_http_response(
                                http::StatusCode::NOT_FOUND,
                                format!("contract `{digest}` not found"),
                            );
                        };

                        let mut wit = String::new();
                        writeln!(wit, "package {CONTRACT_WIT_PACKAGE};\n").unwrap();
                        writeln!(wit, "world {CONTRACT_WIT_WORLD} {{").unwrap();
                        for (name, script) in contract.coordination_scripts() {
                            let script = match script {
                                Ok(script) => script,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                            writeln!(wit, "    import {}", wit_func(name, script.ty(), false))
                                .unwrap();
                        }
                        writeln!(wit, "}}").unwrap();

                        if *accept == APPLICATION_WASM {
                            let mut resolve = wit_parser::Resolve::new();
                            let pkg =
                                match resolve.push_str(format!("{CONTRACT_WIT_WORLD}.wit"), &wit) {
                                    Ok(pkg) => pkg,
                                    Err(err) => {
                                        return build_http_response(
                                            http::StatusCode::INTERNAL_SERVER_ERROR,
                                            format!("failed to parse the rendered WIT: {err:?}"),
                                        );
                                    }
                                };
                            let wasm = match wit_component::encode(&resolve, pkg) {
                                Ok(wasm) => wasm,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("failed to encode the WIT as Wasm: {err:?}"),
                                    );
                                }
                            };
                            http::Response::builder()
                                .header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                                .body(
                                    http_body_util::Full::new(wasm.into())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                )
                        } else {
                            http::Response::builder()
                                .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
                                .body(
                                    http_body_util::Full::new(wit.into())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                )
                        }
                    }
                    ("POST", Some("contracts"), Some(digest), Some("rpc"), None, ..) => {
                        let digest: Box<str> = digest.into();

                        let utxo_import_headers = match req.headers_mut().entry(X_STARSTREAM_UTXO) {
                            http::header::Entry::Occupied(imports) => {
                                let (_, imports) = imports.remove_entry_mult();
                                imports.collect()
                            }
                            http::header::Entry::Vacant(..) => Vec::default(),
                        };

                        let mut utxo_imports: HashMap<Box<str>, Box<str>> =
                            HashMap::with_capacity(utxo_import_headers.len());
                        for h in utxo_import_headers {
                            match h.to_str() {
                                Ok(s) => {
                                    let Some((instance, contract_digest)) = s.split_once('=')
                                    else {
                                        return build_http_response(
                                            http::StatusCode::BAD_REQUEST,
                                            format!(
                                                "`{X_STARSTREAM_UTXO}` header value `{s}` is not valid"
                                            ),
                                        );
                                    };
                                    utxo_imports.insert(instance.into(), contract_digest.into());
                                }
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::BAD_REQUEST,
                                        format!(
                                            "`{X_STARSTREAM_UTXO}` header value `{h:?}` is not valid utf-8: {err}"
                                        ),
                                    );
                                }
                            }
                        }

                        let mut body = wrpc_http::data_reader_from_incoming(req.into_body());
                        let name = match wrpc_transport::frame::Header::read(&mut body).await {
                            Ok(wrpc_transport::frame::Header { instance, name })
                                if instance.is_empty() =>
                            {
                                name
                            }
                            Ok(wrpc_transport::frame::Header { instance, .. }) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("expected instance to be empty, got `{instance}`"),
                                );
                            }
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err}"),
                                );
                            }
                        };

                        let (contract, export, utxo_imports) = {
                            let contracts = contracts.read().await;
                            let Some(Contract {
                                contract, scripts, ..
                            }) = contracts.get(digest.as_ref())
                            else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("contract `{digest}` not found"),
                                );
                            };
                            let Some(export) = scripts.get(name.as_str()) else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("coordination script `{name}` not found"),
                                );
                            };

                            let mut imports = HashMap::with_capacity(utxo_imports.len());
                            for (instance, contract_digest) in utxo_imports {
                                let Some((contract_digest, Contract { wasm, .. })) =
                                    contracts.get_key_value(contract_digest.as_ref())
                                else {
                                    return build_http_response(
                                        http::StatusCode::NOT_FOUND,
                                        format!(
                                            "UTXO import contract `{digest}` not found for instance `{instance}`"
                                        ),
                                    );
                                };
                                imports
                                    .insert(instance, (Arc::clone(contract_digest), wasm.clone()));
                            }
                            (contract.clone(), export.clone(), imports)
                        };

                        let mut instrumented = HashMap::with_capacity(utxo_imports.len());
                        for (_, wasm) in utxo_imports.values() {
                            if let hash_map::Entry::Vacant(entry) = instrumented.entry(wasm.clone())
                            {
                                let (cx, wasm) = match wizer.instrument_component(wasm) {
                                    Ok((cx, wasm)) => (cx, wasm),
                                    Err(err) => {
                                        return build_http_response(
                                            http::StatusCode::BAD_REQUEST,
                                            format!("failed to instrument component: {err}"),
                                        );
                                    }
                                };
                                let contract =
                                    match starstream_runtime_next::Contract::new(&engine, &wasm) {
                                        Ok(contract) => contract,
                                        Err(err) => {
                                            // TODO: Handle error type and set status accordingly
                                            return build_http_response(
                                                http::StatusCode::BAD_REQUEST,
                                                format!("{err:?}"),
                                            );
                                        }
                                    };
                                entry.insert((cx, contract));
                            };
                        }

                        let mut imports = HashMap::with_capacity(utxo_imports.len());
                        for (instance, (_, wasm)) in &utxo_imports {
                            let (_, contract) = &instrumented[wasm];
                            let export = match contract.get_utxo(instance) {
                                Ok(export) => export,
                                Err(err) => {
                                    // TODO: Handle error type and set status accordingly
                                    return build_http_response(
                                        http::StatusCode::BAD_REQUEST,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                            imports.insert(
                                instance.clone(),
                                UtxoImport {
                                    contract: contract.clone(),
                                    export,
                                },
                            );
                        }

                        let mut store = wasmtime::Store::new(
                            &engine,
                            Ctx {
                                cardano,
                                imports,
                                ..Ctx::default()
                            },
                        );

                        let param_tys = export.ty().params();
                        let mut params =
                            vec![wasmtime::component::Val::Bool(false); param_tys.len()];
                        let body = FramedRead::new(body, wrpc_transport::FrameDecoder::default())
                            .map(|frame| {
                                let wrpc_transport::Frame { path, data } = frame?;
                                anyhow::ensure!(path.is_empty(), "async values not supported");
                                Ok(data)
                            });
                        let mut body = StreamReader::new(body.map_err(std::io::Error::other));
                        for (v, (_, ty)) in zip(&mut params, param_tys) {
                            if let Err(err) = read_value(&mut body, v, &ty).await {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        }

                        let result_tys = export.ty().results();
                        let mut results =
                            vec![wasmtime::component::Val::Bool(false); result_tys.len()];
                        if let Err(err) = contract
                            .call_coordination_script(&mut store, &export, &params, &mut results)
                            .await
                        {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                format!("{err:?}"),
                            );
                        }

                        let outputs = mem::take(&mut store.data_mut().outputs);
                        let mut utxos = Vec::with_capacity(outputs.len());
                        let mut res = http::Response::builder();
                        let headers = res.headers_mut().expect("failed to get header map");
                        for UtxoOutput {
                            utxo,
                            instance,
                            implemented,
                        } in outputs
                        {
                            let (contract_digest, wasm) = &utxo_imports[instance.as_ref()];
                            let (cx, contract) = &instrumented[wasm];
                            let Some(storage) = contract
                                .get_utxo(&instance)
                                .expect("missing UTXO instance")
                                .storage()
                                .cloned()
                            else {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    "UTXO does not export storage",
                                );
                            };
                            let storage = match utxo.storage(&storage).get(&mut store).await {
                                Ok(wasm) => wasm,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                            if let Err(err) = utxo.drop(&mut store).await {
                                return build_http_response(
                                    http::StatusCode::INTERNAL_SERVER_ERROR,
                                    format!("failed to drop UTXO resource: {err:?}"),
                                );
                            }
                            let wasm = match wizer
                                .snapshot_component(
                                    cx,
                                    &mut WasmtimeWizerComponent {
                                        store: &mut store,
                                        instance: utxo.instance(),
                                    },
                                )
                                .await
                            {
                                Ok(wasm) => wasm,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                            match http::HeaderValue::from_str(instance.as_ref()) {
                                Ok(instance) => headers.append(X_STARSTREAM_UTXO, instance),
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                            utxos.push(Arc::new(Utxo {
                                contract_digest: Arc::clone(contract_digest),
                                instance,
                                wasm: wasm.into(),
                                implemented,
                                storage,
                            }));
                        }
                        let tx = Arc::new(Transaction {
                            coordination_script_digest: digest,
                            inputs: Box::default(), // TODO: Add input UTXO support
                            outputs: utxos.into(),
                        });
                        {
                            let mut transactions = transactions.write().await;
                            transactions.push(Arc::clone(&tx));

                            let mut blocks = blocks.write().await;
                            let height = blocks.len().saturating_add(1);
                            blocks.push(Block {
                                actions: [Action::Transaction(tx)].into(),
                                height,
                            });
                        }

                        let mut data = BytesMut::with_capacity(result_tys.len());
                        for (v, ty) in zip(results, result_tys) {
                            if let Err(err) = ValEncoder::new(&ty).encode(&v, &mut data) {
                                return build_http_response(
                                    http::StatusCode::INTERNAL_SERVER_ERROR,
                                    format!("{err:?}"),
                                );
                            };
                        }
                        let mut buf = BytesMut::with_capacity(data.len().saturating_add(1 + 10));
                        if let Err(err) = wrpc_transport::FrameEncoder.encode(
                            wrpc_transport::FrameRef {
                                path: &[],
                                data: &data,
                            },
                            &mut buf,
                        ) {
                            return build_http_response(
                                http::StatusCode::INTERNAL_SERVER_ERROR,
                                format!("{err:?}"),
                            );
                        };
                        res.header(CONTENT_TYPE, APPLICATION_OCTET_STREAM.to_string())
                            .body(
                                http_body_util::Full::new(buf.into())
                                    .map_err(|_| unreachable!())
                                    .boxed(),
                            )
                    }
                    (method, Some("contracts"), Some(..), Some("rpc"), None, ..) => {
                        build_http_response(
                            http::StatusCode::METHOD_NOT_ALLOWED,
                            format!("method `{method}` not allowed for path `{}`", pq.path()),
                        )
                    }

                    (
                        "GET",
                        Some("transactions"),
                        Some(tx),
                        Some("utxos"),
                        Some(utxo),
                        Some("rpc"),
                        None,
                        ..,
                    ) => {
                        let tx: usize = match tx.parse() {
                            Ok(tx) => tx,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err}"),
                                );
                            }
                        };
                        let utxo: usize = match utxo.parse() {
                            Ok(utxo) => utxo,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err}"),
                                );
                            }
                        };

                        let accept = if req.headers().contains_key(ACCEPT) {
                            let accept =
                                match Accept::decode(&mut req.headers().get_all(ACCEPT).iter()) {
                                    Ok(accept) => accept,
                                    Err(err) => {
                                        return build_http_response(
                                            http::StatusCode::BAD_REQUEST,
                                            err.to_string(),
                                        );
                                    }
                                };
                            let Some(accept) = accept.negotiate(WIT_MEDIA_TYPES) else {
                                return build_http_response(
                                    http::StatusCode::NOT_ACCEPTABLE,
                                    format!(
                                        "no acceptable media type, available: `{TEXT_PLAIN_UTF_8}`, `{APPLICATION_WASM}`"
                                    ),
                                );
                            };
                            accept
                        } else {
                            &TEXT_PLAIN_UTF_8
                        };

                        let utxo = {
                            let transactions = transactions.read().await;
                            let Some(tx) = transactions.get(tx) else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("transaction `{tx}` not found"),
                                );
                            };
                            let Some(utxo) = tx.outputs.get(utxo) else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("UTXO `{utxo}` not found"),
                                );
                            };
                            Arc::clone(utxo)
                        };

                        let contract = match starstream_runtime_next::Contract::<Ctx>::new(
                            &engine, &utxo.wasm,
                        ) {
                            Ok(contract) => contract,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::INTERNAL_SERVER_ERROR,
                                    format!("{err:?}"),
                                );
                            }
                        };
                        let export = match contract.get_utxo(&utxo.instance) {
                            Ok(export) => export,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::INTERNAL_SERVER_ERROR,
                                    format!("{err:?}"),
                                );
                            }
                        };

                        let world = utxo_world(&utxo.instance);
                        let mut wit = String::new();
                        writeln!(wit, "package {UTXO_WIT_PACKAGE};\n").unwrap();
                        writeln!(wit, "world {world} {{").unwrap();
                        for (name, method) in contract.utxo_methods(&export) {
                            let method = match method {
                                Ok(method) => method,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("{err:?}"),
                                    );
                                }
                            };
                            if utxo.implemented.contains(&method_hash(name)) {
                                writeln!(wit, "    import {}", wit_func(name, method.ty(), true))
                                    .unwrap();
                            }
                        }
                        writeln!(wit, "}}").unwrap();

                        if *accept == APPLICATION_WASM {
                            let mut resolve = wit_parser::Resolve::new();
                            let pkg = match resolve.push_str(format!("{world}.wit"), &wit) {
                                Ok(pkg) => pkg,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("failed to parse the rendered WIT: {err:?}"),
                                    );
                                }
                            };
                            let wasm = match wit_component::encode(&resolve, pkg) {
                                Ok(wasm) => wasm,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::INTERNAL_SERVER_ERROR,
                                        format!("failed to encode the WIT as Wasm: {err:?}"),
                                    );
                                }
                            };
                            http::Response::builder()
                                .header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                                .body(
                                    http_body_util::Full::new(wasm.into())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                )
                        } else {
                            http::Response::builder()
                                .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
                                .body(
                                    http_body_util::Full::new(wit.into())
                                        .map_err(|_| unreachable!())
                                        .boxed(),
                                )
                        }
                    }
                    (
                        "POST",
                        Some("transactions"),
                        Some(tx),
                        Some("utxos"),
                        Some(utxo),
                        Some("rpc"),
                        None,
                        ..,
                    ) => {
                        let tx: usize = match tx.parse() {
                            Ok(tx) => tx,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err}"),
                                );
                            }
                        };
                        let utxo: usize = match utxo.parse() {
                            Ok(utxo) => utxo,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err}"),
                                );
                            }
                        };
                        let mut body = wrpc_http::data_reader_from_incoming(req.into_body());
                        let name = match wrpc_transport::frame::Header::read(&mut body).await {
                            Ok(wrpc_transport::frame::Header { instance, name })
                                if instance.is_empty() =>
                            {
                                name
                            }
                            Ok(wrpc_transport::frame::Header { instance, .. }) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("expected instance to be empty, got `{instance}`"),
                                );
                            }
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err}"),
                                );
                            }
                        };

                        let utxo = {
                            let transactions = transactions.read().await;
                            let Some(tx) = transactions.get(tx) else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("transaction `{tx}` not found"),
                                );
                            };
                            let Some(utxo) = tx.outputs.get(utxo) else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("UTXO `{utxo}` not found"),
                                );
                            };
                            Arc::clone(utxo)
                        };

                        let wasm = {
                            let contracts = contracts.read().await;
                            let Some(Contract { wasm, .. }) = contracts.get(&utxo.contract_digest)
                            else {
                                return build_http_response(
                                    http::StatusCode::NOT_FOUND,
                                    format!("contract `{}` not found", utxo.contract_digest),
                                );
                            };
                            // TODO: Merge UTXO state Wasm with Wasm of the contract
                            _ = wasm;
                            utxo.wasm.clone()
                        };

                        let contract = match starstream_runtime_next::Contract::new(&engine, &wasm)
                        {
                            Ok(contract) => contract,
                            Err(err) => {
                                // TODO: Handle error type and set status accordingly
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        };
                        let utxo_export = match contract.get_utxo(&utxo.instance) {
                            Ok(export) => export,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        };
                        let method_export = match contract
                            .get_utxo_method(&utxo_export, &format!("[method]utxo.{name}"))
                        {
                            Ok(export) => export,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        };
                        let Some(storage_export) = utxo_export.storage() else {
                            return build_http_response(
                                http::StatusCode::BAD_REQUEST,
                                "UTXO does not export storage",
                            );
                        };
                        let mut store = wasmtime::Store::new(
                            &engine,
                            Ctx {
                                cardano,
                                implemented: utxo.implemented.clone(),
                                ..Ctx::default()
                            },
                        );
                        let utxo = match contract
                            .load_utxo(&mut store, storage_export, utxo.storage.clone())
                            .await
                        {
                            Ok(utxo) => utxo,
                            Err(err) => {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        };

                        let param_tys = method_export.ty().params();
                        let mut params =
                            vec![wasmtime::component::Val::Bool(false); param_tys.len()];
                        params[0] = wasmtime::component::Val::Resource(utxo.resource());
                        let body = FramedRead::new(body, wrpc_transport::FrameDecoder::default())
                            .map(|frame| {
                                let wrpc_transport::Frame { path, data } = frame?;
                                anyhow::ensure!(path.is_empty(), "async values not supported");
                                Ok(data)
                            });
                        let mut body = StreamReader::new(body.map_err(std::io::Error::other));
                        for (v, (_, ty)) in zip(&mut params[1..], param_tys.skip(1)) {
                            if let Err(err) = read_value(&mut body, v, &ty).await {
                                return build_http_response(
                                    http::StatusCode::BAD_REQUEST,
                                    format!("{err:?}"),
                                );
                            }
                        }

                        let result_tys = method_export.ty().results();
                        let mut results =
                            vec![wasmtime::component::Val::Bool(false); result_tys.len()];
                        if let Err(err) = utxo
                            .call(&mut store, &method_export, params, &mut results)
                            .await
                        {
                            return build_http_response(
                                http::StatusCode::INTERNAL_SERVER_ERROR,
                                format!("{err:?}"),
                            );
                        };
                        let mut data = BytesMut::with_capacity(result_tys.len());
                        for (v, ty) in zip(results, result_tys) {
                            if let Err(err) = ValEncoder::new(&ty).encode(&v, &mut data) {
                                return build_http_response(
                                    http::StatusCode::INTERNAL_SERVER_ERROR,
                                    format!("{err:?}"),
                                );
                            };
                        }
                        let mut buf = BytesMut::with_capacity(data.len().saturating_add(1 + 10));
                        if let Err(err) = wrpc_transport::FrameEncoder.encode(
                            wrpc_transport::FrameRef {
                                path: &[],
                                data: &data,
                            },
                            &mut buf,
                        ) {
                            return build_http_response(
                                http::StatusCode::INTERNAL_SERVER_ERROR,
                                format!("{err:?}"),
                            );
                        };
                        http::Response::builder()
                            .header(CONTENT_TYPE, APPLICATION_OCTET_STREAM.to_string())
                            .body(
                                http_body_util::Full::new(buf.into())
                                    .map_err(|_| unreachable!())
                                    .boxed(),
                            )
                    }
                    (
                        method,
                        Some("transactions"),
                        Some(..),
                        Some("utxos"),
                        Some(..),
                        Some("rpc"),
                        None,
                        ..,
                    ) => build_http_response(
                        http::StatusCode::METHOD_NOT_ALLOWED,
                        format!("method `{method}` not allowed for path `{}`", pq.path()),
                    ),

                    _ => build_http_response(
                        http::StatusCode::NOT_FOUND,
                        format!("path `{}` not found", pq.path()),
                    ),
                }
            }
        });
        Ok(async move {
            let mut tasks = JoinSet::new();
            // TODO: check for shutdown, gracefully shutdown HTTP
            // TODO: join conn tasks
            loop {
                while let Some(res) = tasks.try_join_next() {
                    if let Err(err) = res {
                        error!(?err, "HTTP task panicked");
                    }
                }
                let stream = match sock.accept().await {
                    Ok((stream, addr)) => {
                        info!(?addr, "accepted TCP connection");
                        stream
                    }
                    Err(err) => {
                        error!(?err, "failed to accept TCP connection");
                        continue;
                    }
                };
                let exe = exe.clone();
                let svc = svc.clone();
                tasks.spawn(
                    async move {
                        let srv = hyper_util::server::conn::auto::Builder::new(exe);
                        let conn = srv.serve_connection(TokioIo::new(stream), svc);
                        if let Err(err) = conn.await {
                            warn!(?err, "failed to serve HTTP connection");
                        }
                    }
                    .in_current_span(),
                );
            }
        }
        .in_current_span())
    }
}
