use core::iter::zip;
use core::mem;
use core::net::SocketAddr;

use std::collections::{HashMap, hash_map};
use std::sync::Arc;

use anyhow::Context as _;
use bytes::{Buf, Bytes, BytesMut};
use coset::{CborSerializable as _, CoseSign1, TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signature, VerifyingKey};
use futures::{StreamExt as _, TryStreamExt as _};
use headers_accept::Accept;
use headers_core::Header as _;
use http::HeaderValue;
use http::header::{ACCEPT, CONTENT_TYPE};
use http_body_util::BodyExt as _;
use hyper::service::service_fn;
use hyper_util::rt::{TokioExecutor, TokioIo};
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use thiserror::Error;
use tokio::io::AsyncRead;
use tokio::net::TcpSocket;
use tokio::sync::{RwLock, Semaphore, TryAcquireError};
use tokio::task::JoinSet;
use tokio_util::codec::{Encoder as _, FramedRead};
use tokio_util::io::StreamReader;
use tracing::{Instrument as _, debug, error, info, instrument, warn};
use wasmtime::component::{Type, types, wasm_wave};
use wasmtime_wizer::{WasmtimeWizerComponent, Wizer};

use crate::codec::{ValEncoder, read_value};
use crate::{
    Account, Action, Block, CardanoCtx, Contract, Ctx, DigestParseError, PUBLISH_CONTEXT,
    Transaction, Utxo, UtxoImport, UtxoOutput, X_STARSTREAM_BLOCK, X_STARSTREAM_TRANSACTION,
    X_STARSTREAM_UTXO, encode_digest, parse_digest,
};

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

fn wit_func(name: &str, ty: &types::ComponentFunc, receiver: bool) -> String {
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

#[derive(Debug, Error)]
enum CoordinationScriptInvocationError {
    #[error("failed to parse contract digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("`{X_STARSTREAM_UTXO}` header value is not a valid string: {0}")]
    UtxoHeaderToStr(http::header::ToStrError),
    #[error("`{X_STARSTREAM_UTXO}` header value format is not valid")]
    UtxoHeaderFormat,
    #[error("failed to parse `{X_STARSTREAM_UTXO}` header digest: {0:#}")]
    UtxoHeaderParsing(DigestParseError),
    #[error("contract not found")]
    ContractNotFound,
    #[error("imported contract not found for instance `{0}`")]
    ImportNotFound(Box<str>),
    #[error("function export not found")]
    FunctionExportNotFound,
    #[error("failed to get UTXO export for instance `{0}`: {1:#}")]
    UtxoInstanceExport(Box<str>, wasmtime::Error),
    #[error("UTXO storage export not found for instance `{0}`")]
    UtxoStorageExportNotFound(Arc<str>),
    #[error("instrumentation failed: {0:#}")]
    Wizer(wasmtime::Error),
    #[error("runtime failed: {0:#}")]
    Runtime(wasmtime::Error),
    #[error("failed to decode parameters: {0}")]
    ParameterDecoding(std::io::Error),
    #[error("failed to encode results: {0:#}")]
    ResultEncoding(wasmtime::Error),
    #[error("failed to encode frame: {0}")]
    FrameEncoding(std::io::Error),
    #[error(transparent)]
    Http(http::Error),
}

impl CoordinationScriptInvocationError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..)
            | Self::UtxoHeaderToStr(..)
            | Self::UtxoHeaderFormat
            | Self::UtxoHeaderParsing(..)
            | Self::UtxoInstanceExport(..)
            | Self::UtxoStorageExportNotFound(..)
            | Self::ParameterDecoding(..) => http::StatusCode::BAD_REQUEST,
            Self::ContractNotFound | Self::ImportNotFound(..) | Self::FunctionExportNotFound => {
                http::StatusCode::NOT_FOUND
            }
            Self::Wizer(..)
            | Self::Runtime(..)
            | Self::ResultEncoding(..)
            | Self::FrameEncoding(..)
            | Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
enum UtxoMethodInvocationError {
    #[error("failed to parse UTXO digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("UTXO not found")]
    UtxoNotFound,
    #[error("contract not found")]
    ContractNotFound,
    #[error("method export not found")]
    MethodExportNotFound,
    #[error("failed to get UTXO export for instance `{0}`: {1:#}")]
    UtxoInstanceExport(Arc<str>, wasmtime::Error),
    #[error("UTXO storage export not found for instance `{0}`")]
    UtxoStorageExportNotFound(Arc<str>),
    #[error("runtime failed: {0:#}")]
    Runtime(wasmtime::Error),
    #[error("failed to decode parameters: {0}")]
    ParameterDecoding(std::io::Error),
    #[error("failed to encode results: {0:#}")]
    ResultEncoding(wasmtime::Error),
    #[error("failed to encode frame: {0}")]
    FrameEncoding(std::io::Error),
    #[error(transparent)]
    Http(http::Error),
}

impl UtxoMethodInvocationError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::UtxoNotFound | Self::ContractNotFound | Self::MethodExportNotFound => {
                http::StatusCode::NOT_FOUND
            }
            Self::DigestParsing(..) | Self::ParameterDecoding(..) => http::StatusCode::BAD_REQUEST,
            Self::UtxoInstanceExport(..)
            | Self::UtxoStorageExportNotFound(..)
            | Self::Runtime(..)
            | Self::ResultEncoding(..)
            | Self::FrameEncoding(..)
            | Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
enum ContractGetError {
    #[error("failed to parse contract digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("contract not found")]
    ContractNotFound,
    #[error(transparent)]
    AcceptHeader(AcceptHeaderError),
    #[error(transparent)]
    Http(http::Error),
}

impl ContractGetError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..) => http::StatusCode::BAD_REQUEST,
            Self::ContractNotFound => http::StatusCode::NOT_FOUND,
            Self::AcceptHeader(err) => err.http_status_code(),
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
enum ContractPutError {
    #[error(transparent)]
    DigestParsing(DigestParseError),
    #[error(transparent)]
    ContentTypeToStr(http::header::ToStrError),
    #[error(transparent)]
    ContentTypeParsing(mediatype::MediaTypeError),
    #[error("expected `{APPLICATION_COSE}` content-type, got `{0}`")]
    UnsupportedContentType(Box<str>),
    #[error(transparent)]
    Body(hyper::Error),
    #[error("body is not a valid COSE_Sign1: {0}")]
    CoseSign1Parsing(coset::CoseError),
    #[error("protected `alg` header must be EdDSA")]
    Algorithm,
    #[error("protected `kid` header must be a raw 32-byte Ed25519 public key")]
    KeyIdFormat,
    #[error("`kid` is not a valid Ed25519 public key: {0}")]
    Key(ed25519_dalek::SignatureError),
    #[error("signature verification failed: {0}")]
    SignatureVerification(ed25519_dalek::SignatureError),
    #[error("COSE_Sign1 payload missing")]
    PayloadMissing,
    #[error("COSE_Sign1 payload is not valid CBOR: {0}")]
    PayloadParsing(ciborium::de::Error<std::io::Error>),
    #[error("publish transaction must be a `[context, network, nonce, wasm]` array")]
    TransactionFormat,
    #[error("unexpected context `{0}`, expected `{PUBLISH_CONTEXT}`")]
    Context(Box<str>),
    #[error("unexpected network `{got}`, expected `{expected}`")]
    Network { got: Box<str>, expected: Arc<str> },
    #[error("publish transaction nonce does not fit in u64")]
    NonceOverflow,
    #[error("digest mismatch, got: `{}`", encode_digest(.0))]
    DigestMismatch([u8; 32]),
    #[error("account ID `{0}` not found")]
    AccountNotFound(Box<str>),
    #[error("nonce must be higher than {last_nonce}, got {nonce}")]
    NonceTooLow { last_nonce: u64, nonce: u64 },
    #[error("balance insufficient, required at least {required}, available {available}")]
    InsufficientBalance { required: u64, available: u64 },
    #[error("{0:?}")]
    Runtime(wasmtime::Error),
    #[error(transparent)]
    Http(http::Error),
}

impl ContractPutError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..)
            | Self::ContentTypeToStr(..)
            | Self::ContentTypeParsing(..)
            | Self::Body(..)
            | Self::CoseSign1Parsing(..)
            | Self::Algorithm
            | Self::KeyIdFormat
            | Self::Key(..)
            | Self::PayloadMissing
            | Self::PayloadParsing(..)
            | Self::TransactionFormat
            | Self::Context(..)
            | Self::Network { .. }
            | Self::NonceOverflow
            | Self::DigestMismatch(..)
            | Self::Runtime(..) => http::StatusCode::BAD_REQUEST,
            Self::UnsupportedContentType(..) => http::StatusCode::UNSUPPORTED_MEDIA_TYPE,
            Self::SignatureVerification(..) => http::StatusCode::UNAUTHORIZED,
            Self::AccountNotFound(..) => http::StatusCode::FORBIDDEN,
            Self::NonceTooLow { .. } => http::StatusCode::CONFLICT,
            Self::InsufficientBalance { .. } => http::StatusCode::PAYMENT_REQUIRED,
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
enum ContractRpcGetError {
    #[error("failed to parse contract digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("contract not found")]
    ContractNotFound,
    #[error(transparent)]
    AcceptHeader(AcceptHeaderError),
    #[error("runtime failed: {0:#}")]
    Runtime(wasmtime::Error),
    #[error("failed to generate Wasm: {0:#}")]
    Wasm(anyhow::Error),
    #[error(transparent)]
    Http(http::Error),
}

impl ContractRpcGetError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..) => http::StatusCode::BAD_REQUEST,
            Self::ContractNotFound => http::StatusCode::NOT_FOUND,
            Self::AcceptHeader(err) => err.http_status_code(),
            Self::Runtime(..) | Self::Wasm(..) | Self::Http(..) => {
                http::StatusCode::INTERNAL_SERVER_ERROR
            }
        }
    }
}

#[derive(Debug, Error)]
enum UtxoRpcGetError {
    #[error("failed to parse UTXO digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("UTXO not found")]
    UtxoNotFound,
    #[error("contract not found")]
    ContractNotFound,
    #[error("UTXO export not found for instance `{0}`")]
    UtxoInstanceExportNotFound(Arc<str>),
    #[error(transparent)]
    AcceptHeader(AcceptHeaderError),
    #[error("runtime failed: {0:#}")]
    Runtime(wasmtime::Error),
    #[error("failed to generate Wasm: {0:#}")]
    Wasm(anyhow::Error),
    #[error(transparent)]
    Http(http::Error),
}

impl UtxoRpcGetError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..) => http::StatusCode::BAD_REQUEST,
            Self::UtxoNotFound | Self::ContractNotFound => http::StatusCode::NOT_FOUND,
            Self::AcceptHeader(err) => err.http_status_code(),
            Self::UtxoInstanceExportNotFound(..)
            | Self::Runtime(..)
            | Self::Wasm(..)
            | Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
enum AcceptHeaderError {
    #[error("failed to decode `Accept` header: {0}")]
    Decoding(headers_core::Error),
    #[error("no acceptable media type, available: {}", format_media_types(.0))]
    NotAcceptable(&'static [MediaType<'static>]),
}

fn format_media_types(available: &[MediaType<'_>]) -> String {
    available
        .iter()
        .map(|mt| format!("`{mt}`"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl AcceptHeaderError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::Decoding(..) => http::StatusCode::BAD_REQUEST,
            Self::NotAcceptable(..) => http::StatusCode::NOT_ACCEPTABLE,
        }
    }
}

fn negotiate_accept(
    headers: &http::HeaderMap,
    available: &'static [MediaType<'static>],
) -> Option<Result<&'static MediaType<'static>, AcceptHeaderError>> {
    headers.contains_key(http::header::ACCEPT).then(|| {
        let accept = Accept::decode(&mut headers.get_all(ACCEPT).iter())
            .map_err(AcceptHeaderError::Decoding)?;
        accept
            .negotiate(available)
            .ok_or(AcceptHeaderError::NotAcceptable(available))
    })
}

fn build_http_response<T>(
    code: http::StatusCode,
    body: impl Into<T>,
) -> http::Result<http::Response<http_body_util::Full<T>>>
where
    T: Buf + Sync + Send + 'static,
{
    http::Response::builder()
        .status(code)
        .body(http_body_util::Full::new(body.into()))
}

/// A ledger node: published contracts, persisted transactions, and the
/// accounts paying for publishes, served over HTTP by [`handle_http`].
///
/// [`handle_http`]: Ledger::handle_http
pub struct Ledger {
    engine: wasmtime::Engine,
    wizer: Wizer,
    blocks: Arc<RwLock<Vec<Block>>>,
    utxos: Arc<RwLock<HashMap<[u8; 32], Arc<Utxo>>>>,
    contracts: Arc<RwLock<HashMap<[u8; 32], Arc<Contract>>>>,
    transactions: Arc<RwLock<Vec<Arc<Transaction>>>>,
    accounts: Arc<RwLock<HashMap<Box<str>, Account>>>,
    cardano: CardanoCtx,
    network: Arc<str>,
    max_requests: u32,
    permits: Arc<Semaphore>,
}

impl Ledger {
    pub fn new(
        engine: wasmtime::Engine,
        max_requests: u32,
        cardano: CardanoCtx,
        network: impl Into<Arc<str>>,
        admin_key: impl Into<Box<str>>,
        admin_balance: u64,
    ) -> Self {
        let max_requests = usize::try_from(max_requests)
            .unwrap_or(Semaphore::MAX_PERMITS)
            .min(Semaphore::MAX_PERMITS);
        Self {
            engine,
            wizer: Wizer::new(),
            blocks: Arc::default(),
            utxos: Arc::default(),
            contracts: Arc::default(),
            transactions: Arc::default(),
            accounts: Arc::new(RwLock::new(HashMap::from([(
                admin_key.into(),
                Account {
                    balance: admin_balance,
                    last_nonce: 0,
                },
            )]))),
            cardano,
            network: network.into(),
            max_requests: max_requests as _,
            permits: Arc::new(Semaphore::new(max_requests)),
        }
    }

    async fn handle_coordination_script_invocation(
        &self,
        mut headers: http::HeaderMap,
        digest: &str,
        name: &str,
        body: impl AsyncRead + Unpin,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, CoordinationScriptInvocationError>
    {
        let digest =
            parse_digest(digest).map_err(CoordinationScriptInvocationError::DigestParsing)?;

        let utxo_import_headers = match headers.entry(X_STARSTREAM_UTXO) {
            http::header::Entry::Occupied(imports) => {
                let (_, imports) = imports.remove_entry_mult();
                imports.collect()
            }
            http::header::Entry::Vacant(..) => Vec::default(),
        };

        let mut imports: HashMap<Box<str>, [u8; 32]> =
            HashMap::with_capacity(utxo_import_headers.len());
        for h in utxo_import_headers {
            let s = h
                .to_str()
                .map_err(CoordinationScriptInvocationError::UtxoHeaderToStr)?;
            let (instance, digest) = s
                .split_once('=')
                .ok_or(CoordinationScriptInvocationError::UtxoHeaderFormat)?;
            let digest = parse_digest(digest)
                .map_err(CoordinationScriptInvocationError::UtxoHeaderParsing)?;
            imports.insert(instance.into(), digest);
        }
        let (contract, resolved_imports) = {
            let contracts = self.contracts.read().await;
            let contract = contracts
                .get(&digest)
                .ok_or(CoordinationScriptInvocationError::ContractNotFound)?;

            let mut resolved_imports = HashMap::with_capacity(imports.len());
            for (instance, digest) in imports {
                let Some(contract) = contracts.get(&digest) else {
                    return Err(CoordinationScriptInvocationError::ImportNotFound(instance));
                };
                resolved_imports.insert(instance, (digest, contract.wasm.clone()));
            }
            (contract.clone(), resolved_imports)
        };
        let export = contract
            .scripts
            .get(name)
            .ok_or(CoordinationScriptInvocationError::FunctionExportNotFound)?;

        let mut instrumented = HashMap::with_capacity(resolved_imports.len());
        for (_, wasm) in resolved_imports.values() {
            if let hash_map::Entry::Vacant(entry) = instrumented.entry(wasm.clone()) {
                let (cx, wasm) = self
                    .wizer
                    .instrument_component(wasm)
                    .map_err(CoordinationScriptInvocationError::Wizer)?;
                let contract = starstream_runtime_next::Contract::new(&self.engine, &wasm)
                    .map_err(CoordinationScriptInvocationError::Runtime)?;
                entry.insert((cx, contract));
            };
        }

        let mut imports = HashMap::with_capacity(resolved_imports.len());
        for (instance, (_, wasm)) in &resolved_imports {
            let (_, contract) = &instrumented[wasm];
            let instance = instance.clone();
            match contract.get_utxo(&instance) {
                Ok(export) => {
                    imports.insert(
                        instance,
                        UtxoImport {
                            contract: contract.clone(),
                            export,
                        },
                    );
                }
                Err(err) => {
                    return Err(CoordinationScriptInvocationError::UtxoInstanceExport(
                        instance, err,
                    ));
                }
            }
        }

        let ctx = Ctx {
            cardano: self.cardano,
            imports,
            ..Ctx::default()
        };
        let mut store = wasmtime::Store::new(&self.engine, ctx);

        let param_tys = export.ty().params();
        let mut params = vec![wasmtime::component::Val::Bool(false); param_tys.len()];
        let body = FramedRead::new(body, wrpc_transport::FrameDecoder::default()).map(|frame| {
            let wrpc_transport::Frame { path, data } = frame?;
            anyhow::ensure!(path.is_empty(), "async values not supported");
            Ok(data)
        });
        let mut body = StreamReader::new(body.map_err(std::io::Error::other));
        for (v, (_, ty)) in zip(&mut params, param_tys) {
            read_value(&mut body, v, &ty)
                .await
                .map_err(CoordinationScriptInvocationError::ParameterDecoding)?;
        }

        let result_tys = export.ty().results();
        let mut results = vec![wasmtime::component::Val::Bool(false); result_tys.len()];
        contract
            .call_coordination_script(&mut store, export, &params, &mut results)
            .await
            .map_err(CoordinationScriptInvocationError::Runtime)?;

        let outputs = mem::take(&mut store.data_mut().outputs);
        let mut output_utxos = Vec::with_capacity(outputs.len());
        let mut res = http::Response::builder();
        let headers = res.headers_mut().expect("failed to get header map");
        for UtxoOutput {
            utxo,
            instance,
            implemented,
        } in outputs
        {
            let (contract_digest, wasm) = &resolved_imports[instance.as_ref()];
            let (cx, contract) = &instrumented[wasm];
            let Some(storage) = contract
                .get_utxo(&instance)
                .expect("missing UTXO instance")
                .storage()
                .cloned()
            else {
                return Err(CoordinationScriptInvocationError::UtxoStorageExportNotFound(instance));
            };
            let storage = utxo
                .storage(&storage)
                .get(&mut store)
                .await
                .map_err(CoordinationScriptInvocationError::Runtime)?;
            utxo.drop(&mut store)
                .await
                .map_err(CoordinationScriptInvocationError::Runtime)?;

            let wasm = self
                .wizer
                .snapshot_component(
                    cx,
                    &mut WasmtimeWizerComponent {
                        store: &mut store,
                        instance: utxo.instance(),
                    },
                )
                .await
                .map_err(CoordinationScriptInvocationError::Wizer)?;
            // TODO: Remove contract code from the snapshot
            output_utxos.push(Arc::new(Utxo {
                contract_digest: *contract_digest,
                instance,
                wasm: wasm.into(),
                implemented,
                storage,
            }));
        }
        {
            let mut utxos = self.utxos.write().await;
            for utxo in &output_utxos {
                // TODO: Ensure that `implemented` and storage are hashed
                let digest = Sha256::digest(&utxo.wasm).into();
                utxos.insert(digest, Arc::clone(utxo));

                let Ok(digest) = http::HeaderValue::from_bytes(encode_digest(&digest).as_bytes())
                else {
                    unreachable!()
                };
                headers.append(X_STARSTREAM_UTXO, digest);
            }
        }

        let tx = Arc::new(Transaction {
            coordination_script_digest: digest,
            inputs: Box::default(), // TODO: Add input UTXO support
            outputs: output_utxos.into(),
        });
        {
            let mut transactions = self.transactions.write().await;
            headers.insert(X_STARSTREAM_TRANSACTION, transactions.len().into());
            transactions.push(Arc::clone(&tx));

            let mut blocks = self.blocks.write().await;
            let height = blocks.len().saturating_add(1);
            headers.insert(X_STARSTREAM_BLOCK, height.into());
            blocks.push(Block {
                actions: [Action::Transaction(tx)].into(),
                height,
            });
        }

        let mut data = BytesMut::with_capacity(result_tys.len());
        for (v, ty) in zip(results, result_tys) {
            ValEncoder::new(&ty)
                .encode(&v, &mut data)
                .map_err(CoordinationScriptInvocationError::ResultEncoding)?;
        }
        let mut buf = BytesMut::with_capacity(data.len().saturating_add(1 + 10));
        wrpc_transport::FrameEncoder
            .encode(
                wrpc_transport::FrameRef {
                    path: &[],
                    data: &data,
                },
                &mut buf,
            )
            .map_err(CoordinationScriptInvocationError::FrameEncoding)?;
        res.header(CONTENT_TYPE, APPLICATION_OCTET_STREAM.to_string())
            .body(http_body_util::Full::new(buf.freeze()))
            .map_err(CoordinationScriptInvocationError::Http)
    }

    async fn handle_utxo_method_invocation(
        &self,
        digest: &str,
        name: &str,
        body: impl AsyncRead + Unpin,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, UtxoMethodInvocationError> {
        let digest = parse_digest(digest).map_err(UtxoMethodInvocationError::DigestParsing)?;

        let utxo = {
            let utxos = self.utxos.read().await;
            let utxo = utxos
                .get(&digest)
                .ok_or(UtxoMethodInvocationError::UtxoNotFound)?;
            Arc::clone(utxo)
        };
        {
            let contracts = self.contracts.read().await;
            contracts
                .get(&utxo.contract_digest)
                .ok_or(UtxoMethodInvocationError::ContractNotFound)?;
            // TODO: Merge UTXO state Wasm with Wasm of the contract
        }
        let contract = starstream_runtime_next::Contract::new(&self.engine, &utxo.wasm)
            .map_err(UtxoMethodInvocationError::Runtime)?;
        let utxo_export = contract.get_utxo(&utxo.instance).map_err(|err| {
            UtxoMethodInvocationError::UtxoInstanceExport(Arc::clone(&utxo.instance), err)
        })?;
        if !utxo.implements_method(&name.replace('-', "_")) {
            return Err(UtxoMethodInvocationError::MethodExportNotFound);
        }
        let method_export = contract
            .get_utxo_method(&utxo_export, &format!("[method]utxo.{name}"))
            .map_err(|_| UtxoMethodInvocationError::MethodExportNotFound)?;
        let storage_export = utxo_export.storage().ok_or_else(|| {
            UtxoMethodInvocationError::UtxoStorageExportNotFound(Arc::clone(&utxo.instance))
        })?;

        let ctx = Ctx {
            cardano: self.cardano,
            implemented: utxo.implemented.clone(),
            ..Ctx::default()
        };
        let mut store = wasmtime::Store::new(&self.engine, ctx);
        let utxo = contract
            .load_utxo(&mut store, storage_export, utxo.storage.clone())
            .await
            .map_err(UtxoMethodInvocationError::Runtime)?;

        let param_tys = method_export.ty().params();
        let mut params = vec![wasmtime::component::Val::Bool(false); param_tys.len()];
        params[0] = wasmtime::component::Val::Resource(utxo.resource());
        let body = FramedRead::new(body, wrpc_transport::FrameDecoder::default()).map(|frame| {
            let wrpc_transport::Frame { path, data } = frame?;
            anyhow::ensure!(path.is_empty(), "async values not supported");
            Ok(data)
        });
        let mut body = StreamReader::new(body.map_err(std::io::Error::other));
        for (v, (_, ty)) in zip(&mut params[1..], param_tys.skip(1)) {
            read_value(&mut body, v, &ty)
                .await
                .map_err(UtxoMethodInvocationError::ParameterDecoding)?;
        }

        let result_tys = method_export.ty().results();
        let mut results = vec![wasmtime::component::Val::Bool(false); result_tys.len()];
        utxo.call(&mut store, &method_export, params, &mut results)
            .await
            .map_err(UtxoMethodInvocationError::Runtime)?;

        let mut data = BytesMut::with_capacity(result_tys.len());
        for (v, ty) in zip(results, result_tys) {
            ValEncoder::new(&ty)
                .encode(&v, &mut data)
                .map_err(UtxoMethodInvocationError::ResultEncoding)?;
        }
        let mut buf = BytesMut::with_capacity(data.len().saturating_add(1 + 10));
        wrpc_transport::FrameEncoder
            .encode(
                wrpc_transport::FrameRef {
                    path: &[],
                    data: &data,
                },
                &mut buf,
            )
            .map_err(UtxoMethodInvocationError::FrameEncoding)?;
        http::Response::builder()
            .header(CONTENT_TYPE, APPLICATION_OCTET_STREAM.to_string())
            .body(http_body_util::Full::new(buf.freeze()))
            .map_err(UtxoMethodInvocationError::Http)
    }

    async fn handle_contract_get(
        &self,
        headers: http::HeaderMap,
        digest: &str,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, ContractGetError> {
        const AVAILABLE_TYPES: &[MediaType] = &[APPLICATION_COSE, APPLICATION_WASM];

        let digest = parse_digest(digest).map_err(ContractGetError::DigestParsing)?;

        let accept = negotiate_accept(&headers, AVAILABLE_TYPES)
            .transpose()
            .map_err(ContractGetError::AcceptHeader)?;

        let contracts = self.contracts.read().await;
        let contract = contracts
            .get(&digest)
            .ok_or(ContractGetError::ContractNotFound)?;

        if accept == Some(&APPLICATION_WASM) {
            http::Response::builder()
                .header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                .body(http_body_util::Full::new(contract.wasm.clone()))
                .map_err(ContractGetError::Http)
        } else {
            http::Response::builder()
                .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
                .body(http_body_util::Full::new(contract.envelope.clone()))
                .map_err(ContractGetError::Http)
        }
    }

    async fn handle_contract_put(
        &self,
        headers: http::HeaderMap,
        digest: &str,
        body: hyper::body::Incoming,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, ContractPutError> {
        let digest = parse_digest(digest).map_err(ContractPutError::DigestParsing)?;

        match headers.get(CONTENT_TYPE).map(HeaderValue::to_str) {
            None => {}
            Some(Ok(ct)) => match MediaType::parse(ct) {
                Ok(ct) if ct.essence() == APPLICATION_COSE => {}
                Ok(ct) => {
                    return Err(ContractPutError::UnsupportedContentType(
                        ct.to_string().into_boxed_str(),
                    ));
                }
                Err(err) => return Err(ContractPutError::ContentTypeParsing(err)),
            },
            Some(Err(err)) => return Err(ContractPutError::ContentTypeToStr(err)),
        }
        // TODO: Check content-length, enforce limit

        let envelope = body
            .collect()
            .await
            .map_err(ContractPutError::Body)?
            .to_bytes();
        let sign1 = CoseSign1::from_tagged_slice(&envelope)
            .or_else(|_| CoseSign1::from_slice(&envelope))
            .map_err(ContractPutError::CoseSign1Parsing)?;
        if sign1.protected.header.alg != Some(coset::Algorithm::Assigned(iana::Algorithm::EdDSA)) {
            return Err(ContractPutError::Algorithm);
        }
        let key = <[u8; 32]>::try_from(sign1.protected.header.key_id.as_slice())
            .map_err(|_| ContractPutError::KeyIdFormat)?;
        let key = VerifyingKey::from_bytes(&key).map_err(ContractPutError::Key)?;
        sign1
            .verify_signature(b"", |signature, data| {
                Signature::from_slice(signature)
                    .and_then(|signature| key.verify_strict(data, &signature))
            })
            .map_err(ContractPutError::SignatureVerification)?;
        let payload = sign1
            .payload
            .as_deref()
            .ok_or(ContractPutError::PayloadMissing)?;
        let tx = match ciborium::from_reader(payload) {
            Ok(ciborium::Value::Array(tx)) => tx,
            Ok(..) => return Err(ContractPutError::TransactionFormat),
            Err(err) => return Err(ContractPutError::PayloadParsing(err)),
        };
        let (context, tx_network, nonce, wasm) = match <[ciborium::Value; 4]>::try_from(tx) {
            Ok(
                [
                    ciborium::Value::Text(context),
                    ciborium::Value::Text(network),
                    ciborium::Value::Integer(nonce),
                    ciborium::Value::Bytes(wasm),
                ],
            ) => (context, network, nonce, wasm),
            _ => return Err(ContractPutError::TransactionFormat),
        };
        if context != PUBLISH_CONTEXT {
            return Err(ContractPutError::Context(context.into_boxed_str()));
        }
        if tx_network != *self.network {
            return Err(ContractPutError::Network {
                got: tx_network.into_boxed_str(),
                expected: Arc::clone(&self.network),
            });
        }
        let nonce = u64::try_from(nonce).map_err(|_| ContractPutError::NonceOverflow)?;
        let wasm_digest: [u8; 32] = Sha256::digest(&wasm).into();
        if wasm_digest != digest {
            return Err(ContractPutError::DigestMismatch(wasm_digest));
        }

        let account_id = hex::encode(&sign1.protected.header.key_id);
        let mut accounts = self.accounts.write().await;
        let Some(account) = accounts.get_mut(account_id.as_str()) else {
            return Err(ContractPutError::AccountNotFound(
                account_id.into_boxed_str(),
            ));
        };
        if nonce <= account.last_nonce {
            return Err(ContractPutError::NonceTooLow {
                last_nonce: account.last_nonce,
                nonce,
            });
        }
        let Some(balance) = account.balance.checked_sub(wasm.len() as _) else {
            return Err(ContractPutError::InsufficientBalance {
                required: wasm.len() as _,
                available: account.balance,
            });
        };

        let mut contracts = self.contracts.write().await;
        let hash_map::Entry::Vacant(entry) = contracts.entry(digest) else {
            return build_http_response(http::StatusCode::OK, "").map_err(ContractPutError::Http);
        };

        // TODO: Split component
        let contract = starstream_runtime_next::Contract::new(&self.engine, &wasm)
            .map_err(ContractPutError::Runtime)?;
        let mut scripts = HashMap::default();
        for (name, export) in contract.coordination_scripts() {
            let export = export.map_err(ContractPutError::Runtime)?;
            scripts.insert(name.into(), export);
        }
        let mut utxos = HashMap::default();
        for (name, export) in contract.utxos() {
            let export = export.map_err(ContractPutError::Runtime)?;
            utxos.insert(name.into(), export);
        }
        entry.insert(Arc::new(Contract {
            contract,
            wasm: wasm.into(),
            scripts,
            utxos,
            envelope: envelope.clone(),
        }));
        {
            let mut blocks = self.blocks.write().await;
            let height = blocks.len().saturating_add(1);
            blocks.push(Block {
                actions: [Action::ContractUpload(envelope)].into(),
                height,
            });
        }
        account.balance = balance;
        account.last_nonce = nonce;
        build_http_response(http::StatusCode::OK, "").map_err(ContractPutError::Http)
    }

    async fn handle_contract_rpc_get(
        &self,
        headers: http::HeaderMap,
        digest: &str,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, ContractRpcGetError> {
        const AVAILABLE_TYPES: &[MediaType] = &[TEXT_PLAIN_UTF_8, APPLICATION_WASM];

        let digest = parse_digest(digest).map_err(ContractRpcGetError::DigestParsing)?;

        let accept = negotiate_accept(&headers, AVAILABLE_TYPES)
            .transpose()
            .map_err(ContractRpcGetError::AcceptHeader)?;

        let contracts = self.contracts.read().await;
        let contract = contracts
            .get(&digest)
            .ok_or(ContractRpcGetError::ContractNotFound)?;

        let mut wit = String::new();
        wit.push_str("package starstream:contract;\n");
        wit.push_str("interface ");
        wit.push_str(&encode_digest(&digest));
        wit.push_str(" {\n");
        for (name, export) in contract.coordination_scripts() {
            let export = export.map_err(ContractRpcGetError::Runtime)?;
            wit.push_str("  ");
            wit.push_str(&wit_func(name, export.ty(), false));
            wit.push('\n');
        }
        wit.push_str("}\n");

        if accept == Some(&APPLICATION_WASM) {
            let mut resolve = wit_parser::Resolve::new();
            let pkg = resolve
                .push_str("contract.wit", &wit)
                .map_err(ContractRpcGetError::Wasm)?;
            let wasm = wit_component::encode(&resolve, pkg).map_err(ContractRpcGetError::Wasm)?;
            http::Response::builder()
                .header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                .body(http_body_util::Full::new(wasm.into()))
                .map_err(ContractRpcGetError::Http)
        } else {
            http::Response::builder()
                .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
                .body(http_body_util::Full::new(wit.into()))
                .map_err(ContractRpcGetError::Http)
        }
    }

    async fn handle_utxo_rpc_get(
        &self,
        headers: http::HeaderMap,
        digest: &str,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, UtxoRpcGetError> {
        const AVAILABLE_TYPES: &[MediaType] = &[TEXT_PLAIN_UTF_8, APPLICATION_WASM];

        let digest = parse_digest(digest).map_err(UtxoRpcGetError::DigestParsing)?;

        let accept = negotiate_accept(&headers, AVAILABLE_TYPES)
            .transpose()
            .map_err(UtxoRpcGetError::AcceptHeader)?;

        let utxo = {
            let utxos = self.utxos.read().await;
            utxos
                .get(&digest)
                .cloned()
                .ok_or(UtxoRpcGetError::UtxoNotFound)?
        };
        let contracts = self.contracts.read().await;
        let contract = contracts
            .get(&utxo.contract_digest)
            .ok_or(UtxoRpcGetError::ContractNotFound)?;
        let export = contract.utxos.get(utxo.instance.as_ref()).ok_or_else(|| {
            UtxoRpcGetError::UtxoInstanceExportNotFound(Arc::clone(&utxo.instance))
        })?;

        let mut wit = String::new();
        wit.push_str("package starstream:utxo;\n");
        wit.push_str("interface ");
        wit.push_str(&encode_digest(&digest));
        wit.push_str(" {\n");
        for (name, method) in contract.utxo_methods(export) {
            let method = method.map_err(UtxoRpcGetError::Runtime)?;
            let Some(name) = name.strip_prefix("[method]utxo.") else {
                error!(name, "unexpected UTXO method name");
                continue;
            };
            if !utxo.implements_method(&name.replace('-', "_")) {
                continue;
            }
            wit.push_str("  ");
            wit.push_str(&wit_func(name, method.ty(), true));
            wit.push('\n');
        }
        wit.push_str("}\n");

        if accept == Some(&APPLICATION_WASM) {
            let mut resolve = wit_parser::Resolve::new();
            let pkg = resolve
                .push_str("utxo.wit", &wit)
                .map_err(UtxoRpcGetError::Wasm)?;
            let wasm = wit_component::encode(&resolve, pkg).map_err(UtxoRpcGetError::Wasm)?;
            http::Response::builder()
                .header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                .body(http_body_util::Full::new(wasm.into()))
                .map_err(UtxoRpcGetError::Http)
        } else {
            http::Response::builder()
                .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
                .body(http_body_util::Full::new(wit.into()))
                .map_err(UtxoRpcGetError::Http)
        }
    }

    /// Bind `address` and return the future serving the ledger HTTP API on
    /// it; the future runs the accept loop until dropped.
    ///
    /// The served endpoints are specified in `docs/ledger.md`.
    #[instrument(skip_all)]
    pub async fn handle_http(
        self: Arc<Self>,
        address: SocketAddr,
    ) -> anyhow::Result<impl Future<Output = ()> + use<>> {
        let exe = TokioExecutor::new();

        let sock = bind_tcp(address)?;
        let sock = sock
            .listen(self.max_requests)
            .context("failed to listen on TCP socket")?;

        let permits = Arc::clone(&self.permits);
        let ledger = self.clone();
        let svc = service_fn(move |req: http::Request<hyper::body::Incoming>| {
            let permits = Arc::clone(&permits);
            let ledger = ledger.clone();
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
                let (
                    http::request::Parts {
                        method,
                        uri,
                        headers,
                        ..
                    },
                    body,
                ) = req.into_parts();
                let Some(pq) = uri.path_and_query() else {
                    return build_http_response(http::StatusCode::OK, "");
                };
                let mut path = pq.path().split('/');
                let Some("") = path.next() else {
                    return build_http_response(http::StatusCode::OK, "");
                };
                match (
                    method.as_str(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                ) {
                    ("POST", Some("rpc"), None, ..) => {
                        let mut body = wrpc_http::data_reader_from_incoming(body);
                        let wrpc_transport::frame::Header { instance, name } =
                            match wrpc_transport::frame::Header::read(&mut body).await {
                                Ok(h) => h,
                                Err(err) => {
                                    return build_http_response(
                                        http::StatusCode::BAD_REQUEST,
                                        format!("failed to decode wRPC header: {err}"),
                                    );
                                }
                            };
                        match instance.split_once('/') {
                            Some(("starstream:contract", digest)) => match ledger
                                .handle_coordination_script_invocation(headers, digest, &name, body)
                                .await
                            {
                                Ok(res) => Ok(res),
                                Err(err) => {
                                    build_http_response(err.http_status_code(), err.to_string())
                                }
                            },
                            Some(("starstream:utxo", digest)) => match ledger
                                .handle_utxo_method_invocation(digest, &name, body)
                                .await
                            {
                                Ok(res) => Ok(res),
                                Err(err) => {
                                    build_http_response(err.http_status_code(), err.to_string())
                                }
                            },
                            _ => build_http_response(
                                http::StatusCode::NOT_IMPLEMENTED,
                                "ledger RPC not supported yet",
                            ),
                        }
                    }
                    (method, Some("rpc"), None, ..) => build_http_response(
                        http::StatusCode::METHOD_NOT_ALLOWED,
                        format!("method `{method}` not allowed for path `{}`", pq.path()),
                    ),

                    ("GET", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_get(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_http_response(err.http_status_code(), err.to_string()),
                    },

                    ("PUT", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_put(headers, digest, body)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_http_response(err.http_status_code(), err.to_string()),
                    },
                    (method, Some("contracts"), Some(..), None, ..) => build_http_response(
                        http::StatusCode::METHOD_NOT_ALLOWED,
                        format!("method `{method}` not allowed for path `{}`", pq.path()),
                    ),

                    ("GET", Some("contracts"), Some(digest), Some("rpc"), None, ..) => match ledger
                        .handle_contract_rpc_get(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_http_response(err.http_status_code(), err.to_string()),
                    },
                    (method, Some("contracts"), Some(..), Some("rpc"), None, ..) => {
                        build_http_response(
                            http::StatusCode::METHOD_NOT_ALLOWED,
                            format!("method `{method}` not allowed for path `{}`", pq.path()),
                        )
                    }

                    ("GET", Some("utxos"), Some(digest), Some("rpc"), None, ..) => match ledger
                        .handle_utxo_rpc_get(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_http_response(err.http_status_code(), err.to_string()),
                    },
                    (method, Some("utxos"), Some(..), Some("rpc"), None, ..) => {
                        build_http_response(
                            http::StatusCode::METHOD_NOT_ALLOWED,
                            format!("method `{method}` not allowed for path `{}`", pq.path()),
                        )
                    }

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
