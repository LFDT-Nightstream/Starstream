use core::future::poll_fn;
use core::iter::zip;
use core::net::SocketAddr;
use core::pin::pin;
use core::sync::atomic::{AtomicU64, Ordering};
use core::task::{Poll, ready};
use core::time::Duration;

use std::collections::{HashMap, HashSet, hash_map};
use std::error::Error as _;
use std::sync::Arc;

use anyhow::Context as _;
use bytes::{Buf, BufMut as _, Bytes, BytesMut};
use ed25519_dalek::VerifyingKey;
use futures::TryStreamExt as _;
use http::header::{
    ACCEPT, ALLOW, CONTENT_LENGTH, CONTENT_TYPE, LOCATION, VARY, X_CONTENT_TYPE_OPTIONS,
};
use http_body::Body as _;
use http_body_util::BodyExt as _;
use hyper::service::service_fn;
use hyper_util::rt::{TokioIo, TokioTimer};
use hyper_util::server::graceful::GracefulShutdown;
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use tokio::net::TcpSocket;
use tokio::sync::{Notify, TryAcquireError};
use tokio::task::JoinSet;
use tokio::time::sleep;
use tokio_io_timeout::TimeoutStream;
use tokio_util::codec::Encoder as _;
use tokio_util::io::StreamReader;
use tower_http::timeout::{TimeoutBody, TimeoutError};
use tracing::{Instrument as _, debug, error, info, instrument, warn};
use wasm_tokio::cm::{AsyncReadValue as _, U64Codec};
use wasm_tokio::{AsyncReadCore as _, AsyncReadLeb128 as _, CoreNameDecoder, CoreVecDecoderBytes};
use wasmparser::WasmFeatures;
use wasmtime::component::{ResourceTable, Type, Val};

use crate::cose::read_envelope;
use crate::runtime::apply_state;
use crate::server::{Contract, Ctx, Ledger, Transaction, UtxoCtx, wit};
use crate::wrpc::bindings;
use crate::wrpc::bindings::starstream::ledger::types::GetError;
use crate::wrpc::codec::{ValEncoder, read_value};
use crate::wrpc::{
    LEDGER_ACCOUNT_INSTANCE, LEDGER_ADMIN_INSTANCE, LEDGER_BLOCK_INSTANCE,
    LEDGER_CONTRACT_INSTANCE, LEDGER_GENESIS_INSTANCE, LEDGER_NETWORK_INSTANCE,
    LEDGER_TRANSACTION_INSTANCE, LEDGER_UTXO_INSTANCE, ensure_eof, flatten_stream, read_params,
};
use crate::{
    APPLICATION_COSE, APPLICATION_WASM, APPLICATION_WRPC, Action, Block, Fund, Message, Publish,
    TransactionInput, TransactionOutput, encode_digest, parse_digest,
};

mod accept;
mod error;
use error::*;

const MAX_ENVELOPE_SIZE: usize = 1 << 24;

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

fn build_text_response<T>(
    code: http::StatusCode,
    body: impl Into<T>,
) -> http::Result<http::Response<http_body_util::Full<T>>>
where
    T: Buf + Sync + Send + 'static,
{
    http::Response::builder()
        .status(code)
        .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
        .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
        .body(http_body_util::Full::new(body.into()))
}

fn build_method_not_allowed(
    allow: &'static str,
    method: &http::Method,
    path: &str,
) -> http::Result<http::Response<http_body_util::Full<Bytes>>> {
    http::Response::builder()
        .status(http::StatusCode::METHOD_NOT_ALLOWED)
        .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
        .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
        .header(ALLOW, allow)
        .body(http_body_util::Full::new(
            format!("method `{method}` not allowed for path `{path}`").into(),
        ))
}

/// Attempt to update the stored nonce.
///
/// On success, returns the previous nonce, which is lower than `nonce`.
/// On failure returns the current nonce, which is greater than or equal to
/// `nonce`.
fn try_update_nonce(last_nonce: &AtomicU64, nonce: u64) -> Result<u64, u64> {
    last_nonce.try_update(Ordering::Relaxed, Ordering::Relaxed, |last_nonce| {
        if nonce > last_nonce {
            Some(nonce)
        } else {
            None
        }
    })
}

fn is_connection_error(err: &std::io::Error) -> bool {
    matches!(
        err.kind(),
        std::io::ErrorKind::ConnectionRefused
            | std::io::ErrorKind::ConnectionAborted
            | std::io::ErrorKind::ConnectionReset
    )
}

fn is_timeout(err: &hyper::Error) -> bool {
    err.is_timeout()
        || err
            .source()
            .and_then(|err| err.downcast_ref::<std::io::Error>())
            .is_some_and(|err| err.kind() == std::io::ErrorKind::TimedOut)
}

impl Ledger {
    async fn handle_contract_get<const HEAD: bool>(
        &self,
        headers: http::HeaderMap,
        digest: &str,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, ContractGetError> {
        const AVAILABLE_TYPES: &[MediaType] = &[APPLICATION_COSE, APPLICATION_WASM];

        let digest = parse_digest(digest).map_err(ContractGetError::DigestParsing)?;

        let accept =
            accept::negotiate(&headers, AVAILABLE_TYPES).map_err(ContractGetError::AcceptHeader)?;

        let contracts = self.contracts.read().await;
        let contract = contracts
            .get(&digest)
            .ok_or(ContractGetError::ContractNotFound)?;

        let res = http::Response::builder()
            .header(VARY, ACCEPT.as_str())
            .header(X_CONTENT_TYPE_OPTIONS, "nosniff");
        if accept == &APPLICATION_WASM {
            res.header(CONTENT_TYPE, APPLICATION_WASM.to_string())
                .header(CONTENT_LENGTH, contract.wasm.len())
                .body(if HEAD {
                    http_body_util::Full::default()
                } else {
                    http_body_util::Full::new(contract.wasm.clone())
                })
        } else {
            res.header(CONTENT_TYPE, APPLICATION_COSE.to_string())
                .header(CONTENT_LENGTH, contract.envelope.len())
                .body(if HEAD {
                    http_body_util::Full::default()
                } else {
                    http_body_util::Full::new(contract.envelope.clone())
                })
        }
        .map_err(ContractGetError::Http)
    }

    async fn handle_publish_post(
        &self,
        envelope: Bytes,
        signers: Vec<VerifyingKey>,
        Publish { nonce, wasm }: Publish,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, PublishPostError> {
        let [account] = signers[..] else {
            return Err(PublishPostError::SignerCount(signers.len()));
        };
        let types = wasmparser::Validator::new_with_features(
            WasmFeatures::default() | WasmFeatures::CM_IMPLEMENTS,
        )
        .validate_all(&wasm)
        .map_err(PublishPostError::Wasm)?;
        let utxo_wits = wit::utxo_wits(&wasm, &types).map_err(PublishPostError::Wit)?;

        let digest: [u8; 32] = Sha256::digest(&wasm).into();
        let mut contracts = self.contracts.write().await;
        let hash_map::Entry::Vacant(entry) = contracts.entry(digest) else {
            return Err(PublishPostError::AlreadyExists(digest));
        };
        {
            let accounts = self.accounts.read().await;
            let Some(account) = accounts.get(&account) else {
                return Err(PublishPostError::AccountNotFound(account));
            };
            if let Err(last_nonce) = try_update_nonce(&account.last_nonce, nonce) {
                return Err(PublishPostError::NonceTooLow { last_nonce, nonce });
            }
            let required = envelope.len() as _;
            if let Err(available) =
                account
                    .balance
                    .try_update(Ordering::Relaxed, Ordering::Relaxed, |balance| {
                        balance.checked_sub(required)
                    })
            {
                return Err(PublishPostError::InsufficientBalance {
                    required,
                    available,
                });
            }
        }
        // TODO: Split component
        entry.insert(Arc::new(Contract {
            wasm: wasm.into(),
            envelope: envelope.clone(),
            utxo_wits,
        }));
        {
            let mut blocks = self.blocks.write().await;
            blocks.push(Block {
                actions: Box::from([Action::UploadContract(envelope)]),
            });
        }
        let digest = encode_digest(&digest);
        http::Response::builder()
            .status(http::StatusCode::CREATED)
            .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
            .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
            .header(LOCATION, format!("/contracts/{digest}"))
            .body(http_body_util::Full::new(digest.into()))
            .map_err(PublishPostError::Http)
    }

    async fn handle_fund_post(
        &self,
        envelope: Bytes,
        signers: Vec<VerifyingKey>,
        Fund {
            nonce,
            account,
            amount,
        }: Fund,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, FundPostError> {
        let [key] = signers[..] else {
            return Err(FundPostError::SignerCount(signers.len()));
        };
        let account = VerifyingKey::from_bytes(&account).map_err(FundPostError::Key)?;
        if account.is_weak() {
            return Err(FundPostError::WeakKey);
        }

        if key != self.admin.key {
            return Err(FundPostError::NotAdmin(key));
        }
        if let Err(last_nonce) = try_update_nonce(&self.admin.last_nonce, nonce) {
            return Err(FundPostError::NonceTooLow { last_nonce, nonce });
        }
        {
            let mut accounts = self.accounts.write().await;
            let account = accounts.entry(account).or_default();
            account
                .balance
                .update(Ordering::Relaxed, Ordering::Relaxed, |balance| {
                    balance.saturating_add(amount)
                });
        }
        {
            let mut blocks = self.blocks.write().await;
            blocks.push(Block {
                actions: Box::from([Action::FundAccount(envelope)]),
            });
        }
        build_text_response(http::StatusCode::OK, "").map_err(FundPostError::Http)
    }

    async fn handle_transaction_get<const HEAD: bool>(
        &self,
        headers: http::HeaderMap,
        digest: &str,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, TransactionGetError> {
        const AVAILABLE_TYPES: &[MediaType] = &[APPLICATION_COSE];

        let digest = parse_digest(digest).map_err(TransactionGetError::DigestParsing)?;

        accept::negotiate(&headers, AVAILABLE_TYPES).map_err(TransactionGetError::AcceptHeader)?;

        let txs = self.transactions.read().await;
        let tx = txs
            .get(&digest)
            .ok_or(TransactionGetError::TransactionNotFound)?;

        http::Response::builder()
            .header(VARY, ACCEPT.as_str())
            .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
            .header(CONTENT_LENGTH, tx.envelope.len())
            .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
            .body(if HEAD {
                http_body_util::Full::default()
            } else {
                http_body_util::Full::new(tx.envelope.clone())
            })
            .map_err(TransactionGetError::Http)
    }

    async fn handle_transaction_post(
        &self,
        envelope: Bytes,
        signers: Vec<VerifyingKey>,
        transaction: crate::Transaction,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, TransactionPostError> {
        let digest = transaction
            .digest()
            .map_err(TransactionPostError::CborEncoding)?;
        let crate::Transaction { inputs, .. } = &transaction;
        if inputs.is_empty() {
            return Err(TransactionPostError::InputsEmpty);
        }
        let mut resolved_inputs = HashSet::with_capacity(inputs.len());
        for TransactionInput { transaction, index } in inputs {
            let index =
                usize::try_from(*index).map_err(|_| TransactionPostError::InputIndexOverflow)?;
            let transaction = if transaction.is_empty() {
                None
            } else {
                let transaction = parse_digest(transaction).map_err(|err| {
                    TransactionPostError::InputTransactionDigestParsing(transaction.clone(), err)
                })?;
                Some(transaction)
            };
            if !resolved_inputs.insert((transaction, index)) {
                return Err(TransactionPostError::InputDuplicate);
            }
        }
        let transaction = bindings::starstream::ledger::types::Transaction::from(transaction);
        let mut encoded = BytesMut::new();
        wrpc_transport::pack_ready(&transaction, &mut encoded)
            .map_err(TransactionPostError::WrpcEncoding)?;

        let mut txs = self.transactions.write().await;
        if txs.contains_key(&digest) {
            return Err(TransactionPostError::AlreadyExists(digest));
        }
        let mut genesis = self.genesis.outputs.write().await;
        for (tx, i) in &resolved_inputs {
            // TODO: Verify transaction signatures
            if let Some(tx) = tx {
                let Transaction { outputs, .. } =
                    txs.get(tx).ok_or(TransactionPostError::InputNotFound)?;
                let utxo = outputs.get(*i).ok_or(TransactionPostError::InputNotFound)?;
                let _utxo = utxo.as_ref().ok_or(TransactionPostError::InputNotFound)?;
            } else {
                genesis
                    .get(*i)
                    .and_then(Option::as_ref)
                    .ok_or(TransactionPostError::InputNotFound)?;
                if !signers.contains(&self.admin.key) {
                    return Err(TransactionPostError::InputUnauthorized);
                }
            }
        }
        // TODO: Verify sum(inputs) >= sum(outputs) + fee
        for (tx, i) in resolved_inputs {
            let utxo = if let Some(tx) = tx {
                let Some(Transaction { outputs, .. }) = txs.get_mut(&tx) else {
                    unreachable!();
                };
                outputs[i].take()
            } else {
                genesis[i].take()
            };
            let Some(..) = utxo else {
                unreachable!();
            };
        }
        let outputs = transaction
            .outputs
            .into_iter()
            .map(|utxo| Some(Arc::new(utxo.into())))
            .collect();
        let tx = Transaction {
            outputs,
            envelope: envelope.clone(),
            encoded: encoded.freeze(),
        };
        txs.insert(digest, tx);

        {
            let mut blocks = self.blocks.write().await;
            blocks.push(Block {
                actions: Box::from([Action::Transaction(envelope)]),
            });
        }
        let digest = encode_digest(&digest);
        http::Response::builder()
            .status(http::StatusCode::CREATED)
            .header(CONTENT_TYPE, TEXT_PLAIN_UTF_8.to_string())
            .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
            .header(LOCATION, format!("/transactions/{digest}"))
            .body(http_body_util::Full::new(digest.into()))
            .map_err(TransactionPostError::Http)
    }

    async fn handle_envelope_post(
        &self,
        body: hyper::body::Incoming,
        io_timeout: Duration,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, EnvelopePostError> {
        if body.size_hint().lower() > MAX_ENVELOPE_SIZE as u64 {
            return Err(EnvelopePostError::BodyTooLarge(MAX_ENVELOPE_SIZE));
        }
        let body = TimeoutBody::new(io_timeout, body);
        let body = http_body_util::Limited::new(body, MAX_ENVELOPE_SIZE)
            .collect()
            .await
            .map_err(|err| {
                if err.is::<http_body_util::LengthLimitError>() {
                    EnvelopePostError::BodyTooLarge(MAX_ENVELOPE_SIZE)
                } else if err.is::<TimeoutError>() {
                    EnvelopePostError::BodyTimeout
                } else {
                    EnvelopePostError::Body(err)
                }
            })?;
        let body = body.to_bytes();
        let (signers, envelope) = read_envelope(&body).map_err(EnvelopePostError::Envelope)?;
        if *envelope.network != *self.network {
            return Err(EnvelopePostError::Network {
                got: envelope.network,
                expected: Arc::clone(&self.network),
            });
        }
        match envelope.message {
            Message::Fund(fund) => self
                .handle_fund_post(body, signers, fund)
                .await
                .map_err(EnvelopePostError::Fund),
            Message::Publish(publish) => self
                .handle_publish_post(body, signers, publish)
                .await
                .map_err(EnvelopePostError::Publish),
            Message::Transaction(tx) => self
                .handle_transaction_post(body, signers, tx)
                .await
                .map_err(EnvelopePostError::Transaction),
        }
    }

    async fn get_utxo(
        &self,
        transaction: Option<[u8; 32]>,
        index: usize,
    ) -> Option<Arc<TransactionOutput>> {
        if let Some(transaction) = transaction {
            let txs = self.transactions.read().await;
            let Transaction { outputs, .. } = txs.get(&transaction)?;
            let utxo = outputs.get(index)?;
            utxo.as_ref().map(Arc::clone)
        } else {
            let outputs = self.genesis.outputs.read().await;
            let utxo = outputs.get(index)?;
            utxo.as_ref().map(Arc::clone)
        }
    }

    async fn handle_rpc_get<const HEAD: bool>(
        &self,
        headers: http::HeaderMap,
        query: Option<&str>,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, RpcGetError> {
        const AVAILABLE_TYPES: &[MediaType] = &[TEXT_PLAIN_UTF_8, APPLICATION_WASM];

        let accept =
            accept::negotiate(&headers, AVAILABLE_TYPES).map_err(RpcGetError::AcceptHeader)?;

        let mut transaction = None;
        let mut index = None;
        for (name, value) in form_urlencoded::parse(query.unwrap_or_default().as_bytes()) {
            match &*name {
                "transaction" if transaction.is_some() => {
                    return Err(RpcGetError::DuplicateQueryParameter(name.into()));
                }
                "transaction" => {
                    let digest =
                        parse_digest(&value).map_err(RpcGetError::TransactionDigestParsing)?;
                    transaction = Some(digest);
                }
                "index" if index.is_some() => {
                    return Err(RpcGetError::DuplicateQueryParameter(name.into()));
                }
                "index" => {
                    let i: u32 = value.parse().map_err(RpcGetError::UtxoIndexParsing)?;
                    let i = usize::try_from(i).map_err(|_| RpcGetError::UtxoIndexOverflow)?;
                    index = Some(i);
                }
                name => return Err(RpcGetError::UnknownQueryParameter(name.into())),
            }
        }
        let (content_type, body) = match (transaction, index) {
            (Some(..), None) => return Err(RpcGetError::UtxoIndexMissing),
            (None, None) => {
                if accept == &APPLICATION_WASM {
                    (APPLICATION_WASM, Bytes::from_static(wit::LEDGER_WASM))
                } else {
                    (
                        TEXT_PLAIN_UTF_8,
                        Bytes::from_static(wit::LEDGER_WIT.as_bytes()),
                    )
                }
            }
            (transaction, Some(index)) => {
                let utxo = self
                    .get_utxo(transaction, index)
                    .await
                    .ok_or(RpcGetError::UtxoNotFound)?;
                let contract =
                    parse_digest(&utxo.contract).map_err(RpcGetError::ContractDigestParsing)?;
                let contracts = self.contracts.read().await;
                let contract = contracts
                    .get(&contract)
                    .ok_or(RpcGetError::ContractNotFound)?;
                let Some((text, utxo_wasm)) = contract.utxo_wits.get(&utxo.instance) else {
                    return Err(RpcGetError::UtxoInstanceNotFound(utxo.instance.clone()));
                };
                if accept == &APPLICATION_WASM {
                    (APPLICATION_WASM, utxo_wasm.clone())
                } else {
                    (TEXT_PLAIN_UTF_8, text.clone())
                }
            }
        };

        http::Response::builder()
            .header(VARY, ACCEPT.as_str())
            .header(CONTENT_TYPE, content_type.to_string())
            .header(CONTENT_LENGTH, body.len())
            .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
            .body(if HEAD {
                http_body_util::Full::default()
            } else {
                http_body_util::Full::new(body)
            })
            .map_err(RpcGetError::Http)
    }

    async fn handle_rpc_post(
        &self,
        headers: http::HeaderMap,
        body: hyper::body::Incoming,
        io_timeout: Duration,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, RpcPostError> {
        fn get_by_digest<'a, T>(
            vs: &'a HashMap<[u8; 32], T>,
            digest: &str,
        ) -> Result<&'a T, GetError> {
            let digest = parse_digest(digest)?;
            vs.get(&digest).ok_or(GetError::NotFound)
        }

        fn get_by_key<'a, T>(
            vs: &'a HashMap<VerifyingKey, T>,
            key: &[u8],
        ) -> Result<&'a T, GetError> {
            let key = VerifyingKey::try_from(key)?;
            vs.get(&key).ok_or(GetError::NotFound)
        }

        let content_type = headers
            .get(CONTENT_TYPE)
            .ok_or(RpcPostError::ContentTypeMissing)?;
        let content_type = content_type
            .to_str()
            .map_err(RpcPostError::ContentTypeToStr)?;
        let content_type =
            MediaType::parse(content_type).map_err(RpcPostError::ContentTypeParsing)?;
        let content_type = content_type.essence();
        if content_type == APPLICATION_COSE {
            return self
                .handle_envelope_post(body, io_timeout)
                .await
                .map_err(RpcPostError::Envelope);
        }
        if content_type != APPLICATION_WRPC {
            return Err(RpcPostError::UnsupportedContentType(
                content_type.to_string().into(),
            ));
        }
        let body = TimeoutBody::new(io_timeout, body).into_data_stream();
        let body = body.map_err(|err| {
            if err.is::<TimeoutError>() {
                std::io::Error::new(std::io::ErrorKind::TimedOut, err)
            } else {
                std::io::Error::other(err)
            }
        });
        let mut body = StreamReader::new(Box::pin(body));
        let wrpc_transport::frame::Header { instance, name } =
            wrpc_transport::frame::Header::read(&mut body)
                .await
                .map_err(RpcPostError::Header)?;
        let mut body = flatten_stream(body);
        let mut data = BytesMut::new();
        match (instance.as_str(), name.as_str()) {
            (LEDGER_ADMIN_INSTANCE, "get-nonce") => {
                ensure_eof(body)
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let nonce = self.admin.last_nonce.load(Ordering::Relaxed);
                wrpc_transport::pack_ready(nonce, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_ACCOUNT_INSTANCE, "get-balance") => {
                let key = read_params(body, <CoreVecDecoderBytes>::default())
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let balance = {
                    let accounts = self.accounts.read().await;
                    get_by_key(&accounts, &key)
                        .map(|account| account.balance.load(Ordering::Relaxed))
                };
                wrpc_transport::pack_ready(balance, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_ACCOUNT_INSTANCE, "get-nonce") => {
                let key = read_params(body, <CoreVecDecoderBytes>::default())
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let nonce = {
                    let accounts = self.accounts.read().await;
                    get_by_key(&accounts, &key)
                        .map(|account| account.last_nonce.load(Ordering::Relaxed))
                };
                wrpc_transport::pack_ready(nonce, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_BLOCK_INSTANCE, "get-height") => {
                ensure_eof(body)
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let height = self.blocks.read().await.len();
                let height = u64::try_from(height)
                    .map_err(|err| RpcPostError::ResultEncoding(std::io::Error::other(err)))?;
                wrpc_transport::pack_ready(height, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_BLOCK_INSTANCE, "get-block") => {
                let height = read_params(body, U64Codec)
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let envelopes = {
                    let blocks = self.blocks.read().await;
                    usize::try_from(height)
                        .ok()
                        .and_then(|height| height.checked_sub(1))
                        .and_then(|i| blocks.get(i))
                        .map(|block| {
                            block
                                .actions
                                .iter()
                                .map(|action| match action {
                                    Action::UploadContract(envelope)
                                    | Action::FundAccount(envelope)
                                    | Action::Transaction(envelope) => envelope.clone(),
                                })
                                .collect::<Vec<_>>()
                        })
                };
                wrpc_transport::pack_ready(envelopes, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_CONTRACT_INSTANCE, "get-envelope") => {
                let digest = read_params(body, <CoreNameDecoder>::default())
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let envelope = {
                    let contracts = self.contracts.read().await;
                    get_by_digest(&contracts, &digest).map(|contract| contract.envelope.clone())
                };
                wrpc_transport::pack_ready(envelope, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_CONTRACT_INSTANCE, "get-wasm") => {
                let digest = read_params(body, <CoreNameDecoder>::default())
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let wasm = {
                    let contracts = self.contracts.read().await;
                    get_by_digest(&contracts, &digest).map(|contract| contract.wasm.clone())
                };
                wrpc_transport::pack_ready(wasm, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_TRANSACTION_INSTANCE, "get-envelope") => {
                let digest = read_params(body, <CoreNameDecoder>::default())
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let envelope = {
                    let txs = self.transactions.read().await;
                    get_by_digest(&txs, &digest).map(|tx| tx.envelope.clone())
                };
                wrpc_transport::pack_ready(envelope, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_TRANSACTION_INSTANCE, "get-transaction") => {
                let digest = read_params(body, <CoreNameDecoder>::default())
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let tx = {
                    let txs = self.transactions.read().await;
                    get_by_digest(&txs, &digest).map(|tx| tx.encoded.clone())
                };
                match tx {
                    Ok(tx) => {
                        // `result::ok` discriminant followed by the stored wRPC-encoded `transaction`
                        data.put_u8(0);
                        data.extend_from_slice(&tx);
                    }
                    Err(err) => wrpc_transport::pack_ready(Err::<(), _>(err), &mut data)
                        .map_err(RpcPostError::ResultEncoding)?,
                }
            }
            (LEDGER_GENESIS_INSTANCE, "get-outputs") => {
                ensure_eof(body)
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                data.extend_from_slice(&self.genesis.encoded);
            }
            (LEDGER_NETWORK_INSTANCE, "get-name") => {
                ensure_eof(body)
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                wrpc_transport::pack_ready(&*self.network, &mut data)
                    .map_err(RpcPostError::ResultEncoding)?;
            }
            (LEDGER_UTXO_INSTANCE, _) => {
                let tx = if body
                    .read_option_status()
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?
                {
                    let mut transaction = String::default();
                    body.read_core_name(&mut transaction)
                        .await
                        .map_err(RpcPostError::ParameterDecoding)?;
                    let transaction = parse_digest(&transaction)
                        .map_err(RpcPostError::TransactionDigestParsing)?;
                    Some(transaction)
                } else {
                    None
                };
                let index = body
                    .read_u32_leb128()
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;
                let index = usize::try_from(index).map_err(|_| RpcPostError::UtxoIndexOverflow)?;

                let utxo = self
                    .get_utxo(tx, index)
                    .await
                    .ok_or(RpcPostError::UtxoNotFound)?;
                let contract =
                    parse_digest(&utxo.contract).map_err(RpcPostError::ContractDigestParsing)?;
                let wasm = {
                    let contracts = self.contracts.read().await;
                    let contract = contracts
                        .get(&contract)
                        .ok_or(RpcPostError::ContractNotFound)?;
                    contract.wasm.clone()
                };
                let wasm = apply_state(&wasm, &utxo.state).map_err(RpcPostError::StateMerge)?;

                // TODO: Insert traps in place of all coordination script imports

                let mut imports = HashMap::default();
                let contract = self
                    .compile(&mut imports, None, &wasm)
                    .await
                    .map_err(RpcPostError::Runtime)?;

                let utxo_export = contract.get_utxo(&utxo.instance).map_err(|source| {
                    RpcPostError::UtxoInstanceNotFound {
                        instance: utxo.instance.clone(),
                        source,
                    }
                })?;
                let storage_export = utxo_export
                    .storage()
                    .ok_or(RpcPostError::UtxoStorageMissing)?;
                let method_export = contract
                    .get_utxo_method(&utxo_export, &format!("[method]utxo.{name}"))
                    .map_err(|source| RpcPostError::UtxoMethodNotFound {
                        instance: utxo.instance.clone(),
                        name,
                        source,
                    })?;

                let methods = utxo.methods.iter().copied().collect();
                let cx = Arc::new(UtxoCtx { methods });

                let mut storage = Val::Record(Vec::default());
                // TODO: use sync API
                read_value(
                    &mut utxo.storage.as_ref(),
                    &mut storage,
                    &Type::Record(storage_export.ty().clone()),
                )
                .await
                .map_err(RpcPostError::StorageDecoding)?;

                let param_tys = method_export.ty().params().skip(1);
                let mut params = vec![Val::Bool(false); param_tys.len() + 1];
                for (v, (_, ty)) in zip(&mut params[1..], param_tys) {
                    read_value(&mut body, v, &ty)
                        .await
                        .map_err(RpcPostError::ParameterDecoding)?;
                }

                ensure_eof(body)
                    .await
                    .map_err(RpcPostError::ParameterDecoding)?;

                let result_tys = method_export.ty().results();
                let mut results = vec![Val::Bool(false); result_tys.len()];

                let mut table = ResourceTable::default();
                let cx_res = table
                    .push(Arc::clone(&cx))
                    .map_err(RpcPostError::ResourceTable)?;

                let ctx = Ctx {
                    table,
                    cardano: self.cardano,
                };
                let mut store = wasmtime::Store::new(&self.engine, ctx);
                store.set_fuel(self.fuel).map_err(RpcPostError::Runtime)?;
                let cx_res = cx_res
                    .try_into_resource_any(&mut store)
                    .map_err(RpcPostError::Runtime)?;

                let contract = contract
                    .instantiate(&mut store)
                    .await
                    .map_err(RpcPostError::execution)?;
                let utxo = contract
                    .load_utxo(
                        &mut store,
                        &utxo_export,
                        storage_export,
                        cx,
                        [Val::Resource(cx_res), storage],
                    )
                    .await
                    .map_err(RpcPostError::execution)?;
                params[0] = Val::Resource(utxo.resource());
                utxo.call_method(&mut store, &method_export, &params, &mut results)
                    .await
                    .map_err(RpcPostError::execution)?;
                for (v, ty) in zip(results, result_tys) {
                    ValEncoder::new(&ty)
                        .encode(&v, &mut data)
                        .map_err(RpcPostError::CallResultEncoding)?;
                }
            }
            _ => return Err(RpcPostError::FunctionNotFound { instance, name }),
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
            .map_err(RpcPostError::FrameEncoding)?;
        http::Response::builder()
            .header(CONTENT_TYPE, APPLICATION_WRPC.to_string())
            .header(X_CONTENT_TYPE_OPTIONS, "nosniff")
            .body(http_body_util::Full::new(buf.freeze()))
            .map_err(RpcPostError::Http)
    }

    /// Bind `address` and return the future serving the ledger HTTP API.
    ///
    /// At most `max_requests` connections are served at once, and a connection
    /// is closed if it does not send a complete request head within
    /// `io_timeout` of opening or of finishing its previous response, if it
    /// sends no chunk of a request body for `io_timeout`, or if a response
    /// write to it makes no progress for `io_timeout`.
    ///
    /// Calling [`Notify::notify_one`] on the returned handle shuts the server
    /// down; [`Notify::notify_waiters`] is lost if it races the first poll of
    /// the future.
    #[instrument(skip_all)]
    pub async fn handle_http(
        self: Arc<Self>,
        address: SocketAddr,
        io_timeout: Duration,
    ) -> anyhow::Result<(impl Future<Output = ()> + use<>, Arc<Notify>)> {
        let sock = bind_tcp(address)?;
        let sock = sock
            .listen(self.max_requests)
            .context("failed to listen on TCP socket")?;

        let ledger = self.clone();
        let svc = service_fn(move |req: http::Request<hyper::body::Incoming>| {
            let ledger = ledger.clone();
            async move {
                let _permit = match ledger.permits.try_acquire() {
                    Ok(permit) => permit,
                    Err(TryAcquireError::NoPermits) => {
                        return build_text_response(
                            http::StatusCode::SERVICE_UNAVAILABLE,
                            "maximum concurrent request count reached",
                        );
                    }
                    Err(TryAcquireError::Closed) => {
                        return build_text_response(
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
                    return build_text_response(
                        http::StatusCode::BAD_REQUEST,
                        "request target unsupported",
                    );
                };
                let path = pq.path();
                let path = path
                    .strip_suffix('/')
                    .filter(|path| !path.is_empty())
                    .unwrap_or(path);
                let mut path = path.split('/');
                let Some("") = path.next() else {
                    return build_text_response(
                        http::StatusCode::BAD_REQUEST,
                        "request target unsupported",
                    );
                };
                match (
                    method.as_str(),
                    path.next(),
                    path.next(),
                    path.next(),
                    path.next(),
                ) {
                    ("GET", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_get::<false>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_get::<true>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("contracts"), Some(..), None, ..) => {
                        build_method_not_allowed("GET, HEAD", &method, pq.path())
                    }

                    ("GET", Some("transactions"), Some(digest), None, ..) => match ledger
                        .handle_transaction_get::<false>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("transactions"), Some(digest), None, ..) => match ledger
                        .handle_transaction_get::<true>(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("transactions"), Some(..), None, ..) => {
                        build_method_not_allowed("GET, HEAD", &method, pq.path())
                    }

                    ("POST", Some("rpc"), None, ..) => {
                        match ledger.handle_rpc_post(headers, body, io_timeout).await {
                            Ok(res) => Ok(res),
                            Err(err) => {
                                build_text_response(err.http_status_code(), err.to_string())
                            }
                        }
                    }
                    ("GET", Some("rpc"), None, ..) => match ledger
                        .handle_rpc_get::<false>(headers, pq.query())
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("rpc"), None, ..) => match ledger
                        .handle_rpc_get::<true>(headers, pq.query())
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("rpc"), None, ..) => {
                        build_method_not_allowed("GET, HEAD, POST", &method, pq.path())
                    }

                    _ => build_text_response(
                        http::StatusCode::NOT_FOUND,
                        format!("path `{}` not found", pq.path()),
                    ),
                }
            }
        });
        let shutdown = Arc::new(Notify::new());
        let mut srv = hyper::server::conn::http1::Builder::new();
        srv.timer(TokioTimer::new()).header_read_timeout(io_timeout);
        let max_connections = self.max_requests as usize;
        Ok((
            {
                let shutdown = Arc::clone(&shutdown).notified_owned();
                async move {
                    let graceful = GracefulShutdown::default();
                    let mut tasks = JoinSet::new();
                    let mut shutdown = pin!(shutdown);
                    loop {
                        match poll_fn(|cx| {
                            while let Poll::Ready(Some(res)) = tasks.poll_join_next(cx) {
                                if let Err(err) = res {
                                    error!(?err, "HTTP task panicked");
                                }
                            }
                            match shutdown.as_mut().poll(cx) {
                                Poll::Ready(()) => Poll::Ready(None),
                                // Postpone accepting until a connection task
                                // completes, which wakes this future via
                                // `poll_join_next` above.
                                Poll::Pending if tasks.len() >= max_connections => Poll::Pending,
                                Poll::Pending => {
                                    let res = ready!(sock.poll_accept(cx));
                                    Poll::Ready(Some(res))
                                }
                            }
                        })
                        .await
                        {
                            Some(Ok((stream, addr))) => {
                                info!(?addr, "accepted TCP connection");
                                let mut stream = TimeoutStream::new(stream);
                                stream.set_write_timeout(Some(io_timeout));
                                let conn = srv
                                    .serve_connection(TokioIo::new(Box::pin(stream)), svc.clone());
                                let conn = graceful.watch(conn);
                                tasks.spawn(
                                    async move {
                                        if let Err(err) = conn.await {
                                            if is_timeout(&err) {
                                                debug!(?err, "closed stalled HTTP connection");
                                            } else {
                                                warn!(?err, "failed to serve HTTP connection");
                                            }
                                        }
                                    }
                                    .in_current_span(),
                                );
                            }
                            Some(Err(err)) if is_connection_error(&err) => {
                                debug!(?err, "failed to accept TCP connection")
                            }
                            Some(Err(err)) => {
                                error!(?err, "failed to accept TCP connection");
                                sleep(Duration::from_secs(1)).await;
                            }
                            None => break,
                        }
                    }
                    graceful.shutdown().await;
                    while let Some(res) = tasks.join_next().await {
                        if let Err(err) = res {
                            error!(?err, "HTTP task panicked");
                        }
                    }
                }
                .in_current_span()
            },
            shutdown,
        ))
    }
}
