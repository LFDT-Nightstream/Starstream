use core::future::poll_fn;
use core::net::SocketAddr;
use core::pin::pin;
use core::sync::atomic::{AtomicU64, Ordering};
use core::task::{Poll, ready};
use core::time::Duration;

use std::collections::{HashMap, hash_map};
use std::sync::Arc;

use anyhow::Context as _;
use bytes::{Buf, Bytes};
use coset::{CborSerializable as _, CoseSign1, TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signature, VerifyingKey};
use headers_accept::Accept;
use headers_core::Header as _;
use http::HeaderValue;
use http::header::{ACCEPT, ALLOW, CONTENT_LENGTH, CONTENT_TYPE, X_CONTENT_TYPE_OPTIONS};
use http_body::Body as _;
use http_body_util::BodyExt as _;
use hyper::service::service_fn;
use hyper_util::rt::{TokioExecutor, TokioIo};
use hyper_util::server::graceful::GracefulShutdown;
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use tokio::net::TcpSocket;
use tokio::sync::{Notify, TryAcquireError};
use tokio::task::JoinSet;
use tokio::time::sleep;
use tracing::{Instrument as _, debug, error, info, instrument, warn};

use crate::server::lookup::ContractLookup;
use crate::server::{Contract, Ledger};
use crate::{
    APPLICATION_COSE, APPLICATION_WASM, Action, Block, FUND_CONTEXT, PUBLISH_CONTEXT, parse_digest,
};

mod error;
use error::*;

const MAX_CONTRACT_PUT_BODY_SIZE: u64 = 1 << 20;
const MAX_FUND_POST_BODY_SIZE: u64 = 1 << 10;

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

async fn read_signed_envelope<const MAX: u64>(
    headers: &http::HeaderMap,
    body: hyper::body::Incoming,
) -> Result<(Vec<u8>, VerifyingKey, Option<Vec<u8>>), EnvelopeReadError> {
    debug_assert!(usize::try_from(MAX).is_ok());

    match headers.get(CONTENT_TYPE).map(HeaderValue::to_str) {
        None => return Err(EnvelopeReadError::ContentTypeMissing),
        Some(Ok(ct)) => match MediaType::parse(ct) {
            Ok(ct) if ct.essence() == APPLICATION_COSE => {}
            Ok(ct) => {
                return Err(EnvelopeReadError::UnsupportedContentType(
                    ct.to_string().into(),
                ));
            }
            Err(err) => return Err(EnvelopeReadError::ContentTypeParsing(err)),
        },
        Some(Err(err)) => return Err(EnvelopeReadError::ContentTypeToStr(err)),
    }

    if body.size_hint().lower() > MAX {
        return Err(EnvelopeReadError::BodyTooLarge(MAX));
    }
    let envelope = http_body_util::Limited::new(body, MAX as _)
        .collect()
        .await
        .map_err(|err| {
            if err.is::<http_body_util::LengthLimitError>() {
                EnvelopeReadError::BodyTooLarge(MAX)
            } else {
                EnvelopeReadError::Body(err)
            }
        })?
        .to_bytes();
    let sign1 = CoseSign1::from_tagged_slice(&envelope)
        .or_else(|_| CoseSign1::from_slice(&envelope))
        .map_err(EnvelopeReadError::CoseSign1Parsing)?;
    if !sign1.unprotected.is_empty() {
        return Err(EnvelopeReadError::UnprotectedHeader);
    }
    if sign1.protected.header.alg != Some(coset::Algorithm::Assigned(iana::Algorithm::EdDSA)) {
        return Err(EnvelopeReadError::Algorithm);
    }
    let key = <[u8; 32]>::try_from(sign1.protected.header.key_id.as_slice())
        .map_err(|_| EnvelopeReadError::KeyIdFormat)?;
    let key = VerifyingKey::from_bytes(&key).map_err(EnvelopeReadError::Key)?;
    sign1
        .verify_signature(b"", |sig, data| {
            Signature::from_slice(sig).and_then(|sig| key.verify_strict(data, &sig))
        })
        .map_err(EnvelopeReadError::SignatureVerification)?;
    let payload = sign1.payload.clone();

    // Reencode the envelope in a canonical form
    let envelope = sign1.to_tagged_vec().map_err(EnvelopeReadError::Reencode)?;
    Ok((envelope, key, payload))
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

fn build_text_response<T>(
    code: http::StatusCode,
    body: impl Into<T>,
) -> http::Result<http::Response<http_body_util::Full<T>>>
where
    T: Buf + Sync + Send + 'static,
{
    http::Response::builder()
        .status(code)
        .header(CONTENT_TYPE, "text/plain; charset=utf-8")
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
        .header(CONTENT_TYPE, "text/plain; charset=utf-8")
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

impl Ledger {
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
        } else {
            http::Response::builder()
                .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
                .body(http_body_util::Full::new(contract.envelope.clone()))
        }
        .map_err(ContractGetError::Http)
    }

    async fn handle_contract_head(
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
                .header(CONTENT_LENGTH, contract.wasm.len())
        } else {
            http::Response::builder()
                .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
                .header(CONTENT_LENGTH, contract.envelope.len())
        }
        .body(http_body_util::Full::default())
        .map_err(ContractGetError::Http)
    }

    async fn handle_contract_put(
        &self,
        headers: http::HeaderMap,
        digest: &str,
        body: hyper::body::Incoming,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, ContractPutError> {
        let digest = parse_digest(digest).map_err(ContractPutError::DigestParsing)?;

        let (envelope, account, payload) =
            read_signed_envelope::<MAX_CONTRACT_PUT_BODY_SIZE>(&headers, body)
                .await
                .map_err(ContractPutError::Envelope)?;
        let payload = payload.as_deref().ok_or(ContractPutError::PayloadMissing)?;
        let payload = match ciborium::from_reader(payload) {
            Ok(ciborium::Value::Array(payload)) => payload,
            Ok(..) => return Err(ContractPutError::TransactionFormat),
            Err(err) => return Err(ContractPutError::PayloadParsing(err)),
        };
        let Ok(
            [
                ciborium::Value::Text(context),
                ciborium::Value::Text(network),
                ciborium::Value::Integer(nonce),
                ciborium::Value::Bytes(wasm),
            ],
        ) = <[_; _]>::try_from(payload)
        else {
            return Err(ContractPutError::TransactionFormat);
        };
        if context != PUBLISH_CONTEXT {
            return Err(ContractPutError::Context(context.into()));
        }
        if network != *self.network {
            return Err(ContractPutError::Network {
                got: network.into(),
                expected: Arc::clone(&self.network),
            });
        }
        let nonce = u64::try_from(nonce).map_err(|_| ContractPutError::NonceOverflow)?;
        let wasm_digest: [u8; 32] = Sha256::digest(&wasm).into();
        if wasm_digest != digest {
            return Err(ContractPutError::DigestMismatch(wasm_digest));
        }

        {
            let accounts = self.accounts.read().await;
            let Some(account) = accounts.get(&account) else {
                return Err(ContractPutError::AccountNotFound(account));
            };
            if let Err(last_nonce) = try_update_nonce(&account.last_nonce, nonce) {
                return Err(ContractPutError::NonceTooLow { last_nonce, nonce });
            }
            let required = envelope.len() as _;
            if let Err(available) =
                account
                    .balance
                    .try_update(Ordering::Relaxed, Ordering::Relaxed, |balance| {
                        balance.checked_sub(required)
                    })
            {
                return Err(ContractPutError::InsufficientBalance {
                    required,
                    available,
                });
            }
        }

        let envelope = Bytes::from(envelope);
        {
            let contracts = self.contracts.read().await;
            if contracts.contains_key(&digest) {
                return build_text_response(http::StatusCode::OK, "")
                    .map_err(ContractPutError::Http);
            }

            // TODO: Split component

            let (_wizer_cx, contract_wasm) = self
                .wizer
                .instrument_component(&wasm)
                .map_err(ContractPutError::Wizer)?;
            let contract = starstream_runtime_next::Contract::new(
                &self.engine,
                ContractLookup(&contracts),
                &contract_wasm,
            )
            .map_err(ContractPutError::Runtime)?;
            drop(contracts);

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

            let mut contracts = self.contracts.write().await;
            let hash_map::Entry::Vacant(entry) = contracts.entry(digest) else {
                return build_text_response(http::StatusCode::OK, "")
                    .map_err(ContractPutError::Http);
            };
            entry.insert(Arc::new(Contract {
                contract,
                contract_wasm: contract_wasm.into(),
                scripts,
                utxos,
                wasm: wasm.into(),
                envelope: envelope.clone(),
            }));
        }
        {
            let mut blocks = self.blocks.write().await;
            let height = blocks.len().saturating_add(1);
            blocks.push(Block {
                actions: Box::from([Action::UploadContract(envelope)]),
                height,
            });
        }
        build_text_response(http::StatusCode::OK, "").map_err(ContractPutError::Http)
    }

    async fn handle_fund_post(
        &self,
        headers: http::HeaderMap,
        body: hyper::body::Incoming,
    ) -> Result<http::Response<http_body_util::Full<Bytes>>, AccountFundError> {
        let (envelope, key, payload) =
            read_signed_envelope::<MAX_FUND_POST_BODY_SIZE>(&headers, body)
                .await
                .map_err(AccountFundError::Envelope)?;
        let payload = payload.as_deref().ok_or(AccountFundError::PayloadMissing)?;
        let payload = match ciborium::from_reader(payload) {
            Ok(ciborium::Value::Array(tx)) => tx,
            Ok(..) => return Err(AccountFundError::TransactionFormat),
            Err(err) => return Err(AccountFundError::PayloadParsing(err)),
        };
        let Ok(
            [
                ciborium::Value::Text(context),
                ciborium::Value::Text(network),
                ciborium::Value::Integer(nonce),
                ciborium::Value::Bytes(account),
                ciborium::Value::Integer(amount),
            ],
        ) = <[_; _]>::try_from(payload)
        else {
            return Err(AccountFundError::TransactionFormat);
        };

        if context != FUND_CONTEXT {
            return Err(AccountFundError::Context(context.into()));
        }
        if network != *self.network {
            return Err(AccountFundError::Network {
                got: network.into(),
                expected: Arc::clone(&self.network),
            });
        }
        let nonce = u64::try_from(nonce).map_err(|_| AccountFundError::NonceOverflow)?;
        let account =
            <[u8; 32]>::try_from(account.as_slice()).map_err(|_| AccountFundError::KeyIdFormat)?;
        let account = VerifyingKey::from_bytes(&account).map_err(AccountFundError::Key)?;
        if account.is_weak() {
            return Err(AccountFundError::WeakKey);
        }
        let amount = u64::try_from(amount).map_err(|_| AccountFundError::AmountOverflow)?;

        if key != self.admin.key {
            return Err(AccountFundError::NotAdmin(key));
        }
        if let Err(last_nonce) = try_update_nonce(&self.admin.last_nonce, nonce) {
            return Err(AccountFundError::NonceTooLow { last_nonce, nonce });
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
            let height = blocks.len().saturating_add(1);
            blocks.push(Block {
                actions: Box::from([Action::FundAccount(envelope.into())]),
                height,
            });
        }
        build_text_response(http::StatusCode::OK, "").map_err(AccountFundError::Http)
    }

    /// Bind `address` and return the future serving the ledger HTTP API.
    #[instrument(skip_all)]
    pub async fn handle_http(
        self: Arc<Self>,
        address: SocketAddr,
    ) -> anyhow::Result<(impl Future<Output = ()> + use<>, Arc<Notify>)> {
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
                        .handle_contract_get(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("HEAD", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_head(headers, digest)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    ("PUT", Some("contracts"), Some(digest), None, ..) => match ledger
                        .handle_contract_put(headers, digest, body)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("contracts"), Some(..), None, ..) => {
                        build_method_not_allowed("GET, HEAD, PUT", &method, pq.path())
                    }

                    ("POST", Some("fund"), None, ..) => match ledger
                        .handle_fund_post(headers, body)
                        .await
                    {
                        Ok(res) => Ok(res),
                        Err(err) => build_text_response(err.http_status_code(), err.to_string()),
                    },
                    (_, Some("fund"), None, ..) => {
                        build_method_not_allowed("POST", &method, pq.path())
                    }

                    _ => build_text_response(
                        http::StatusCode::NOT_FOUND,
                        format!("path `{}` not found", pq.path()),
                    ),
                }
            }
        });
        let shutdown = Arc::new(Notify::new());
        let srv = hyper_util::server::conn::auto::Builder::<TokioExecutor>::default();
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
                                let conn = srv.serve_connection(TokioIo::new(stream), svc.clone());
                                let conn = graceful.watch(conn.into_owned());
                                tasks.spawn(
                                    async move {
                                        if let Err(err) = conn.await {
                                            warn!(?err, "failed to serve HTTP connection");
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
