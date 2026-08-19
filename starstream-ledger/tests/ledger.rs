//! End-to-end tests for signed contract publishing and invocation.
//!
//! Each test starts a real `Ledger` HTTP server on a loopback socket and
//! drives it with a `hyper` client. A publish is a COSE_Sign1 envelope
//! (RFC 9052, Ed25519) whose payload is the CBOR publish transaction
//! `[nonce, wasm]`. The signer's raw 32-byte public key is the protected
//! `kid` header; its lowercase hex is the account identifier. Coordination
//! scripts and UTXO methods are invoked through the typed wRPC client API
//! (`InvokeExt::invoke_values_blocking` over `wrpc_http::Client`), addressed
//! by the `starstream:contract/<digest>` and `starstream:utxo/<digest>`
//! instances of the very WIT the ledger serves. The `X-Starstream-*`
//! response envelope and rejection statuses are not observable through the
//! wRPC client abstraction, so those are asserted over plain HTTP.

use core::net::{Ipv4Addr, SocketAddr};
use core::time::Duration;

use std::net::TcpListener;
use std::sync::{Arc, LazyLock};

use bytes::{Bytes, BytesMut};
use coset::{TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signer as _, SigningKey};
use http::header::{ACCEPT, CONTENT_TYPE};
use http::{Request, StatusCode};
use http_body_util::{BodyExt as _, Empty, Full};
use hyper_util::rt::{TokioExecutor, TokioIo};
use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_ledger::{
    CardanoCtx, FUND_ACCOUNT_CONTEXT, Ledger, PUBLISH_CONTEXT, X_STARSTREAM_BLOCK,
    X_STARSTREAM_TRANSACTION, X_STARSTREAM_UTXO, encode_digest,
};
use starstream_runtime_next::componentize;
use tokio::net::TcpStream;
use wrpc_transport::InvokeExt as _;

const NETWORK: &str = "starstream:test";

/// Compile a Starstream contract source to a Wasm component, the publishable
/// representation.
fn compile_contract(source: &str) -> Vec<u8> {
    let (program, errors) = parse_program(source).into_output_errors();
    assert!(errors.is_empty(), "parsing failed: {errors:?}");
    let program = program.expect("parser produced no program");
    let typed = typecheck_program(&program, TypecheckOptions::default())
        .unwrap_or_else(|failure| panic!("typechecking failed: {:?}", failure.errors));
    let result = starstream_to_wasm::compile(&typed.program);
    assert!(
        result.errors.is_empty(),
        "compiling failed: {:?}",
        result.errors
    );
    let wasm = result.wasm.expect("compiling produced no Wasm");
    componentize(wasm).expect("failed to componentize contract")
}

static SCORE: LazyLock<Vec<u8>> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")));

fn signing_key() -> SigningKey {
    SigningKey::from_bytes(&[0x42; 32])
}

fn other_signing_key() -> SigningKey {
    SigningKey::from_bytes(&[0x24; 32])
}

/// Start a `Ledger` HTTP server on an ephemeral loopback port with the admin
/// account funded for the given key, returning its address once it is bound
/// and listening.
async fn spawn_ledger(admin_key: &SigningKey, admin_balance: u64) -> SocketAddr {
    let admin_key = hex::encode(admin_key.verifying_key().to_bytes());
    let engine = wasmtime::Engine::default();
    let ledger = Ledger::new(
        engine,
        128,
        CardanoCtx::default(),
        NETWORK,
        admin_key,
        admin_balance,
    );
    let ledger = Arc::new(ledger);

    // Grab a free port, then let the ledger rebind it (SO_REUSEADDR).
    let addr = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))
        .unwrap()
        .local_addr()
        .unwrap();
    let server = ledger.handle_http(addr).await.unwrap();
    tokio::spawn(server);
    addr
}

/// Send a request over a fresh HTTP/1 connection and return the response
/// with its body collected.
async fn request<B>(addr: SocketAddr, req: Request<B>) -> http::Response<Bytes>
where
    B: hyper::body::Body + Send + 'static,
    B::Data: Send,
    B::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    let stream = TcpStream::connect(addr).await.unwrap();
    let (mut sender, conn) = hyper::client::conn::http1::handshake(TokioIo::new(stream))
        .await
        .unwrap();
    tokio::spawn(async move {
        let _ = conn.await;
    });
    let (parts, body) = sender.send_request(req).await.unwrap().into_parts();
    let body = body.collect().await.unwrap().to_bytes();
    http::Response::from_parts(parts, body)
}

/// Send a request over a fresh HTTP/1 connection and return the response
/// status and body.
async fn send<B>(addr: SocketAddr, req: Request<B>) -> (StatusCode, Bytes)
where
    B: hyper::body::Body + Send + 'static,
    B::Data: Send,
    B::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    let res = request(addr, req).await;
    (res.status(), res.into_body())
}

/// The CBOR publish transaction `[context, network, nonce, wasm]`.
fn publish_tx(context: &str, network: &str, nonce: u64, wasm: &[u8]) -> Vec<u8> {
    let mut payload = Vec::new();
    ciborium::into_writer(
        &ciborium::Value::Array(vec![
            ciborium::Value::Text(context.into()),
            ciborium::Value::Text(network.into()),
            ciborium::Value::Integer(nonce.into()),
            ciborium::Value::Bytes(wasm.to_vec()),
        ]),
        &mut payload,
    )
    .unwrap();
    payload
}

/// The CBOR fund-account transaction
/// `[context, network, nonce, account, amount]`.
fn fund_tx(context: &str, network: &str, nonce: u64, account: &[u8], amount: u64) -> Vec<u8> {
    let mut payload = Vec::new();
    ciborium::into_writer(
        &ciborium::Value::Array(vec![
            ciborium::Value::Text(context.into()),
            ciborium::Value::Text(network.into()),
            ciborium::Value::Integer(nonce.into()),
            ciborium::Value::Bytes(account.to_vec()),
            ciborium::Value::Integer(amount.into()),
        ]),
        &mut payload,
    )
    .unwrap();
    payload
}

/// A tagged COSE_Sign1 envelope over `payload`, signed with `key` and carrying
/// `kid` as the protected key ID.
fn sign_envelope(key: &SigningKey, kid: &[u8], payload: Vec<u8>) -> Vec<u8> {
    let protected = coset::HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(kid.to_vec())
        .build();
    coset::CoseSign1Builder::new()
        .protected(protected)
        .payload(payload)
        .create_signature(b"", |data| key.sign(data).to_bytes().to_vec())
        .build()
        .to_tagged_vec()
        .unwrap()
}

/// Build a PUT publishing `envelope` at the content-addressed URL of `wasm`.
fn put_contract(addr: SocketAddr, wasm: &[u8], envelope: Vec<u8>) -> Request<Full<Bytes>> {
    let digest = encode_digest(&Sha256::digest(wasm).into());
    Request::builder()
        .method("PUT")
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(Bytes::from(envelope)))
        .unwrap()
}

/// Build a POST funding the account at its hex-addressed URL with `envelope`.
fn post_fund(addr: SocketAddr, account: &str, envelope: Vec<u8>) -> Request<Full<Bytes>> {
    Request::builder()
        .method("POST")
        .uri(format!("http://{addr}/accounts/{account}"))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(Bytes::from(envelope)))
        .unwrap()
}

/// A typed wRPC client over the ledger HTTP API.
fn rpc_client() -> wrpc_http::Client<
    hyper_util::client::legacy::Client<
        hyper_util::client::legacy::connect::HttpConnector,
        wrpc_http::OutgoingBody,
    >,
> {
    wrpc_http::Client::new(
        hyper_util::client::legacy::Client::builder(TokioExecutor::new()).build_http(),
    )
}

/// The context of a wRPC invocation `POST`ed to the `/rpc` endpoint of the
/// ledger at `addr`.
fn rpc_context(addr: SocketAddr) -> http::request::Parts {
    let (parts, ()) = Request::builder()
        .uri(format!("http://{addr}/rpc"))
        .body(())
        .unwrap()
        .into_parts();
    parts
}

/// Build a wRPC invocation of `func` on `instance`, with no parameters,
/// `POST`ed to `/rpc` as plain HTTP: for asserting the parts of a response
/// the wRPC client abstraction does not expose — its status and its
/// `X-Starstream-*` headers.
fn post_invocation(
    addr: SocketAddr,
    headers: &[(&str, String)],
    instance: &str,
    func: &str,
) -> Request<Full<Bytes>> {
    let mut body = BytesMut::new();
    wrpc_transport::frame::encode_invocation(&mut body, instance, func, &[]).unwrap();
    let mut req = Request::builder()
        .method("POST")
        .uri(format!("http://{addr}/rpc"));
    for (name, value) in headers {
        req = req.header(*name, value);
    }
    req.body(Full::new(body.freeze())).unwrap()
}

/// A valid signature over a known key reaches the account lookup, proving the
/// whole verification pipeline ran; with no such account it is 403.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn valid_signature_reaches_account_lookup() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let key = signing_key();
    let wasm = b"\0asm\x01\0\0\0";
    let envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, wasm),
    );
    let (status, body) = send(addr, put_contract(addr, wasm, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::FORBIDDEN, "body: {body}");
    assert!(body.contains("account"), "unexpected body: {body}");
}

/// The full happy path, observed through the API alone: the publish succeeds,
/// replaying it fails on the bumped nonce, a further publish fails on the
/// charged balance, the wasm is served back, and the stored envelope can be
/// fetched and re-verified offline — the durable record another node could
/// validate.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn publish_charges_account_and_stores_envelope() {
    let key = signing_key();
    let wasm = wat::parse_str("(component)").unwrap();
    let next_wasm = wat::parse_str("(component (core module))").unwrap();
    assert_ne!(wasm, next_wasm);

    // Enough for the first publish, one byte short for the second.
    let balance = (wasm.len() + next_wasm.len() - 1) as u64;
    let addr = spawn_ledger(&key, balance).await;

    let envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, &wasm),
    );
    let (status, body) = send(addr, put_contract(addr, &wasm, envelope.clone())).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // Replaying the accepted transaction fails: the nonce moved on.
    let (status, body) = send(addr, put_contract(addr, &wasm, envelope.clone())).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::CONFLICT, "body: {body}");
    assert!(body.contains("nonce"), "unexpected body: {body}");

    // The first publish was charged: the remaining balance is short.
    let next_envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 2, &next_wasm),
    );
    let (status, body) = send(addr, put_contract(addr, &next_wasm, next_envelope)).await;
    assert_eq!(
        status,
        StatusCode::PAYMENT_REQUIRED,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    let digest = encode_digest(&Sha256::digest(&wasm).into());
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(ACCEPT, "application/wasm")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, wasm);

    // The envelope is the server-preferred representation.
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(ACCEPT, "*/*")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, envelope);

    // Quality values are respected.
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(ACCEPT, "application/wasm;q=0.5, application/cose")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, envelope);

    // Multiple `Accept` header lines are merged.
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(ACCEPT, "text/html")
        .header(ACCEPT, "application/wasm;q=0.5")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, wasm);

    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(ACCEPT, "text/html")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::NOT_ACCEPTABLE, "body: {body}");
    assert!(body.contains("application/wasm"), "unexpected body: {body}");

    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}"))
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, envelope);

    let sign1 = coset::CoseSign1::from_tagged_slice(&body).unwrap();
    let verifying_key = key.verifying_key();
    sign1
        .verify_signature(b"", |signature, data| {
            ed25519_dalek::Signature::from_slice(signature)
                .and_then(|signature| verifying_key.verify_strict(data, &signature))
        })
        .unwrap();
}

/// The full fund-account happy path: an admin-signed transfer creates a new
/// account, replaying it fails on the bumped admin nonce, the funded account
/// can pay for a publish, and the admin balance was debited by the transfer.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fund_account_transfers_and_enables_publishing() {
    let admin = signing_key();
    let other = other_signing_key();
    let other_account = hex::encode(other.verifying_key().to_bytes());
    let wasm = wat::parse_str("(component)").unwrap();

    // Exactly enough to fund the other account for one publish.
    let addr = spawn_ledger(&admin, wasm.len() as u64).await;

    let envelope = sign_envelope(
        &admin,
        admin.verifying_key().as_bytes(),
        fund_tx(
            FUND_ACCOUNT_CONTEXT,
            NETWORK,
            1,
            other.verifying_key().as_bytes(),
            wasm.len() as u64,
        ),
    );
    let (status, body) = send(addr, post_fund(addr, &other_account, envelope.clone())).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // Replaying the accepted transfer fails: the admin nonce moved on.
    let (status, body) = send(addr, post_fund(addr, &other_account, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::CONFLICT, "body: {body}");
    assert!(body.contains("nonce"), "unexpected body: {body}");

    // The funded account can pay for a publish...
    let envelope = sign_envelope(
        &other,
        other.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, &wasm),
    );
    let (status, body) = send(addr, put_contract(addr, &wasm, envelope)).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // ...while the transfer left the admin unable to pay for one.
    let next_wasm = wat::parse_str("(component (core module))").unwrap();
    let envelope = sign_envelope(
        &admin,
        admin.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 2, &next_wasm),
    );
    let (status, body) = send(addr, put_contract(addr, &next_wasm, envelope)).await;
    assert_eq!(
        status,
        StatusCode::PAYMENT_REQUIRED,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

/// Only the admin may fund accounts: a validly signed envelope from any
/// other key is rejected.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fund_account_requires_admin_signer() {
    let addr = spawn_ledger(&signing_key(), u64::MAX).await;
    let other = other_signing_key();
    let account = hex::encode(other.verifying_key().to_bytes());
    let envelope = sign_envelope(
        &other,
        other.verifying_key().as_bytes(),
        fund_tx(
            FUND_ACCOUNT_CONTEXT,
            NETWORK,
            1,
            other.verifying_key().as_bytes(),
            42,
        ),
    );
    let (status, body) = send(addr, post_fund(addr, &account, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::FORBIDDEN, "body: {body}");
    assert!(body.contains("admin"), "unexpected body: {body}");
}

/// Mismatched or malformed fund-account transactions are rejected without
/// consuming the admin nonce.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fund_account_rejections() {
    let admin = signing_key();
    let other = other_signing_key();
    let account = hex::encode(other.verifying_key().to_bytes());
    let addr = spawn_ledger(&admin, 100).await;

    // The signed account must match the addressed one.
    let envelope = sign_envelope(
        &admin,
        admin.verifying_key().as_bytes(),
        fund_tx(
            FUND_ACCOUNT_CONTEXT,
            NETWORK,
            1,
            admin.verifying_key().as_bytes(),
            42,
        ),
    );
    let (status, body) = send(addr, post_fund(addr, &account, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "body: {body}");
    assert!(body.contains("account"), "unexpected body: {body}");

    // A signature over another protocol's payload must not count as a
    // transfer.
    let envelope = sign_envelope(
        &admin,
        admin.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, b"\0asm\x01\0\0\0"),
    );
    let (status, body) = send(addr, post_fund(addr, &account, envelope)).await;
    assert_eq!(
        status,
        StatusCode::BAD_REQUEST,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // Transferring more than the admin balance is rejected.
    let envelope = sign_envelope(
        &admin,
        admin.verifying_key().as_bytes(),
        fund_tx(
            FUND_ACCOUNT_CONTEXT,
            NETWORK,
            1,
            other.verifying_key().as_bytes(),
            101,
        ),
    );
    let (status, body) = send(addr, post_fund(addr, &account, envelope)).await;
    assert_eq!(
        status,
        StatusCode::PAYMENT_REQUIRED,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // None of the rejections consumed the nonce: the transfer still goes
    // through with it.
    let envelope = sign_envelope(
        &admin,
        admin.verifying_key().as_bytes(),
        fund_tx(
            FUND_ACCOUNT_CONTEXT,
            NETWORK,
            1,
            other.verifying_key().as_bytes(),
            100,
        ),
    );
    let (status, body) = send(addr, post_fund(addr, &account, envelope)).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn raw_wasm_body_is_unsupported_media_type() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let body = b"\0asm\x01\0\0\0";
    let digest = encode_digest(&Sha256::digest(body).into());
    let req = Request::builder()
        .method("PUT")
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(CONTENT_TYPE, "application/wasm")
        .body(Full::new(Bytes::copy_from_slice(body)))
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(
        status,
        StatusCode::UNSUPPORTED_MEDIA_TYPE,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn garbage_envelope_is_bad_request() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let (status, body) = send(
        addr,
        put_contract(addr, b"\0asm\x01\0\0\0", b"not cose".to_vec()),
    )
    .await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "body: {body}");
    assert!(body.contains("COSE_Sign1"), "unexpected body: {body}");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn invalid_digest_is_bad_request() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let req = Request::builder()
        .method("PUT")
        .uri(format!("http://{addr}/contracts/not-a-digest"))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(Bytes::from_static(b"whatever")))
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(
        status,
        StatusCode::BAD_REQUEST,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

/// Signed with one key but advertising a different public key in `kid`.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wrong_key_is_unauthorized() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let wasm = b"\0asm\x01\0\0\0";
    let kid = other_signing_key().verifying_key().to_bytes();
    let envelope = sign_envelope(
        &signing_key(),
        &kid,
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, wasm),
    );
    let (status, body) = send(addr, put_contract(addr, wasm, envelope)).await;
    assert_eq!(
        status,
        StatusCode::UNAUTHORIZED,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

/// Keep the signature, swap the signed transaction.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn tampered_payload_is_unauthorized() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let key = signing_key();
    let wasm = b"\0asm\x01\0\0\0";
    let tampered = b"tampered payload";
    let envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, wasm),
    );
    let mut sign1 = coset::CoseSign1::from_tagged_slice(&envelope).unwrap();
    sign1.payload = Some(publish_tx(PUBLISH_CONTEXT, NETWORK, 1, tampered));
    let envelope = sign1.to_tagged_vec().unwrap();
    let (status, body) = send(addr, put_contract(addr, tampered, envelope)).await;
    assert_eq!(
        status,
        StatusCode::UNAUTHORIZED,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

/// A signature over another protocol's payload must not count as a publish.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wrong_context_is_rejected() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let key = signing_key();
    let wasm = b"\0asm\x01\0\0\0";
    let tx = publish_tx("starstream:other", NETWORK, 0, wasm);
    let envelope = sign_envelope(&key, key.verifying_key().as_bytes(), tx);
    let (status, body) = send(addr, put_contract(addr, wasm, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "body: {body}");
    assert!(body.contains("context"), "unexpected body: {body}");
}

/// A publish signed for one network must not replay on another.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn wrong_network_is_rejected() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let key = signing_key();
    let wasm = b"\0asm\x01\0\0\0";
    let tx = publish_tx(PUBLISH_CONTEXT, "starstream:mainnet", 0, wasm);
    let envelope = sign_envelope(&key, key.verifying_key().as_bytes(), tx);
    let (status, body) = send(addr, put_contract(addr, wasm, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "body: {body}");
    assert!(body.contains("network"), "unexpected body: {body}");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn missing_alg_is_bad_request() {
    let addr = spawn_ledger(&other_signing_key(), 0).await;
    let key = signing_key();
    let wasm = b"\0asm\x01\0\0\0";
    let protected = coset::HeaderBuilder::new()
        .key_id(key.verifying_key().to_bytes().to_vec())
        .build();
    let envelope = coset::CoseSign1Builder::new()
        .protected(protected)
        .payload(publish_tx(PUBLISH_CONTEXT, NETWORK, 0, wasm))
        .create_signature(b"", |data| key.sign(data).to_bytes().to_vec())
        .build()
        .to_tagged_vec()
        .unwrap();
    let (status, body) = send(addr, put_contract(addr, wasm, envelope)).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "body: {body}");
    assert!(body.contains("alg"), "unexpected body: {body}");
}

/// The full contract flow, driven through the HTTP API alone: publish the
/// compiled score contract, run its `example` coordination script — which
/// constructs a `ScoreProgress` UTXO persisted under its snapshot digest —
/// then invoke methods on that UTXO.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn score_contract_flow() {
    let key = signing_key();
    let wasm = SCORE.as_slice();
    let digest = encode_digest(&Sha256::digest(wasm).into());

    let addr = spawn_ledger(&key, wasm.len() as u64).await;

    let envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, wasm),
    );
    let (status, body) = send(addr, put_contract(addr, wasm, envelope)).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // The coordination-script ABI of the published contract is served as
    // WIT, textual by default.
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}/rpc"))
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    let wit = String::from_utf8(body.to_vec()).unwrap();
    assert_eq!(status, StatusCode::OK, "body: {wit}");
    assert_eq!(
        wit,
        format!(
            "package starstream:contract;
interface {digest} {{
  example: func();
}}
"
        )
    );

    // The same WIT is served as a Wasm-encoded package on request.
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}/rpc"))
        .header(ACCEPT, "application/wasm")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    let mut resolve = wit_parser::Resolve::new();
    let pkg = resolve.push_str("score.wit", &wit).unwrap();
    let expected = wit_component::encode(&resolve, pkg).unwrap();
    assert_eq!(body, expected);

    // `ScoreProgress::new()` in the script resolves through the UTXO import,
    // mapped back to this same contract; the script returns no results. The
    // resulting UTXO is persisted under the digest of its snapshot, carried
    // by the response header.
    let res = request(
        addr,
        post_invocation(
            addr,
            &[(X_STARSTREAM_UTXO, format!("score-progress={digest}"))],
            &format!("starstream:contract/{digest}"),
            "example",
        ),
    )
    .await;
    assert_eq!(
        res.status(),
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(res.body())
    );
    let utxos: Vec<_> = res.headers().get_all(X_STARSTREAM_UTXO).iter().collect();
    let [utxo] = utxos.as_slice() else {
        panic!("expected exactly one persisted UTXO, got {utxos:?}");
    };
    let utxo_digest = utxo.to_str().unwrap().to_string();
    assert_ne!(utxo_digest, digest);

    // The first invocation is transaction 0, recorded in the block after the
    // publish.
    let transaction = res.headers().get(X_STARSTREAM_TRANSACTION).unwrap();
    assert_eq!(transaction.to_str().unwrap(), "0");
    let block = res.headers().get(X_STARSTREAM_BLOCK).unwrap();
    assert_eq!(block.to_str().unwrap(), "2");

    // The ABI the persisted UTXO declared via `implements-method` is served
    // as WIT, textual by default.
    let req = Request::builder()
        .uri(format!("http://{addr}/utxos/{utxo_digest}/rpc"))
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    let wit = String::from_utf8(body.to_vec()).unwrap();
    assert_eq!(status, StatusCode::OK, "body: {wit}");
    assert_eq!(
        wit,
        format!(
            "package starstream:utxo;
interface {utxo_digest} {{
  plus-chips: func(chips2: u64);
  plus-mult: func(mult2: u64);
  mult-mult: func(mult-pct: u64);
  finish: func();
}}
"
        )
    );

    // The same WIT is served as a Wasm-encoded package on request.
    let req = Request::builder()
        .uri(format!("http://{addr}/utxos/{utxo_digest}/rpc"))
        .header(ACCEPT, "application/wasm")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    let mut resolve = wit_parser::Resolve::new();
    let pkg = resolve.push_str("score-progress.wit", &wit).unwrap();
    let expected = wit_component::encode(&resolve, pkg).unwrap();
    assert_eq!(body, expected);

    // The text representation is the server-preferred one.
    let req = Request::builder()
        .uri(format!("http://{addr}/utxos/{utxo_digest}/rpc"))
        .header(ACCEPT, "*/*")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    assert_eq!(status, StatusCode::OK);
    assert_eq!(body, wit);

    let req = Request::builder()
        .uri(format!("http://{addr}/utxos/{utxo_digest}/rpc"))
        .header(ACCEPT, "application/json")
        .body(Empty::<Bytes>::new())
        .unwrap();
    let (status, body) = send(addr, req).await;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::NOT_ACCEPTABLE, "body: {body}");
    assert!(body.contains("application/wasm"), "unexpected body: {body}");

    // Every method the UTXO implements is invocable against the persisted
    // snapshot through the typed wRPC client; none of them return results.
    let rpc = rpc_client();
    let utxo_instance = format!("starstream:utxo/{utxo_digest}");
    for (func, arg) in [("plus-chips", 7u64), ("plus-mult", 42), ("mult-mult", 200)] {
        let () = rpc
            .invoke_values_blocking(rpc_context(addr), &utxo_instance, func, (arg,), &[[]; 0])
            .await
            .unwrap_or_else(|err| panic!("`{func}` failed: {err:#}"));
    }
    let () = rpc
        .invoke_values_blocking(rpc_context(addr), &utxo_instance, "finish", (), &[[]; 0])
        .await
        .unwrap_or_else(|err| panic!("`finish` failed: {err:#}"));

    // A method the UTXO does not export is not found; the rejection precedes
    // parameter decoding.
    let (status, body) = send(
        addr,
        post_invocation(addr, &[], &utxo_instance, "no-such-method"),
    )
    .await;
    assert_eq!(
        status,
        StatusCode::NOT_FOUND,
        "body: {}",
        String::from_utf8_lossy(&body)
    );

    // An unknown UTXO digest is not found.
    let (status, body) = send(
        addr,
        post_invocation(
            addr,
            &[],
            &format!("starstream:utxo/{digest}"),
            "plus-chips",
        ),
    )
    .await;
    assert_eq!(
        status,
        StatusCode::NOT_FOUND,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

/// The compiled `starstream-ledger` binary, given an admin account via
/// `--admin-key` (funded with the default `u64::MAX` balance), accepts a
/// publish signed with the corresponding key.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn exe_funded_account_can_publish() {
    let key = SigningKey::from_bytes(&Sha256::digest("starstream:test:account:0").into());
    let account = hex::encode(key.verifying_key().to_bytes());

    // Grab a free port, then let the ledger rebind it (SO_REUSEADDR).
    let addr = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))
        .unwrap()
        .local_addr()
        .unwrap();
    let mut ledger = tokio::process::Command::new(env!("CARGO_BIN_EXE_starstream-ledger"))
        .args([
            "--network",
            NETWORK,
            "--addr",
            &addr.to_string(),
            "--admin-key",
            &account,
        ])
        .kill_on_drop(true)
        .spawn()
        .unwrap();
    for _ in 0..100 {
        if let Some(status) = ledger.try_wait().unwrap() {
            panic!("ledger exited early: {status}");
        }
        if TcpStream::connect(addr).await.is_ok() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    let wasm = wat::parse_str("(component)").unwrap();
    let envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, &wasm),
    );
    let (status, body) = send(addr, put_contract(addr, &wasm, envelope)).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}

/// The compiled `starstream-ledger` binary, run without `--admin-key`, uses
/// the well-known pre-seeded admin key and accepts a publish signed with it.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn exe_default_admin_key_can_publish() {
    let key = SigningKey::from_bytes(&Sha256::digest("admin").into());

    // Grab a free port, then let the ledger rebind it (SO_REUSEADDR).
    let addr = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))
        .unwrap()
        .local_addr()
        .unwrap();
    let mut ledger = tokio::process::Command::new(env!("CARGO_BIN_EXE_starstream-ledger"))
        .args(["--network", NETWORK, "--addr", &addr.to_string()])
        .kill_on_drop(true)
        .spawn()
        .unwrap();

    for _ in 0..100 {
        if let Some(status) = ledger.try_wait().unwrap() {
            panic!("ledger exited early: {status}");
        }
        if TcpStream::connect(addr).await.is_ok() {
            break;
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    let wasm = wat::parse_str("(component)").unwrap();
    let envelope = sign_envelope(
        &key,
        key.verifying_key().as_bytes(),
        publish_tx(PUBLISH_CONTEXT, NETWORK, 1, &wasm),
    );
    let (status, body) = send(addr, put_contract(addr, &wasm, envelope)).await;
    assert_eq!(
        status,
        StatusCode::OK,
        "body: {}",
        String::from_utf8_lossy(&body)
    );
}
