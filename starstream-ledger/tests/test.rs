//! End-to-end tests for signed contract publishing and invocation.
//!
//! Each test starts a real `Ledger` HTTP server on a loopback socket and
//! drives it with a `hyper` client. A publish is a COSE_Sign1 envelope
//! (RFC 9052, Ed25519) whose payload is the CBOR publish transaction
//! `[nonce, wasm]`. The signer's raw 32-byte public key is the protected
//! `kid` header; its lowercase hex is the account identifier. Coordination
//! scripts and UTXO methods are invoked over wRPC framing.

use core::net::{Ipv4Addr, SocketAddr};
use core::time::Duration;

use std::collections::HashMap;
use std::net::TcpListener;
use std::sync::LazyLock;

use bytes::Bytes;
use coset::{TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signer as _, SigningKey};
use http::header::{ACCEPT, CONTENT_TYPE};
use http::{Request, StatusCode};
use http_body_util::{BodyExt as _, Empty, Full};
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::{TokioExecutor, TokioIo};
use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_ledger::{Account, CardanoCtx, Ledger, PUBLISH_CONTEXT, X_STARSTREAM_UTXO};
use starstream_runtime_next::componentize;
use tokio::net::TcpStream;
use wrpc_transport::{InvokeExt as _, TupleDecode, TupleEncode};

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

/// Start a `Ledger` HTTP server on an ephemeral loopback port with the given
/// initial accounts, returning its address once it is bound and listening.
async fn spawn_ledger(accounts: HashMap<Box<str>, Account>) -> SocketAddr {
    let engine = wasmtime::Engine::default();
    let ledger = Ledger::new(engine, 128, CardanoCtx::default(), NETWORK, accounts);

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
/// status and body.
async fn send<B>(addr: SocketAddr, req: Request<B>) -> (StatusCode, Bytes)
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
    let resp = sender.send_request(req).await.unwrap();
    let status = resp.status();
    let body = resp.into_body().collect().await.unwrap().to_bytes();
    (status, body)
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
    let digest = hex::encode(Sha256::digest(wasm));
    Request::builder()
        .method("PUT")
        .uri(format!("http://{addr}/contracts/{digest}"))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(Bytes::from(envelope)))
        .unwrap()
}

type WrpcClient =
    wrpc_http::Client<hyper_util::client::legacy::Client<HttpConnector, wrpc_http::OutgoingBody>>;

fn wrpc_client() -> WrpcClient {
    wrpc_http::Client::new(
        hyper_util::client::legacy::Client::builder(TokioExecutor::new()).build_http(),
    )
}

/// Invoke `func` over wRPC, addressed by the request `req` (URI and headers),
/// with statically typed parameters and results.
async fn invoke<Params, Results>(
    client: &WrpcClient,
    req: Request<()>,
    func: &str,
    params: Params,
) -> anyhow::Result<Results>
where
    Params: TupleEncode + Send,
    Results: TupleDecode + Send,
    <Params::Encoder as tokio_util::codec::Encoder<Params>>::Error:
        std::error::Error + Send + Sync + 'static,
    <Results::Decoder as tokio_util::codec::Decoder>::Error:
        std::error::Error + Send + Sync + 'static,
{
    let (parts, ()) = req.into_parts();
    let paths: [&[Option<usize>]; 0] = [];
    let (results, io) = client.invoke_values(parts, "", func, params, paths).await?;
    if let Some(io) = io {
        io.await?;
    }
    Ok(results)
}

/// Invoke an export of the UTXO persisted by transaction `tx` at index `utxo`.
async fn call_utxo<Params, Results>(
    client: &WrpcClient,
    addr: SocketAddr,
    tx: usize,
    utxo: usize,
    name: &str,
    params: Params,
) -> anyhow::Result<Results>
where
    Params: TupleEncode + Send,
    Results: TupleDecode + Send,
    <Params::Encoder as tokio_util::codec::Encoder<Params>>::Error:
        std::error::Error + Send + Sync + 'static,
    <Results::Decoder as tokio_util::codec::Decoder>::Error:
        std::error::Error + Send + Sync + 'static,
{
    let req = Request::builder()
        .uri(format!("http://{addr}/transactions/{tx}/utxos/{utxo}/rpc"))
        .body(())
        .unwrap();
    invoke(client, req, name, params).await
}

/// A valid signature over a known key reaches the account lookup, proving the
/// whole verification pipeline ran; with no such account it is 403.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn valid_signature_reaches_account_lookup() {
    let addr = spawn_ledger(HashMap::default()).await;
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
    let account = hex::encode(key.verifying_key().to_bytes());
    let balance = (wasm.len() + next_wasm.len() - 1) as u64;
    let addr = spawn_ledger(HashMap::from([(
        account.into(),
        Account {
            balance,
            last_nonce: 0,
        },
    )]))
    .await;

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

    let digest = hex::encode(Sha256::digest(&wasm));
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

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn raw_wasm_body_is_unsupported_media_type() {
    let addr = spawn_ledger(HashMap::default()).await;
    let body = b"\0asm\x01\0\0\0";
    let digest = hex::encode(Sha256::digest(body));
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
    let addr = spawn_ledger(HashMap::default()).await;
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
async fn non_hex_digest_is_bad_request() {
    let addr = spawn_ledger(HashMap::default()).await;
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
    let addr = spawn_ledger(HashMap::default()).await;
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
    let addr = spawn_ledger(HashMap::default()).await;
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
    let addr = spawn_ledger(HashMap::default()).await;
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
    let addr = spawn_ledger(HashMap::default()).await;
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
    let addr = spawn_ledger(HashMap::default()).await;
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
/// constructs a `ScoreProgress` UTXO persisted as transaction 0 — then invoke
/// exports on that UTXO.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn score_contract_flow() {
    let key = signing_key();
    let wasm = SCORE.as_slice();
    let digest = hex::encode(Sha256::digest(wasm));

    let account = hex::encode(key.verifying_key().to_bytes());
    let addr = spawn_ledger(HashMap::from([(
        account.into(),
        Account {
            balance: wasm.len() as u64,
            last_nonce: 0,
        },
    )]))
    .await;

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

    // `ScoreProgress::new()` in the script resolves through the UTXO import,
    // mapped back to this same contract; the script returns no results. The
    // resulting UTXO is persisted as transaction 0.
    let client = wrpc_client();
    let req = Request::builder()
        .uri(format!("http://{addr}/contracts/{digest}/rpc"))
        .header(X_STARSTREAM_UTXO, format!("score-progress={digest}"))
        .body(())
        .unwrap();
    let () = invoke(&client, req, "example", ()).await.unwrap();

    let () = call_utxo(&client, addr, 0, 0, "plus-chips", (7u64,))
        .await
        .unwrap();

    let () = call_utxo(&client, addr, 0, 0, "plus-mult", (42u64,))
        .await
        .unwrap();

    let () = call_utxo(&client, addr, 0, 0, "mult-mult", (200u64,))
        .await
        .unwrap();

    let () = call_utxo(&client, addr, 0, 0, "finish", ()).await.unwrap();
}

/// The compiled `starstream-ledger` binary, given a funded account via
/// `--account`, accepts a publish signed with the corresponding key.
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
            "--account",
            &format!("{account}={}", 1u64 << 40),
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
