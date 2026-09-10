#![cfg(all(feature = "client", feature = "server"))]

use core::net::Ipv6Addr;
use core::str::FromStr as _;

use std::sync::{Arc, LazyLock};

use anyhow::{Context as _, anyhow, ensure};
use bytes::Bytes;
use coset::{CoseSign1Builder, HeaderBuilder, TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signer as _, SigningKey};
use http::header::{CONTENT_TYPE, VARY, X_CONTENT_TYPE_OPTIONS};
use http::{StatusCode, Uri};
use http_body_util::{BodyExt as _, Full};
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::TokioExecutor;
use sha2::{Digest as _, Sha256};
use starstream_compiler::typecheck::TypecheckSuccess;
use starstream_compiler::{TypecheckFailure, TypecheckOptions, parse_program, typecheck_program};
use starstream_ledger::client::build_publish_envelope;
use starstream_ledger::client::http::{
    ClientBuilder, build_contract_get_request, build_contract_publish_request, build_fund_request,
};
use starstream_ledger::encode_digest;
use starstream_ledger::server::Ledger;
use starstream_to_wasm::CompileResult;
use tokio::net::TcpListener;
use wit_component::ComponentEncoder;

fn compile_contract(source: &str) -> anyhow::Result<Vec<u8>> {
    let (program, errs) = parse_program(source).into_output_errors();
    ensure!(errs.is_empty(), "failed to parse program: {errs:?}");
    let program = program.context("parser did not produce a program")?;

    let TypecheckSuccess { program, .. } = typecheck_program(&program, TypecheckOptions::default())
        .map_err(|TypecheckFailure { errors, .. }| {
            anyhow!("failed to typecheck program: {:?}", errors)
        })?;

    let CompileResult { errors, wasm, .. } = starstream_to_wasm::compile(&program);
    ensure!(errors.is_empty(), "failed to compile program: {errors:?}");

    let wasm = wasm.context("compilation did not produce Wasm")?;
    ComponentEncoder::default()
        .validate(true)
        .module(&wasm)
        .context("failed to set core component module")?
        .encode()
        .context("failed to encode a component")
}

const NETWORK: &str = "starstream:test";

fn build_importing_component(contract_id: &str) -> Vec<u8> {
    use wasm_encoder::{
        ComponentImportSection, ComponentTypeRef, ComponentTypeSection, ComponentValType,
        InstanceType, TypeBounds,
    };

    let mut component = wasm_encoder::Component::new();

    let mut types = ComponentTypeSection::new();
    let mut utxo = InstanceType::new();
    utxo.export("utxo", ComponentTypeRef::Type(TypeBounds::SubResource));
    types.instance(&utxo);
    let mut scripts = InstanceType::new();
    scripts
        .ty()
        .function()
        .params([] as [(&str, ComponentValType); 0])
        .result(None);
    scripts.export("example", ComponentTypeRef::Func(0));
    types.instance(&scripts);
    component.section(&types);

    let mut imports = ComponentImportSection::new();
    imports.import(
        format!("starstream:contract/{contract_id}/utxo/score-progress"),
        ComponentTypeRef::Instance(0),
    );
    imports.import(
        format!("starstream:contract/{contract_id}/scripts"),
        ComponentTypeRef::Instance(1),
    );
    component.section(&imports);

    component.finish()
}

static SCORE_WASM: LazyLock<Vec<u8>> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")).unwrap());
static SCORE_WASM_DIGEST: LazyLock<[u8; 32]> =
    LazyLock::new(|| Sha256::digest(&*SCORE_WASM).into());

static ADMIN: LazyLock<SigningKey> = LazyLock::new(|| SigningKey::from_bytes(&[0x42; 32]));

async fn http_request(
    client: &hyper_util::client::legacy::Client<HttpConnector, Full<Bytes>>,
    req: http::Request<Full<Bytes>>,
) -> anyhow::Result<(http::response::Parts, Bytes)> {
    let res = client
        .request(req)
        .await
        .context("failed to send request")?;
    let (parts, body) = res.into_parts();
    let body = body
        .collect()
        .await
        .context("failed to receive response body")?;
    Ok((parts, body.to_bytes()))
}

#[tokio::test]
async fn http() -> anyhow::Result<()> {
    let addr = {
        let lis = TcpListener::bind((Ipv6Addr::LOCALHOST, 0))
            .await
            .context("failed to bind TCP listener")?;
        lis.local_addr()
            .context("failed to get TCP listener local address")?
    };

    let mut config = wasmtime::Config::default();
    config.wasm_component_model_implements(true);
    config.wasm_component_model_nested_names(true);
    let engine = wasmtime::Engine::new(&config)?;

    let ledger = Ledger::new(engine, 128, NETWORK, ADMIN.verifying_key());
    let ledger = Arc::new(ledger);
    let (ledger, shutdown) = ledger
        .handle_http(addr)
        .await
        .context("failed to handle HTTP")?;
    let ledger = tokio::spawn(ledger);

    let http = hyper_util::client::legacy::Client::builder(TokioExecutor::new()).build_http();
    let api_base = Uri::from_str(&format!("http://{addr}"))?;
    let client = ClientBuilder::new(http.clone(), api_base.clone())
        .network(NETWORK)
        .build();

    let score_publish_envelope =
        build_publish_envelope(ADMIN.clone(), NETWORK, 1, SCORE_WASM.as_slice())?;
    let score_publish_req = build_contract_publish_request(
        &api_base,
        ADMIN.clone(),
        NETWORK,
        1,
        SCORE_WASM.as_slice(),
    )?;

    let (http::response::Parts { status, .. }, body) =
        http_request(&http, score_publish_req.clone()).await?;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::PAYMENT_REQUIRED, "{body}");
    assert_eq!(
        body,
        "account ID `2152f8d19b791d24453242e15f2eab6cb7cffa7b6a5ed30097960e069881db12` not found"
    );

    let protected = HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(ADMIN.verifying_key().to_bytes().into())
        .add_critical(iana::HeaderParameter::Alg)
        .build();
    let crit_envelope = CoseSign1Builder::new()
        .protected(protected)
        .payload(Vec::default())
        .create_signature(b"", |data| ADMIN.sign(data).to_bytes().into())
        .build()
        .to_tagged_vec()
        .context("failed to serialize envelope")?;
    let req = http::Request::builder()
        .method(http::Method::PUT)
        .uri(format!(
            "http://{addr}/contracts/{}",
            encode_digest(&SCORE_WASM_DIGEST)
        ))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(Bytes::from(crit_envelope)))?;
    let (http::response::Parts { status, .. }, body) = http_request(&http, req).await?;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert_eq!(body, "envelope must not contain critical headers");

    let score_publish_cost = score_publish_envelope.len();
    let balance = score_publish_cost.saturating_sub(1000);

    client
        .fund(ADMIN.clone(), 1, &ADMIN.verifying_key(), balance as _)
        .await?;

    let req = build_fund_request(
        &api_base,
        ADMIN.clone(),
        NETWORK,
        1,
        &ADMIN.verifying_key(),
        balance as _,
    )?;
    let (http::response::Parts { status, .. }, body) = http_request(&http, req).await?;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::CONFLICT, "{body}");
    assert_eq!(body, "nonce must be higher than 1, got 1");

    let (http::response::Parts { status, .. }, body) =
        http_request(&http, score_publish_req.clone()).await?;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::PAYMENT_REQUIRED, "{body}");
    assert_eq!(
        body,
        format!(
            "balance insufficient, required at least {score_publish_cost}, available {balance}"
        )
    );
    let (http::response::Parts { status, .. }, body) =
        http_request(&http, score_publish_req).await?;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::CONFLICT, "{body}");
    assert_eq!(body, "nonce must be higher than 1, got 1");

    client
        .fund(
            ADMIN.clone(),
            2,
            &ADMIN.verifying_key(),
            (score_publish_cost - balance) as _,
        )
        .await?;

    client
        .publish_contract(ADMIN.clone(), 2, SCORE_WASM.as_slice())
        .await?;

    let wasm = client.get_contract_wasm(&SCORE_WASM_DIGEST).await?;
    assert_eq!(wasm, *SCORE_WASM);

    let score_publish_envelope =
        build_publish_envelope(ADMIN.clone(), NETWORK, 2, SCORE_WASM.as_slice())?;

    let envelope = client.get_contract_envelope(&SCORE_WASM_DIGEST).await?;
    assert_eq!(envelope, score_publish_envelope);

    let req = build_contract_get_request(&api_base, &SCORE_WASM_DIGEST, None)?;
    let (
        http::response::Parts {
            status, headers, ..
        },
        body,
    ) = http_request(&http, req).await?;
    assert_eq!(status, StatusCode::OK, "{}", String::from_utf8_lossy(&body));
    assert_eq!(body, score_publish_envelope);
    assert_eq!(
        headers.get(VARY).map(|v| v.as_bytes()),
        Some(b"accept".as_slice())
    );
    assert_eq!(
        headers.get(X_CONTENT_TYPE_OPTIONS).map(|v| v.as_bytes()),
        Some(b"nosniff".as_slice())
    );

    let importing_wasm = build_importing_component(&encode_digest(&SCORE_WASM_DIGEST));
    let importing_cost =
        build_publish_envelope(ADMIN.clone(), NETWORK, 3, importing_wasm.as_slice())?.len();
    client
        .fund(
            ADMIN.clone(),
            3,
            &ADMIN.verifying_key(),
            (importing_cost * 2 + 1024) as _,
        )
        .await?;
    client
        .publish_contract(ADMIN.clone(), 3, importing_wasm.as_slice())
        .await
        .context("failed to publish contract importing score")?;

    let unknown_id = encode_digest(&[0x55; 32]);
    let missing_import_wasm = build_importing_component(&unknown_id);
    let req = build_contract_publish_request(
        &api_base,
        ADMIN.clone(),
        NETWORK,
        4,
        missing_import_wasm.as_slice(),
    )?;
    let (http::response::Parts { status, .. }, body) = http_request(&http, req).await?;
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::NOT_FOUND, "{body}");
    assert_eq!(body, format!("imported contract `{unknown_id}` not found"));

    shutdown.notify_one();
    ledger.await.context("ledger task panicked")
}
