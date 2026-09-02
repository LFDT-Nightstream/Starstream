#![cfg(all(feature = "client", feature = "server"))]

use core::net::Ipv6Addr;
use core::str::FromStr as _;

use std::sync::{Arc, LazyLock};

use anyhow::Context as _;
use bytes::Bytes;
use ed25519_dalek::SigningKey;
use http::{StatusCode, Uri};
use http_body_util::{BodyExt as _, Full};
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::TokioExecutor;
use sha2::{Digest as _, Sha256};
use starstream_ledger::client::build_publish_envelope;
use starstream_ledger::client::http::{
    ClientBuilder, build_contract_get_request, build_contract_publish_request, build_fund_request,
};
use starstream_ledger::server::Ledger;
use tokio::net::TcpListener;

fn compile_contract(source: &str) -> Vec<u8> {
    let (program, errors) = starstream_compiler::parse_program(source).into_output_errors();
    assert!(errors.is_empty(), "parsing failed: {errors:?}");
    let program = program.expect("parser produced no program");
    let typed = starstream_compiler::typecheck_program(&program, Default::default())
        .unwrap_or_else(|failure| panic!("typechecking failed: {:?}", failure.errors));
    let result = starstream_to_wasm::compile(&typed.program);
    assert!(
        result.errors.is_empty(),
        "compiling failed: {:?}",
        result.errors
    );
    let wasm = result.wasm.expect("compiling produced no Wasm");
    starstream_runtime_next::componentize(wasm).expect("failed to componentize contract")
}

const NETWORK: &str = "starstream:test";

static SCORE_WASM: LazyLock<Vec<u8>> =
    LazyLock::new(|| compile_contract(include_str!("../../examples/score.star")));
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

    let ledger = Ledger::new(
        wasmtime::Engine::default(),
        128,
        NETWORK,
        ADMIN.verifying_key(),
    );
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
    let (http::response::Parts { status, .. }, body) = http_request(&http, req).await?;
    assert_eq!(status, StatusCode::OK, "{}", String::from_utf8_lossy(&body));
    assert_eq!(body, score_publish_envelope);

    shutdown.notify_waiters();
    ledger.await.context("ledger task panicked")
}
