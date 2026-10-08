#![cfg(all(feature = "client", feature = "server"))]

use core::net::SocketAddr;
use core::str::FromStr as _;

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context as _;
use bytes::{Bytes, BytesMut};
use coset::{CoseSign1Builder, HeaderBuilder, TaggedCborSerializable as _, iana};
use ed25519_dalek::Signer as _;
use http::header::{CONTENT_TYPE, VARY, X_CONTENT_TYPE_OPTIONS};
use http::{Method, StatusCode, Uri};
use http_body_util::{BodyExt as _, Full};
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::TokioExecutor;
use sha2::{Digest as _, Sha256};
use starstream_ledger::client::build_publish_envelope;
use starstream_ledger::client::http::{
    ClientBuilder, build_contract_get_request, build_contract_publish_request, build_fund_request,
    build_transaction_get_request,
};
use starstream_ledger::client::runtime::compile_contract;
use starstream_ledger::server::Ledger;
use starstream_ledger::wrpc::bindings;
use starstream_ledger::wrpc::codec::ValEncoder;
use starstream_ledger::{
    APPLICATION_WRPC, Envelope, Message, Transaction, TransactionInput, TransactionOutput,
    encode_digest,
};
use tokio::io::AsyncReadExt as _;
use tokio_util::codec::Encoder as _;
use wasmtime::Store;
use wasmtime::component::{Component, Type, Val};
use wasmtime_wizer::WasmtimeWizerComponent;

pub mod common;
use common::*;

pub async fn http_request(
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

fn admin_protected_header() -> coset::Header {
    HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(ADMIN.verifying_key().to_bytes().into())
        .build()
}

fn build_sign1_envelope(protected: coset::Header, payload: Vec<u8>) -> Vec<u8> {
    CoseSign1Builder::new()
        .protected(protected)
        .payload(payload)
        .create_signature(b"", |data| ADMIN.sign(data).to_bytes().into())
        .build()
        .to_tagged_vec()
        .unwrap()
}

async fn post_envelope(
    client: &hyper_util::client::legacy::Client<HttpConnector, Full<Bytes>>,
    addr: SocketAddr,
    envelope: Vec<u8>,
) -> (StatusCode, String) {
    let req = http::Request::builder()
        .method(Method::POST)
        .uri(format!("http://{addr}/rpc"))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(Bytes::from(envelope)))
        .unwrap();
    let (parts, body) = http_request(client, req).await.unwrap();
    (parts.status, String::from_utf8_lossy(&body).into_owned())
}

fn wrpc_context(addr: SocketAddr) -> http::request::Parts {
    let req = http::Request::builder()
        .uri(format!("http://{addr}/rpc"))
        .header(CONTENT_TYPE, APPLICATION_WRPC.to_string())
        .body(())
        .unwrap();
    let (cx, ()) = req.into_parts();
    cx
}

#[tokio::test]
async fn http() {
    let mut config = wasmtime::Config::default();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config).unwrap();

    let wizer = wasmtime_wizer::Wizer::new();
    let (wizer_cx, score) = wizer.instrument_component(&SCORE_WASM).unwrap();
    let score = Component::from_binary(&engine, &score)
        .map_err(anyhow::Error::from)
        .unwrap();
    let score = starstream_runtime::Contract::new(&score, None, NoopContractLookup).unwrap();
    let score_digest = encode_digest(&SCORE_WASM_DIGEST);

    let score_example_export = score.get_coordination_script("example").unwrap();
    let scope_progress_utxo_export = score.get_utxo("score-progress").unwrap();
    let score_progress_utxo_storage_export = scope_progress_utxo_export.storage().unwrap();
    let mut store = Store::new(&engine, Ctx::default());
    let score = score.instantiate(&mut store).await.unwrap();
    score
        .call_coordination_script(&mut store, &score_example_export, &[], &mut [])
        .await
        .unwrap();
    let Ctx { outputs, .. } = store.data_mut();
    let [score_progress_utxo] = outputs.as_slice() else {
        panic!("invalid outputs: {outputs:?}")
    };
    let score_progress_utxo_instance = score_progress_utxo.instance();
    let score_progress_utxo_storage = score_progress_utxo
        .storage(score_progress_utxo_storage_export)
        .call_get(&mut store)
        .await
        .unwrap();
    let mut score_progress_utxo_storage_buf = BytesMut::default();
    ValEncoder::new(&Type::Record(
        score_progress_utxo_storage_export.ty().clone(),
    ))
    .encode(
        &Val::Record(score_progress_utxo_storage),
        &mut score_progress_utxo_storage_buf,
    )
    .unwrap();
    let score_progress_utxo = wizer
        .snapshot_component(
            &wizer_cx,
            &mut WasmtimeWizerComponent {
                store: &mut store,
                instance: score_progress_utxo_instance,
            },
        )
        .await
        .unwrap();
    let score_progress_genesis_utxo = TransactionOutput {
        contract: score_digest.as_str().into(),
        instance: "score-progress".into(),
        methods: SCORE_EXAMPLE_METHODS.clone(),
        storage: score_progress_utxo_storage_buf.to_vec().into(),
        state: starstream_ledger::runtime::parse_state(&score_progress_utxo)
            .collect::<Result<_, _>>()
            .unwrap(),
    };
    let genesis = [
        score_progress_genesis_utxo.clone(),
        score_progress_genesis_utxo,
    ];

    let ledger = Ledger::new(engine, 128, NETWORK, ADMIN.verifying_key(), genesis.clone());
    let ledger = Arc::new(ledger);
    let addr = free_tcp_addr().await.unwrap();
    let (ledger, shutdown) = ledger.handle_http(addr).await.unwrap();
    let ledger = tokio::spawn(ledger);

    let http = hyper_util::client::legacy::Client::builder(TokioExecutor::new());
    let api_base = Uri::from_str(&format!("http://{addr}")).unwrap();
    let client = ClientBuilder::new(http.clone(), HttpConnector::new(), api_base.clone())
        .network(NETWORK)
        .build();
    let wrpc = wrpc_http::Client::new(http.build(HttpConnector::new()));
    let http = http.build_http();

    let height = client.block_height().await.unwrap();
    assert_eq!(height, 0);

    let outputs = client.get_genesis().await.unwrap();
    assert_eq!(outputs, genesis);

    let score_publish_envelope = build_publish_envelope(
        ADMIN.clone(),
        NETWORK,
        starstream_ledger::Publish {
            nonce: 1,
            wasm: SCORE_WASM.clone(),
        },
    )
    .unwrap();
    let score_publish_req = build_contract_publish_request(
        &api_base,
        ADMIN.clone(),
        NETWORK,
        starstream_ledger::Publish {
            nonce: 1,
            wasm: SCORE_WASM.clone(),
        },
    )
    .unwrap();

    let (http::response::Parts { status, .. }, body) =
        http_request(&http, score_publish_req.clone())
            .await
            .unwrap();
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
    let crit_envelope = build_sign1_envelope(protected, Vec::default());
    let (status, body) = post_envelope(&http, addr, crit_envelope).await;
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert_eq!(body, "envelope must not contain critical headers");

    let score_publish_cost = score_publish_envelope.len();
    let balance = score_publish_cost.saturating_sub(1000);

    client
        .fund(ADMIN.clone(), 1, &ADMIN.verifying_key(), balance as _)
        .await
        .unwrap();

    let height = client.block_height().await.unwrap();
    assert_eq!(height, 1);

    let req = build_fund_request(
        &api_base,
        ADMIN.clone(),
        NETWORK,
        starstream_ledger::Fund {
            nonce: 1,
            account: ADMIN.verifying_key().to_bytes(),
            amount: balance as _,
        },
    )
    .unwrap();
    let (http::response::Parts { status, .. }, body) = http_request(&http, req).await.unwrap();
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::CONFLICT, "{body}");
    assert_eq!(body, "nonce must be higher than 1, got 1");

    let (http::response::Parts { status, .. }, body) =
        http_request(&http, score_publish_req.clone())
            .await
            .unwrap();
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::PAYMENT_REQUIRED, "{body}");
    assert_eq!(
        body,
        format!(
            "balance insufficient, required at least {score_publish_cost}, available {balance}"
        )
    );
    let (http::response::Parts { status, .. }, body) =
        http_request(&http, score_publish_req).await.unwrap();
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
        .await
        .unwrap();

    let req = build_contract_publish_request(
        &api_base,
        ADMIN.clone(),
        NETWORK,
        starstream_ledger::Publish {
            nonce: 2,
            wasm: Box::from(*b"not wasm"),
        },
    )
    .unwrap();
    let (http::response::Parts { status, .. }, body) = http_request(&http, req).await.unwrap();
    let body = String::from_utf8_lossy(&body);
    assert_eq!(status, StatusCode::BAD_REQUEST, "{body}");
    assert!(body.starts_with("invalid Wasm: "), "{body}");

    let digest = client
        .publish_contract(ADMIN.clone(), 2, SCORE_WASM.clone())
        .await
        .unwrap();
    assert_eq!(digest, *SCORE_WASM_DIGEST);

    let wasm = client.get_contract_wasm(*SCORE_WASM_DIGEST).await.unwrap();
    assert_eq!(wasm, SCORE_WASM.as_ref());

    let score_publish_envelope = build_publish_envelope(
        ADMIN.clone(),
        NETWORK,
        starstream_ledger::Publish {
            nonce: 2,
            wasm: SCORE_WASM.clone(),
        },
    )
    .unwrap();

    let envelope = client
        .get_contract_envelope(&SCORE_WASM_DIGEST)
        .await
        .unwrap();
    assert_eq!(envelope, score_publish_envelope);

    let req = build_contract_get_request(&api_base, &SCORE_WASM_DIGEST, None).unwrap();
    let (
        http::response::Parts {
            status, headers, ..
        },
        body,
    ) = http_request(&http, req).await.unwrap();
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

    let height = client.block_height().await.unwrap();
    assert_eq!(height, 3);

    let score_contract = compile_contract(
        &client,
        client.engine(),
        client.wizer(),
        &SCORE_WASM,
        None,
        &mut HashMap::default(),
    )
    .await
    .unwrap();
    let score_example_export = score_contract.get_coordination_script("example").unwrap();
    let mut utxos = Vec::default();
    let Transaction {
        inputs,
        outputs,
        events,
        proof,
    } = client
        .call_coordination_script(
            &mut Store::new(
                client.engine(),
                starstream_ledger::client::runtime::Ctx::default(),
            ),
            &score_contract,
            &SCORE_WASM,
            &score_example_export,
            &mut HashMap::default(),
            Vec::default(),
            &mut [],
            &mut utxos,
        )
        .await
        .unwrap();
    assert_eq!(inputs, []);
    let [
        TransactionOutput {
            ref contract,
            ref instance,
            ref methods,
            ref storage,
            ..
        },
    ] = *outputs
    else {
        panic!("invalid outputs: {outputs:?}");
    };
    assert_eq!(contract.as_ref(), score_digest);
    assert_eq!(instance.as_ref(), "score-progress");
    assert_eq!(*methods, *SCORE_EXAMPLE_METHODS);
    assert_eq!(events, []);
    assert_eq!(proof.as_ref(), [0; 0]);
    assert_eq!(storage.as_ref(), score_progress_utxo_storage_buf);

    client
        .transact(
            ADMIN.clone(),
            Transaction {
                inputs,
                outputs: outputs.clone(),
                events: events.clone(),
                proof: proof.clone(),
            },
        )
        .await
        .expect_err("transactions with no inputs must fail");

    let err = client
        .transact(
            ADMIN.clone(),
            Transaction {
                inputs: vec![
                    TransactionInput {
                        transaction: Box::default(),
                        index: 0,
                    },
                    TransactionInput {
                        transaction: Box::default(),
                        index: 0,
                    },
                ],
                outputs: outputs.clone(),
                events: events.clone(),
                proof: proof.clone(),
            },
        )
        .await
        .expect_err("duplicate inputs must be rejected");
    assert_eq!(err.to_string(), "duplicate input");

    let tx = Transaction {
        inputs: vec![TransactionInput {
            transaction: Box::default(),
            index: 0,
        }],
        outputs: outputs.clone(),
        events,
        proof,
    };
    let tx_cbor = minicbor::to_vec(&tx).unwrap();
    let tx_digest: [u8; 32] = Sha256::digest(&tx_cbor).into();
    let digest = client.transact(ADMIN.clone(), tx.clone()).await.unwrap();
    assert_eq!(digest, tx_digest);
    let height = client.block_height().await.unwrap();
    assert_eq!(height, 4);
    let got = client.get_transaction(tx_digest).await.unwrap();
    assert_eq!(got, tx);

    let tx_envelope = bindings::starstream::ledger::transaction::get_envelope(
        &wrpc,
        wrpc_context(addr),
        &encode_digest(&tx_digest),
    )
    .await
    .unwrap()
    .unwrap();
    let req = build_transaction_get_request(&api_base, &tx_digest, None).unwrap();
    let (_, body) = http_request(&http, req).await.unwrap();
    assert_eq!(tx_envelope, body);

    let err = client
        .transact(
            ADMIN.clone(),
            Transaction {
                outputs: Vec::default(),
                ..tx
            },
        )
        .await
        .expect_err("spent inputs must be rejected");
    assert_eq!(err.to_string(), "input not found");

    let mut rx = client
        .call_utxo_method(
            &TransactionInput {
                transaction: Box::default(),
                index: 1,
            },
            "get-chips",
            &[],
        )
        .await
        .expect("genesis output must remain callable");
    let mut buf = Vec::default();
    rx.read_to_end(&mut buf).await.unwrap();
    assert_eq!(buf, [42]);

    let mut rx = client
        .call_utxo_method(
            &TransactionInput {
                transaction: encode_digest(&tx_digest).into(),
                index: 0,
            },
            "get-chips",
            &[],
        )
        .await
        .expect("transaction output must be callable");
    let mut buf = Vec::default();
    rx.read_to_end(&mut buf).await.unwrap();
    assert_eq!(buf, [42]);

    let tx = Transaction {
        inputs: vec![TransactionInput {
            transaction: Box::default(),
            index: 1,
        }],
        outputs: outputs.clone(),
        events: Vec::default(),
        proof: Box::default(),
    };
    let tx_digest: [u8; 32] = Sha256::digest(minicbor::to_vec(&tx).unwrap()).into();
    let tx_payload = minicbor::to_vec(Envelope {
        network: NETWORK.into(),
        message: Message::Transaction(tx.clone()),
    })
    .unwrap();
    let protected = admin_protected_header();
    let tx_envelope = build_sign1_envelope(protected, tx_payload);
    let (status, body) = post_envelope(&http, addr, tx_envelope).await;
    assert_eq!(status, StatusCode::CREATED, "{body}");
    let got = client.get_transaction(tx_digest).await.unwrap();
    assert_eq!(got, tx);

    shutdown.notify_one();
    ledger.await.expect("ledger task panicked")
}
