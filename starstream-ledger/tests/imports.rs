#![cfg(feature = "client")]

use std::collections::HashMap;

use anyhow::{bail, ensure};
use bytes::Bytes;
use starstream_ledger::client::runtime::{
    Client, call_coordination_script, compile_component, new_contract,
};
use starstream_ledger::{Transaction, TransactionInput, TransactionOutput, encode_digest};

pub mod common;
use common::*;

struct ScoreClient;

impl Client for ScoreClient {
    async fn get_contract_wasm(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        ensure!(digest == *SCORE_WASM_DIGEST, "unexpected contract digest");
        Ok(Bytes::from(SCORE_WASM.to_vec()))
    }

    async fn get_input_utxo(&self, _input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        bail!("no inputs")
    }
}

#[tokio::test]
async fn imported_script_output() {
    let mut config = wasmtime::Config::new();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config).unwrap();
    let wizer = wasmtime_wizer::Wizer::new();

    let score_digest = encode_digest(&SCORE_WASM_DIGEST);
    let wasm = wat::parse_str(format!(
        r#"(component
    (import "starstream:contract/scripts" (instance $scripts
        (export "example" (external-id "{score_digest}") (func))
    ))
    (core func $example (canon lower (func $scripts "example")))
    (core module $m
        (import "" "example" (func $example))
        (func (export "run") (call $example))
    )
    (core instance $i (instantiate $m (with "" (instance (export "example" (func $example))))))
    (func (export "run") (canon lift (core func $i "run")))
)"#
    ))
    .unwrap();

    let mut imports = HashMap::default();
    let component = compile_component(&engine, &wizer, &wasm).unwrap();
    let contract = new_contract(&ScoreClient, &wizer, &component, None, &mut imports)
        .await
        .unwrap();
    let run = contract.get_coordination_script("run").unwrap();
    let Transaction {
        inputs, outputs, ..
    } = call_coordination_script(
        &ScoreClient,
        &wizer,
        &contract,
        &wasm,
        &run,
        &mut imports,
        [],
        &mut [],
    )
    .await
    .unwrap();
    assert_eq!(inputs, []);

    let mut score_imports = HashMap::default();
    let score_component = compile_component(&engine, &wizer, &SCORE_WASM).unwrap();
    let score = new_contract(
        &ScoreClient,
        &wizer,
        &score_component,
        None,
        &mut score_imports,
    )
    .await
    .unwrap();
    let example = score.get_coordination_script("example").unwrap();
    let Transaction {
        outputs: score_outputs,
        ..
    } = call_coordination_script(
        &ScoreClient,
        &wizer,
        &score,
        &SCORE_WASM,
        &example,
        &mut score_imports,
        [],
        &mut [],
    )
    .await
    .unwrap();

    let [TransactionOutput { contract, .. }] = outputs.as_slice() else {
        panic!("invalid outputs: {outputs:?}")
    };
    assert_eq!(contract.as_ref(), score_digest);
    assert_eq!(outputs, score_outputs);
}
