#![cfg(feature = "client")]

use std::collections::HashMap;

use anyhow::{Context as _, ensure};
use bytes::Bytes;
use starstream_ledger::client::CoordinationScriptArg;
use starstream_ledger::client::runtime::{Client, Ctx, call_coordination_script, compile_contract};
use starstream_ledger::{TransactionInput, TransactionOutput, encode_digest};
use wasmtime::{Engine, Store};
use wasmtime_wizer::Wizer;

pub mod common;
use common::{SCORE_WASM, SCORE_WASM_DIGEST};

struct TestClient(Option<TransactionOutput>);

impl Client for TestClient {
    async fn get_contract_wasm(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        ensure!(digest == *SCORE_WASM_DIGEST, "unexpected contract");
        Ok(Bytes::copy_from_slice(&SCORE_WASM))
    }

    async fn get_input_utxo(&self, input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        ensure!(input.index == 0, "unexpected input");
        self.0.clone().context("unexpected input")
    }
}

#[tokio::test]
async fn coordination_script_execution_is_checked() -> anyhow::Result<()> {
    let mut config = starstream_proving_runtime::new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = Engine::new(&config)?;
    let wizer = Wizer::new();
    let mut imports = HashMap::new();
    let contract = compile_contract(
        &TestClient(None),
        &engine,
        &wizer,
        &SCORE_WASM,
        None,
        &mut imports,
    )
    .await?;

    let example = contract.get_coordination_script("example")?;
    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &TestClient(None),
        &wizer,
        &contract,
        &SCORE_WASM,
        &example,
        &mut imports,
        [],
        &mut [],
        &mut Vec::new(),
    )
    .await?;
    let execution = execution?;
    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);
    Ok(())
}

#[tokio::test]
async fn coordination_script_loads_existing_utxo() -> anyhow::Result<()> {
    let wasm = common::compile_contract(&format!(
        "{}\nscript fn update(prog: ScoreProgress) {{ prog.plus_chips(5); }}",
        include_str!("../../examples/score.star")
    ))?;
    let mut config = starstream_proving_runtime::new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = Engine::new(&config)?;
    let wizer = Wizer::new();
    let mut imports = HashMap::new();
    let contract = compile_contract(
        &TestClient(None),
        &engine,
        &wizer,
        &wasm,
        None,
        &mut imports,
    )
    .await?;

    let example = contract.get_coordination_script("example")?;
    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &TestClient(None),
        &wizer,
        &contract,
        &wasm,
        &example,
        &mut imports,
        [],
        &mut [],
        &mut Vec::new(),
    )
    .await?;
    let execution = execution?;
    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);

    let client = TestClient(Some(transaction.outputs[0].clone()));
    let update = contract.get_coordination_script("update")?;
    let input = TransactionInput {
        transaction: "fixture".into(),
        index: 0,
    };
    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &client,
        &wizer,
        &contract,
        &wasm,
        &update,
        &mut imports,
        [CoordinationScriptArg::Utxo(input.clone())],
        &mut [],
        &mut Vec::new(),
    )
    .await?;
    let execution = execution?;
    let utxo = client.0.unwrap();
    assert_eq!(transaction.inputs, [input]);
    assert_eq!(transaction.outputs.len(), 1);
    assert_ne!(transaction.outputs[0].storage, utxo.storage);
    assert_eq!(transaction.outputs[0].methods, utxo.methods);
    assert_eq!(execution.transaction.statement.inputs.len(), 1);
    assert_eq!(execution.transaction.statement.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);
    Ok(())
}

#[tokio::test]
async fn coordination_script_calls_utxo_from_another_contract() -> anyhow::Result<()> {
    let digest = encode_digest(&SCORE_WASM_DIGEST);
    let wat = wasmprinter::print_bytes(&*SCORE_WASM)?
        .replace(
            "starstream:self/score-progress",
            "starstream:utxo/score-progress",
        )
        .replacen(
            "(import \"starstream:utxo/score-progress\" (instance",
            &format!(
                "(import \"starstream:utxo/score-progress\" (external-id \"{digest}\") (instance"
            ),
            1,
        );
    let wasm = wat::parse_str(wat)?;
    let mut config = starstream_proving_runtime::new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = Engine::new(&config)?;
    let wizer = Wizer::new();
    let mut imports = HashMap::new();
    let contract = compile_contract(
        &TestClient(None),
        &engine,
        &wizer,
        &wasm,
        None,
        &mut imports,
    )
    .await?;
    assert!(imports.contains_key(&*SCORE_WASM_DIGEST));

    let example = contract.get_coordination_script("example")?;
    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &TestClient(None),
        &wizer,
        &contract,
        &wasm,
        &example,
        &mut imports,
        [],
        &mut [],
        &mut Vec::new(),
    )
    .await?;
    let execution = execution?;
    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(transaction.outputs[0].contract.as_ref(), digest);
    assert_eq!(execution.traces.len(), 2);
    assert!(execution.traces.iter().all(|trace| !trace.0.is_empty()));
    Ok(())
}
