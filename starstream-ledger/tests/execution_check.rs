use std::collections::HashMap;

use anyhow::{bail, ensure};
use bytes::Bytes;
use starstream_ledger::client::runtime::{
    Client, Ctx, call_coordination_script, compile_component, new_contract,
};
use starstream_ledger::{TransactionInput, TransactionOutput};
use wasmtime::Store;

pub mod common;

struct NoopClient;

impl Client for NoopClient {
    async fn get_contract_wasm(&self, _digest: [u8; 32]) -> anyhow::Result<Bytes> {
        bail!("the same-contract fixture resolves no imports")
    }

    async fn get_input_utxo(&self, _input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        bail!("the same-contract fixture has no inputs")
    }
}

#[tokio::test]
async fn coordination_script_execution_is_checked() -> wasmtime::Result<()> {
    let mut config = starstream_proving_runtime::new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config)?;
    let wizer = wasmtime_wizer::Wizer::new();
    let component = compile_component(&engine, &wizer, &common::SCORE_WASM)?;
    let mut imports = HashMap::new();
    let contract = new_contract(&NoopClient, &wizer, &component, None, &mut imports).await?;
    let export = contract.get_coordination_script("example")?;

    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &NoopClient,
        &wizer,
        &contract,
        &common::SCORE_WASM,
        &export,
        &mut imports,
        [],
        &mut [],
        &mut vec![],
    )
    .await?;

    let execution = execution.unwrap();

    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);
    Ok(())
}

struct ScoreClient;

struct InputClient(TransactionOutput);

impl Client for InputClient {
    async fn get_contract_wasm(&self, _: [u8; 32]) -> anyhow::Result<Bytes> {
        bail!("the input belongs to the root contract")
    }

    async fn get_input_utxo(&self, input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        ensure!(input.index == 0, "unexpected input");
        Ok(self.0.clone())
    }
}

#[tokio::test]
async fn coordination_script_loads_existing_utxo() -> wasmtime::Result<()> {
    let wasm = common::compile_contract(&format!(
        "{}\nscript fn update(prog: ScoreProgress) {{ prog.plus_chips(5); }}",
        include_str!("../../examples/score.star")
    ))?;
    let mut config = starstream_proving_runtime::new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config)?;
    let wizer = wasmtime_wizer::Wizer::new();
    let component = compile_component(&engine, &wizer, &wasm)?;
    let mut imports = HashMap::new();
    let contract = new_contract(&NoopClient, &wizer, &component, None, &mut imports).await?;
    let create = contract.get_coordination_script("example")?;
    let (initial, _) = call_coordination_script(
        &mut wasmtime::Store::new(&engine, Default::default()),
        &NoopClient,
        &wizer,
        &contract,
        &wasm,
        &create,
        &mut imports,
        [],
        &mut [],
        &mut Vec::new(),
    )
    .await?;
    let client = InputClient(initial.outputs[0].clone());
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
        [starstream_ledger::client::CoordinationScriptArg::Utxo(
            input.clone(),
        )],
        &mut [],
        &mut Vec::new(),
    )
    .await?;

    let execution = execution.unwrap();

    assert_eq!(transaction.inputs, [input]);
    assert_eq!(transaction.outputs.len(), 1);
    assert_ne!(transaction.outputs[0].storage, client.0.storage);
    assert_eq!(transaction.outputs[0].methods, client.0.methods);
    assert_eq!(execution.transaction.statement.inputs.len(), 1);
    assert_eq!(execution.transaction.statement.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);
    Ok(())
}

impl Client for ScoreClient {
    async fn get_contract_wasm(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        ensure!(digest == *common::SCORE_WASM_DIGEST, "unexpected contract");
        Ok(Bytes::copy_from_slice(&common::SCORE_WASM))
    }

    async fn get_input_utxo(&self, _input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        bail!("the fixture constructs its UTXO")
    }
}

#[tokio::test]
async fn coordination_script_calls_utxo_from_another_contract() -> wasmtime::Result<()> {
    let digest = starstream_ledger::encode_digest(&common::SCORE_WASM_DIGEST);
    // Redirect the compiler's self interface to the published score contract.
    // Both the component import and the core import names must agree.
    let root = wasmprinter::print_bytes(&*common::SCORE_WASM)
        .unwrap()
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
    let wasm = wat::parse_str(root).unwrap();
    let mut config = starstream_proving_runtime::new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = wasmtime::Engine::new(&config)?;
    let wizer = wasmtime_wizer::Wizer::new();
    let component = compile_component(&engine, &wizer, &wasm)?;
    let mut imports = HashMap::new();
    let contract = new_contract(&ScoreClient, &wizer, &component, None, &mut imports).await?;
    assert!(imports.contains_key(&*common::SCORE_WASM_DIGEST));
    let export = contract.get_coordination_script("example")?;
    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &ScoreClient,
        &wizer,
        &contract,
        &wasm,
        &export,
        &mut imports,
        [],
        &mut [],
        &mut vec![],
    )
    .await?;

    let execution = execution.unwrap();

    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(transaction.outputs[0].contract.as_ref(), digest);
    assert_eq!(execution.traces.len(), 2);
    assert!(execution.traces.iter().all(|trace| !trace.0.is_empty()));
    Ok(())
}
