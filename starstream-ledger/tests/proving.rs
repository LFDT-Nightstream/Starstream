use std::collections::HashMap;
use std::slice;

use anyhow::{Context as _, ensure};
use bytes::Bytes;
use sha2::{Digest as _, Sha256};
use starstream_ledger::Transaction;
use starstream_ledger::client::CoordinationScriptArg;
use starstream_ledger::client::runtime::{Client, Ctx, call_coordination_script, compile_contract};
use starstream_ledger::client::runtime::{TracedCall, TransactionProver, verify_transaction_proof};
use starstream_ledger::{TransactionInput, TransactionOutput, encode_digest};
use wasmtime::{Engine, Store};
use wasmtime_wizer::Wizer;

pub mod common;
use common::{SCORE_WASM, SCORE_WASM_DIGEST};

struct TestClient(Option<TransactionOutput>);

impl Client for TestClient {
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
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
    let mut engine_config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut engine_config);
    let engine = Engine::new(&engine_config)?;
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
        Vec::default(),
        &mut [],
        &mut Vec::default(),
        None,
    )
    .await?;
    let execution = execution?.execution;
    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);
    Ok(())
}

#[tokio::test]
async fn coordination_script_loads_existing_utxo() -> anyhow::Result<()> {
    let wasm = common::compile_contract(&format!(
        "{}\nscript fn update(prog: ScoreProgress) {{ prog.plus_chips(5); }}\nscript fn end(prog: ScoreProgress) {{ prog.finish(); }}",
        include_str!("../../examples/score.star")
    ))?;
    let mut engine_config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut engine_config);
    let engine = Engine::new(&engine_config)?;
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
        Vec::default(),
        &mut [],
        &mut Vec::default(),
        None,
    )
    .await?;
    let execution = execution?.execution;
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
        vec![CoordinationScriptArg::Utxo(input.clone())],
        &mut [],
        &mut Vec::default(),
        None,
    )
    .await?;
    let execution = execution?.execution;
    let utxo = client.0.unwrap();
    assert_eq!(transaction.inputs, slice::from_ref(&input));
    assert_eq!(transaction.outputs.len(), 1);
    assert_ne!(transaction.outputs[0].storage, utxo.storage);
    assert_eq!(transaction.outputs[0].methods, utxo.methods);
    assert_eq!(execution.transaction.statement.inputs.len(), 1);
    assert_eq!(execution.transaction.statement.outputs.len(), 1);
    assert_eq!(execution.traces.len(), 2);

    let end = contract.get_coordination_script("end")?;
    let (transaction, execution) = call_coordination_script(
        &mut Store::new(&engine, Ctx::default()),
        &TestClient(Some(utxo)),
        &wizer,
        &contract,
        &wasm,
        &end,
        &mut imports,
        vec![CoordinationScriptArg::Utxo(input)],
        &mut [],
        &mut Vec::default(),
        None,
    )
    .await?;
    execution?;
    let [event] = transaction.events.as_slice() else {
        panic!("invalid events: {:?}", transaction.events);
    };
    assert_eq!(event.abi_name.as_ref(), "score");
    assert_eq!(event.name.as_ref(), "finish");
    assert_eq!(event.params.as_ref(), [0xa8, 0x01]);
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
                "(import \"starstream:utxo/score-progress\" (external-id \"{}\") (instance",
                digest.to_uppercase()
            ),
            1,
        );
    let wasm = wat::parse_str(wat)?;
    let mut engine_config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut engine_config);
    let engine = Engine::new(&engine_config)?;
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
        Vec::default(),
        &mut [],
        &mut Vec::default(),
        None,
    )
    .await?;
    let execution = execution?.execution;
    assert!(transaction.inputs.is_empty());
    assert_eq!(transaction.outputs.len(), 1);
    assert_eq!(transaction.outputs[0].contract.as_ref(), digest);
    assert_eq!(execution.traces.len(), 2);
    assert!(execution.traces.iter().all(|trace| !trace.0.is_empty()));
    Ok(())
}

struct ProofFixture<'a> {
    wasm: &'a [u8],
    previous: Option<&'a Transaction>,
}

impl Client for ProofFixture<'_> {
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        ensure!(
            <[u8; 32]>::from(Sha256::digest(self.wasm)) == digest,
            "unknown fixture contract"
        );
        Ok(Bytes::copy_from_slice(self.wasm))
    }

    async fn get_input_utxo(&self, input: &TransactionInput) -> anyhow::Result<TransactionOutput> {
        let previous = self.previous.context("missing previous transaction")?;
        ensure!(
            input.transaction.as_ref() == encode_digest(&previous.digest()?),
            "unknown fixture transaction"
        );
        previous
            .outputs
            .get(input.index as usize)
            .cloned()
            .context("unknown fixture output")
    }
}

impl ProofFixture<'_> {
    async fn round_trip(
        &self,
        prover: &mut TransactionProver,
        store: Store<Ctx>,
        call: TracedCall,
        transaction: Transaction,
    ) -> anyhow::Result<Transaction> {
        let (timings, sizes) = call
            .proof_stats
            .as_ref()
            .context("missing attached proof statistics")?;
        for instance in &timings.wasm {
            eprintln!(
                "{:?} ({:?}, circuit reused: {}): {:?}",
                instance.coroutine, instance.size, instance.circuit_reused, instance.phases
            );
        }
        eprintln!(
            "interleaving (circuit reused: {}): {:?}",
            timings.interleaving_circuit_reused, timings.interleaving
        );
        eprintln!("total: {:?}", timings.total);
        eprintln!("{} proof bytes: {sizes:?}", sizes.total());
        drop(call);
        let engine = store.engine().clone();
        drop(store);
        let bytes = minicbor::to_vec(&transaction)?;
        drop(transaction);
        let decoded: Transaction = minicbor::decode(&bytes)?;
        eprintln!("{} transaction proof bytes", decoded.proof.len());
        let started = std::time::Instant::now();
        verify_transaction_proof(prover, &decoded, self, &engine, &Wizer::new()).await?;
        eprintln!("prepared and verified in {:?}", started.elapsed());
        if let Some(previous) = self.previous {
            for change_storage in [true, false] {
                let mut other = previous.clone();
                let output = &mut other.outputs[0];
                if change_storage {
                    // Change the low bit of the first encoded integer, keeping its LEB length.
                    output.storage[0] ^= 1;
                } else {
                    output.methods.clear();
                }
                let mut changed = decoded.clone();
                changed.inputs[0] = TransactionInput::new(other.digest()?, 0);
                let other_store = ProofFixture {
                    wasm: self.wasm,
                    previous: Some(&other),
                };
                let err = verify_transaction_proof(
                    prover,
                    &changed,
                    &other_store,
                    &engine,
                    &Wizer::new(),
                )
                .await
                .unwrap_err();
                ensure!(
                    format!("{err:#}").contains("interleaving proof is invalid"),
                    "expected a rejected input commitment, got {err:#}"
                );
            }
        }
        Ok(decoded)
    }
}

async fn authorized_amount_workflow(prove: bool) -> anyhow::Result<()> {
    use starstream_ledger::client::runtime::{ProofEngine, TransactionProver};
    let wasm = common::compile_contract(
        r#"
        abi Amount {
            fn amount() -> u64;
            fn spend();
            effect Authorize(owner: u64);
            event Observed(value: u64);
        }
        utxo StoredAmount {
            storage {
                let mut balance: u64;
                let mut owner: u64;
            }
            main fn new(pub value: u64, pub recipient: u64) {
                balance = value;
                owner = recipient;
                yield(Amount);
            }
            impl Amount {
                fn amount() -> u64 { return balance; }
                fn spend() {
                    raise Authorize(owner);
                    resume;
                }
            }
        }
        script fn create() { let instance = StoredAmount::new(100, 42); }
        script fn spend(instance: StoredAmount) {
            let value = instance.amount();
            emit Observed(value);
            try {
                instance.spend();
            } with Authorize(owner) {
            }
        }
        script fn denied(instance: StoredAmount) {
            instance.spend();
        }
        "#,
    )?;
    let mut config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut config);
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
    let mut prover = prove.then(|| TransactionProver::new(ProofEngine::Optimized));
    let mut store = Store::new(&engine, Ctx::default());
    let create = contract.get_coordination_script("create")?;
    if prove {
        eprintln!("create stored amount");
    }
    let (created, execution) = call_coordination_script(
        &mut store,
        &TestClient(None),
        &wizer,
        &contract,
        &wasm,
        &create,
        &mut imports,
        vec![],
        &mut [],
        &mut vec![],
        prover.as_mut(),
    )
    .await?;
    let call = execution?;
    assert_eq!(call.programs.len(), 1);
    assert!(created.inputs.is_empty());
    assert_eq!(created.outputs.len(), 1);
    let created = if let Some(prover) = &mut prover {
        ProofFixture {
            wasm: &wasm,
            previous: None,
        }
        .round_trip(prover, store, call, created)
        .await?
    } else {
        assert!(created.proof.is_empty());
        assert!(call.proof_stats.is_none());
        created
    };

    let client = TestClient(Some(created.outputs[0].clone()));
    // Each branch spends the same fixture independently.
    for (script, authorized) in [("denied", false), ("spend", true)] {
        let spend = contract.get_coordination_script(script)?;
        let mut store = Store::new(&engine, Ctx::default());
        let input = TransactionInput {
            transaction: encode_digest(&created.digest()?).into(),
            index: 0,
        };
        if prove && authorized {
            eprintln!("spend stored amount");
        }
        let result = call_coordination_script(
            &mut store,
            &client,
            &wizer,
            &contract,
            &wasm,
            &spend,
            &mut imports,
            vec![input.clone().into()],
            &mut [],
            &mut vec![],
            prover.as_mut(),
        )
        .await;
        if !authorized {
            let error = result.expect_err("unhandled authorization must abort the transaction");
            ensure!(
                format!("{error:#}").contains("effect not handled: amount::authorize"),
                "unexpected error: {error:#}"
            );
            continue;
        }
        let (transaction, execution) = result?;
        let call = execution?;
        assert_eq!(
            call.programs.len(),
            2,
            "retain the input snapshot as well as the coordinator"
        );
        if !prove {
            starstream_ledger::client::runtime::prepare_verification(
                &call.programs,
                [starstream_proving_runtime::WasmSize::Small],
            )?;
        }
        let execution = &call.execution;
        let [event] = transaction.events.as_slice() else {
            anyhow::bail!("expected one amount event");
        };
        assert_eq!(event.name.as_ref(), "observed");
        assert_eq!(event.params.as_ref(), [100]);
        assert_eq!(transaction.inputs, [input]);
        assert!(transaction.outputs.is_empty());
        assert!(execution.transaction.statement.outputs.is_empty());
        if let Some(prover) = &mut prover {
            ProofFixture {
                wasm: &wasm,
                previous: Some(&created),
            }
            .round_trip(prover, store, call, transaction)
            .await?;
        }
    }
    Ok(())
}

#[tokio::test]
async fn authorized_amount_execution_is_checked() -> anyhow::Result<()> {
    authorized_amount_workflow(false).await
}

#[tokio::test]
#[ignore = "Nebula proofs; run with --release -- --ignored --nocapture"]
async fn authorized_amount_transactions_prove_and_verify() -> anyhow::Result<()> {
    authorized_amount_workflow(true).await
}

/// Reuse one prover across UTXO creation and a subsequent spend.
#[tokio::test]
#[ignore = "Nebula proofs; run with --release -- --ignored --nocapture"]
async fn coordination_script_transactions_prove_and_verify() -> anyhow::Result<()> {
    use starstream_ledger::client::runtime::{ProofEngine, TransactionProver};

    let wasm = common::compile_contract(
        "abi MethodCall { fn accept(value: u64); }
        utxo MethodCallUtxo {
            main fn new(initial: u64) { yield(MethodCall); }
            impl MethodCall { fn accept(pub value: u64) {} }
        }
        script fn example() {
            let instance = MethodCallUtxo::new(55);
            instance.accept(13);
        }
        script fn again(instance: MethodCallUtxo) {
            instance.accept(21);
        }",
    )?;
    let mut engine_config = wasmtime::Config::default();
    starstream_ledger::configure_wasmtime(&mut engine_config);
    let engine = Engine::new(&engine_config)?;
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
    let mut prover = TransactionProver::new(ProofEngine::Optimized);

    let example = contract.get_coordination_script("example")?;
    let mut store = Store::new(&engine, Ctx::default());
    let (transaction, execution) = call_coordination_script(
        &mut store,
        &TestClient(None),
        &wizer,
        &contract,
        &wasm,
        &example,
        &mut imports,
        Vec::default(),
        &mut [],
        &mut Vec::default(),
        Some(&mut prover),
    )
    .await?;
    assert_eq!(transaction.outputs.len(), 1);
    let created = ProofFixture {
        wasm: &wasm,
        previous: None,
    }
    .round_trip(&mut prover, store, execution?, transaction)
    .await?;

    let client = TestClient(Some(created.outputs[0].clone()));
    let again = contract.get_coordination_script("again")?;
    let mut store = Store::new(&engine, Ctx::default());
    let (transaction, execution) = call_coordination_script(
        &mut store,
        &client,
        &wizer,
        &contract,
        &wasm,
        &again,
        &mut imports,
        vec![CoordinationScriptArg::Utxo(TransactionInput {
            transaction: encode_digest(&created.digest()?).into(),
            index: 0,
        })],
        &mut [],
        &mut Vec::default(),
        Some(&mut prover),
    )
    .await?;
    assert_eq!(transaction.inputs.len(), 1);
    ProofFixture {
        wasm: &wasm,
        previous: Some(&created),
    }
    .round_trip(&mut prover, store, execution?, transaction)
    .await?;
    Ok(())
}
