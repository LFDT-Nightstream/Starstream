mod common;

use common::Ctx;
use sha2::{Digest, Sha256};
use starstream_compiler::{ModuleGraph, TypecheckOptions, typecheck_modules};
use starstream_interleaving_spec::{QuintVerifier, Step};
use starstream_proving_runtime::{
    WasmTraceSink, build_component_templates, decode_captured_execution, enable_tracing,
    flat_value_root, register_tracing_component,
};
use starstream_runtime::{Contract, ContractLookup};
use std::{collections::HashMap, path::Path};
use wasmtime::{Engine, Store, component::Component};

#[derive(Default)]
struct Contracts(HashMap<String, Contract<Ctx>>);
impl ContractLookup<Ctx> for &Contracts {
    fn get_contract(&self, id: &str) -> wasmtime::Result<Contract<Ctx>> {
        self.0
            .get(id)
            .cloned()
            .ok_or_else(|| wasmtime::format_err!("missing {id}"))
    }
}

async fn execute(
    file: &str,
    script_name: &str,
) -> wasmtime::Result<starstream_proving_runtime::CapturedExecution> {
    execute_with(file, script_name, &[]).await
}

/// Run `script_name` of `file` as the transaction's entry script. Every
/// contract of the workspace is registered once, with the same templates
/// whether it turns out to be the entry or a callee.
async fn execute_with(
    file: &str,
    script_name: &str,
    arguments: &[wasmtime::component::Val],
) -> wasmtime::Result<starstream_proving_runtime::CapturedExecution> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/coordinator_calls")
        .join(file);
    let graph = ModuleGraph::from_workspace(
        &mut starstream_types::FileSystem::default(),
        path.parent().unwrap(),
    )
    .unwrap();
    let root = graph
        .modules()
        .iter()
        .find(|m| m.abs_path == path)
        .unwrap()
        .id;
    let typed = typecheck_modules(&graph, TypecheckOptions::default()).unwrap();
    let mut config = wasmtime::Config::default();
    starstream_runtime::configure_wasmtime(&mut config);
    let engine = Engine::new(&config)?;
    let mut store = Store::new(&engine, Ctx::default());
    let mut contracts = Contracts::default();
    let mut entry = None;
    for id in typed.contract_entries() {
        let compiled = starstream_to_wasm::compile_contract(&typed, id);
        assert!(compiled.errors.is_empty(), "{:?}", compiled.errors);
        let wasm = compiled
            .to_component()
            .map_err(|e| wasmtime::format_err!("{e:?}"))?;
        let hash = typed
            .module(id)
            .export
            .borrow()
            .as_ref()
            .unwrap()
            .hash
            .clone();
        let component = Component::from_binary(&engine, &wasm)?;
        let contract = Contract::new(&component, None, &contracts)?;
        let names: Vec<_> = contract.coordination_script_names().collect();
        let templates = build_component_templates(&wasm, &names, &Sha256::digest(&wasm).into())?;
        register_tracing_component(store.data_mut(), &wasm, &templates.bindings)?;
        if id == root {
            entry = Some(contract.clone());
        }
        contracts.0.insert(hash, contract);
    }
    enable_tracing(&mut store)?;
    let contract = entry.unwrap();
    let script = contract.get_coordination_script(script_name)?;
    contract
        .instantiate(&mut store)
        .await?
        .call_coordination_script(&mut store, &script, arguments, [])
        .await?;
    decode_captured_execution(store.data().wasm_trace_registry(), [])
}

/// The interleaving relation accepts the calls and binds every instance's
/// final trace root, callees included, into the transaction digest.
fn assert_accepted(execution: &starstream_proving_runtime::CapturedExecution) {
    use starstream_interleaving_prover::{CoroutineId, TraceCommitments, verify_transaction_sat};
    starstream_interleaving_prover::verify_sat(&execution.transaction.execution).unwrap();
    // These fixtures create no UTXOs: instances are coordinators, allocated
    // in the order they were instantiated.
    let commitments: TraceCommitments = execution
        .traces
        .iter()
        .zip(1..)
        .map(|(trace, id)| {
            (
                CoroutineId::Coord(id).encoded(),
                common::encoded_chain(trace),
            )
        })
        .collect();
    let verify = |commitments: &TraceCommitments| {
        verify_transaction_sat(
            &execution.transaction.trace,
            1,
            &execution.transaction.statement,
            commitments,
        )
    };
    verify(&commitments).unwrap();
    for instance in commitments.keys() {
        let mut tampered = commitments.clone();
        tampered.get_mut(instance).unwrap()[0] ^= 1;
        assert!(verify(&tampered).is_err(), "instance {instance}");
    }
}

#[tokio::test]
async fn nested_and_repeated_calls_have_separate_traces_and_match_boundaries()
-> wasmtime::Result<()> {
    let execution = execute("main.star", "main").await?;
    // root + two middle invocations + four leaf invocations, despite only 3 modules.
    assert_eq!(execution.traces.len(), 7);
    let steps = &execution.transaction.trace.0;
    assert_eq!(
        steps
            .iter()
            .filter(|s| matches!(s, Step::CallCoordinator { .. }))
            .count(),
        6
    );
    assert_eq!(
        steps
            .iter()
            .filter(|s| matches!(s, Step::EnterCall { .. }))
            .count(),
        // the entry script and the six imported calls
        7
    );
    assert!(matches!(steps[0], Step::BeginExecution { .. }));
    assert_eq!(
        steps
            .iter()
            .filter(|s| matches!(s, Step::ReceiveCallResult { .. }))
            .count(),
        6
    );
    assert_eq!(
        steps
            .iter()
            .filter(|s| matches!(s, Step::FinalizeCoordinator))
            .count(),
        7
    );
    let results: Vec<_> = execution.traces[0]
        .0
        .iter()
        .filter_map(|step| match step {
            Step::ReceiveCallResult { result } => Some(result.0.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        results,
        [23, 8589934595].map(|value| starstream_proving_runtime::flat_value_root(
            &[wasmparser::ValType::I64],
            &[value]
        ))
    );
    // The entry script opens with the same event as its callees.
    for trace in &execution.traces {
        assert!(matches!(trace.0.first(), Some(Step::EnterCall { .. })));
        assert!(matches!(trace.0.last(), Some(Step::Return { .. })));
    }
    assert_accepted(&execution);
    Ok(())
}

#[tokio::test]
#[ignore = "requires repository-pinned Quint"]
async fn captured_coordinator_calls_replay_in_quint() -> wasmtime::Result<()> {
    let execution = execute("main.star", "main").await?;
    let verifier = QuintVerifier::new()?;
    let effects = execute("effect_main.star", "effect-main").await?;
    verifier.verify_transaction(&effects.transaction.trace, &effects.transaction.statement)?;
    verifier.verify_transaction(
        &execution.transaction.trace,
        &execution.transaction.statement,
    )?;
    // Tamper with the entry script's event (index 1), then with a callee's.
    let callee = execution
        .transaction
        .trace
        .0
        .iter()
        .position(|s| matches!(s, Step::CallCoordinator { .. }))
        .unwrap()
        + 1;
    for index in [1, callee] {
        let mut wrong = execution.transaction.trace.clone();
        let Step::EnterCall { method, .. } = &mut wrong.0[index] else {
            panic!("step {index} must be an entry event");
        };
        method.0[0] ^= 1;
        assert!(
            verifier
                .verify_transaction(&wrong, &execution.transaction.statement)
                .is_err()
        );
    }
    let mut wrong = execution.transaction.statement.clone();
    wrong.entry.script.0[0] ^= 1;
    assert!(
        verifier
            .verify_transaction(&execution.transaction.trace, &wrong)
            .is_err()
    );
    let (entry, called) = poke_as_entry_and_callee().await?;
    for execution in [entry, called] {
        verifier.verify_transaction(
            &execution.transaction.trace,
            &execution.transaction.statement,
        )?;
    }
    Ok(())
}

/// `poke` run directly as a transaction's entry, and through an import.
async fn poke_as_entry_and_callee() -> wasmtime::Result<(
    starstream_proving_runtime::CapturedExecution,
    starstream_proving_runtime::CapturedExecution,
)> {
    Ok((
        execute_with("poke.star", "poke", &[wasmtime::component::Val::S64(3)]).await?,
        execute("poke_main.star", "poke-main").await?,
    ))
}

#[tokio::test]
async fn a_script_has_one_identity_as_entry_and_as_callee() -> wasmtime::Result<()> {
    let (entry, called) = poke_as_entry_and_callee().await?;
    let arguments = flat_value_root(&[wasmparser::ValType::I64], &[3]);
    // As the entry, the statement binds the script and its arguments.
    let statement = &entry.transaction.statement.entry;
    assert_eq!(statement.arguments, arguments);
    assert_eq!(
        entry.transaction.trace.0[..2],
        [
            Step::BeginExecution {
                script: statement.script,
                arguments: arguments.clone(),
            },
            Step::EnterCall {
                method: statement.script,
                arguments: arguments.clone(),
            },
        ]
    );
    // As a callee, the importer names the same identity and arguments, and
    // the callee's own trace is the same as when it was the entry.
    assert!(called.transaction.trace.0.contains(&Step::CallCoordinator {
        script: statement.script,
        arguments,
    }));
    assert_eq!(called.traces[1], entry.traces[0]);
    assert_ne!(called.transaction.statement.entry.script, statement.script);
    Ok(())
}

#[test]
fn scripts_with_by_value_signatures_can_be_an_entry() {
    for source in [
        "script fn helper(value: Utxo) {}",
        "struct Pair { a: i64, b: i64 } script fn helper(value: Pair) {}",
    ] {
        // Exports whose arguments are passed by value get an identity. The
        // arguments are opaque to the interleaving proof, but committed.
        let wasm = common::compile_contract(source);
        build_component_templates(&wasm, &["helper"], &[0; 32]).unwrap();
    }
    // The compiler cannot emit a signature passed through memory yet; the
    // identity unit tests cover their rejection.
}

#[tokio::test]
async fn caller_effect_handler_can_invoke_another_script() -> wasmtime::Result<()> {
    let execution = execute("effect_main.star", "effect-main").await?;
    assert_eq!(execution.traces.len(), 3);
    let steps = &execution.transaction.trace.0;
    assert_eq!(
        steps
            .iter()
            .filter(|s| matches!(s, Step::EnterEffectHandler { .. }))
            .count(),
        1
    );
    assert_eq!(
        steps
            .iter()
            .filter(|s| matches!(s, Step::CallCoordinator { .. }))
            .count(),
        2
    );
    assert_accepted(&execution);
    Ok(())
}
