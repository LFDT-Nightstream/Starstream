//! Run compiled Starstream contracts on `starstream-runtime-next` under
//! Neo-Wasm's single-step tracer, normalize the captured instructions with the
//! generated host-event bindings, decode the absorbed blocks, and check the
//! interleaved transaction against the circuit, the per-coroutine event
//! commitments Neo-Wasm proves, and the Quint specification.

mod common;

use starstream_interleaving_prover::{
    Error, TraceCommitments, Unsatisfied, verify_sat, verify_transaction_sat,
};
use starstream_interleaving_spec::{
    MethodHash, Out, OutputUtxo, QuintVerifier, ResourceHandle, StarstreamValue, Step, Trace,
    TransactionStatement,
};
use starstream_proving_runtime::{flat_value_root, method_hash_from_name};
use wasmparser::ValType;
use wasmtime::bail;

use crate::common::{
    MINIMAL_METHOD_CALL, SCORE, STORAGE_COUNTER, TracedExecution, trace_coordination_script,
};

/// Batch size of the interleaving relation used by the checks below.
const BATCH_SIZE: usize = 3;

const UNIT: StarstreamValue = StarstreamValue::UNIT_VALUE;

/// The root a single `u64` argument commits to.
fn u64_root(value: u64) -> StarstreamValue {
    flat_value_root(&[ValType::I64], &[value])
}

fn ret() -> Step {
    Step::Return { result: Out(UNIT) }
}

fn method_hash(name: &str) -> MethodHash {
    method_hash_from_name(name)
}

fn call(resource: ResourceHandle, method: &str, argument: u64) -> Step {
    Step::CallMethod {
        resource,
        method: method_hash(method),
        arguments: u64_root(argument),
        result: Out(UNIT),
    }
}

fn enter(method: &str, argument: u64) -> Step {
    Step::EnterMethod {
        method: method_hash(method),
        arguments: u64_root(argument),
    }
}

fn register(method: &str) -> Step {
    Step::RegisterMethod {
        method: method_hash(method),
    }
}

/// The resource handle the coordinator observed for its single constructor.
fn constructed_resource(coordinator: &Trace) -> ResourceHandle {
    match coordinator.0.first() {
        Some(Step::NewUtxo { resource, .. }) => resource.0,
        other => panic!("coordinator trace must start with NewUtxo, got {other:?}"),
    }
}

/// What the runtime emits for the counter script, per core instance, before
/// the UTXO's finalization storage read.
///
/// The compiler only calls `utxo-context.resume` for the `resume;` statement,
/// never at a `yield`, so a constructor's first yield registers its methods
/// without a `YieldBegin`. The specification matches: constructor entry opens
/// the initial ABI window.
fn expected_minimal_method_call_traces(resource: ResourceHandle) -> [Trace; 2] {
    let coordinator = Trace::new([
        Step::NewUtxo {
            arguments: u64_root(55),
            resource: Out(resource),
        },
        call(resource, "accept", 13),
        ret(),
    ]);
    let utxo = Trace::new([
        Step::EnterConstructor {
            arguments: u64_root(55),
        },
        register("accept"),
        ret(),
        enter("accept", 13),
        ret(),
    ]);
    [coordinator, utxo]
}

/// What the runtime emits for `examples/score.star`'s `example` script.
fn expected_score_traces(resource: ResourceHandle) -> [Trace; 2] {
    let calls = [("plus_chips", 42), ("plus_mult", 2), ("mult_mult", 200)];
    let mut coordinator = vec![Step::NewUtxo {
        arguments: UNIT,
        resource: Out(resource),
    }];
    coordinator.extend(calls.map(|(method, argument)| call(resource, method, argument)));
    coordinator.push(ret());

    let mut utxo = vec![Step::EnterConstructor { arguments: UNIT }];
    utxo.extend(["plus_chips", "plus_mult", "mult_mult", "finish"].map(register));
    utxo.push(ret());
    for (method, argument) in calls {
        utxo.extend([enter(method, argument), ret()]);
    }
    [Trace::new(coordinator), Trace::new(utxo)]
}

/// Check the execution phase against `expected` and return the storage root
/// the UTXO published in finalization.
fn check_execution(
    execution: &TracedExecution,
    expected: impl FnOnce(ResourceHandle) -> [Trace; 2],
) -> wasmtime::Result<StarstreamValue> {
    common::check_wasm_constraints(execution)?;
    let traces = execution.semantic_traces();
    let [coordinator, utxo] = traces.as_slice() else {
        bail!("expected one coordinator and one UTXO trace, got {traces:?}");
    };
    let [expected_coordinator, expected_utxo] = expected(constructed_resource(coordinator));
    assert_eq!(*coordinator, expected_coordinator);
    let Some((Step::GetStorage { storage }, executed)) = utxo.0.split_last() else {
        bail!("the surviving UTXO must end with its storage read: {utxo:?}");
    };
    assert_eq!(executed, expected_utxo.0);
    Ok(storage.0.clone())
}

fn check_transaction(
    execution: &TracedExecution,
    storage: StarstreamValue,
    methods: &[&str],
) -> wasmtime::Result<()> {
    let transaction = &execution.transaction;
    assert_eq!(
        transaction.statement,
        TransactionStatement {
            inputs: vec![],
            outputs: vec![OutputUtxo {
                utxo: 0,
                storage,
                methods: methods.iter().map(|name| method_hash(name)).collect(),
            }],
        }
    );
    verify_sat(&transaction.execution)
        .map_err(|error| wasmtime::format_err!("circuit rejected the execution: {error}"))?;
    verify_transaction_sat(
        &transaction.trace,
        BATCH_SIZE,
        &transaction.statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("circuit rejected the transaction: {error}"))?;

    // The circuit binds every coroutine's events to the chain Neo-Wasm proved
    // for it, and the transaction digest to those roots: another UTXO chain
    // is rejected by one of the two.
    let mut tampered: TraceCommitments = execution.commitments.clone();
    let utxo_chain = tampered.get_mut(&1).expect("Utxo(0) commitment");
    utxo_chain[0] = utxo_chain[0].wrapping_add(1);
    let rejected = verify_transaction_sat(
        &transaction.trace,
        BATCH_SIZE,
        &transaction.statement,
        &tampered,
    );
    assert!(
        matches!(
            rejected,
            Err(Error::Unsatisfied(
                Unsatisfied::TraceCommitments | Unsatisfied::TransactionCommitment
            ))
        ),
        "a tampered UTXO commitment must be rejected: {rejected:?}"
    );
    Ok(())
}

#[tokio::test]
async fn minimal_method_call_transaction_satisfies_the_circuit() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(MINIMAL_METHOD_CALL, "example").await?;
    let storage = check_execution(&execution, expected_minimal_method_call_traces)?;
    check_transaction(&execution, storage, &["accept"])
}

#[tokio::test]
async fn score_transaction_satisfies_the_circuit() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(SCORE, "example").await?;
    let storage = check_execution(&execution, expected_score_traces)?;
    check_transaction(
        &execution,
        storage,
        &["plus_chips", "plus_mult", "mult_mult", "finish"],
    )
}

#[tokio::test]
#[ignore = "requires Quint; run with --include-ignored"]
async fn traced_transactions_satisfy_the_quint_specification() -> wasmtime::Result<()> {
    let verifier = QuintVerifier::new()
        .map_err(|error| wasmtime::format_err!("Quint is unavailable: {error}"))?;
    for (name, source) in [
        ("minimal method call", MINIMAL_METHOD_CALL),
        ("score", SCORE),
        ("multiple UTXOs", common::MULTI_UTXO),
        ("distinct UTXO types", common::DISTINCT_UTXO_TYPES),
    ] {
        let transaction = trace_coordination_script(source, "example")
            .await?
            .transaction;
        verifier.verify(&transaction.execution).map_err(|error| {
            wasmtime::format_err!("Quint rejected the {name} execution: {error}")
        })?;
        verifier
            .verify_transaction(&transaction.trace, &transaction.statement)
            .map_err(|error| {
                wasmtime::format_err!("Quint rejected the {name} transaction: {error}")
            })?;
        if source == common::DISTINCT_UTXO_TYPES {
            let (invalid, call_index) = call_other_types_method(&transaction.execution);
            let error = verifier
                .verify(&invalid)
                .expect_err("wrong-type method must be rejected");
            assert!(
                matches!(
                    error,
                    starstream_interleaving_spec::QuintError::RejectedStep { .. }
                ),
                "{error:?}"
            );
            // The circuit checks membership at the call, before entering the callee.
            assert!(matches!(
                verify_sat(&invalid),
                Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
            ));
            assert!(matches!(invalid.0[call_index], Step::CallMethod { .. }));
        }
    }
    Ok(())
}

#[tokio::test]
async fn multiple_utxos_finalize_in_allocation_order_with_consumed_gap() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(common::MULTI_UTXO, "example").await?;
    assert_eq!(execution.instances.len(), 4);
    let coordinator = &execution.instances[0].steps;
    let handles: Vec<_> = coordinator
        .0
        .iter()
        .filter_map(|step| match step {
            Step::NewUtxo {
                resource: Out(handle),
                ..
            } => Some(*handle),
            _ => None,
        })
        .collect();
    assert_eq!(handles.len(), 3);
    let mut expected = Vec::new();
    for (handle, initial) in handles.iter().zip([10, 20, 30]) {
        expected.extend([
            Step::NewUtxo {
                arguments: u64_root(initial),
                resource: Out(*handle),
            },
            Step::EnterConstructor {
                arguments: u64_root(initial),
            },
            register("increment"),
            register("finish"),
            ret(),
        ]);
    }
    for (utxo, method) in [(2, "increment"), (0, "increment"), (1, "finish")] {
        expected.extend([
            Step::CallMethod {
                resource: handles[utxo],
                method: method_hash(method),
                arguments: UNIT,
                result: Out(UNIT),
            },
            Step::EnterMethod {
                method: method_hash(method),
                arguments: UNIT,
            },
        ]);
        if method == "finish" {
            expected.push(Step::YieldBegin);
        }
        expected.push(ret());
    }
    expected.push(ret());
    assert_eq!(execution.transaction.execution, Trace::new(expected));

    let storage_root = |instance: usize| match execution.instances[instance].steps.0.last() {
        Some(Step::GetStorage { storage }) => storage.0.clone(),
        other => panic!("expected final storage read, got {other:?}"),
    };
    let a = storage_root(1);
    let c = storage_root(3);
    assert_ne!(a, c);
    assert!(
        !execution.instances[2]
            .steps
            .0
            .iter()
            .any(|step| matches!(step, Step::GetStorage { .. }))
    );
    assert_eq!(execution.storage.len(), 2);
    for (record, expected_count) in execution.storage.iter().zip([11, 31]) {
        assert!(record.iter().any(|(name, value)| name == "count" && matches!(value, wasmtime::component::Val::S64(count) if *count == expected_count)), "unexpected storage: {record:?}");
    }
    let transaction = &execution.transaction;
    assert_eq!(
        transaction.statement,
        TransactionStatement {
            inputs: vec![],
            outputs: vec![
                OutputUtxo {
                    utxo: 0,
                    storage: a.clone(),
                    methods: vec![method_hash("increment"), method_hash("finish")]
                },
                OutputUtxo {
                    utxo: 2,
                    storage: c.clone(),
                    methods: vec![method_hash("increment"), method_hash("finish")]
                },
            ],
        }
    );
    assert_eq!(
        &transaction.trace.0[transaction.execution.0.len()..],
        &[
            Step::GetStorage { storage: Out(a) },
            Step::ReadAbi {
                method: method_hash("increment")
            },
            Step::ReadAbi {
                method: method_hash("finish")
            },
            Step::SkipConsumed,
            Step::GetStorage { storage: Out(c) },
            Step::ReadAbi {
                method: method_hash("increment")
            },
            Step::ReadAbi {
                method: method_hash("finish")
            },
            Step::FinalizeCoordinator,
            Step::FinishTransaction,
        ]
    );
    verify_transaction_sat(
        &transaction.trace,
        BATCH_SIZE,
        &transaction.statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("circuit rejected transaction: {error}"))?;
    Ok(())
}

fn call_other_types_method(trace: &Trace) -> (Trace, usize) {
    let mut invalid = trace.clone();
    let index = invalid
        .0
        .iter()
        .position(|step| {
            matches!(step,
                Step::CallMethod { method, .. } if *method == method_hash("reset")
            )
        })
        .expect("call to ResettableCounter.reset");
    // Change both boundaries so argument/expected-method equality cannot mask
    // the missing membership in the target UTXO's ABI.
    for step in &mut invalid.0[index..=index + 1] {
        match step {
            Step::CallMethod { method, .. } | Step::EnterMethod { method, .. } => {
                *method = method_hash("finish")
            }
            other => panic!("expected call/enter pair, got {other:?}"),
        }
    }
    (invalid, index)
}

#[tokio::test]
async fn distinct_utxo_types_route_shared_methods_and_reject_foreign_methods()
-> wasmtime::Result<()> {
    let execution = trace_coordination_script(common::DISTINCT_UTXO_TYPES, "example").await?;
    assert_eq!(execution.instances.len(), 3);
    assert_eq!(execution.storage.len(), 2);
    for (record, (field, expected)) in execution.storage.iter().zip([("count", 12), ("value", 1)]) {
        assert!(
            record.iter().any(|(name, value)| name == field
                && matches!(value, wasmtime::component::Val::S64(value) if *value == expected)),
            "unexpected storage: {record:?}"
        );
    }
    let transaction = &execution.transaction;
    assert!(transaction.statement.inputs.is_empty());
    assert_eq!(transaction.statement.outputs.len(), 2);
    for (index, (output, methods)) in transaction
        .statement
        .outputs
        .iter()
        .zip([["bump", "finish"], ["advance", "reset"]])
        .enumerate()
    {
        assert_eq!(output.utxo, index as u32);
        assert_eq!(output.methods, methods.map(method_hash).to_vec());
        let steps = &execution.instances[index + 1].steps.0;
        let entered: Vec<_> = steps
            .iter()
            .filter_map(|step| match step {
                Step::EnterMethod { method, .. } => Some(*method),
                _ => None,
            })
            .collect();
        let expected = if index == 0 {
            vec![method_hash("bump"); 2]
        } else {
            vec![
                method_hash("advance"),
                method_hash("reset"),
                method_hash("advance"),
            ]
        };
        assert_eq!(entered, expected);
        assert_eq!(
            steps.last(),
            Some(&Step::GetStorage {
                storage: Out(output.storage.clone())
            })
        );
    }
    verify_transaction_sat(
        &transaction.trace,
        BATCH_SIZE,
        &transaction.statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("circuit rejected transaction: {error}"))?;
    let (invalid, _) = call_other_types_method(&transaction.execution);
    // No stale statement/root check: this must fail the actual memory checks.
    assert!(matches!(
        verify_sat(&invalid),
        Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
    ));
    Ok(())
}

#[cfg(feature = "proof-tests")]
mod proving {
    use super::*;
    use crate::common::interleaving_params;

    #[tokio::test]
    #[ignore = "folding proof; run with --release --features proof-tests -- --ignored"]
    async fn minimal_method_call_transaction_proves_and_verifies() -> wasmtime::Result<()> {
        let execution = trace_coordination_script(MINIMAL_METHOD_CALL, "example").await?;
        prove_interleaving(&execution)
    }

    #[tokio::test]
    #[ignore = "Wasm and interleaving folding proofs; run in release mode (this is really slow/expensive, can take 10min+)"]
    async fn minimal_method_call_wasm_and_interleaving_proofs_verify() -> wasmtime::Result<()> {
        let execution = trace_coordination_script(MINIMAL_METHOD_CALL, "example").await?;
        common::prove_wasm_instances(&execution)?;
        prove_interleaving(&execution)
    }

    fn prove_interleaving(execution: &TracedExecution) -> wasmtime::Result<()> {
        use starstream_interleaving_prover::proving::TransactionProofContext;

        let transaction = &execution.transaction;
        let start = std::time::Instant::now();
        let context = TransactionProofContext::new(BATCH_SIZE, interleaving_params(), [7; 32])
            .map_err(|error| wasmtime::format_err!("preprocessing failed: {error}"))?;
        eprintln!("interleaving preprocessing: {:?}", start.elapsed());
        let start = std::time::Instant::now();
        let proof = context
            .prove(
                &transaction.trace,
                &transaction.statement,
                &execution.commitments,
            )
            .map_err(|error| wasmtime::format_err!("proving failed: {error}"))?;
        eprintln!("interleaving proving: {:?}", start.elapsed());
        let start = std::time::Instant::now();
        context
            .verify(&proof, &transaction.statement, &execution.commitments)
            .map_err(|error| wasmtime::format_err!("verification failed: {error}"))?;
        eprintln!("interleaving verification: {:?}", start.elapsed());

        let mut other_statement = transaction.statement.clone();
        other_statement.outputs[0].storage.0[0] =
            other_statement.outputs[0].storage.0[0].wrapping_add(1);
        assert!(
            context
                .verify(&proof, &other_statement, &execution.commitments)
                .is_err(),
            "a proof must not verify against another statement"
        );
        let mut other_commitments = execution.commitments.clone();
        other_commitments.get_mut(&2).expect("Coord(1)")[1] ^= 1;
        assert!(
            context
                .verify(&proof, &transaction.statement, &other_commitments)
                .is_err(),
            "a proof must not verify against other coroutine roots"
        );
        Ok(())
    }
}

#[tokio::test]
async fn storage_counter_transaction_satisfies_the_circuit() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(STORAGE_COUNTER, "example").await?;
    common::check_wasm_constraints(&execution)?;
    assert_eq!(execution.instances.len(), 2);
    assert_eq!(
        execution.transaction.statement,
        common::storage_counter_statement()
    );
    verify_transaction_sat(
        &execution.transaction.trace,
        BATCH_SIZE,
        &execution.transaction.statement,
        &execution.commitments,
    )
    .map_err(|error| wasmtime::format_err!("{error}"))?;
    Ok(())
}
