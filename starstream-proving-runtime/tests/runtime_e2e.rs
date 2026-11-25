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
use starstream_proving_runtime::{
    constructor_identity, effect_hash_from_names, flat_value_root, method_hash_from_name,
};
use wasmparser::ValType;
use wasmtime::bail;

use crate::common::{
    MINIMAL_METHOD_CALL, SCORE, STORAGE_COUNTER, TracedExecution, trace_coordination_script,
};

/// Batch size of the interleaving relation used by the checks below.
const BATCH_SIZE: usize = 16;

const UNIT: StarstreamValue = StarstreamValue::UNIT_VALUE;

const EFFECT_REENTRY: &str = r#"
    abi Foo {
        effect Hello();
        event Log(point: u32);
    }

    script fn main() {
        emit Log(1);
        try {
            emit Log(2);
            raise Hello();
            emit Log(4);
        } with Hello() {
            emit Log(3);
        }
        emit Log(5);
    }
"#;

const UNHANDLED_EFFECT: &str = r#"
    abi Foo {
        effect Hello();
    }

    script fn main() {
        try {
        } with Hello() {
        }
        raise Hello();
    }
"#;

/// Start at script entry with a detached token restored in its own component.
#[tokio::test]
async fn send_single_token_from_storage() -> wasmtime::Result<()> {
    use starstream_proving_runtime::{
        WasmTraceSink, build_component_templates, decode_captured_instance, enable_tracing,
        register_tracing_component,
    };
    use starstream_runtime::{Contract, Host};
    use wasmtime::component::{Component, Val};
    use wasmtime::error::Context as _;

    let mut config = wasmtime::Config::default();
    starstream_runtime::configure_wasmtime(&mut config);
    let engine = wasmtime::Engine::new(&config)?;
    let mut store = wasmtime::Store::new(&engine, common::Ctx::default());
    let mut contract = |source| -> wasmtime::Result<Contract<common::Ctx>> {
        let wasm = common::compile_component(source)?;
        let contract = Contract::new(
            &Component::from_binary(&engine, &wasm)?,
            None,
            common::NoopContractLookup,
        )?;
        let names = contract.coordination_script_names().collect::<Vec<_>>();
        let digest = <sha2::Sha256 as sha2::Digest>::digest(&wasm).into();
        let templates = build_component_templates(&wasm, &names, &digest)?;
        register_tracing_component(store.data_mut(), &wasm, &templates.bindings)?;
        Ok(contract)
    };
    let payment = contract(include_str!("../../examples/pay_to_public_key.star"))?;
    let token_contract = contract(include_str!(
        "../../starstream-to-wasm/tests/inputs/token_attach.star"
    ))?;
    enable_tracing(&mut store)?;
    let coordinator = payment.instantiate(&mut store).await?;
    let token_instance = token_contract.instantiate(&mut store).await?;
    let export = token_contract.get_token("my-token")?;
    let token = token_instance
        .load_token(
            &mut store,
            &export,
            export.storage(),
            [(String::from("attached"), Val::S64(0))],
        )
        .await?;
    let token_id = store
        .data()
        .wasm_trace_registry()
        .instances()?
        .next()
        .context("missing token load")?
        .0;
    let handle = store.data_mut().table().push(token.clone())?;
    let handle = handle.try_into_resource_any(&mut store)?;
    let recipient = Val::Record(vec![(String::from("dummy"), Val::U64(17))]);
    coordinator
        .call_coordination_script(
            &mut store,
            &payment.get_coordination_script("send-single-token")?,
            [recipient, Val::Resource(handle)],
            [],
        )
        .await?;
    let storage = token.storage(export.storage()).call_get(&mut store).await?;
    assert_eq!(storage, [("attached".to_owned(), Val::S64(1))]);
    let registry = store.data().wasm_trace_registry();
    let (_, coordinator_trace) = registry
        .instances()?
        .next()
        .context("missing coordinator")?;
    let coordinator_steps = decode_captured_instance(coordinator_trace, &mut std::iter::empty())?;
    let Some(Step::EnterCall { arguments, .. }) = coordinator_steps.0.first() else {
        panic!("missing payment entry")
    };
    // The by-value entry arguments are the recipient's two limbs followed by
    // the token handle in this coordination instance's canonical ABI table.
    let [17, 0, token_handle, 0] = arguments.0 else {
        panic!("unexpected payment entry arguments: {arguments:?}")
    };
    let handle = ResourceHandle(u32::try_from(token_handle)?);
    assert_ne!(handle, constructed_resource(&coordinator_steps));
    let token_steps =
        decode_captured_instance(registry.instance(token_id)?, &mut [handle].into_iter())?;
    assert!(matches!(token_steps.0.first(), Some(Step::SetStorage {
        kind: starstream_proving_runtime::StorageKind::Token,
        storage,
        ..
    }) if *storage == flat_value_root(&[ValType::I64], &[0])));
    assert_eq!(
        token_steps.0.last(),
        Some(&Step::GetStorage {
            storage: Out(flat_value_root(&[ValType::I64], &[1])),
        })
    );
    let [output] = store.data().created.as_slice() else {
        panic!("expected one PayToPublicKey output")
    };
    let output = output.clone();
    let pay_to = payment.get_utxo("pay-to-public-key")?;
    let storage = output
        .storage(pay_to.storage().context("missing payment storage")?)
        .call_get(&mut store)
        .await?;
    assert!(storage.contains(&("recipient".to_owned(), Val::S64(17))));
    Ok(())
}

#[test]
fn verification_preparation_rejects_wrong_setter_kind() -> wasmtime::Result<()> {
    use starstream_proving_runtime::{VerificationProgram, WasmSize, prepare_verification};

    let wasm = common::compile_component(
        r#"
        token MyToken {
            storage { let mut count: u32; }
            mint fn new() { count = 0; }
            impl Token {
                fn attach(to: Utxo) { count = count + 1; }
                fn detach(source: Utxo) { count = count - 1; }
            }
        }
        utxo Holder {
            main fn new() { yield(); }
        }
    "#,
    )?;
    let digest = <sha2::Sha256 as sha2::Digest>::digest(&wasm).into();
    let prepare = |utxo: &str, token: &str| {
        prepare_verification(
            &[VerificationProgram {
                wasm: &wasm,
                contract_digest: &digest,
                coordination_exports: &[],
                is_original_image: true,
                input_utxos: &[(0, utxo.to_owned())],
                input_tokens: &[(0, token.to_owned())],
                new_utxos: &[],
            }],
            [WasmSize::Large],
        )
    };
    prepare("holder", "my-token")?;
    for (utxo, token) in [("holder", "holder"), ("my-token", "my-token")] {
        let error = prepare(utxo, token).err().expect("wrong setter kind");
        assert!(error.to_string().contains("wrong resource kind"), "{error}");
    }
    Ok(())
}

/// Identity of the `new` constructor of `utxo`, as the contract that ran exports it.
fn constructor(execution: &TracedExecution, utxo: &str) -> MethodHash {
    let digest = <sha2::Sha256 as sha2::Digest>::digest(&execution.wasm).into();
    constructor_identity(&execution.wasm, &digest, utxo, "new").unwrap()
}

#[test]
fn constructor_identity_distinguishes_signatures_with_equal_inline_roots() {
    let identity = |params: &str, args: &str| {
        let source = MINIMAL_METHOD_CALL
            .replace("initial: u64", params)
            .replace("::new(55)", &format!("::new({args})"));
        let wasm = common::compile_contract(&source);
        // Fix the digest to isolate the parameter signature.
        constructor_identity(&wasm, &[0; 32], "method-call-utxo", "new").unwrap()
    };
    assert_eq!(
        flat_value_root(&[ValType::I64], &[7]),
        flat_value_root(&[ValType::I32; 2], &[7, 0])
    );
    assert_ne!(identity("x: u64", "7"), identity("x: u32, y: u32", "7, 0"));
    assert_ne!(identity("x: u32", "7"), identity("x: u8", "7"));
    assert_ne!(
        identity("x: (u32, u32)", "(7, 0)"),
        identity("x: u32, y: u32", "7, 0")
    );
    assert_eq!(identity("x: u64", "7"), identity("renamed: u64", "7"));
}

fn u64_root(value: u64) -> StarstreamValue {
    flat_value_root(&[ValType::I64], &[value])
}

fn ret() -> Step {
    Step::Return { result: Out(UNIT) }
}

fn method_hash(name: &str) -> MethodHash {
    method_hash_from_name(name)
}

/// The entry call the coordinator's own trace commits to: these fixtures'
/// entry scripts take no arguments. The statement must carry the same call.
fn entry_call(execution: &TracedExecution) -> starstream_interleaving_spec::EntryCall {
    let Some(Step::EnterCall { method, arguments }) = execution.instances[0].steps.0.first() else {
        panic!("the entry coordinator must open with its entry event");
    };
    assert_eq!(*arguments, UNIT);
    starstream_interleaving_spec::EntryCall {
        script: *method,
        arguments: arguments.clone(),
    }
}

fn call(resource: ResourceHandle, method: &str, argument: u64) -> Step {
    Step::CallMethod {
        resource,
        method: method_hash(method),
        arguments: u64_root(argument),
    }
}

fn receive_call_result() -> Step {
    Step::ReceiveCallResult { result: Out(UNIT) }
}

fn enter(method: &str, argument: u64) -> Step {
    Step::EnterCall {
        method: method_hash(method),
        arguments: u64_root(argument),
    }
}

fn register(method: &str) -> Step {
    Step::RegisterMethod {
        method: method_hash(method),
    }
}

/// The entry coordinator's own trace: its entry event, then `body`.
fn entry_coordinator(
    entry: &starstream_interleaving_spec::EntryCall,
    body: impl IntoIterator<Item = Step>,
) -> Trace {
    Trace::new(Trace::with_entry(entry, body).0.split_off(1))
}

/// The resource handle the coordinator observed for its single constructor.
fn constructed_resource(coordinator: &Trace) -> ResourceHandle {
    coordinator
        .0
        .iter()
        .find_map(|step| match step {
            Step::ReceiveNewUtxoHandle { resource } => Some(resource.0),
            _ => None,
        })
        .unwrap_or_else(|| panic!("coordinator trace has no received UTXO handle: {coordinator:?}"))
}

/// What the runtime emits for the counter script, per core instance, before
/// the UTXO's finalization storage read.
///
/// The compiler only calls `utxo-context.resume` for the `resume;` statement,
/// never at a `yield`, so a constructor's first yield registers its methods
/// without a `YieldBegin`. The specification matches: constructor entry opens
/// the initial ABI window.
fn expected_minimal_method_call_traces(
    execution: &TracedExecution,
    resource: ResourceHandle,
) -> [Trace; 2] {
    let constructor = constructor(execution, "method-call-utxo");
    let coordinator = Trace::new([
        Step::NewUtxo {
            constructor,
            arguments: u64_root(55),
        },
        Step::ReceiveNewUtxoHandle {
            resource: Out(resource),
        },
        call(resource, "accept", 13),
        receive_call_result(),
        ret(),
    ]);
    let utxo = Trace::new([
        Step::EnterConstructor {
            constructor,
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
fn expected_score_traces(execution: &TracedExecution, resource: ResourceHandle) -> [Trace; 2] {
    let constructor = constructor(execution, "score-progress");
    let calls = [("plus_chips", 42), ("plus_mult", 2), ("mult_mult", 200)];
    let mut coordinator = vec![
        Step::NewUtxo {
            constructor,
            arguments: UNIT,
        },
        Step::ReceiveNewUtxoHandle {
            resource: Out(resource),
        },
    ];
    for (method, argument) in calls {
        coordinator.extend([call(resource, method, argument), receive_call_result()]);
    }
    coordinator.push(ret());

    let mut utxo = vec![Step::EnterConstructor {
        constructor,
        arguments: UNIT,
    }];
    utxo.extend(
        [
            "get_chips",
            "get_mult",
            "plus_chips",
            "plus_mult",
            "mult_mult",
            "finish",
        ]
        .map(register),
    );
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
    expected: impl FnOnce(&TracedExecution, ResourceHandle) -> [Trace; 2],
) -> wasmtime::Result<StarstreamValue> {
    common::check_wasm_constraints(execution)?;
    let traces = execution.semantic_traces();
    let [coordinator, utxo] = traces.as_slice() else {
        bail!("expected one coordinator and one UTXO trace, got {traces:?}");
    };
    let [expected_coordinator, expected_utxo] =
        expected(execution, constructed_resource(coordinator));
    // The coordinator's body follows its entry event.
    assert_eq!(
        *coordinator,
        entry_coordinator(&entry_call(execution), expected_coordinator.0)
    );
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
            input_tokens: Vec::new(),
            output_tokens: Vec::new(),
            signers: Default::default(),
            entry: entry_call(execution),
            inputs: vec![],
            outputs: vec![OutputUtxo {
                utxo: 0,
                storage,
                methods: methods.iter().map(|name| method_hash(name)).collect(),
            }],
        }
    );
    verify_sat(&transaction.execution, &Default::default())
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
        &[
            "get_chips",
            "get_mult",
            "plus_chips",
            "plus_mult",
            "mult_mult",
            "finish",
        ],
    )
}

#[tokio::test]
async fn effect_handler_reentry_satisfies_the_circuit() -> wasmtime::Result<()> {
    let execution = trace_coordination_script(EFFECT_REENTRY, "main").await?;
    common::check_wasm_constraints(&execution)?;

    let effect = effect_hash_from_names("foo", "hello");
    // The compiler numbers `try` blocks from 1.
    let frame_id = 1;
    let entry = entry_call(&execution);
    let body = [
        Step::PushEffectHandlerFrame { frame_id },
        Step::AddEffectHandler { effect },
        Step::RaiseEffect {
            effect,
            arguments: UNIT,
        },
        Step::EnterEffectHandler {
            frame_id,
            effect,
            arguments: UNIT,
        },
        Step::ExitEffectHandler {
            handled: true,
            result: Out(UNIT),
        },
        Step::ResumeEffect { result: Out(UNIT) },
        Step::PopEffectHandlerFrame,
        ret(),
    ];
    assert_eq!(
        execution.semantic_traces().as_slice(),
        [entry_coordinator(&entry, body.clone())]
    );
    // The interleaved execution opens with the host's entry call.
    assert_eq!(
        execution.transaction.execution,
        Trace::with_entry(&entry, body)
    );
    verify_sat(&execution.transaction.execution, &Default::default())
        .map_err(|error| wasmtime::format_err!("circuit rejected effect re-entry: {error}"))?;
    Ok(())
}

#[tokio::test]
async fn effect_raised_after_its_scope_is_popped_is_unhandled() {
    let Err(error) = trace_coordination_script(UNHANDLED_EFFECT, "main").await else {
        panic!("the popped scope must not handle a later raise");
    };
    assert!(
        format!("{error:?}").contains("effect not handled"),
        "{error:?}"
    );
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
                verify_sat(&invalid, &Default::default()),
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
            Step::ReceiveNewUtxoHandle { resource } => Some(resource.0),
            _ => None,
        })
        .collect();
    assert_eq!(handles.len(), 3);
    let constructor = constructor(&execution, "counter-utxo");
    let mut expected = Vec::new();
    for (handle, initial) in handles.iter().zip([10, 20, 30]) {
        expected.extend([
            Step::NewUtxo {
                constructor,
                arguments: u64_root(initial),
            },
            Step::EnterConstructor {
                constructor,
                arguments: u64_root(initial),
            },
            register("increment"),
            register("finish"),
            ret(),
            Step::ReceiveNewUtxoHandle {
                resource: Out(*handle),
            },
        ]);
    }
    for (utxo, method) in [(2, "increment"), (0, "increment"), (1, "finish")] {
        expected.extend([
            Step::CallMethod {
                resource: handles[utxo],
                method: method_hash(method),
                arguments: UNIT,
            },
            Step::EnterCall {
                method: method_hash(method),
                arguments: UNIT,
            },
        ]);
        if method == "finish" {
            expected.push(Step::YieldBegin);
        }
        expected.push(ret());
        expected.push(receive_call_result());
    }
    expected.push(ret());
    assert_eq!(
        execution.transaction.execution,
        Trace::with_entry(&entry_call(&execution), expected)
    );

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
            input_tokens: Vec::new(),
            output_tokens: Vec::new(),
            signers: Default::default(),
            entry: entry_call(&execution),
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
            Step::CallMethod { method, .. } | Step::EnterCall { method, .. } => {
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
                Step::EnterCall { method, .. } => Some(*method),
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
        verify_sat(&invalid, &Default::default()),
        Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
    ));
    Ok(())
}

#[cfg(feature = "proof-tests")]
mod proving {
    use super::*;

    #[tokio::test]
    #[ignore = "Nebula proofs; run with --release --features proof-tests -- --ignored"]
    async fn two_utxo_proofs_keep_order_and_bind_each_finalized_root() -> wasmtime::Result<()> {
        use starstream_proving_runtime::{Engine, TransactionProver, decode_captured_execution};

        let (contracts, _) = common::DISTINCT_UTXO_TYPES
            .split_once("script fn example()")
            .expect("two UTXO definitions followed by the example script");
        let source = format!(
            "{contracts}script fn spend() {{
                let a = FinishableCounter::new(10);
                let b = ResettableCounter::new(20);
                b.advance();
                a.finish();
            }}"
        );
        let (traced, registry) =
            common::trace_coordination_script_with_registry(&source, "spend").await?;
        let mut execution = decode_captured_execution(&registry, [])?;
        let [output] = execution.transaction.statement.outputs.as_slice() else {
            panic!("expected one surviving UTXO");
        };
        assert_eq!(output.utxo, 1);
        let ids = [0, 1, 2].map(common::coroutine_id);
        assert_ne!(traced.commitments[&ids[1]], traced.commitments[&ids[2]]);
        let wasm = traced.wasm;
        let mut prover = TransactionProver::new(Engine::Optimized);
        let (cold, _) = prover.prove(&registry, &execution)?;
        let expected = cold.interleaving.to_bytes()?;
        drop(cold);
        let (warm, timings) = prover.prove(&registry, &execution)?;
        assert!(timings.interleaving_circuit_reused);
        assert!(timings.wasm.iter().all(|timing| timing.circuit_reused));
        assert!(
            warm.instances
                .iter()
                .map(|instance| instance.coroutine)
                .eq(ids)
        );
        assert!(warm.interleaving.to_bytes()? == expected);
        let verification = common::prepare_contract_verification(&wasm, "spend", &warm)?;
        prover.verify(&warm, &verification, &execution.transaction.statement)?;
        drop(warm);

        execution.transaction.utxos.swap(0, 1);
        let error = prover
            .prove(&registry, &execution)
            .err()
            .expect("swapped Wasm owners");
        assert!(
            format!("{error:#}").contains("transaction commitment does not match the statement")
        );
        Ok(())
    }

    #[tokio::test]
    #[ignore = "Nebula proof; run with --release --features proof-tests -- --ignored"]
    async fn minimal_method_call_transaction_proves_and_verifies() -> wasmtime::Result<()> {
        let execution = trace_coordination_script(MINIMAL_METHOD_CALL, "example").await?;
        prove_interleaving(&execution)
    }

    #[tokio::test]
    #[ignore = "Wasm and interleaving Nebula proofs; run with --release --features proof-tests -- --ignored"]
    async fn minimal_method_call_wasm_and_interleaving_proofs_verify() -> wasmtime::Result<()> {
        let execution = trace_coordination_script(MINIMAL_METHOD_CALL, "example").await?;
        common::prove_wasm_instances(&execution)?;
        prove_interleaving(&execution)
    }

    fn prove_interleaving(execution: &TracedExecution) -> wasmtime::Result<()> {
        use starstream_interleaving_prover::proving::{
            InterleavingProofContext, Limits, ProofConfig, method_dictionary,
        };

        let transaction = &execution.transaction;
        let start = std::time::Instant::now();
        let batch_size = std::env::var("STARSTREAM_PROOF_BATCH_SIZE")
            .ok()
            .map(|s| s.parse::<usize>())
            .transpose()?
            .unwrap_or(BATCH_SIZE);
        let segment_batches = std::env::var("STARSTREAM_PROOF_SEGMENT_BATCHES")
            .ok()
            .map(|s| s.parse::<usize>())
            .transpose()?
            .unwrap_or(8);
        let mut config = if std::env::var_os("STARSTREAM_PROOF_TIGHT_MEMORY").is_some() {
            ProofConfig::for_trace_benchmark(
                &transaction.trace,
                &transaction.statement.signers,
                batch_size,
                segment_batches,
            )?
        } else {
            ProofConfig::for_limits(Limits::SMALL_DEFAULTS, batch_size)?
                .with_segment_batches(segment_batches)?
        };
        config.engine = common::proof_engine();
        eprintln!(
            "interleaving: {} rows, batch {batch_size}, {:?}, {:?}",
            transaction.trace.0.len(),
            config.geometry,
            config.engine
        );
        let context = InterleavingProofContext::new(config)
            .map_err(|error| wasmtime::format_err!("preprocessing failed: {error}"))?;
        let methods = method_dictionary(&transaction.trace);
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
            .verify(
                &methods,
                &proof,
                &transaction.statement,
                &execution.commitments,
            )
            .map_err(|error| wasmtime::format_err!("verification failed: {error}"))?;
        eprintln!("interleaving verification: {:?}", start.elapsed());

        let mut other_statement = transaction.statement.clone();
        other_statement.outputs[0].storage.0[0] =
            other_statement.outputs[0].storage.0[0].wrapping_add(1);
        assert!(
            context
                .verify(&methods, &proof, &other_statement, &execution.commitments)
                .is_err(),
            "a proof must not verify against another statement"
        );
        let mut other_commitments = execution.commitments.clone();
        other_commitments
            .get_mut(&starstream_interleaving_prover::CoroutineId::Coord(1).encoded())
            .expect("Coord(1)")[1] ^= 1;
        assert!(
            context
                .verify(&methods, &proof, &transaction.statement, &other_commitments)
                .is_err(),
            "a proof must not verify against other coroutine roots"
        );
        let start = std::time::Instant::now();
        let setup = context
            .compression_setup()
            .map_err(|error| wasmtime::format_err!("compression setup failed: {error}"))?;
        let key = context
            .compression_key()
            .map_err(|error| wasmtime::format_err!("compression key failed: {error}"))?;
        eprintln!("interleaving compression setup: {:?}", start.elapsed());
        let start = std::time::Instant::now();
        let compressed = context
            .compress(&proof, &setup)
            .map_err(|error| wasmtime::format_err!("compression failed: {error}"))?;
        eprintln!("interleaving compression: {:?}", start.elapsed());
        let start = std::time::Instant::now();
        context
            .verify_compressed(
                &methods,
                &compressed,
                &key,
                &transaction.statement,
                &execution.commitments,
            )
            .map_err(|error| wasmtime::format_err!("compressed verification failed: {error}"))?;
        eprintln!(
            "interleaving compressed verification: {:?}",
            start.elapsed()
        );
        assert!(
            context
                .verify_compressed(
                    &methods,
                    &compressed,
                    &key,
                    &other_statement,
                    &execution.commitments
                )
                .is_err(),
            "a compressed proof must not verify against another statement"
        );
        assert!(
            context
                .verify_compressed(
                    &methods,
                    &compressed,
                    &key,
                    &transaction.statement,
                    &other_commitments
                )
                .is_err(),
            "a compressed proof must not verify against other coroutine roots"
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
        common::storage_counter_statement(entry_call(&execution))
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

#[tokio::test]
async fn minimal_method_call_runs_through_nebula() -> wasmtime::Result<()> {
    let execution =
        common::trace_coordination_script(common::MINIMAL_METHOD_CALL, "example").await?;
    let mut preload = neo_wasm::preload_from_program_artifacts(&execution.artifacts);
    neo_wasm::memory_semantics::preload_host_event_tables(
        &mut preload,
        &execution.templates.bindings,
        &execution.artifacts,
    );
    for instance in &execution.instances {
        let size = neo_wasm::nebula::WasmSize::for_run(&preload, &instance.trace)?;
        let app = size.application()?;
        let program = app.program(&preload)?;
        let mut run = nightstream::nebula::Run::new(app, &program, instance.trace[0].state_before);
        for segment in size.segments(&instance.trace)? {
            run.segment(&segment?)?;
        }
        assert_eq!(*run.state(), instance.trace.last().unwrap().state_after);
    }
    Ok(())
}
