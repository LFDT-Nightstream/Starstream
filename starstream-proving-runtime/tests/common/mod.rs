//! Shared harness: compile a Starstream contract, run one of its coordination
//! scripts on `starstream-runtime-next` under Neo-Wasm's single-step tracer,
//! and normalize and decode every core instance's trace.

// Each integration test binary uses a different subset of this module.
#![allow(dead_code)]

use std::pin::Pin;
use std::sync::{Arc, Mutex};

use neo_math::F;
use neo_wasm::comm_chain::{CommChainState, fold_event_blocks};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_interleaving_prover::{CoroutineId, TraceCommitments};
use starstream_interleaving_spec::interleaver::{InterleavedTransaction, interleave_transaction};
use starstream_interleaving_spec::{Trace, events};
use starstream_proving_runtime::{
    ComponentTemplates, build_component_templates, decode_absorbed_blocks,
    new_tracing_wasmtime_store, new_wasmtime_config,
};
use starstream_runtime_next::{
    Contract, ContractLookup, Host, StorageExport, Token, Utxo, bindings,
};
use starstream_to_wasm::compile;
use wasmtime::component::{Component, Resource, ResourceTable, Val};
use wasmtime::{AsContextMut, Engine, StoreContextMut, bail};

pub const MINIMAL_METHOD_CALL: &str = r#"
    abi MethodCall {
        fn accept(value: u64);
    }

    utxo MethodCallUtxo {
        main fn new(initial: u64) {
            yield(MethodCall);
        }

        impl MethodCall {
            fn accept(pub value: u64) {
            }
        }
    }

    script fn example() {
        let instance = MethodCallUtxo::new(55);
        instance.accept(13);
    }
"#;

pub const STORAGE_COUNTER: &str = r#"
    abi Counter {
        fn add(value: u64);
    }

    utxo CounterUtxo {
        storage { let mut count: u64; }

        main fn new(pub initial: u64) {
            count = initial;
            yield(Counter);
        }

        impl Counter {
            fn add(pub value: u64) {
                count = count + value;
            }
        }
    }

    script fn example() {
        let counter = CounterUtxo::new(55);
        counter.add(13);
    }
"#;

pub const SCORE: &str = include_str!("../../../examples/score.star");

pub const DISTINCT_UTXO_TYPES: &str = r#"
    abi Finishable {
        fn bump();
        fn finish();
    }
    abi Resettable {
        fn advance();
        fn reset();
    }
    utxo FinishableCounter {
        storage { let mut count: u64; }
        main fn new(pub initial: u64) {
            count = initial;
            yield(Finishable);
        }
        impl Finishable {
            fn bump() { count = count + 1; }
            fn finish() { resume; }
        }
    }
    utxo ResettableCounter {
        storage { let mut value: u64; }
        main fn new(pub initial: u64) {
            value = initial;
            yield(Resettable);
        }
        impl Resettable {
            fn advance() { value = value + 1; }
            fn reset() { value = 0; }
        }
    }
    script fn example() {
        let a = FinishableCounter::new(10);
        let b = ResettableCounter::new(20);
        a.bump();
        b.advance();
        a.bump();
        b.reset();
        b.advance();
    }
"#;

pub const MULTI_UTXO: &str = r#"
    abi Counter {
        fn increment();
        fn finish();
    }

    utxo CounterUtxo {
        storage { let mut count: u64; }

        main fn new(pub initial: u64) {
            count = initial;
            yield(Counter);
        }

        impl Counter {
            fn increment() {
                count = count + 1;
            }
            fn finish() {
                resume;
            }
        }
    }

    script fn example() {
        let a = CounterUtxo::new(10);
        let b = CounterUtxo::new(20);
        let c = CounterUtxo::new(30);
        c.increment();
        a.increment();
        b.finish();
    }
"#;

pub fn compile_contract(source: &str) -> Vec<u8> {
    let (program, errors) = parse_program(source).into_output_errors();
    assert!(errors.is_empty(), "parsing failed: {errors:?}");
    let program = program.expect("parser produced no program");
    let typed = typecheck_program(&program, TypecheckOptions::default())
        .unwrap_or_else(|failure| panic!("typechecking failed: {:?}", failure.errors));
    let result = compile(&typed.program);
    assert!(
        result.errors.is_empty(),
        "compiling failed: {:?}",
        result.errors
    );
    result.wasm.expect("compiling produced no Wasm")
}

pub struct NoopContractLookup;

impl<T> ContractLookup<T> for NoopContractLookup {
    fn get_contract(&self, external_id: &str) -> wasmtime::Result<Contract<T>> {
        bail!("contract with external_id `{external_id}` unknown")
    }
}

#[derive(Debug, Default)]
pub struct UtxoCtx {
    pub methods: Vec<(u64, u64, u64, u64)>,
}

pub struct Ctx {
    table: ResourceTable,
    traces: neo_wasm::WasmtimeTraceRegistry,
    /// Every UTXO constructed during the script, in allocation order.
    created: Vec<Utxo<Arc<Mutex<UtxoCtx>>>>,
}

impl bindings::starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        Ok(0)
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        Ok(0)
    }
}

impl Host for Ctx {
    type UtxoContext = Arc<Mutex<UtxoCtx>>;

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn call_utxo_main(
        mut store: StoreContextMut<'_, Self>,
        f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        let cx = Self::UtxoContext::default();
        let utxo = f(store.as_context_mut(), Arc::clone(&cx)).await?;
        store.data_mut().created.push(utxo.clone());
        Ok(utxo)
    }

    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let utxo = store.data().table.get(&utxo)?;
        Ok(utxo.context().lock().unwrap().methods.contains(&hash))
    }

    fn drop_utxo(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        store.data_mut().table.delete(utxo)?;
        Ok(())
    }

    fn implements_method(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get_mut(&cx)?;
        cx.lock().unwrap().methods.push(hash);
        Ok(())
    }

    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let cx = store.data_mut().table.get_mut(&cx)?;
        cx.lock().unwrap().methods.clear();
        Ok(())
    }

    fn drop_utxo_context(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        store.data_mut().table.delete(cx)?;
        Ok(())
    }

    fn drop_token(
        mut store: StoreContextMut<Self>,
        token: Resource<Token>,
    ) -> wasmtime::Result<()> {
        store.data_mut().table.delete(token)?;
        Ok(())
    }

    fn emit_event(
        _store: StoreContextMut<Self>,
        _abi_name: &Arc<str>,
        _name: &Arc<str>,
        _params: &[Val],
    ) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl neo_wasm::WasmTraceSink for Ctx {
    fn wasm_trace_registry(&self) -> &neo_wasm::WasmtimeTraceRegistry {
        &self.traces
    }

    fn wasm_trace_registry_mut(&mut self) -> &mut neo_wasm::WasmtimeTraceRegistry {
        &mut self.traces
    }
}

pub struct TracedInstance {
    pub entry_fref: u32,
    pub captured: Vec<neo_wasm::WasmtimeTraceStep>,
    pub trace: Vec<neo_wasm::WasmVmStep>,
    pub steps: Trace,
}

pub struct TracedExecution {
    pub templates: ComponentTemplates,
    pub artifacts: Arc<neo_wasm::WasmProgramArtifacts>,
    pub instances: Vec<TracedInstance>,
    pub transaction: InterleavedTransaction,
    pub commitments: TraceCommitments,
    pub storage: Vec<Vec<(String, Val)>>,
}

impl TracedExecution {
    pub fn semantic_traces(&self) -> Vec<Trace> {
        self.instances
            .iter()
            .map(|instance| instance.steps.clone())
            .collect()
    }
}

/// The interleaving prover's packed coroutine id of core instance `index`:
/// instance 0 is the coordination script `Coord(1)`, later instances are
/// `Utxo(0)`, `Utxo(1)`, ... in allocation order.
pub fn coroutine_id(index: usize) -> u32 {
    let id = match index.checked_sub(1) {
        None => CoroutineId::Coord(1),
        Some(utxo) => CoroutineId::Utxo(u32::try_from(utxo).expect("few UTXOs")),
    };
    id.encoded()
}

pub fn encoded_chain(steps: &Trace) -> [u64; 4] {
    let blocks = steps
        .0
        .iter()
        .flat_map(events::encode)
        .map(|block| block.map(F::new))
        .collect::<Vec<_>>();
    fold_event_blocks(CommChainState::default(), &blocks).canonical_u64()
}

// the single contract has more than one utxo, so this does a linear scan to
// find the storage export
//
// TODO: maybe there is a more direct way of getting this
fn storage_export_for(
    contract: &Contract<Ctx>,
    utxo: &Utxo<Arc<Mutex<UtxoCtx>>>,
    mut store: StoreContextMut<'_, Ctx>,
) -> wasmtime::Result<StorageExport> {
    let instance = utxo.instance();
    for (name, export) in contract.utxos() {
        let export = export?;
        let (_, interface) = instance
            .get_export(store.as_context_mut(), None, name)
            .ok_or_else(|| wasmtime::format_err!("missing UTXO interface {name}"))?;
        let (_, resource) = instance
            .get_export(store.as_context_mut(), Some(&interface), "utxo")
            .ok_or_else(|| wasmtime::format_err!("missing UTXO resource in {name}"))?;
        if instance.get_resource(store.as_context_mut(), resource) == Some(utxo.resource().ty()) {
            return export
                .storage()
                .cloned()
                .ok_or_else(|| wasmtime::format_err!("{name} has no storage export"));
        }
    }
    bail!("no storage export matches the UTXO resource type")
}

/// Compile `source`, run its `script` coordination script under tracing, and
/// normalize and decode one trace per core instance in instantiation order.
pub async fn trace_coordination_script(
    source: &str,
    script: &str,
) -> wasmtime::Result<TracedExecution> {
    let wasm = wit_component::ComponentEncoder::default()
        .validate(true)
        .module(&compile_contract(source))
        .map_err(|error| wasmtime::format_err!("failed to set core module: {error:#}"))?
        .encode()
        .map_err(|error| wasmtime::format_err!("failed to componentize contract: {error:#}"))?;
    let templates = build_component_templates(&wasm, &[script])
        .map_err(|error| wasmtime::format_err!("failed to build templates: {error}"))?;
    let mut config = new_wasmtime_config();
    config.wasm_component_model_implements(true);
    let engine = Engine::new(&config)?;
    let component = Component::from_binary(&engine, &wasm)?;
    let contract = Contract::new(&component, NoopContractLookup)?;
    let artifacts = Arc::new(
        neo_wasm::extract_first_component_core_program_artifacts(&wasm).map_err(|error| {
            wasmtime::format_err!("failed to extract program artifacts: {error}")
        })?,
    );
    let export = contract.get_coordination_script(script)?;
    let mut store = new_tracing_wasmtime_store(
        &engine,
        Ctx {
            table: ResourceTable::default(),
            traces: neo_wasm::WasmtimeTraceRegistry::default(),
            created: Vec::new(),
        },
        &wasm,
        &templates.bindings,
    )?;

    let instance = contract.instantiate(&mut store).await?;
    instance
        .call_coordination_script(&mut store, &export, [], [])
        .await?;

    // Finalization: the host reads the storage of every surviving UTXO (one
    // whose ABI is non-empty) in allocation order, which its `get-storage`
    // export publishes on the UTXO's own chain.
    let created = store.data().created.clone();
    let mut storage_records = Vec::new();
    for utxo in &created {
        if utxo.context().lock().unwrap().methods.is_empty() {
            continue;
        }
        let storage = storage_export_for(&contract, utxo, store.as_context_mut())?;
        storage_records.push(utxo.storage(&storage).call_get(&mut store).await?);
    }

    let ctx = store.into_data();
    let mut instances = Vec::new();
    for (instance_index, captured) in ctx
        .traces
        .instances()
        .map_err(|error| wasmtime::format_err!("trace capture failed: {error}"))?
    {
        let rows = captured.steps();
        let entry_fref = rows
            .first()
            .and_then(|row| row.current_function_ref)
            .ok_or_else(|| wasmtime::format_err!("core instance {instance_index} ran nothing"))?;
        let trace = neo_wasm::traces_from_wasmtime_steps_with_host_events(
            rows,
            captured.artifacts(),
            neo_wasm::CommChainState::default(),
        )
        .map_err(|error| wasmtime::format_err!("failed to normalize trace: {error}"))?;
        let absorbed = neo_wasm::comm_chain::absorbed_event_blocks(&trace);
        let steps = decode_absorbed_blocks(&templates.decoder, &absorbed)
            .map_err(|error| wasmtime::format_err!("failed to decode blocks: {error}"))?;
        instances.push(TracedInstance {
            entry_fref,
            captured: rows.to_vec(),
            trace,
            steps,
        });
    }
    let semantic = instances
        .iter()
        .map(|instance| instance.steps.clone())
        .collect::<Vec<_>>();
    let transaction = interleave_transaction(&semantic)
        .map_err(|error| wasmtime::format_err!("failed to interleave traces: {error}"))?;

    // Host-call commitment cross-check: the chain Neo-Wasm proves for each
    // instance must be the fold of the specification's encoding of the steps
    // we decoded from it.
    let mut commitments = TraceCommitments::new();
    for (index, instance) in instances.iter().enumerate() {
        let Some(last) = instance.trace.last() else {
            bail!("core instance {index} has an empty normalized trace");
        };
        let chain = last.state_after.comm_chain;
        let expected = encoded_chain(&instance.steps);
        if chain != expected {
            bail!(
                "core instance {index}: Neo-Wasm's event chain {chain:?} is not the fold of the \
                 decoded steps' specification encoding {expected:?}"
            );
        }
        commitments.insert(coroutine_id(index), chain);
    }

    Ok(TracedExecution {
        templates,
        artifacts,
        instances,
        transaction,
        commitments,
        storage: storage_records,
    })
}

/// Test-only interleaving-relation parameters matching the prover's own
/// tests (kappa = 2, λ = 40). Not a security claim.
pub fn interleaving_params() -> neo_fold_clean::paper::params::Params {
    use neo_params::goldilocks_paper_b2 as b2;
    neo_fold_clean::paper::params::Params::test_only_from_neo_params(
        neo_params::NeoParams::new(
            b2::Q,
            b2::ETA as u32,
            b2::D as u32,
            2,
            1 << 24,
            b2::B_BASE,
            b2::K_RHO,
            b2::T,
            b2::EXTENSION_DEGREE,
            40,
        )
        .expect("test parameters"),
    )
}

/// Expected count and suspended constructor state for STORAGE_COUNTER.
pub fn storage_counter_statement() -> starstream_interleaving_spec::TransactionStatement {
    use starstream_interleaving_spec::{OutputUtxo, TransactionStatement};
    use starstream_proving_runtime::{flat_value_root, method_hash_from_name};
    use wasmparser::ValType::{I32, I64};
    TransactionStatement {
        inputs: vec![],
        outputs: vec![OutputUtxo {
            utxo: 0,
            // yield, count, saved context, saved initial argument, saved resource.
            storage: flat_value_root(&[I32, I64, I32, I64, I32], &[1, 68, 1, 55, 2]),
            methods: vec![method_hash_from_name("add")],
        }],
    }
}

/// Check each program's memory accesses before recursive preprocessing.
pub fn check_wasm_memory(execution: &TracedExecution) -> wasmtime::Result<()> {
    let layout = neo_wasm::build_wasm_relation_layout();
    let mut preload = neo_wasm::preload_from_program_artifacts(&execution.artifacts);
    neo_wasm::memory_semantics::preload_host_event_tables(
        &mut preload,
        &execution.templates.bindings,
    );
    for (index, instance) in execution.instances.iter().enumerate() {
        let witnesses = instance
            .trace
            .iter()
            .map(neo_wasm::witness_builder::build_witness_vector)
            .collect::<Vec<_>>();
        neo_wasm::sanity_check_memory_rows(layout, &witnesses, &preload)
            .map_err(|error| wasmtime::format_err!("Wasm instance {index}: {error}"))?;
    }
    Ok(())
}

pub fn check_wasm_constraints(execution: &TracedExecution) -> wasmtime::Result<()> {
    check_wasm_memory(execution)?;
    let relation = neo_wasm::build_wasm_relation()
        .map_err(|error| wasmtime::format_err!("Wasm relation: {error}"))?;
    let layout = neo_wasm::build_wasm_relation_layout();
    for (instance_index, instance) in execution.instances.iter().enumerate() {
        for (row_index, row) in instance.trace.iter().enumerate() {
            let witness = neo_wasm::witness_builder::build_witness_vector(row);
            let (public, private) = witness.split_at(relation.r1cs().public_input_count());
            neo_ccs::check_ccs_rowwise_zero(relation.r1cs().structure(), public, private).map_err(
                |error| {
                    wasmtime::format_err!(
                        "Wasm instance {instance_index}, row {row_index}: {error}"
                    )
                },
            )?;
            neo_wasm::sanity_check_lookup_row(&layout.auxiliary, &witness).map_err(|error| {
                wasmtime::format_err!("Wasm instance {instance_index}, row {row_index}: {error}")
            })?;
        }
    }
    Ok(())
}

/// Relation-only recursive proofs: RAM/ROM and lookups remain host-checked.
/// TODO: Replace these sanity checks with the Nebula memory/lookup argument.
pub fn prove_wasm_instances(execution: &TracedExecution) -> wasmtime::Result<()> {
    use neo_fold_clean::frontends::r1cs_f_prime::ivc::{R1csIvc, R1csIvcPreprocessing};
    use neo_fold_clean::lifecycle::verify_uncompressed;
    use neo_wasm::preprocess::{
        canonical_wasm_f_prime_shape_batched_with_initial_state_digest, semantic_state_digest,
    };

    const BATCH_SIZE: usize = 16;
    check_wasm_constraints(execution)?;
    for (index, instance) in execution.instances.iter().enumerate() {
        let entry_pc = execution
            .templates
            .program_tables
            .function_entries
            .iter()
            .find(|&&(fref, _)| fref == u64::from(instance.entry_fref))
            .map(|&(_, pc)| pc)
            .ok_or_else(|| wasmtime::format_err!("missing entry function"))?;
        let initial = neo_wasm::host_event_top_level_initial_state(
            &execution.templates.program_tables,
            entry_pc,
            &execution.templates.bindings,
            instance.entry_fref,
            CommChainState::default(),
        )?;
        assert_eq!(instance.trace[0].state_before, initial);
        let batches = neo_wasm::batch::batch_count(instance.trace.len(), BATCH_SIZE);
        eprintln!(
            "Wasm instance {index}: {} rows, {batches} batches",
            instance.trace.len()
        );
        let start = std::time::Instant::now();
        let shape = canonical_wasm_f_prime_shape_batched_with_initial_state_digest(
            BATCH_SIZE,
            semantic_state_digest(initial),
        )?;
        let prep = R1csIvcPreprocessing::new_seeded(
            interleaving_params(),
            shape.sparse_r1cs,
            shape.plan,
            u64::from_le_bytes(*b"SSWASM01"),
        )?;
        eprintln!("Wasm instance {index} preprocessing: {:?}", start.elapsed());
        let start = std::time::Instant::now();
        let mut chain = R1csIvc::new(&prep);
        for batch in 0..batches {
            chain.extend(neo_wasm::batch::build_batched_witness(
                &instance.trace,
                BATCH_SIZE,
                batch,
            ))?;
            if (batch + 1) % 10 == 0 || batch + 1 == batches {
                eprintln!(
                    "Wasm instance {index}: proved {}/{batches} batches in {:?}",
                    batch + 1,
                    start.elapsed()
                );
            }
        }
        let proof = chain.finish()?;
        let start = std::time::Instant::now();
        verify_uncompressed(&prep.prep, &proof)?;
        let final_state = instance.trace.last().expect("nonempty trace").state_after;
        assert_eq!(
            proof.state.semantic_state_digest,
            semantic_state_digest(final_state)
        );
        assert!(final_state.halted && !final_state.trapped);
        assert_eq!(
            final_state.comm_chain,
            execution.commitments[&coroutine_id(index)]
        );
        let mut changed = final_state;
        changed.comm_chain[0] ^= 1;
        assert_ne!(
            proof.state.semantic_state_digest,
            semantic_state_digest(changed)
        );
        eprintln!("Wasm instance {index} verification: {:?}", start.elapsed());
    }
    Ok(())
}
