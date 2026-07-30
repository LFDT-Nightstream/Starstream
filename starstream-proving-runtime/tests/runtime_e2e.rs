use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use neo_wasm::event_grammar::TurnClaims;
use sha2::{Digest as _, Sha256};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_interleaving_spec::{ExecutionEvent, ExecutionTrace, MethodHash, interleave_traces};
use starstream_proving_runtime::{
    TracedContract, WasmTraceHost, build_component_templates, decode_absorbed_blocks,
    new_tracing_wasmtime_store, new_wasmtime_config,
};
use starstream_runtime_next::{ConstructorExport, EventHandler, Utxo, UtxoHandler, bindings};
use starstream_to_wasm::compile;
use wasmtime::component::{Resource, ResourceTable, Val};
use wasmtime::{Engine, StoreContextMut, bail};

const SOURCE: &str = r#"
    abi Counter {
        fn add(value: u64);
    }

    utxo CounterUtxo {
        main fn new() {
            yield(Counter);
        }

        impl Counter {
            fn add(pub value: u64) {
            }
        }
    }

    script fn example() {
        let counter = CounterUtxo::new();
    }
"#;

fn compile_contract(source: &str) -> Vec<u8> {
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

struct Ctx {
    contract: TracedContract<Self>,
    constructor: ConstructorExport,
    table: ResourceTable,
    advertised_methods: Vec<(u64, u64, u64, u64)>,
    traces: HashMap<u32, neo_wasm::WasmtimeTraceState>,
    trace_order: Vec<u32>,
    untraced_instances: HashSet<u32>,
}

impl bindings::starstream::std::builtin::Host for Ctx {
    fn abis_clear(&mut self) -> wasmtime::Result<()> {
        self.advertised_methods.clear();
        Ok(())
    }

    fn implements_method(&mut self, hash: (u64, u64, u64, u64)) -> wasmtime::Result<()> {
        self.advertised_methods.push(hash);
        Ok(())
    }
}

impl bindings::starstream::std::builtin::HostUtxo for Ctx {
    fn drop(&mut self, _utxo: Resource<Utxo>) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl bindings::starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> i64 {
        0
    }

    fn current_slot(&mut self) -> i64 {
        0
    }
}

impl EventHandler for Ctx {
    fn emit_event(&mut self, _instance: &str, _name: &str, _params: &[Val]) {}
}

impl UtxoHandler for Ctx {
    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn construct_utxo(
        store: StoreContextMut<'_, Self>,
        instance: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<Utxo>
    where
        Self: Sized,
    {
        if (&**instance, &**name) != ("counter-utxo", "[static]utxo.new") {
            bail!("unexpected UTXO constructor call `{instance}#{name}`");
        }
        let contract = store.data().contract.clone();
        let constructor = store.data().constructor.clone();
        contract.create_utxo(store, &constructor, params).await
    }
}

impl neo_wasm::WasmTraceSink for Ctx {
    fn wasm_trace_state(&self, instance_index: u32) -> Option<&neo_wasm::WasmtimeTraceState> {
        self.traces.get(&instance_index)
    }

    fn wasm_trace_state_mut(
        &mut self,
        instance_index: u32,
    ) -> Option<&mut neo_wasm::WasmtimeTraceState> {
        self.traces.get_mut(&instance_index)
    }

    fn record_untraced_instance(&mut self, instance_index: u32) {
        self.untraced_instances.insert(instance_index);
    }
}

impl WasmTraceHost for Ctx {
    fn register_wasm_trace(
        &mut self,
        instance_index: u32,
        trace: neo_wasm::WasmtimeTraceState,
    ) -> wasmtime::Result<()> {
        if self.traces.insert(instance_index, trace).is_some() {
            bail!("trace state already registered for core instance {instance_index}");
        }
        self.trace_order.push(instance_index);
        Ok(())
    }
}

fn method_hash(name: &str) -> MethodHash {
    let digest = Sha256::digest(name.as_bytes());
    let mut limbs = [0; 4];
    for (limb, bytes) in limbs.iter_mut().zip(digest.chunks_exact(8)) {
        *limb = u64::from_le_bytes(bytes.try_into().unwrap());
    }
    MethodHash(limbs)
}

#[tokio::test]
async fn compiled_coordination_script_traces_utxo_creation_and_abi_publication()
-> wasmtime::Result<()> {
    let wasm = compile_contract(SOURCE);
    let templates = build_component_templates(&wasm)
        .map_err(|error| wasmtime::format_err!("failed to build templates: {error}"))?;
    let engine = Engine::new(&new_wasmtime_config())?;
    let contract = TracedContract::new(&engine, &wasm)?;

    let utxo = contract.get_utxo("counter-utxo")?;
    let constructor = contract.get_utxo_constructor(&utxo, "[static]utxo.new")?;
    let script = contract.get_coordination_script("example")?;
    let mut store = new_tracing_wasmtime_store(
        &engine,
        Ctx {
            contract: contract.clone(),
            constructor,
            table: ResourceTable::default(),
            advertised_methods: Vec::new(),
            traces: HashMap::new(),
            trace_order: Vec::new(),
            untraced_instances: HashSet::new(),
        },
    )?;

    contract
        .call_coordination_script(&mut store, &script, [], [])
        .await?;

    assert!(
        store.data().untraced_instances.is_empty(),
        "every core instance must be registered before guest execution"
    );
    assert_eq!(
        store.data().traces.len(),
        2,
        "the coordination script and constructed UTXO each instantiate one core module"
    );

    let mut instance_events = Vec::new();
    for instance_index in &store.data().trace_order {
        let trace_state = &store.data().traces[instance_index];
        let trace = neo_wasm::traces_from_wasmtime_steps_with_grammar(
            trace_state.steps(),
            &templates.grammar,
            &[TurnClaims::default()],
            neo_wasm::CommChainState::default(),
        )
        .map_err(|error| wasmtime::format_err!("failed to normalize trace: {error}"))?;
        neo_wasm::comm_chain::sanity_check_comm_chain(&trace)
            .map_err(|error| wasmtime::format_err!("invalid commitment chain: {error}"))?;
        let absorbed = neo_wasm::comm_chain::absorbed_event_blocks(&trace);
        instance_events.push(
            decode_absorbed_blocks(&templates.decoder, &absorbed)
                .map_err(|error| wasmtime::format_err!("failed to decode blocks: {error}"))?,
        );
    }

    let [coord_events, utxo_events] = instance_events.as_slice() else {
        bail!("unexpected per-instance events: {instance_events:?}");
    };
    let [
        ExecutionEvent::BeginNewUtxo { arguments },
        ExecutionEvent::NewUtxoReturn { resource: _ },
        ExecutionEvent::CoordReturn,
    ] = coord_events.0.as_slice()
    else {
        bail!("unexpected coordination events: {coord_events:?}");
    };
    assert!(arguments.0.is_empty());
    let [
        ExecutionEvent::ClearAbi,
        ExecutionEvent::AdvertiseMethod { method },
        ExecutionEvent::ReturnControl,
    ] = utxo_events.0.as_slice()
    else {
        bail!("unexpected UTXO events: {utxo_events:?}");
    };
    assert_eq!(*method, method_hash("add"));

    let merged = interleave_traces(&instance_events)
        .map_err(|error| wasmtime::format_err!("failed to interleave traces: {error}"))?;
    assert_eq!(
        merged,
        ExecutionTrace::new([
            ExecutionEvent::Init,
            coord_events.0[0].clone(),
            utxo_events.0[0].clone(),
            utxo_events.0[1].clone(),
            utxo_events.0[2].clone(),
            coord_events.0[1].clone(),
            coord_events.0[2].clone(),
        ])
    );
    Ok(())
}

fn method_calling_coord_component() -> Vec<u8> {
    wat::parse_str(
        r#"
        (component
          (import "counter-utxo" (instance $counter
            (export "utxo" (type (sub resource)))
            (export "[static]utxo.new" (func $construct (result (own 0))))
            (export "[method]utxo.add"
              (func $add (param "self" (borrow 0)) (param "value" u64)))
          ))

          (alias export $counter "utxo" (type $utxo))
          (core func $construct-lowered
            (canon lower (func $counter "[static]utxo.new")))
          (core func $add-lowered
            (canon lower (func $counter "[method]utxo.add")))

          (core module $m
            (import "counter-utxo" "[static]utxo.new"
              (func $construct (result i32)))
            (import "counter-utxo" "[method]utxo.add"
              (func $add (param i32 i64)))
            (func (export "example") (local i32)
              call $construct
              local.set 0
              local.get 0
              i64.const 13
              call $add)
          )

          (core instance $i
            (instantiate $m
              (with "counter-utxo" (instance
                (export "[static]utxo.new" (func $construct-lowered))
                (export "[method]utxo.add" (func $add-lowered))
              ))
            )
          )

          (func (export "example") (canon lift (core func $i "example"))))
        "#,
    )
    .expect("coordination test component compiles")
}

#[tokio::test]
async fn coordination_script_trace_decodes_method_call() -> wasmtime::Result<()> {
    let utxo_wasm = compile_contract(SOURCE);
    let coord_wasm = method_calling_coord_component();
    let templates = build_component_templates(&coord_wasm)
        .map_err(|error| wasmtime::format_err!("failed to build templates: {error}"))?;
    let engine = Engine::new(&new_wasmtime_config())?;
    let utxo_contract = TracedContract::new(&engine, &utxo_wasm)?;
    let coord_contract = TracedContract::new(&engine, &coord_wasm)?;

    let utxo = utxo_contract.get_utxo("counter-utxo")?;
    let constructor = utxo_contract.get_utxo_constructor(&utxo, "[static]utxo.new")?;
    let script = coord_contract.get_coordination_script("example")?;
    let mut store = new_tracing_wasmtime_store(
        &engine,
        Ctx {
            contract: utxo_contract,
            constructor,
            table: ResourceTable::default(),
            advertised_methods: Vec::new(),
            traces: HashMap::new(),
            trace_order: Vec::new(),
            untraced_instances: HashSet::new(),
        },
    )?;

    coord_contract
        .call_coordination_script(&mut store, &script, [], [])
        .await?;
    assert!(store.data().untraced_instances.is_empty());
    let coord_index = store.data().trace_order[0];
    let coord_steps = store.data().traces[&coord_index].steps();
    let trace = neo_wasm::traces_from_wasmtime_steps_with_grammar(
        coord_steps,
        &templates.grammar,
        &[TurnClaims::default()],
        neo_wasm::CommChainState::default(),
    )
    .map_err(|error| wasmtime::format_err!("failed to normalize trace: {error}"))?;
    neo_wasm::comm_chain::sanity_check_comm_chain(&trace)
        .map_err(|error| wasmtime::format_err!("invalid commitment chain: {error}"))?;
    let events = decode_absorbed_blocks(
        &templates.decoder,
        &neo_wasm::comm_chain::absorbed_event_blocks(&trace),
    )
    .map_err(|error| wasmtime::format_err!("failed to decode blocks: {error}"))?;

    let [
        ExecutionEvent::BeginNewUtxo {
            arguments: constructor_arguments,
        },
        ExecutionEvent::NewUtxoReturn {
            resource: returned_resource,
        },
        ExecutionEvent::CallMethod {
            resource,
            method,
            arguments,
        },
        ExecutionEvent::CoordReturn,
    ] = events.0.as_slice()
    else {
        bail!("unexpected semantic events: {:?}", events.0);
    };
    assert!(constructor_arguments.0.is_empty());
    assert_eq!(resource, returned_resource);
    assert_eq!(*method, method_hash("add"));
    assert_eq!(
        *arguments,
        starstream_interleaving_spec::StarstreamValue(vec![13, 0])
    );
    Ok(())
}
