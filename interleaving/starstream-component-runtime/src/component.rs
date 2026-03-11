use crate::abi::{FUNCTION_ID_STEP, HostImportCall};
use crate::executor::{ExecutorError, HostImportOutcome, ProgramDefinition, StarstreamExecutor};
use starstream_interleaving_spec::{ProcessId, Ref, Value, WitLedgerEffect};
use std::collections::{HashMap, HashSet};
use wasmtime::component::types::ComponentItem;
use wasmtime::component::{Component, Linker, Resource, ResourceType, Val};
use wasmtime::{Config, Engine, Store};

pub type ScalarComponent = Component;
pub type ComponentStore<T> = Store<T>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum ComponentValue {
    U64(u64),
    Bool(bool),
    Tuple(Vec<ComponentValue>),
    Record(Vec<(String, ComponentValue)>),
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
enum ValueSchema {
    U64,
    Bool,
    Tuple(Vec<ValueSchema>),
    Record(Vec<(String, ValueSchema)>),
}

#[derive(Debug)]
pub struct UtxoResource;

#[derive(Debug, thiserror::Error)]
pub enum ScalarComponentError {
    #[error("wasmtime error: {0}")]
    Wasmtime(#[from] anyhow::Error),
    #[error("executor error: {0}")]
    Executor(#[from] ExecutorError),
    #[error("missing export `{0}`")]
    MissingExport(&'static str),
}

pub fn component_runtime() -> Engine {
    let mut config = Config::new();
    config.wasm_component_model(true);
    Engine::new(&config).expect("component runtime engine initialization must succeed")
}

#[derive(Debug)]
pub struct ComponentHostState {
    pub current_process: ProcessId,
    pub input_resources: Vec<u32>,
    pub next_resource_rep: u32,
    pub executed_steps: Vec<ProcessId>,
    pub pending_activations: HashMap<ProcessId, Ref>,
    pub instances: HashMap<ProcessId, wasmtime::component::Instance>,
    pub interface_resource_bindings: HashMap<String, u32>,
    import_instance_schemas: HashMap<String, ImportInstanceSchema>,
    pub executor: StarstreamExecutor<u32>,
}

impl Default for ComponentHostState {
    fn default() -> Self {
        Self {
            current_process: ProcessId(0),
            input_resources: Vec::new(),
            next_resource_rep: 1,
            executed_steps: Vec::new(),
            pending_activations: HashMap::new(),
            instances: HashMap::new(),
            interface_resource_bindings: HashMap::new(),
            import_instance_schemas: HashMap::new(),
            executor: StarstreamExecutor::new(),
        }
    }
}

pub struct WasmtimeComponentStarstreamExecutor {
    pub engine: Engine,
    pub store: ComponentStore<ComponentHostState>,
    linker: Linker<ComponentHostState>,
}

impl WasmtimeComponentStarstreamExecutor {
    pub fn new() -> Result<Self, ScalarComponentError> {
        let engine = component_runtime();
        let store = Store::new(&engine, ComponentHostState::default());
        let linker = build_linker(&engine, &HashMap::new())?;
        Ok(Self {
            engine,
            store,
            linker,
        })
    }

    pub fn executor(&self) -> &StarstreamExecutor<u32> {
        &self.store.data().executor
    }

    pub fn executed_steps(&self) -> &[ProcessId] {
        &self.store.data().executed_steps
    }

    pub fn effect_log(&self) -> &[(ProcessId, WitLedgerEffect)] {
        self.store.data().executor.effect_log()
    }

    pub fn executor_mut(&mut self) -> &mut StarstreamExecutor<u32> {
        &mut self.store.data_mut().executor
    }

    pub fn set_input_resources(&mut self, resources: Vec<u32>) {
        self.store.data_mut().input_resources = resources;
    }

    pub fn bind_import_resource(&mut self, import_name: impl Into<String>, resource: u32) {
        self.store
            .data_mut()
            .interface_resource_bindings
            .insert(import_name.into(), resource);
    }

    pub fn compile_component(
        &mut self,
        program: &ProgramDefinition,
    ) -> Result<(ProcessId, ScalarComponent), ScalarComponentError> {
        let pid = self.executor_mut().register_program(program);
        let component = Component::new(&self.engine, &program.module_bytes)?;
        let schemas = extract_import_instance_schemas(&component, &self.engine);
        self.store
            .data_mut()
            .import_instance_schemas
            .extend(schemas);
        let import_schemas = self.store.data().import_instance_schemas.clone();
        self.linker = build_linker(&self.engine, &import_schemas)?;
        Ok((pid, component))
    }

    pub fn instantiate_component(
        &mut self,
        component: &ScalarComponent,
    ) -> Result<wasmtime::component::Instance, ScalarComponentError> {
        Ok(self.linker.instantiate(&mut self.store, component)?)
    }

    pub fn instantiate_process(
        &mut self,
        pid: ProcessId,
        component: &ScalarComponent,
    ) -> Result<wasmtime::component::Instance, ScalarComponentError> {
        let instance = self.instantiate_component(component)?;
        self.store.data_mut().instances.insert(pid, instance);
        Ok(instance)
    }

    pub fn run_step(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
    ) -> Result<(), ScalarComponentError> {
        self.run_export(pid, instance, "step")
    }

    pub fn run_export(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
        export_name: &'static str,
    ) -> Result<(), ScalarComponentError> {
        self.store.data_mut().current_process = pid;
        self.store.data_mut().executed_steps.push(pid);
        let step = instance
            .get_func(&mut self.store, export_name)
            .ok_or(ScalarComponentError::MissingExport(export_name))?;
        let mut results = [];
        step.call(&mut self.store, &[], &mut results)?;
        Ok(())
    }
}

impl ComponentValue {
    fn to_ref_words(&self) -> anyhow::Result<Vec<[Value; 4]>> {
        let scalars = self.flatten_scalars()?;
        let mut words = Vec::new();
        for chunk in scalars.chunks(4) {
            let mut word = [Value::nil(), Value::nil(), Value::nil(), Value::nil()];
            for (idx, scalar) in chunk.iter().enumerate() {
                word[idx] = *scalar;
            }
            words.push(word);
        }
        if words.is_empty() {
            words.push([Value::nil(), Value::nil(), Value::nil(), Value::nil()]);
        }
        Ok(words)
    }

    fn flatten_scalars(&self) -> anyhow::Result<Vec<Value>> {
        let mut out = Vec::new();
        self.flatten_scalars_into(&mut out)?;
        Ok(out)
    }

    fn flatten_scalars_into(&self, out: &mut Vec<Value>) -> anyhow::Result<()> {
        match self {
            ComponentValue::U64(value) => out.push(Value(*value)),
            ComponentValue::Bool(value) => out.push(Value(u64::from(*value))),
            ComponentValue::Tuple(values) => {
                for value in values {
                    value.flatten_scalars_into(out)?;
                }
            }
            ComponentValue::Record(fields) => {
                for (_, value) in fields {
                    value.flatten_scalars_into(out)?;
                }
            }
        }
        Ok(())
    }
}

impl ValueSchema {
    fn decode(&self, value: &Val, func: &str) -> anyhow::Result<ComponentValue> {
        match (self, value) {
            (ValueSchema::U64, Val::U64(value)) => Ok(ComponentValue::U64(*value)),
            (ValueSchema::Bool, Val::Bool(value)) => Ok(ComponentValue::Bool(*value)),
            (ValueSchema::Tuple(schemas), Val::Tuple(values)) => {
                if schemas.len() != values.len() {
                    anyhow::bail!("tuple arity mismatch for {func}");
                }
                schemas
                    .iter()
                    .zip(values)
                    .map(|(schema, value)| schema.decode(value, func))
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(ComponentValue::Tuple)
            }
            (ValueSchema::Record(field_schemas), Val::Record(fields)) => {
                let mut decoded = Vec::with_capacity(field_schemas.len());
                for (name, schema) in field_schemas {
                    let value = fields
                        .iter()
                        .find(|(field_name, _)| field_name == name)
                        .map(|(_, value)| value)
                        .ok_or_else(|| {
                            anyhow::anyhow!("missing record field `{name}` for {func}")
                        })?;
                    decoded.push((name.clone(), schema.decode(value, func)?));
                }
                Ok(ComponentValue::Record(decoded))
            }
            _ => anyhow::bail!("unexpected component value for {func}: {value:?}"),
        }
    }

    fn encode(&self, value: &ComponentValue) -> anyhow::Result<Val> {
        match (self, value) {
            (ValueSchema::U64, ComponentValue::U64(value)) => Ok(Val::U64(*value)),
            (ValueSchema::Bool, ComponentValue::Bool(value)) => Ok(Val::Bool(*value)),
            (ValueSchema::Tuple(schemas), ComponentValue::Tuple(values)) => {
                if schemas.len() != values.len() {
                    anyhow::bail!("tuple arity mismatch during encode");
                }
                schemas
                    .iter()
                    .zip(values)
                    .map(|(schema, value)| schema.encode(value))
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(Val::Tuple)
            }
            (ValueSchema::Record(field_schemas), ComponentValue::Record(fields)) => {
                let mut encoded = Vec::with_capacity(field_schemas.len());
                for (name, schema) in field_schemas {
                    let value = fields
                        .iter()
                        .find(|(field_name, _)| field_name == name)
                        .map(|(_, value)| value)
                        .ok_or_else(|| {
                            anyhow::anyhow!("missing record field `{name}` during encode")
                        })?;
                    encoded.push((name.clone(), schema.encode(value)?));
                }
                Ok(Val::Record(encoded))
            }
            _ => anyhow::bail!("schema/value mismatch during encode"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FunctionSchema {
    params: Vec<Option<ValueSchema>>,
    results: Vec<ValueSchema>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct ImportInstanceSchema {
    resources: HashSet<String>,
    functions: HashMap<String, FunctionSchema>,
}

fn build_linker(
    engine: &Engine,
    import_schemas: &HashMap<String, ImportInstanceSchema>,
) -> Result<Linker<ComponentHostState>, ScalarComponentError> {
    let mut linker = Linker::<ComponentHostState>::new(engine);
    {
        let mut ledger = linker.instance("ledger")?;
        ledger.resource("utxo", ResourceType::host::<UtxoResource>(), |_, _| Ok(()))?;

        ledger.func_new("starstream-new-ref", |mut ctx, params, results| {
            let size_words = expect_u32(params, 0, "starstream-new-ref")?;
            let caller = ctx.data().current_process;
            let outcome = ctx
                .data_mut()
                .executor
                .record_import(caller, HostImportCall::NewRef { size_words })?;
            let reff = match outcome {
                HostImportOutcome::Ref(reff) => reff,
                _ => anyhow::bail!("starstream-new-ref must return a ref"),
            };
            results[0] = Val::U64(reff.0);
            Ok(())
        })?;

        ledger.func_new("starstream-ref-get", |mut ctx, params, results| {
            let reff = Ref(expect_u64(params, 0, "starstream-ref-get")?);
            let offset_words = expect_u32(params, 1, "starstream-ref-get")?;
            let caller = ctx.data().current_process;
            let outcome = ctx
                .data_mut()
                .executor
                .record_import(caller, HostImportCall::RefGet { reff, offset_words })?;
            let lanes = match outcome {
                HostImportOutcome::Lanes(lanes) => lanes,
                _ => anyhow::bail!("starstream-ref-get must return lanes"),
            };
            for (idx, lane) in lanes.into_iter().enumerate() {
                results[idx] = Val::U64(lane.0);
            }
            Ok(())
        })?;

        ledger.func_new("starstream-ref-write", |mut ctx, params, _results| {
            let reff = Ref(expect_u64(params, 0, "starstream-ref-write")?);
            let offset_words = expect_u32(params, 1, "starstream-ref-write")?;
            let lanes = [
                Value(expect_u64(params, 2, "starstream-ref-write")?),
                Value(expect_u64(params, 3, "starstream-ref-write")?),
                Value(expect_u64(params, 4, "starstream-ref-write")?),
                Value(expect_u64(params, 5, "starstream-ref-write")?),
            ];
            let caller = ctx.data().current_process;
            ctx.data_mut().executor.record_import(
                caller,
                HostImportCall::RefWrite {
                    reff,
                    offset_words,
                    lanes,
                },
            )?;
            Ok(())
        })?;

        ledger.func_wrap("input-utxo", |ctx, (index,): (u32,)| {
            let Some(resource) = ctx.data().input_resources.get(index as usize).copied() else {
                anyhow::bail!("invalid input-utxo index {index}");
            };
            Ok((Resource::<UtxoResource>::new_own(resource),))
        })?;

        ledger.func_wrap(
            "create-utxo",
            |mut ctx, (h0, h1, h2, h3, init): (u64, u64, u64, u64, u64)| {
                let caller = ctx.data().current_process;
                let outcome = ctx.data_mut().executor.record_import(
                    caller,
                    HostImportCall::NewUtxo {
                        program_hash: starstream_interleaving_spec::Hash(
                            [h0, h1, h2, h3],
                            std::marker::PhantomData,
                        ),
                        init: Ref(init),
                    },
                )?;
                match outcome {
                    HostImportOutcome::None => {}
                    _ => anyhow::bail!("create-utxo should not produce a direct effect result"),
                }
                let created_pid = ctx
                    .data()
                    .executor
                    .traces()
                    .get(&caller)
                    .and_then(|trace| trace.last())
                    .and_then(|effect| match effect {
                        starstream_interleaving_spec::WitLedgerEffect::NewUtxo { id, .. } => {
                            let starstream_interleaving_spec::WitEffectOutput::Resolved(pid) = id;
                            Some(*pid)
                        }
                        _ => None,
                    })
                    .ok_or_else(|| {
                        anyhow::anyhow!("create-utxo did not record a new-utxo effect")
                    })?;
                let resource = ctx.data().next_resource_rep;
                ctx.data_mut().next_resource_rep += 1;
                ctx.data_mut().executor.bind_resource(resource, created_pid);
                Ok((Resource::<UtxoResource>::new_own(resource),))
            },
        )?;

        ledger.func_wrap(
            "resume",
            |mut ctx, (target, payload): (Resource<UtxoResource>, u64)| {
                let caller = ctx.data().current_process;
                let target_pid = ctx
                    .data()
                    .executor
                    .state()
                    .resolve_resource(target.rep())
                    .ok_or_else(|| anyhow::anyhow!("unknown utxo resource {}", target.rep()))?;
                ctx.data_mut().executor.record_import(
                    caller,
                    HostImportCall::Resume {
                        target: target.rep(),
                        payload: Ref(payload),
                        function_id: FUNCTION_ID_STEP,
                    },
                )?;
                ctx.data_mut()
                    .pending_activations
                    .insert(target_pid, Ref(payload));
                Ok(())
            },
        )?;

        register_resume_value(&mut ledger, "resume-flat2")?;
        register_resume_value(&mut ledger, "resume-record2")?;
        register_activation_value(&mut ledger, "activation-flat2")?;
        register_activation_value(&mut ledger, "activation-record2")?;

        ledger.func_wrap("amount", |mut ctx, (target,): (Resource<UtxoResource>,)| {
            let schema = import_function_schema(&ctx, "ledger", "amount")
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("missing schema for `ledger.amount`"))?;
            let mut results =
                vec![default_val_for_schema(schema.results.first().ok_or_else(
                    || anyhow::anyhow!("`ledger.amount` must have one result"),
                )?)];
            let target_resource = target.try_into_resource_any(&mut ctx)?;
            call_resource_method(
                &mut ctx,
                "ledger",
                "amount",
                "amount",
                &schema,
                &[Val::Resource(target_resource)],
                &mut results,
            )?;
            match results.pop() {
                Some(Val::U64(amount)) => Ok((amount,)),
                other => anyhow::bail!("unexpected ledger.amount result: {other:?}"),
            }
        })?;

        ledger.func_new("yield", |mut ctx, params, _results| {
            let payload = Ref(expect_u64(params, 0, "starstream-yield")?);
            let caller = ctx.data().current_process;
            ctx.data_mut()
                .executor
                .record_import(caller, HostImportCall::Yield { payload })?;
            Ok(())
        })?;

        ledger.func_new("return", |mut ctx, _params, _results| {
            let caller = ctx.data().current_process;
            ctx.data_mut()
                .executor
                .record_import(caller, HostImportCall::Return)?;
            Ok(())
        })?;
    }

    for (import_name, schema) in import_schemas {
        if import_name == "ledger" {
            continue;
        }

        let mut instance = linker.instance(import_name)?;
        for resource_name in &schema.resources {
            instance.resource(
                resource_name,
                ResourceType::host::<UtxoResource>(),
                |_, _| Ok(()),
            )?;
        }

        for (func_name, func_schema) in &schema.functions {
            if func_name == "handle" {
                let import_name = import_name.clone();
                instance.func_wrap("handle", move |ctx, (): ()| {
                    let resource = ctx
                        .data()
                        .interface_resource_bindings
                        .get(&import_name)
                        .copied()
                        .or_else(|| ctx.data().input_resources.first().copied())
                        .ok_or_else(|| {
                            anyhow::anyhow!("no handle bound for import `{import_name}`")
                        })?;
                    Ok((Resource::<UtxoResource>::new_own(resource),))
                })?;
                continue;
            }

            if let Some((_, method_name)) = parse_resource_method_name(func_name) {
                let import_name = import_name.clone();
                let func_name = func_name.clone();
                let method_name = method_name.to_owned();
                let func_schema = func_schema.clone();
                instance.func_new(&func_name.clone(), move |mut ctx, params, results| {
                    call_resource_method(
                        &mut ctx,
                        &import_name,
                        &func_name,
                        &method_name,
                        &func_schema,
                        params,
                        results,
                    )
                })?;
            }
        }
    }

    Ok(linker)
}

fn call_resource_method(
    ctx: &mut wasmtime::StoreContextMut<'_, ComponentHostState>,
    import_name: &str,
    func_name: &str,
    method_name: &str,
    func_schema: &FunctionSchema,
    params: &[Val],
    results: &mut [Val],
) -> anyhow::Result<()> {
    let target = expect_utxo_resource(ctx, params, 0, func_name)?;
    let caller = ctx.data().current_process;
    let target_pid = ctx
        .data()
        .executor
        .state()
        .resolve_resource(target)
        .ok_or_else(|| anyhow::anyhow!("unknown utxo resource {target} for `{import_name}`"))?;

    let value_args = decode_function_value_args(params, func_schema, func_name)?;
    let payload_value = match value_args.len() {
        0 => ComponentValue::Tuple(vec![]),
        1 => value_args[0].clone(),
        _ => ComponentValue::Tuple(value_args.clone()),
    };
    let payload = allocate_payload_ref(ctx.data_mut(), caller, payload_value)?;
    ctx.data_mut().executor.record_import(
        caller,
        HostImportCall::Resume {
            target,
            payload,
            function_id: method_function_id(import_name, func_name),
        },
    )?;

    // TODO(interleaving-proof): method dispatch currently executes the target export
    // synchronously in the host and only records the caller-side `Resume`. Once the
    // witness/circuit preserves `function_id`, this should become a traced callee turn.
    let instance = *ctx
        .data()
        .instances
        .get(&target_pid)
        .ok_or_else(|| anyhow::anyhow!("missing instantiated target for {target_pid:?}"))?;
    let previous_process = ctx.data().current_process;
    ctx.data_mut().current_process = target_pid;
    ctx.data_mut().executed_steps.push(target_pid);

    let trace_len_before = ctx
        .data()
        .executor
        .traces()
        .get(&target_pid)
        .map_or(0, |trace| trace.len());
    let callee_params = value_args
        .iter()
        .zip(func_schema.params.iter().skip(1))
        .filter_map(|(value, schema)| schema.as_ref().map(|schema| schema.encode(value)))
        .collect::<anyhow::Result<Vec<_>>>()?;

    let func = instance
        .get_func(&mut *ctx, method_name)
        .ok_or_else(|| anyhow::anyhow!("missing target export `{method_name}`"))?;
    let mut ignored_results = Vec::new();
    func.call(&mut *ctx, &callee_params, &mut ignored_results)?;
    ctx.data_mut().current_process = previous_process;

    let yielded_ref = ctx
        .data()
        .executor
        .traces()
        .get(&target_pid)
        .and_then(|trace| trace.get(trace_len_before..))
        .and_then(|trace| trace.last())
        .and_then(|effect| match effect {
            starstream_interleaving_spec::WitLedgerEffect::Yield { val } => Some(*val),
            _ => None,
        })
        .ok_or_else(|| anyhow::anyhow!("method `{method_name}` did not yield a result"))?;

    for (dst, schema) in results.iter_mut().zip(func_schema.results.iter()) {
        let value = read_ref_value(
            ctx.data().executor.state(),
            yielded_ref,
            schema,
            method_name,
        )?;
        *dst = schema.encode(&value)?;
    }
    Ok(())
}

fn expect_u32(params: &[Val], idx: usize, func: &str) -> anyhow::Result<u32> {
    match params.get(idx) {
        Some(Val::U32(value)) => Ok(*value),
        _ => anyhow::bail!("invalid arg {idx} for {func}"),
    }
}

fn expect_u64(params: &[Val], idx: usize, func: &str) -> anyhow::Result<u64> {
    match params.get(idx) {
        Some(Val::U64(value)) => Ok(*value),
        _ => anyhow::bail!("invalid arg {idx} for {func}"),
    }
}

fn expect_utxo_resource(
    ctx: &mut wasmtime::StoreContextMut<'_, ComponentHostState>,
    params: &[Val],
    idx: usize,
    func: &str,
) -> anyhow::Result<u32> {
    let resource = match params.get(idx) {
        Some(Val::Resource(resource)) => *resource,
        _ => anyhow::bail!("invalid resource arg {idx} for {func}"),
    };
    let resource = Resource::<UtxoResource>::try_from_resource_any(resource, &mut *ctx)?;
    Ok(resource.rep())
}

fn decode_value_arg(
    params: &[Val],
    idx: usize,
    schema: &ValueSchema,
    func: &str,
) -> anyhow::Result<ComponentValue> {
    let value = params
        .get(idx)
        .ok_or_else(|| anyhow::anyhow!("missing arg {idx} for {func}"))?;
    schema.decode(value, func)
}

fn flat_arg_count(schema: &ValueSchema) -> usize {
    match schema {
        ValueSchema::U64 | ValueSchema::Bool => 1,
        ValueSchema::Tuple(items) => items.iter().map(flat_arg_count).sum(),
        ValueSchema::Record(fields) => fields
            .iter()
            .map(|(_, schema)| flat_arg_count(schema))
            .sum(),
    }
}

fn allocate_payload_ref(
    state: &mut ComponentHostState,
    caller: ProcessId,
    value: ComponentValue,
) -> anyhow::Result<Ref> {
    let words = value.to_ref_words()?;
    let payload = match state.executor.record_import(
        caller,
        HostImportCall::NewRef {
            size_words: words.len() as u32,
        },
    )? {
        HostImportOutcome::Ref(reff) => reff,
        _ => anyhow::bail!("payload allocation expected a ref result"),
    };
    for (offset_words, lanes) in words.into_iter().enumerate() {
        state.executor.record_import(
            caller,
            HostImportCall::RefWrite {
                reff: payload,
                offset_words: offset_words as u32,
                lanes,
            },
        )?;
    }
    Ok(payload)
}

fn read_activation_lanes(
    ctx: &mut wasmtime::StoreContextMut<'_, ComponentHostState>,
    func: &str,
) -> anyhow::Result<[Value; 4]> {
    let current = ctx.data().current_process;
    let payload = ctx
        .data()
        .pending_activations
        .get(&current)
        .copied()
        .ok_or_else(|| anyhow::anyhow!("no pending activation for {current:?}"))?;
    match ctx.data_mut().executor.record_import(
        current,
        HostImportCall::RefGet {
            reff: payload,
            offset_words: 0,
        },
    )? {
        HostImportOutcome::Lanes(lanes) => Ok(lanes),
        _ => anyhow::bail!("{func} expected ref read"),
    }
}

fn read_activation_value(
    ctx: &mut wasmtime::StoreContextMut<'_, ComponentHostState>,
    schema: &ValueSchema,
    func: &str,
) -> anyhow::Result<ComponentValue> {
    decode_lanes_to_value(read_activation_lanes(ctx, func)?, schema, func)
}

fn read_ref_value(
    state: &crate::state::StarstreamState<u32>,
    reff: Ref,
    schema: &ValueSchema,
    func: &str,
) -> anyhow::Result<ComponentValue> {
    let lanes = state
        .refs
        .read_lanes(reff, 0)
        .ok_or_else(|| anyhow::anyhow!("missing ref payload {reff:?} for {func}"))?;
    decode_lanes_to_value(lanes, schema, func)
}

fn import_function_schema<'a>(
    ctx: &'a wasmtime::StoreContextMut<'_, ComponentHostState>,
    import_name: &str,
    name: &str,
) -> Option<&'a FunctionSchema> {
    ctx.data()
        .import_instance_schemas
        .get(import_name)
        .and_then(|schema| schema.functions.get(name))
}

fn extract_import_instance_schemas(
    component: &Component,
    engine: &Engine,
) -> HashMap<String, ImportInstanceSchema> {
    let mut schemas = HashMap::new();
    for (import_name, item) in component.component_type().imports(engine) {
        let ComponentItem::ComponentInstance(instance) = item else {
            continue;
        };
        let mut schema = ImportInstanceSchema::default();
        for (name, item) in instance.exports(engine) {
            match item {
                ComponentItem::ComponentFunc(func) => {
                    let params = func
                        .params()
                        .map(|(_, ty)| value_schema_from_type(&ty))
                        .collect();
                    let results = func
                        .results()
                        .filter_map(|ty| value_schema_from_type(&ty))
                        .collect();
                    schema
                        .functions
                        .insert(name.to_owned(), FunctionSchema { params, results });
                }
                ComponentItem::Resource(_) => {
                    schema.resources.insert(name.to_owned());
                }
                _ => {}
            }
        }
        schemas.insert(import_name.to_owned(), schema);
    }
    schemas
}

fn parse_resource_method_name(name: &str) -> Option<(&str, &str)> {
    let suffix = name.strip_prefix("[method]")?;
    let (resource, method) = suffix.split_once('.')?;
    Some((resource, method))
}

fn method_function_id(import_name: &str, func_name: &str) -> u32 {
    let mut hash = 2166136261u32;
    for byte in import_name.bytes().chain([0]).chain(func_name.bytes()) {
        hash ^= u32::from(byte);
        hash = hash.wrapping_mul(16777619);
    }
    hash
}

fn default_val_for_schema(schema: &ValueSchema) -> Val {
    match schema {
        ValueSchema::U64 => Val::U64(0),
        ValueSchema::Bool => Val::Bool(false),
        ValueSchema::Tuple(items) => Val::Tuple(items.iter().map(default_val_for_schema).collect()),
        ValueSchema::Record(fields) => Val::Record(
            fields
                .iter()
                .map(|(name, schema)| (name.clone(), default_val_for_schema(schema)))
                .collect(),
        ),
    }
}

fn decode_function_value_args(
    params: &[Val],
    func_schema: &FunctionSchema,
    func_name: &str,
) -> anyhow::Result<Vec<ComponentValue>> {
    let mut decoded = Vec::new();
    for (idx, schema) in func_schema.params.iter().enumerate().skip(1) {
        if let Some(schema) = schema {
            decoded.push(decode_value_arg(params, idx, schema, func_name)?);
        }
    }
    Ok(decoded)
}

fn value_schema_from_type(ty: &wasmtime::component::types::Type) -> Option<ValueSchema> {
    match ty {
        wasmtime::component::types::Type::U64 => Some(ValueSchema::U64),
        wasmtime::component::types::Type::Bool => Some(ValueSchema::Bool),
        wasmtime::component::types::Type::Tuple(tuple) => Some(ValueSchema::Tuple(
            tuple
                .types()
                .filter_map(|ty| value_schema_from_type(&ty))
                .collect(),
        )),
        wasmtime::component::types::Type::Record(record) => Some(ValueSchema::Record(
            record
                .fields()
                .filter_map(|field| {
                    Some((field.name.to_owned(), value_schema_from_type(&field.ty)?))
                })
                .collect(),
        )),
        wasmtime::component::types::Type::Own(_) | wasmtime::component::types::Type::Borrow(_) => {
            None
        }
        _ => None,
    }
}

fn tuple_items_to_scalar(value: &Val) -> anyhow::Result<&Val> {
    match value {
        Val::Tuple(items) if items.len() == 1 => Ok(&items[0]),
        _ => anyhow::bail!("expected single-item tuple"),
    }
}

fn decode_lanes_to_value(
    lanes: [Value; 4],
    schema: &ValueSchema,
    func: &str,
) -> anyhow::Result<ComponentValue> {
    let arity = flat_arg_count(schema);
    let scalars: Vec<Val> = lanes
        .into_iter()
        .take(arity)
        .map(|value| Val::U64(value.0))
        .collect();
    let flat = Val::Tuple(scalars);
    match schema {
        ValueSchema::Tuple(_) => schema.decode(&flat, func),
        ValueSchema::Record(fields) => {
            let tuple_items = match &flat {
                Val::Tuple(items) => items,
                _ => anyhow::bail!("expected tuple lowering for record decode in {func}"),
            };
            let record = Val::Record(
                fields
                    .iter()
                    .enumerate()
                    .map(|(idx, (name, _))| (name.clone(), tuple_items[idx].clone()))
                    .collect(),
            );
            schema.decode(&record, func)
        }
        _ => schema.decode(tuple_items_to_scalar(&flat)?, func),
    }
}

fn register_resume_value(
    ledger: &mut wasmtime::component::LinkerInstance<'_, ComponentHostState>,
    name: &'static str,
) -> Result<(), ScalarComponentError> {
    ledger.func_new(name, move |mut ctx, params, _results| {
        let target = expect_utxo_resource(&mut ctx, params, 0, name)?;
        let payload_schema = import_function_schema(&ctx, "ledger", name)
            .and_then(|schema| schema.params.get(1))
            .and_then(|schema| schema.clone())
            .ok_or_else(|| anyhow::anyhow!("missing ledger schema for `{name}` payload"))?;
        let payload_value = decode_value_arg(params, 1, &payload_schema, name)?;
        let caller = ctx.data().current_process;
        let target_pid = ctx
            .data()
            .executor
            .state()
            .resolve_resource(target)
            .ok_or_else(|| anyhow::anyhow!("unknown utxo resource {target}"))?;
        let payload = allocate_payload_ref(ctx.data_mut(), caller, payload_value)?;
        ctx.data_mut().executor.record_import(
            caller,
            HostImportCall::Resume {
                target,
                payload,
                function_id: FUNCTION_ID_STEP,
            },
        )?;
        ctx.data_mut()
            .pending_activations
            .insert(target_pid, payload);
        Ok(())
    })?;
    Ok(())
}

fn register_activation_value(
    ledger: &mut wasmtime::component::LinkerInstance<'_, ComponentHostState>,
    name: &'static str,
) -> Result<(), ScalarComponentError> {
    ledger.func_new(name, move |mut ctx, _params, results| {
        let result_schema = import_function_schema(&ctx, "ledger", name)
            .and_then(|schema| schema.results.first())
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("missing ledger schema for `{name}` result"))?;
        let value = read_activation_value(&mut ctx, &result_schema, name)?;
        results[0] = result_schema.encode(&value)?;
        Ok(())
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    #![allow(dead_code)]

    use super::*;
    use crate::state::ProcessKind;
    use starstream_interleaving_spec::{Hash, WasmModule, WitLedgerEffect};

    fn dummy_hash() -> Hash<WasmModule> {
        Hash([5, 6, 7, 8], std::marker::PhantomData)
    }

    fn yielding_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
                (export "yield" (func $yield (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))
              (core func $yield-lowered (canon lower (func $ledger "yield")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))
                (import "ledger" "yield" (func $yield (param i64)))

                (func (export "step") (local i64)
                  i32.const 1
                  call $new-ref
                  local.tee 0
                  i32.const 0
                  i64.const 42
                  i64.const 0
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 0
                  call $yield))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                    (export "yield" (func $yield-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn amount_component(amount: u64) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
                (export "yield" (func $yield (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))
              (core func $yield-lowered (canon lower (func $ledger "yield")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))
                (import "ledger" "yield" (func $yield (param i64)))

                (func $emit (local i64)
                  i32.const 1
                  call $new-ref
                  local.tee 0
                  i32.const 0
                  i64.const {amount}
                  i64.const 0
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 0
                  call $yield)

                (func (export "step")
                  call $emit)

                (func (export "amount")
                  call $emit))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                    (export "yield" (func $yield-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step")))
              (func (export "amount") (canon lift (core func $i "amount"))))
            "#,
            amount = amount,
        ))
        .unwrap()
    }

    fn queryable_amount_component(amount: u64) -> Vec<u8> {
        amount_component(amount)
    }

    fn amount_query_coord_component(import_name: &str) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "{import_name}" (instance $utxo-api
                (export "utxo" (type (sub resource)))
                (export "handle" (func $handle (result (own 0))))
                (export "[method]utxo.amount" (func $amount (param "self" (borrow 0)) (result u64)))
              ))
              (import "ledger" (instance $ledger
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
                (export "yield" (func $yield (param "payload" u64)))
              ))

              (core func $handle-lowered (canon lower (func $utxo-api "handle")))
              (core func $amount-lowered (canon lower (func $utxo-api "[method]utxo.amount")))
              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))
              (core func $yield-lowered (canon lower (func $ledger "yield")))

              (core module $m
                (import "{import_name}" "handle" (func $handle (result i32)))
                (import "{import_name}" "[method]utxo.amount" (func $amount (param i32) (result i64)))
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))
                (import "ledger" "yield" (func $yield (param i64)))
                (func (export "step") (local i32) (local i64) (local i64)
                  call $handle
                  local.set 0
                  local.get 0
                  call $amount
                  local.set 1
                  i32.const 1
                  call $new-ref
                  local.set 2
                  local.get 2
                  i32.const 0
                  local.get 1
                  i64.const 0
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 2
                  call $yield))

              (core instance $i
                (instantiate $m
                  (with "{import_name}" (instance
                    (export "handle" (func $handle-lowered))
                    (export "[method]utxo.amount" (func $amount-lowered))
                  ))
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                    (export "yield" (func $yield-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
            import_name = import_name,
        ))
        .unwrap()
    }

    fn resuming_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "utxo" (type (sub resource)))
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "input-utxo" (func $input-utxo (param "index" u32) (result (own 0))))
                (export "resume" (func $resume (param "target" (borrow 0)) (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $input-utxo-lowered (canon lower (func $ledger "input-utxo")))
              (core func $resume-lowered (canon lower (func $ledger "resume")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "input-utxo" (func $input-utxo (param i32) (result i32)))
                (import "ledger" "resume" (func $resume (param i32 i64)))
                (func (export "step") (local i32) (local i64)
                  i32.const 0
                  call $input-utxo
                  local.set 0
                  i32.const 1
                  call $new-ref
                  local.set 1
                  local.get 0
                  local.get 1
                  call $resume))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "input-utxo" (func $input-utxo-lowered))
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "resume" (func $resume-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn creating_component(program_hash: [u64; 4]) -> Vec<u8> {
        wat::parse_str(&format!(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "utxo" (type (sub resource)))
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "create-utxo"
                  (func $create-utxo
                    (param "h0" u64)
                    (param "h1" u64)
                    (param "h2" u64)
                    (param "h3" u64)
                    (param "init" u64)
                    (result (own 0))))
                (export "resume" (func $resume (param "target" (borrow 0)) (param "payload" u64)))
              ))

              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $create-utxo-lowered (canon lower (func $ledger "create-utxo")))
              (core func $resume-lowered (canon lower (func $ledger "resume")))

              (core module $m
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "create-utxo" (func $create-utxo (param i64 i64 i64 i64 i64) (result i32)))
                (import "ledger" "resume" (func $resume (param i32 i64)))
                (func (export "step") (local i32) (local i64)
                  i64.const {h0}
                  i64.const {h1}
                  i64.const {h2}
                  i64.const {h3}
                  i64.const 0
                  call $create-utxo
                  local.set 0
                  i32.const 1
                  call $new-ref
                  local.set 1
                  local.get 0
                  local.get 1
                  call $resume))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "create-utxo" (func $create-utxo-lowered))
                    (export "resume" (func $resume-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
            h0 = program_hash[0],
            h1 = program_hash[1],
            h2 = program_hash[2],
            h3 = program_hash[3],
        ))
        .unwrap()
    }

    fn flat_payload_utxo_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (type $pair' (tuple u64 u64))
                (export "pair" (type $pair (eq $pair')))
                (export "activation-flat2" (func $activation-flat2 (result $pair)))
                (export "yield" (func $yield (param "payload" u64)))
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
              ))

              (core module $abi
                (memory (export "memory") 1)
                (global $heap (mut i32) (i32.const 32))
                (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32)
                  global.get $heap
                  global.get $heap
                  local.get 3
                  i32.add
                  global.set $heap))
              (core instance $abi-instance (instantiate $abi))
              (alias core export $abi-instance "memory" (core memory $memory))
              (alias core export $abi-instance "cabi_realloc" (core func $realloc))

              (core func $activation-flat2-lowered
                (canon lower (func $ledger "activation-flat2") (memory $memory) (realloc $realloc)))
              (core func $yield-lowered (canon lower (func $ledger "yield")))
              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))

              (core module $m
                (import "env" "memory" (memory 1))
                (import "ledger" "activation-flat2" (func $activation-flat2 (param i32)))
                (import "ledger" "yield" (func $yield (param i64)))
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))

                (func (export "step") (local i32) (local i64) (local i64) (local i64)
                  i32.const 32
                  local.tee 0
                  call $activation-flat2
                  local.get 0
                  i64.load
                  local.set 1
                  local.get 0
                  i32.const 8
                  i32.add
                  i64.load
                  local.set 2
                  i32.const 1
                  call $new-ref
                  local.set 3
                  local.get 3
                  i32.const 0
                  local.get 1
                  local.get 2
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 3
                  call $yield))

              (core instance $i
                (instantiate $m
                  (with "env" (instance
                    (export "memory" (memory $memory))
                  ))
                  (with "ledger" (instance
                    (export "activation-flat2" (func $activation-flat2-lowered))
                    (export "yield" (func $yield-lowered))
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn flat_payload_coord_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "utxo" (type (sub resource)))
                (type $pair' (tuple u64 u64))
                (export "pair" (type $pair (eq $pair')))
                (export "input-utxo" (func $input-utxo (param "index" u32) (result (own 0))))
                (export "resume-flat2" (func $resume-flat2 (param "target" (borrow 0)) (param "payload" $pair)))
              ))

              (core func $input-utxo-lowered (canon lower (func $ledger "input-utxo")))
              (core func $resume-flat2-lowered (canon lower (func $ledger "resume-flat2")))

              (core module $m
                (import "ledger" "input-utxo" (func $input-utxo (param i32) (result i32)))
                (import "ledger" "resume-flat2" (func $resume-flat2 (param i32 i64 i64)))
                (func (export "step") (local i32)
                  i32.const 0
                  call $input-utxo
                  local.set 0
                  local.get 0
                  i64.const 7
                  i64.const 11
                  call $resume-flat2))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "input-utxo" (func $input-utxo-lowered))
                    (export "resume-flat2" (func $resume-flat2-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn record_payload_utxo_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (type $pair' (record (field "left" u64) (field "right" u64)))
                (export "pair" (type $pair (eq $pair')))
                (export "activation-record2" (func $activation-record2 (result $pair)))
                (export "yield" (func $yield (param "payload" u64)))
                (export "starstream-new-ref" (func $new-ref (param "size-words" u32) (result u64)))
                (export "starstream-ref-write" (func $ref-write
                  (param "reff" u64)
                  (param "offset" u32)
                  (param "a0" u64)
                  (param "a1" u64)
                  (param "a2" u64)
                  (param "a3" u64)))
              ))

              (core module $abi
                (memory (export "memory") 1)
                (global $heap (mut i32) (i32.const 32))
                (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32)
                  global.get $heap
                  global.get $heap
                  local.get 3
                  i32.add
                  global.set $heap))
              (core instance $abi-instance (instantiate $abi))
              (alias core export $abi-instance "memory" (core memory $memory))
              (alias core export $abi-instance "cabi_realloc" (core func $realloc))

              (core func $activation-record2-lowered
                (canon lower (func $ledger "activation-record2") (memory $memory) (realloc $realloc)))
              (core func $yield-lowered (canon lower (func $ledger "yield")))
              (core func $new-ref-lowered (canon lower (func $ledger "starstream-new-ref")))
              (core func $ref-write-lowered (canon lower (func $ledger "starstream-ref-write")))

              (core module $m
                (import "env" "memory" (memory 1))
                (import "ledger" "activation-record2" (func $activation-record2 (param i32)))
                (import "ledger" "yield" (func $yield (param i64)))
                (import "ledger" "starstream-new-ref" (func $new-ref (param i32) (result i64)))
                (import "ledger" "starstream-ref-write"
                  (func $ref-write (param i64 i32 i64 i64 i64 i64)))

                (func (export "step") (local i32) (local i64) (local i64) (local i64)
                  i32.const 32
                  local.tee 0
                  call $activation-record2
                  local.get 0
                  local.set 0
                  local.get 0
                  i64.load
                  local.set 1
                  local.get 0
                  i32.const 8
                  i32.add
                  i64.load
                  local.set 2
                  i32.const 1
                  call $new-ref
                  local.set 3
                  local.get 3
                  i32.const 0
                  local.get 1
                  local.get 2
                  i64.const 0
                  i64.const 0
                  call $ref-write
                  local.get 3
                  call $yield))

              (core instance $i
                (instantiate $m
                  (with "env" (instance
                    (export "memory" (memory $memory))
                  ))
                  (with "ledger" (instance
                    (export "activation-record2" (func $activation-record2-lowered))
                    (export "yield" (func $yield-lowered))
                    (export "starstream-new-ref" (func $new-ref-lowered))
                    (export "starstream-ref-write" (func $ref-write-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    fn record_payload_coord_component() -> Vec<u8> {
        wat::parse_str(
            r#"
            (component
              (import "ledger" (instance $ledger
                (export "utxo" (type (sub resource)))
                (type $pair' (record (field "left" u64) (field "right" u64)))
                (export "pair" (type $pair (eq $pair')))
                (export "input-utxo" (func $input-utxo (param "index" u32) (result (own 0))))
                (export "resume-record2" (func $resume-record2 (param "target" (borrow 0)) (param "payload" $pair)))
              ))

              (core func $input-utxo-lowered (canon lower (func $ledger "input-utxo")))
              (core func $resume-record2-lowered (canon lower (func $ledger "resume-record2")))

              (core module $m
                (import "ledger" "input-utxo" (func $input-utxo (param i32) (result i32)))
                (import "ledger" "resume-record2" (func $resume-record2 (param i32 i64 i64)))
                (func (export "step") (local i32)
                  i32.const 0
                  call $input-utxo
                  local.set 0
                  local.get 0
                  i64.const 7
                  i64.const 11
                  call $resume-record2))

              (core instance $i
                (instantiate $m
                  (with "ledger" (instance
                    (export "input-utxo" (func $input-utxo-lowered))
                    (export "resume-record2" (func $resume-record2-lowered))
                  ))
                )
              )

              (func (export "step") (canon lift (core func $i "step"))))
            "#,
        )
        .unwrap()
    }

    #[test]
    fn handwritten_component_can_run_amount_export() {
        let mut runtime = WasmtimeComponentStarstreamExecutor::new().unwrap();
        let component = amount_component(55);

        let program = ProgramDefinition {
            kind: ProcessKind::Utxo,
            program_hash: dummy_hash(),
            module_bytes: component,
        };

        let (pid, component) = runtime.compile_component(&program).unwrap();
        let instance = runtime.instantiate_component(&component).unwrap();
        runtime.run_export(pid, &instance, "amount").unwrap();

        let trace = runtime.executor().traces().get(&pid).unwrap();
        assert!(matches!(trace[0], WitLedgerEffect::NewRef { .. }));
        assert!(matches!(trace[1], WitLedgerEffect::RefWrite { .. }));
        assert!(matches!(trace[2], WitLedgerEffect::Yield { .. }));
    }

    #[test]
    fn coord_component_can_call_utxo_amount_method() {
        let mut runtime = WasmtimeComponentStarstreamExecutor::new().unwrap();

        let utxo_program = ProgramDefinition {
            kind: ProcessKind::Utxo,
            program_hash: dummy_hash(),
            module_bytes: queryable_amount_component(55),
        };
        let (utxo_pid, utxo_component) = runtime.compile_component(&utxo_program).unwrap();
        runtime.executor_mut().bind_resource(7, utxo_pid);
        runtime.bind_import_resource("std-utxo-api", 7);
        let _utxo_instance = runtime
            .instantiate_process(utxo_pid, &utxo_component)
            .unwrap();

        let coord_program = ProgramDefinition {
            kind: ProcessKind::Coord,
            program_hash: Hash([9, 10, 11, 12], std::marker::PhantomData),
            module_bytes: amount_query_coord_component("std-utxo-api"),
        };
        let (coord_pid, coord_component) = runtime.compile_component(&coord_program).unwrap();
        let coord_instance = runtime.instantiate_component(&coord_component).unwrap();

        runtime.run_step(coord_pid, &coord_instance).unwrap();

        let coord_trace = runtime.executor().traces().get(&coord_pid).unwrap();
        assert!(matches!(coord_trace[0], WitLedgerEffect::NewRef { .. }));
        assert!(matches!(coord_trace[1], WitLedgerEffect::RefWrite { .. }));
        assert!(matches!(
            coord_trace[2],
            WitLedgerEffect::Resume { target, .. } if target == utxo_pid
        ));
        assert!(matches!(coord_trace[3], WitLedgerEffect::NewRef { .. }));
        assert!(matches!(coord_trace[4], WitLedgerEffect::RefWrite { .. }));
        assert!(matches!(coord_trace[5], WitLedgerEffect::Yield { .. }));

        let utxo_trace = runtime.executor().traces().get(&utxo_pid).unwrap();
        assert!(matches!(utxo_trace[0], WitLedgerEffect::NewRef { .. }));
        assert!(matches!(utxo_trace[1], WitLedgerEffect::RefWrite { .. }));
        assert!(matches!(utxo_trace[2], WitLedgerEffect::Yield { .. }));
    }

    #[test]
    fn coord_component_can_call_utxo_amount_method_from_arbitrary_import_name() {
        let mut runtime = WasmtimeComponentStarstreamExecutor::new().unwrap();

        let utxo_program = ProgramDefinition {
            kind: ProcessKind::Utxo,
            program_hash: dummy_hash(),
            module_bytes: queryable_amount_component(88),
        };
        let (utxo_pid, utxo_component) = runtime.compile_component(&utxo_program).unwrap();
        runtime.executor_mut().bind_resource(9, utxo_pid);
        runtime.bind_import_resource("other-utxo-api", 9);
        let _utxo_instance = runtime
            .instantiate_process(utxo_pid, &utxo_component)
            .unwrap();

        let coord_program = ProgramDefinition {
            kind: ProcessKind::Coord,
            program_hash: Hash([13, 14, 15, 16], std::marker::PhantomData),
            module_bytes: amount_query_coord_component("other-utxo-api"),
        };
        let (coord_pid, coord_component) = runtime.compile_component(&coord_program).unwrap();
        let coord_instance = runtime.instantiate_component(&coord_component).unwrap();

        runtime.run_step(coord_pid, &coord_instance).unwrap();

        let coord_trace = runtime.executor().traces().get(&coord_pid).unwrap();
        assert!(
            matches!(coord_trace[2], WitLedgerEffect::Resume { target, .. } if target == utxo_pid)
        );
        assert!(
            matches!(coord_trace[4], WitLedgerEffect::RefWrite { vals, .. } if vals[0] == Value(88))
        );
        let utxo_trace = runtime.executor().traces().get(&utxo_pid).unwrap();
        assert!(matches!(utxo_trace[2], WitLedgerEffect::Yield { .. }));
    }
}
