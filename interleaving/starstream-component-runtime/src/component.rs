use crate::abi::HostImportCall;
use crate::executor::{ExecutorError, HostImportOutcome, ProgramDefinition, StarstreamExecutor};
use starstream_interleaving_spec::{
    FunctionId, ProcessId, Ref, Value, WitEffectOutput, WitLedgerEffect,
};
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
    pub pending_activations: HashMap<ProcessId, (Ref, ProcessId)>,
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
    import_instance_schemas: HashMap<String, ImportInstanceSchema>,
}

impl WasmtimeComponentStarstreamExecutor {
    pub fn new() -> Result<Self, ScalarComponentError> {
        let engine = component_runtime();
        let mut store = Store::new(&engine, ComponentHostState::default());
        let import_instance_schemas = HashMap::new();
        store.data_mut().import_instance_schemas = import_instance_schemas.clone();
        let linker = build_linker(&engine, &import_instance_schemas)?;
        Ok(Self {
            engine,
            store,
            linker,
            import_instance_schemas,
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
        if merge_import_instance_schemas(&mut self.import_instance_schemas, schemas)? {
            self.rebuild_linker()?;
        }
        Ok((pid, component))
    }

    fn rebuild_linker(&mut self) -> Result<(), ScalarComponentError> {
        self.store.data_mut().import_instance_schemas = self.import_instance_schemas.clone();
        self.linker = build_linker(&self.engine, &self.import_instance_schemas)?;
        Ok(())
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

    pub fn run_main(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
    ) -> Result<(), ScalarComponentError> {
        self.run_export(pid, instance, "main")
    }

    pub fn run_export(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
        export_name: &'static str,
    ) -> Result<(), ScalarComponentError> {
        self.store.data_mut().current_process = pid;
        self.store.data_mut().executed_steps.push(pid);
        let trace_len_before = self
            .store
            .data()
            .executor
            .traces()
            .get(&pid)
            .map_or(0, |trace| trace.len());
        let main = instance
            .get_func(&mut self.store, export_name)
            .ok_or(ScalarComponentError::MissingExport(export_name))?;
        let mut results = [];
        main.call(&mut self.store, &[], &mut results)?;
        main.post_return(&mut self.store)?;
        let needs_implicit_return = self
            .store
            .data()
            .executor
            .traces()
            .get(&pid)
            .and_then(|trace| trace.get(trace_len_before..))
            .and_then(|trace| trace.last())
            .is_none_or(|effect| {
                !matches!(
                    effect,
                    WitLedgerEffect::Yield { .. }
                        | WitLedgerEffect::Return { .. }
                        | WitLedgerEffect::Burn { .. }
                )
            });
        if needs_implicit_return {
            self.store
                .data_mut()
                .executor
                .append_effect(pid, WitLedgerEffect::Return {});
        }
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

        ledger.func_new("burn", |mut ctx, _params, _results| {
            let caller = ctx.data().current_process;
            let payload =
                allocate_payload_ref(ctx.data_mut(), caller, ComponentValue::Tuple(vec![]))?;
            ctx.data_mut()
                .executor
                .record_import(caller, HostImportCall::Burn { payload })?;
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
    let method_f_id = FunctionId(method_function_id(import_name, func_name) as usize);
    ctx.data_mut()
        .executor
        .append_effect(target_pid, WitLedgerEffect::Enter { f_id: method_f_id });
    ctx.data_mut().executor.append_effect(
        target_pid,
        WitLedgerEffect::Activation {
            val: WitEffectOutput::Resolved(payload),
            caller: WitEffectOutput::Resolved(caller),
        },
    );

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
    let mut callee_results = func_schema
        .results
        .iter()
        .map(default_val_for_schema)
        .collect::<Vec<_>>();
    func.call(&mut *ctx, &callee_params, &mut callee_results)?;
    func.post_return(&mut *ctx)?;

    let yielded_ref = ctx
        .data()
        .executor
        .traces()
        .get(&target_pid)
        .and_then(|trace| trace.get(trace_len_before..))
        .and_then(|trace| trace.last())
        .and_then(|effect| match effect {
            starstream_interleaving_spec::WitLedgerEffect::Yield { val } => Some(*val),
            starstream_interleaving_spec::WitLedgerEffect::Burn { ret } => Some(*ret),
            _ => None,
        });
    let yielded_ref = match yielded_ref {
        Some(reff) => reff,
        None => {
            let returned_values = func_schema
                .results
                .iter()
                .zip(callee_results.iter())
                .map(|(schema, value)| schema.decode(value, method_name))
                .collect::<anyhow::Result<Vec<_>>>()?;
            let payload_value = match returned_values.len() {
                0 => ComponentValue::Tuple(vec![]),
                1 => returned_values[0].clone(),
                _ => ComponentValue::Tuple(returned_values),
            };
            let payload = allocate_payload_ref(ctx.data_mut(), target_pid, payload_value)?;
            ctx.data_mut()
                .executor
                .record_import(target_pid, HostImportCall::Yield { payload })?;
            payload
        }
    };
    ctx.data_mut().current_process = previous_process;
    ctx.data_mut()
        .executor
        .resolve_resume_output(caller, target_pid, yielded_ref)?;

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
    for lanes in words {
        state
            .executor
            .record_import(caller, HostImportCall::RefPush { lanes })?;
    }
    Ok(payload)
}

fn read_ref_value(
    state: &crate::state::StarstreamState<u32>,
    reff: Ref,
    schema: &ValueSchema,
    func: &str,
) -> anyhow::Result<ComponentValue> {
    if flat_arg_count(schema) == 0 {
        return decode_lanes_to_value([Value::nil(); 4], schema, func);
    }
    let lanes = state
        .refs
        .read_lanes(reff, 0)
        .ok_or_else(|| anyhow::anyhow!("missing ref payload {reff:?} for {func}"))?;
    decode_lanes_to_value(lanes, schema, func)
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

fn merge_import_instance_schemas(
    dst: &mut HashMap<String, ImportInstanceSchema>,
    src: HashMap<String, ImportInstanceSchema>,
) -> anyhow::Result<bool> {
    let mut changed = false;
    for (import_name, incoming) in src {
        match dst.get_mut(&import_name) {
            Some(existing) => {
                changed |= merge_single_import_schema(&import_name, existing, incoming)?;
            }
            None => {
                dst.insert(import_name, incoming);
                changed = true;
            }
        }
    }
    Ok(changed)
}

fn merge_single_import_schema(
    import_name: &str,
    existing: &mut ImportInstanceSchema,
    incoming: ImportInstanceSchema,
) -> anyhow::Result<bool> {
    let mut changed = false;

    for resource in incoming.resources {
        changed |= existing.resources.insert(resource);
    }

    for (func_name, incoming_schema) in incoming.functions {
        match existing.functions.get(&func_name) {
            Some(current) if current != &incoming_schema => {
                anyhow::bail!(
                    "conflicting schemas for import `{import_name}` function `{func_name}`"
                );
            }
            Some(_) => {}
            None => {
                existing.functions.insert(func_name, incoming_schema);
                changed = true;
            }
        }
    }

    Ok(changed)
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
