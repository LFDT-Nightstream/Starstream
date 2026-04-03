use crate::abi::HostImportCall;
use crate::executor::{ExecutorError, HostImportOutcome, ProgramDefinition, StarstreamExecutor};
use starstream_interleaving_spec::{
    FunctionId, ProcessId, Ref, Value, WitEffectOutput, WitLedgerEffect,
};
use std::collections::{HashMap, HashSet};
use wasmtime::component::types::ComponentItem;
use wasmtime::component::{
    Component, Linker, Resource, ResourceAny, ResourceType, Val as WasmtimeVal,
};
use wasmtime::{Config, Engine, Store};

pub type ScalarComponent = Component;
pub type ComponentStore<T> = Store<T>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum ComponentValueIr {
    U64(u64),
    Bool(bool),
    Tuple(Vec<ComponentValueIr>),
    Record(Vec<(String, ComponentValueIr)>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ComponentTypeIr {
    U64,
    Bool,
    Tuple(Vec<ComponentTypeIr>),
    Record(Vec<(String, ComponentTypeIr)>),
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

    pub fn run_main_with_resource(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
        resource: u32,
    ) -> Result<(), ScalarComponentError> {
        let arg = ResourceAny::try_from_resource(
            Resource::<UtxoResource>::new_own(resource),
            &mut self.store,
        )?;
        self.run_export_with_args(pid, instance, "main", &[WasmtimeVal::Resource(arg)])
    }

    pub fn run_export(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
        export_name: &'static str,
    ) -> Result<(), ScalarComponentError> {
        self.run_export_with_args(pid, instance, export_name, &[])
    }

    fn run_export_with_args(
        &mut self,
        pid: ProcessId,
        instance: &wasmtime::component::Instance,
        export_name: &'static str,
        params: &[WasmtimeVal],
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
        main.call(&mut self.store, params, &mut results)?;
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

impl ComponentValueIr {
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
            ComponentValueIr::U64(value) => out.push(Value(*value)),
            ComponentValueIr::Bool(value) => out.push(Value(u64::from(*value))),
            ComponentValueIr::Tuple(values) => {
                for value in values {
                    value.flatten_scalars_into(out)?;
                }
            }
            ComponentValueIr::Record(fields) => {
                for (_, value) in fields {
                    value.flatten_scalars_into(out)?;
                }
            }
        }
        Ok(())
    }
}

impl ComponentTypeIr {
    fn decode(&self, value: &WasmtimeVal, func: &str) -> anyhow::Result<ComponentValueIr> {
        match (self, value) {
            (ComponentTypeIr::U64, WasmtimeVal::U64(value)) => Ok(ComponentValueIr::U64(*value)),
            (ComponentTypeIr::Bool, WasmtimeVal::Bool(value)) => Ok(ComponentValueIr::Bool(*value)),
            (ComponentTypeIr::Tuple(schemas), WasmtimeVal::Tuple(values)) => {
                if schemas.len() != values.len() {
                    anyhow::bail!("tuple arity mismatch for {func}");
                }
                schemas
                    .iter()
                    .zip(values)
                    .map(|(schema, value)| schema.decode(value, func))
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(ComponentValueIr::Tuple)
            }
            (ComponentTypeIr::Record(field_schemas), WasmtimeVal::Record(fields)) => {
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
                Ok(ComponentValueIr::Record(decoded))
            }
            _ => anyhow::bail!("unexpected component value for {func}: {value:?}"),
        }
    }

    fn encode(&self, value: &ComponentValueIr) -> anyhow::Result<WasmtimeVal> {
        match (self, value) {
            (ComponentTypeIr::U64, ComponentValueIr::U64(value)) => Ok(WasmtimeVal::U64(*value)),
            (ComponentTypeIr::Bool, ComponentValueIr::Bool(value)) => Ok(WasmtimeVal::Bool(*value)),
            (ComponentTypeIr::Tuple(schemas), ComponentValueIr::Tuple(values)) => {
                if schemas.len() != values.len() {
                    anyhow::bail!("tuple arity mismatch during encode");
                }
                schemas
                    .iter()
                    .zip(values)
                    .map(|(schema, value)| schema.encode(value))
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(WasmtimeVal::Tuple)
            }
            (ComponentTypeIr::Record(field_schemas), ComponentValueIr::Record(fields)) => {
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
                Ok(WasmtimeVal::Record(encoded))
            }
            _ => anyhow::bail!("schema/value mismatch during encode"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FunctionSchema {
    params: Vec<Option<ComponentTypeIr>>,
    results: Vec<ComponentTypeIr>,
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

        ledger.func_new("burn", |mut ctx, _ty, _params, _results| {
            let caller = ctx.data().current_process;
            let payload =
                allocate_payload_ref(ctx.data_mut(), caller, ComponentValueIr::Tuple(vec![]))?;
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

            if parse_resource_method_name(func_name).is_some() {
                let import_name = import_name.clone();
                let func_name = func_name.clone();
                let func_schema = func_schema.clone();
                instance.func_new(&func_name.clone(), move |mut ctx, _ty, params, results| {
                    call_resource_method(
                        &mut ctx,
                        &import_name,
                        &func_name,
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
    func_schema: &FunctionSchema,
    params: &[WasmtimeVal],
    results: &mut [WasmtimeVal],
) -> anyhow::Result<()> {
    let target = match params.first() {
        Some(WasmtimeVal::Resource(resource)) => {
            Resource::<UtxoResource>::try_from_resource_any(*resource, &mut *ctx)?.rep()
        }
        _ => anyhow::bail!("invalid resource arg 0 for {func_name}"),
    };

    let caller = ctx.data().current_process;
    let target_pid = ctx
        .data()
        .executor
        .state()
        .resolve_resource(target)
        .ok_or_else(|| anyhow::anyhow!("unknown utxo resource {target} for `{import_name}`"))?;

    let value_args = decode_function_value_args(params, func_schema, func_name)?;
    let payload_value = match value_args.len() {
        0 => ComponentValueIr::Tuple(vec![]),
        1 => value_args[0].clone(),
        _ => ComponentValueIr::Tuple(value_args.clone()),
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

    let (_, method_name) = parse_resource_method_name(func_name)
        .ok_or_else(|| anyhow::anyhow!("invalid resource method name `{func_name}`"))?;
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
                0 => ComponentValueIr::Tuple(vec![]),
                1 => returned_values[0].clone(),
                _ => ComponentValueIr::Tuple(returned_values),
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

fn decode_value_arg(
    params: &[WasmtimeVal],
    idx: usize,
    schema: &ComponentTypeIr,
    func: &str,
) -> anyhow::Result<ComponentValueIr> {
    let value = params
        .get(idx)
        .ok_or_else(|| anyhow::anyhow!("missing arg {idx} for {func}"))?;
    schema.decode(value, func)
}

fn flat_arg_count(schema: &ComponentTypeIr) -> usize {
    match schema {
        ComponentTypeIr::U64 | ComponentTypeIr::Bool => 1,
        ComponentTypeIr::Tuple(items) => items.iter().map(flat_arg_count).sum(),
        ComponentTypeIr::Record(fields) => fields
            .iter()
            .map(|(_, schema)| flat_arg_count(schema))
            .sum(),
    }
}

fn allocate_payload_ref(
    state: &mut ComponentHostState,
    caller: ProcessId,
    value: ComponentValueIr,
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
    schema: &ComponentTypeIr,
    func: &str,
) -> anyhow::Result<ComponentValueIr> {
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
                        .map(|(_, ty)| component_type_ir_from_type(&ty))
                        .collect();
                    let results = func
                        .results()
                        .filter_map(|ty| component_type_ir_from_type(&ty))
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

fn default_val_for_schema(schema: &ComponentTypeIr) -> WasmtimeVal {
    match schema {
        ComponentTypeIr::U64 => WasmtimeVal::U64(0),
        ComponentTypeIr::Bool => WasmtimeVal::Bool(false),
        ComponentTypeIr::Tuple(items) => {
            WasmtimeVal::Tuple(items.iter().map(default_val_for_schema).collect())
        }
        ComponentTypeIr::Record(fields) => WasmtimeVal::Record(
            fields
                .iter()
                .map(|(name, schema)| (name.clone(), default_val_for_schema(schema)))
                .collect(),
        ),
    }
}

fn decode_function_value_args(
    params: &[WasmtimeVal],
    func_schema: &FunctionSchema,
    func_name: &str,
) -> anyhow::Result<Vec<ComponentValueIr>> {
    let mut decoded = Vec::new();
    for (idx, schema) in func_schema.params.iter().enumerate().skip(1) {
        if let Some(schema) = schema {
            decoded.push(decode_value_arg(params, idx, schema, func_name)?);
        }
    }
    Ok(decoded)
}

fn component_type_ir_from_type(ty: &wasmtime::component::types::Type) -> Option<ComponentTypeIr> {
    match ty {
        wasmtime::component::types::Type::U64 => Some(ComponentTypeIr::U64),
        wasmtime::component::types::Type::Bool => Some(ComponentTypeIr::Bool),
        wasmtime::component::types::Type::Tuple(tuple) => Some(ComponentTypeIr::Tuple(
            tuple
                .types()
                .filter_map(|ty| component_type_ir_from_type(&ty))
                .collect(),
        )),
        wasmtime::component::types::Type::Record(record) => Some(ComponentTypeIr::Record(
            record
                .fields()
                .filter_map(|field| {
                    Some((
                        field.name.to_owned(),
                        component_type_ir_from_type(&field.ty)?,
                    ))
                })
                .collect(),
        )),
        wasmtime::component::types::Type::Own(_) | wasmtime::component::types::Type::Borrow(_) => {
            None
        }
        _ => None,
    }
}

fn tuple_items_to_scalar(value: &WasmtimeVal) -> anyhow::Result<&WasmtimeVal> {
    match value {
        WasmtimeVal::Tuple(items) if items.len() == 1 => Ok(&items[0]),
        _ => anyhow::bail!("expected single-item tuple"),
    }
}

fn decode_lanes_to_value(
    lanes: [Value; 4],
    schema: &ComponentTypeIr,
    func: &str,
) -> anyhow::Result<ComponentValueIr> {
    let arity = flat_arg_count(schema);
    let scalars: Vec<WasmtimeVal> = lanes
        .into_iter()
        .take(arity)
        .map(|value| WasmtimeVal::U64(value.0))
        .collect();
    let flat = WasmtimeVal::Tuple(scalars);
    match schema {
        ComponentTypeIr::Tuple(_) => schema.decode(&flat, func),
        ComponentTypeIr::Record(fields) => {
            let tuple_items = match &flat {
                WasmtimeVal::Tuple(items) => items,
                _ => anyhow::bail!("expected tuple lowering for record decode in {func}"),
            };
            let record = WasmtimeVal::Record(
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
