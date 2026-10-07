use core::mem;
use core::pin::Pin;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::Arc;

use bytes::{Bytes, BytesMut};
use sha2::{Digest as _, Sha256};
use starstream_proving_runtime::{
    CapturedExecution, MethodHash, WasmTraceSink, WasmtimeTraceRegistry, build_component_templates,
    check_captured_transaction, enable_tracing, register_tracing_component,
};
use starstream_runtime::bindings::starstream;
use starstream_runtime::{
    CoordinationScriptExport, CoordinationScriptImport, Token, Utxo, UtxoExport, UtxoImport,
    get_coordination_script_instance_import, utxo_imports,
};
use tokio_util::codec::Encoder as _;
use tracing::error;
use wasmtime::component::{Component, Resource, ResourceTable, Type, Val};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut as _, Engine, Store, StoreContextMut, bail, ensure, format_err};
use wasmtime_wizer::{WasmtimeWizerComponent, Wizer};

use crate::client::CoordinationScriptArg;
use crate::runtime::{apply_state, parse_state};
use crate::wrpc::codec::{ValEncoder, read_value};
use crate::{
    Transaction, TransactionEvent, TransactionInput, TransactionOutput, encode_digest, parse_digest,
};

pub fn compile_component(
    engine: &Engine,
    wizer: &Wizer,
    wasm: &[u8],
) -> wasmtime::Result<Component> {
    let (_wizer_cx, instrumented) = wizer
        .instrument_component(wasm)
        .context("failed to instrument component")?;
    let component = wasmtime::component::Component::from_binary(engine, &instrumented)
        .context("failed to compile component")?;
    Ok(component)
}

#[derive(Clone)]
pub struct Contract {
    pub contract: Option<starstream_runtime::Contract<Ctx>>,
    pub wasm: Bytes,
}

pub trait Client {
    fn get_contract_wasm(&self, digest: [u8; 32]) -> impl Future<Output = anyhow::Result<Bytes>>;

    fn get_input_utxo(
        &self,
        input: &TransactionInput,
    ) -> impl Future<Output = anyhow::Result<TransactionOutput>>;
}

enum ResolvedCoordinationScriptArg {
    Val(Val),
    Utxo(ResolvedUtxoInput),
}

struct ResolvedUtxoInput {
    input: TransactionInput,
    output: TransactionOutput,
    contract: starstream_runtime::Contract<Ctx>,
    external_id: Option<Arc<str>>,
    wasm: Vec<u8>,
}

struct TracingComponent {
    instrumented: Vec<u8>,
    templates: starstream_proving_runtime::ComponentTemplates,
}

struct TracingSetup {
    components: Vec<TracingComponent>,
    input_locals: Vec<usize>,
    input_methods: Vec<Vec<MethodHash>>,
}

fn tracing_components(
    wizer: &Wizer,
    root_wasm: &[u8],
    scripts: &[&str],
    imports: &HashMap<[u8; 32], Contract>,
) -> wasmtime::Result<Vec<TracingComponent>> {
    let root_digest: [u8; 32] = Sha256::digest(root_wasm).into();
    let mut components = Vec::with_capacity(imports.len() + 1);
    let (_, root_instrumented) = wizer.instrument_component(root_wasm)?;
    let templates = build_component_templates(&root_instrumented, scripts)
        .context("failed to build root trace templates")?;

    components.push(TracingComponent {
        instrumented: root_instrumented,
        templates,
    });

    for (digest, Contract { wasm, contract }) in imports {
        if *digest == root_digest {
            continue;
        }
        let Some(contract) = contract else { continue };
        let (_, instrumented) = wizer.instrument_component(wasm)?;
        let scripts = contract
            .coordination_scripts()
            .map(|(name, export)| export.map(|_| name))
            .collect::<wasmtime::Result<Vec<_>>>()?;

        let templates = build_component_templates(&instrumented, &scripts)
            .context("failed to build imported trace templates")?;

        components.push(TracingComponent {
            instrumented,
            templates,
        });
    }

    Ok(components)
}

async fn resolve_coordination_script_args(
    client: &(impl Client + ?Sized),
    wizer: &Wizer,
    contract: &starstream_runtime::Contract<Ctx>,
    wasm: &[u8],
    export: &CoordinationScriptExport,
    imports: &mut HashMap<[u8; 32], Contract>,
    args: impl IntoIterator<Item = CoordinationScriptArg>,
) -> wasmtime::Result<Vec<ResolvedCoordinationScriptArg>> {
    let engine = contract.component().engine();
    let digest: [u8; 32] = Sha256::digest(wasm).into();
    let mut args = args.into_iter();
    let mut resolved = Vec::with_capacity(export.ty().params().len());
    for (name, _ty) in export.ty().params() {
        let arg = args
            .next()
            .with_context(|| format!("missing argument for parameter `{name}`"))?;
        let arg = match arg {
            CoordinationScriptArg::Val(value) => ResolvedCoordinationScriptArg::Val(value),
            CoordinationScriptArg::Utxo(input) => {
                let output = client
                    .get_input_utxo(&input)
                    .await
                    .map_err(wasmtime::Error::from_anyhow)?;
                let input_digest = parse_digest(&output.contract).with_context(|| {
                    format!(
                        "failed to parse `{}` as multibase multihash",
                        output.contract
                    )
                })?;
                let (external_id, input_wasm) = if input_digest == digest {
                    let input_wasm =
                        apply_state(wasm, &output.state).map_err(wasmtime::Error::from_anyhow)?;
                    (None, Bytes::from(input_wasm))
                } else if let Some(Contract { wasm, .. }) = imports.get(&input_digest) {
                    let input_wasm =
                        apply_state(wasm, &output.state).map_err(wasmtime::Error::from_anyhow)?;
                    (
                        Some(Arc::from(output.contract.as_ref())),
                        Bytes::from(input_wasm),
                    )
                } else {
                    let input_wasm = client
                        .get_contract_wasm(input_digest)
                        .await
                        .map_err(wasmtime::Error::from_anyhow)?;
                    let input_wasm = apply_state(&input_wasm, &output.state)
                        .map_err(wasmtime::Error::from_anyhow)?;
                    let input_wasm = Bytes::from(input_wasm);
                    imports.insert(
                        input_digest,
                        Contract {
                            contract: None,
                            wasm: input_wasm.clone(),
                        },
                    );
                    (Some(Arc::from(output.contract.as_ref())), input_wasm)
                };
                let component = compile_component(engine, wizer, &input_wasm)?;
                let input_contract =
                    new_contract(client, wizer, &component, external_id.as_deref(), imports)
                        .await?;
                ResolvedCoordinationScriptArg::Utxo(ResolvedUtxoInput {
                    input,
                    output,
                    contract: input_contract,
                    external_id,
                    wasm: input_wasm.to_vec(),
                })
            }
        };
        resolved.push(arg);
    }
    ensure!(args.next().is_none(), "trailing arguments");
    Ok(resolved)
}

pub async fn new_contract(
    client: &(impl Client + ?Sized),
    wizer: &Wizer,
    component: &Component,
    external_id: Option<&str>,
    imports: &mut HashMap<[u8; 32], Contract>,
) -> wasmtime::Result<starstream_runtime::Contract<Ctx>> {
    struct ContractLookup<'a>(pub &'a HashMap<[u8; 32], Contract>);
    impl starstream_runtime::ContractLookup<Ctx> for ContractLookup<'_> {
        fn get_contract(
            &self,
            external_id: &str,
        ) -> wasmtime::Result<starstream_runtime::Contract<Ctx>> {
            let digest = parse_digest(external_id)?;
            let contract = self.0.get(&digest).with_context(|| {
                error!(external_id, "unresolved contract import");
                format!("contract identified by `external-id` `{external_id}` not found")
            })?;
            contract.contract.clone().with_context(|| {
                error!(external_id, "uncompiled contract import");
                format!("contract identified by `external-id` `{external_id}` was not compiled")
            })
        }
    }

    let engine = component.engine();
    let ty = component.component_type();
    let script_instance = get_coordination_script_instance_import(engine, &ty);
    let script_external_ids = script_instance.as_ref().map(|instance| {
        instance
            .coordination_scripts()
            .map(|import| import.map(|CoordinationScriptImport { external_id, .. }| external_id))
    });
    let utxo_external_ids = utxo_imports(engine, &ty)
        .map(|import| import.map(|UtxoImport { external_id, .. }| external_id));
    for external_id in utxo_external_ids.chain(script_external_ids.into_iter().flatten()) {
        let external_id = external_id?;
        let digest = parse_digest(external_id).with_context(|| {
            format!("failed to parse `external-id` `{external_id}` as multibase multihash")
        })?;
        let wasm = match imports.get(&digest) {
            Some(Contract {
                contract: Some(..), ..
            }) => continue,
            Some(Contract {
                contract: None,
                wasm,
            }) => wasm.clone(),
            None => client
                .get_contract_wasm(digest)
                .await
                .map_err(wasmtime::Error::from_anyhow)?,
        };
        let component = compile_component(engine, wizer, wasm.as_ref())?;
        let contract = Box::pin(new_contract(
            client,
            wizer,
            &component,
            Some(external_id),
            imports,
        ))
        .await?;
        imports.insert(
            digest,
            Contract {
                contract: Some(contract),
                wasm,
            },
        );
    }
    starstream_runtime::Contract::new(component, external_id, ContractLookup(imports))
}

#[allow(clippy::too_many_arguments)]
pub async fn call_coordination_script(
    store: &mut Store<Ctx>,
    client: &(impl Client + ?Sized),
    wizer: &Wizer,
    contract: &starstream_runtime::Contract<Ctx>,
    wasm: &[u8],
    export: &CoordinationScriptExport,
    imports: &mut HashMap<[u8; 32], Contract>,
    args: impl IntoIterator<Item = CoordinationScriptArg>,
    results: &mut [Val],
    utxos: &mut Vec<Utxo<Arc<std::sync::Mutex<UtxoCtx>>>>,
) -> wasmtime::Result<(Transaction, wasmtime::Result<CapturedExecution>)> {
    let args =
        resolve_coordination_script_args(client, wizer, contract, wasm, export, imports, args)
            .await?;

    let tracing_setup = prepare_tracing(store, wizer, contract, wasm, export, imports, &args);

    let transaction = run_resolved_coordination_script(
        wizer, contract, wasm, export, imports, args, results, store, utxos,
    )
    .await?;

    let registry = &store.data().traces;
    let mut handles = Vec::new();

    // tracing checks are non-blocking/best-effort for now
    //
    // mainly to not block execution on configurations not supported by the
    // current instrumentation
    //
    // it is up to the caller to report the error (if any)
    let execution = tracing_setup.and_then(|prepared| {
        postprocess_trace(
            export,
            prepared.components,
            prepared.input_locals,
            registry,
            &mut handles,
        )?;
        // TODO: Authenticate these input handles against the coordinator argument root.
        check_captured_transaction(registry, handles, &prepared.input_methods)
    });

    Ok((transaction, execution))
}

// recovers coord-local numeric source ids for the SetStorage advice field
fn postprocess_trace(
    export: &CoordinationScriptExport,
    instrumented_components: Vec<TracingComponent>,
    input_locals: Vec<usize>,
    registry: &WasmtimeTraceRegistry,
    handles: &mut Vec<starstream_proving_runtime::ResourceHandle>,
) -> Result<(), wasmtime::Error> {
    let Some(root) = instrumented_components.first() else {
        unreachable!("prepare_tracing pushes the received root wasm module unconditionally")
    };

    if !input_locals.is_empty() {
        // The root coordinator is instantiated before input UTXOs;
        // the registry iterates in instance-index order.
        let (_, coordinator) = registry
            .instances()
            .context("trace capture failed: {error}")?
            .next()
            .context("missing coordinator trace")?;
        let fref = root
            .templates
            .export_fref(export.name())
            .context("missing coordinator export")?;
        let entry = coordinator
            .steps()
            .iter()
            .find(|step| step.current_function_ref == Some(fref))
            .context("missing coordinator entry arguments")?;
        for local in input_locals {
            let &(handle, _) = entry
                .locals_words
                .get(local)
                .context("missing input handle local")?;
            handles.push(starstream_proving_runtime::ResourceHandle(handle));
        }
    }

    Ok(())
}

fn prepare_tracing(
    store: &mut Store<Ctx>,
    wizer: &Wizer,
    contract: &starstream_runtime::Contract<Ctx>,
    wasm: &[u8],
    export: &CoordinationScriptExport,
    imports: &mut HashMap<[u8; 32], Contract>,
    args: &[ResolvedCoordinationScriptArg],
) -> wasmtime::Result<TracingSetup> {
    let scripts = contract
        .coordination_scripts()
        .map(|(name, export)| export.map(|_| name))
        .collect::<wasmtime::Result<Vec<_>>>()?;
    let mut components = tracing_components(wizer, wasm, &scripts, imports)?;
    let mut input_methods = Vec::new();
    let mut input_locals = Vec::new();
    for (index, arg) in args.iter().enumerate() {
        if let ResolvedCoordinationScriptArg::Utxo(input) = arg {
            input_locals.push(index);
            input_methods.push(
                input
                    .output
                    .methods
                    .iter()
                    .map(|&(a, b, c, d)| MethodHash::from_u64_words([a, b, c, d]))
                    .collect::<Vec<_>>(),
            );
            let (_, instrumented) = wizer.instrument_component(&input.wasm)?;
            let scripts = input
                .contract
                .coordination_scripts()
                .map(|(name, export)| export.map(|_| name))
                .collect::<wasmtime::Result<Vec<_>>>()?;
            let templates = build_component_templates(&instrumented, &scripts)
                .context("failed to build input trace templates")?;
            components.push(TracingComponent {
                instrumented,
                templates,
            });
        }
    }
    if !input_locals.is_empty() {
        // Direct scalar/resource parameters each lower to _one_ core local.
        // TODO: Support composite and indirectly lowered coordinator arguments.
        // (the indexes of utxo inputs will be invalid as computed right now otherwise)
        ensure!(
            export.ty().params().len() <= 16
                && export.ty().params().all(|(_, ty)| matches!(
                    ty,
                    Type::Bool
                        | Type::S8
                        | Type::U8
                        | Type::S16
                        | Type::U16
                        | Type::S32
                        | Type::U32
                        | Type::S64
                        | Type::U64
                        | Type::Float32
                        | Type::Float64
                        | Type::Char
                        | Type::Own(_)
                        | Type::Borrow(_)
                )),
            "traced input loading requires direct scalar/resource arguments"
        );
    }

    enable_tracing(store)?;

    for component in components.as_slice() {
        register_tracing_component(
            store.data_mut(),
            &component.instrumented,
            &component.templates.bindings,
        )?;
    }

    Ok(TracingSetup {
        components,
        input_locals,
        input_methods,
    })
}

#[allow(clippy::too_many_arguments)]
async fn run_resolved_coordination_script(
    wizer: &Wizer,
    contract: &starstream_runtime::Contract<Ctx>,
    wasm: &[u8],
    export: &CoordinationScriptExport,
    imports: &mut HashMap<[u8; 32], Contract>,
    args: Vec<ResolvedCoordinationScriptArg>,
    results: &mut [Val],
    store: &mut Store<Ctx>,
    utxos: &mut Vec<Utxo<Arc<std::sync::Mutex<UtxoCtx>>>>,
) -> wasmtime::Result<Transaction> {
    let digest: [u8; 32] = Sha256::digest(wasm).into();
    let instance = contract.instantiate(&mut *store).await?;

    let mut inputs = Vec::default();
    let mut params = Vec::with_capacity(export.ty().params().len());
    for arg in args {
        let v = match arg {
            ResolvedCoordinationScriptArg::Val(value) => value,
            ResolvedCoordinationScriptArg::Utxo(input) => {
                let utxo_export = input.contract.get_utxo(&input.output.instance)?;
                let storage_export = utxo_export.storage().context("UTXO has no storage")?;
                let mut storage = Val::Record(Vec::default());
                // TODO: Use sync decoder
                read_value(
                    &mut input.output.storage.as_ref(),
                    &mut storage,
                    &Type::Record(storage_export.ty().clone()),
                )
                .await
                .context("failed to decode UTXO storage")?;
                let cx = Arc::new(std::sync::Mutex::new(UtxoCtx {
                    export: utxo_export.clone(),
                    instance: input.output.instance.into(),
                    external_id: input.external_id,
                    methods: input.output.methods.into_iter().collect(),
                    dropped: false,
                }));
                let cx_res = store.data_mut().table.push(Arc::clone(&cx))?;
                let cx_res = cx_res.try_into_resource_any(&mut *store)?;
                let contract = input.contract.instantiate(&mut *store).await?;
                let utxo = contract
                    .load_utxo(
                        &mut *store,
                        &utxo_export,
                        storage_export,
                        cx,
                        [Val::Resource(cx_res), storage],
                    )
                    .await?;
                let Ctx { table, outputs, .. } = store.data_mut();
                outputs.push(utxo.clone());
                let utxo = table.push(utxo)?;
                let utxo = utxo.try_into_resource_any(&mut *store)?;
                inputs.push(input.input);
                Val::Resource(utxo)
            }
        };
        params.push(v);
    }
    instance
        .call_coordination_script(&mut *store, export, &params, results)
        .await?;
    let Ctx {
        outputs, events, ..
    } = store.data_mut();
    let events = mem::take(events);
    let mut tx_outputs = Vec::with_capacity(outputs.len());
    for utxo in mem::take(outputs) {
        let cx = {
            let cx = lock(utxo.context())?;
            if cx.dropped {
                continue;
            }
            cx.clone()
        };
        let mut instance = WasmtimeWizerComponent {
            store,
            instance: utxo.instance(),
        };
        let (contract, wasm) = if let Some(external_id) = cx.external_id.as_deref() {
            let digest = parse_digest(external_id)?;
            let Contract { wasm, .. } = imports
                .get(&digest)
                .with_context(|| format!("`{external_id}` import not found"))?;
            let (wizer_cx, ..) = wizer.instrument_component(wasm)?;
            let wasm = wizer.snapshot_component(&wizer_cx, &mut instance).await?;
            (external_id.into(), wasm)
        } else {
            let (wizer_cx, ..) = wizer.instrument_component(wasm)?;
            let wasm = wizer.snapshot_component(&wizer_cx, &mut instance).await?;
            (encode_digest(&digest).into(), wasm)
        };
        let state = parse_state(&wasm)
            .collect::<anyhow::Result<_>>()
            .map_err(wasmtime::Error::from_anyhow)
            .context("failed to parse UTXO state")?;
        let storage = if let Some(export) = cx.export.storage() {
            let storage = utxo.storage(export).call_get(&mut *store).await?;
            let mut buf = BytesMut::new();
            ValEncoder::new(&Type::Record(export.ty().clone()))
                .encode(&Val::Record(storage), &mut buf)
                .context("failed to encode storage")?;
            buf.to_vec().into()
        } else {
            Box::default()
        };
        let mut methods = BTreeSet::default();
        for &(a, b, c, d) in &cx.methods {
            methods.insert((a, b, c, d));
        }
        utxos.push(utxo);
        tx_outputs.push(TransactionOutput {
            contract,
            instance: cx.instance.as_ref().into(),
            methods,
            storage,
            state,
        });
    }
    Ok(Transaction {
        inputs,
        outputs: tx_outputs,
        events,
        proof: Box::default(), // TODO: add proof
    })
}

#[derive(Debug, Default)]
pub struct Ctx {
    pub table: ResourceTable,
    pub events: Vec<TransactionEvent>,
    pub outputs: Vec<starstream_runtime::Utxo<<Self as starstream_runtime::Host>::UtxoContext>>,
    pub traces: WasmtimeTraceRegistry,
}

#[derive(Clone, Debug)]
pub struct UtxoCtx {
    pub export: UtxoExport,
    pub instance: Arc<str>,
    pub external_id: Option<Arc<str>>,
    pub methods: HashSet<(u64, u64, u64, u64)>,
    pub dropped: bool,
}

pub fn lock<T>(mu: &std::sync::Mutex<T>) -> wasmtime::Result<std::sync::MutexGuard<'_, T>> {
    mu.lock().map_err(|err| format_err!("{err}"))
}

impl WasmTraceSink for Ctx {
    fn wasm_trace_registry(&self) -> &WasmtimeTraceRegistry {
        &self.traces
    }

    fn wasm_trace_registry_mut(&mut self) -> &mut WasmtimeTraceRegistry {
        &mut self.traces
    }
}

impl starstream::std::cardano::Host for Ctx {
    fn block_height(&mut self) -> wasmtime::Result<i64> {
        bail!("TODO")
    }

    fn current_slot(&mut self) -> wasmtime::Result<i64> {
        bail!("TODO")
    }
}

impl starstream_runtime::Host for Ctx {
    type UtxoContext = Arc<std::sync::Mutex<UtxoCtx>>;

    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    async fn call_utxo_main(
        mut store: StoreContextMut<'_, Self>,
        instance_name: Arc<str>,
        external_id: Option<Arc<str>>,
        export: UtxoExport,
        f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> wasmtime::Result<Utxo<Self::UtxoContext>> {
        let cx = UtxoCtx {
            export,
            instance: instance_name,
            external_id,
            methods: HashSet::default(),
            dropped: false,
        };
        let utxo = f(store.as_context_mut(), Arc::new(std::sync::Mutex::new(cx))).await?;
        store.as_context_mut().data_mut().outputs.push(utxo.clone());
        Ok(utxo)
    }

    fn has_method(
        store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool> {
        let Ctx { table, .. } = store.data();
        let utxo = table.get(&utxo)?;
        let cx = lock(utxo.context())?;
        Ok(cx.methods.contains(&hash))
    }

    fn drop_utxo(
        mut store: StoreContextMut<Self>,
        utxo: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        table.delete(utxo)?;
        Ok(())
    }

    fn implements_method(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        let mut cx = lock(cx)?;
        cx.methods.insert(hash);
        Ok(())
    }

    fn resume(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.get_mut(&cx)?;
        let mut cx = lock(cx)?;
        cx.methods.clear();
        Ok(())
    }

    fn drop_utxo_context(
        mut store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()> {
        let Ctx { table, .. } = store.data_mut();
        let cx = table.delete(cx)?;
        let mut cx = lock(&cx)?;
        cx.dropped = true;
        Ok(())
    }

    fn drop_token(_store: StoreContextMut<Self>, _token: Resource<Token>) -> wasmtime::Result<()> {
        bail!("TODO")
    }

    fn emit_event(
        mut store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()> {
        let Ctx { events, .. } = store.data_mut();
        // TODO: Encode params
        let params = format!("{params:?}").into_bytes();
        events.push(TransactionEvent {
            abi_name: abi_name.as_ref().into(),
            name: name.as_ref().into(),
            params: params.into(),
        });
        Ok(())
    }
}
