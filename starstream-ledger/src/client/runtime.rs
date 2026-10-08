use core::mem;
use core::ops::Deref;
use core::pin::Pin;

use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::Arc;

use bytes::{Bytes, BytesMut};
use sha2::{Digest as _, Sha256};
use starstream_proving_runtime::{
    CapturedExecution, ComponentTemplates, WasmTraceSink, WasmtimeTraceRegistry,
    build_component_templates, check_captured_transaction, enable_tracing,
    register_tracing_component,
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

/// Contract compiled from the instrumented component bytes
#[derive(Clone)]
pub struct CompiledContract {
    pub contract: starstream_runtime::Contract<Ctx>,
    pub instrumented: Bytes,
}

impl Deref for CompiledContract {
    type Target = starstream_runtime::Contract<Ctx>;

    fn deref(&self) -> &Self::Target {
        &self.contract
    }
}

#[derive(Clone)]
pub struct Contract {
    pub contract: Option<CompiledContract>,
    pub wasm: Bytes,
}

pub trait Client {
    /// Get Wasm of the contract corresponding to `digest`
    fn get_contract(&self, digest: [u8; 32]) -> impl Future<Output = anyhow::Result<Bytes>>;

    /// Get [TransactionOutput] of the UTXO corresponding to `input`
    fn get_input_utxo(
        &self,
        input: &TransactionInput,
    ) -> impl Future<Output = anyhow::Result<TransactionOutput>>;
}

/// Instrument and compile `wasm`, compiling the imports missing from `imports` via `client`
pub async fn compile_contract(
    client: &(impl Client + ?Sized),
    engine: &Engine,
    wizer: &Wizer,
    wasm: &[u8],
    external_id: Option<&str>,
    imports: &mut HashMap<[u8; 32], Contract>,
) -> wasmtime::Result<CompiledContract> {
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
            let contract = contract.contract.as_ref().with_context(|| {
                error!(external_id, "uncompiled contract import");
                format!("contract identified by `external-id` `{external_id}` was not compiled")
            })?;
            Ok(contract.contract.clone())
        }
    }

    let (.., instrumented) = wizer
        .instrument_component(wasm)
        .context("failed to instrument component")?;
    let component =
        Component::from_binary(engine, &instrumented).context("failed to compile component")?;
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
                .get_contract(digest)
                .await
                .map_err(wasmtime::Error::from_anyhow)?,
        };
        let contract = Box::pin(compile_contract(
            client,
            engine,
            wizer,
            &wasm,
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
    let contract =
        starstream_runtime::Contract::new(&component, external_id, ContractLookup(imports))?;
    Ok(CompiledContract {
        contract,
        instrumented: instrumented.into(),
    })
}

fn build_templates(contract: &CompiledContract) -> wasmtime::Result<ComponentTemplates> {
    let scripts = contract
        .contract
        .coordination_script_names()
        .collect::<Vec<_>>();
    build_component_templates(&contract.instrumented, &scripts)
        .context("failed to build trace templates")
}

fn register_tracing(cx: &mut Ctx, contract: &CompiledContract) -> wasmtime::Result<()> {
    let templates = build_templates(contract)?;
    register_tracing_component(cx, &contract.instrumented, &templates.bindings)
}

/// Register compiled imports that have not been registered yet
fn register_imports(
    cx: &mut Ctx,
    imports: &HashMap<[u8; 32], Contract>,
    registered: &mut HashSet<[u8; 32]>,
) -> wasmtime::Result<()> {
    for (digest, Contract { contract, .. }) in imports {
        let Some(contract) = contract else {
            continue;
        };
        if registered.insert(*digest) {
            register_tracing(cx, contract)?;
        }
    }
    Ok(())
}

/// Call the coordination script `export` exported by `contract` with `args`,
/// loading UTXO arguments through `client`.
///
/// `wasm` must be the original component bytes `contract` was compiled from.
///
/// The execution check is best-effort for now, mainly to not block execution
/// on configurations not supported by the current instrumentation, so its
/// result is returned next to the transaction and it is up to the caller to
/// report the error, if any.
#[allow(clippy::too_many_arguments)]
pub async fn call_coordination_script(
    store: &mut Store<Ctx>,
    client: &(impl Client + ?Sized),
    wizer: &Wizer,
    contract: &CompiledContract,
    wasm: &[u8],
    export: &CoordinationScriptExport,
    imports: &mut HashMap<[u8; 32], Contract>,
    args: Vec<CoordinationScriptArg>,
    results: &mut [Val],
    utxos: &mut Vec<Utxo<Arc<std::sync::Mutex<UtxoCtx>>>>,
) -> wasmtime::Result<(Transaction, wasmtime::Result<CapturedExecution>)> {
    let args_len = args.len();
    let param_len = export.ty().params().len();
    ensure!(
        args_len == param_len,
        "argument length mismatch, expected: {args_len}, got: {param_len}"
    );

    let engine = contract.component().engine();
    let digest: [u8; 32] = Sha256::digest(wasm).into();
    let mut registered = HashSet::from([digest]);
    let mut tracing = (|| {
        if args
            .iter()
            .any(|arg| matches!(arg, CoordinationScriptArg::Utxo(_)))
        {
            // Direct scalar/resource parameters each lower to _one_ core local.
            // TODO: Support composite and indirectly lowered coordinator arguments.
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
        let templates = build_templates(contract)?;
        enable_tracing(store)?;
        let cx = store.data_mut();
        register_tracing_component(cx, &contract.instrumented, &templates.bindings)?;
        register_imports(cx, imports, &mut registered)?;
        Ok(templates)
    })();
    let instance = contract.instantiate(&mut *store).await?;

    let mut inputs = Vec::default();
    let mut input_locals = Vec::default();
    let mut input_methods = Vec::default();
    let mut params = Vec::with_capacity(param_len);
    for arg in args {
        let v = match arg {
            CoordinationScriptArg::Val(v) => v,
            CoordinationScriptArg::Utxo(input) => {
                let utxo = client
                    .get_input_utxo(&input)
                    .await
                    .map_err(wasmtime::Error::from_anyhow)?;
                let utxo_contract_digest = parse_digest(&utxo.contract).with_context(|| {
                    format!("failed to parse `{}` as multibase multihash", utxo.contract)
                })?;
                let (external_id, wasm) = if utxo_contract_digest == digest {
                    let wasm =
                        apply_state(wasm, &utxo.state).map_err(wasmtime::Error::from_anyhow)?;
                    (None, wasm)
                } else if let Some(Contract { wasm, .. }) = imports.get(&utxo_contract_digest) {
                    let wasm =
                        apply_state(wasm, &utxo.state).map_err(wasmtime::Error::from_anyhow)?;
                    (Some(Arc::from(utxo.contract)), wasm)
                } else {
                    let wasm = client
                        .get_contract(utxo_contract_digest)
                        .await
                        .map_err(wasmtime::Error::from_anyhow)?;
                    imports.insert(
                        utxo_contract_digest,
                        Contract {
                            contract: None,
                            wasm: wasm.clone(),
                        },
                    );
                    let wasm =
                        apply_state(&wasm, &utxo.state).map_err(wasmtime::Error::from_anyhow)?;
                    (Some(Arc::from(utxo.contract)), wasm)
                };
                let contract = compile_contract(
                    client,
                    engine,
                    wizer,
                    &wasm,
                    external_id.as_deref(),
                    &mut *imports,
                )
                .await?;
                tracing = tracing.and_then(|templates| {
                    let cx = store.data_mut();
                    register_tracing(cx, &contract)?;
                    register_imports(cx, imports, &mut registered)?;
                    Ok(templates)
                });
                input_locals.push(params.len());
                input_methods.push(
                    utxo.methods
                        .iter()
                        .map(|&(a, b, c, d)| {
                            starstream_proving_runtime::MethodHash::from_u64_words([a, b, c, d])
                        })
                        .collect::<Vec<_>>(),
                );
                let utxo_export = contract.get_utxo(&utxo.instance)?;
                let storage_export = utxo_export.storage().context("UTXO has no storage")?;
                let mut storage = Val::Record(Vec::default());
                // TODO: Use sync decoder
                read_value(
                    &mut utxo.storage.as_ref(),
                    &mut storage,
                    &Type::Record(storage_export.ty().clone()),
                )
                .await
                .context("failed to decode UTXO storage")?;
                let cx = Arc::new(std::sync::Mutex::new(UtxoCtx {
                    export: utxo_export.clone(),
                    instance: utxo.instance.into(),
                    external_id,
                    methods: utxo.methods.into_iter().collect(),
                    dropped: false,
                }));
                let cx_res = store.data_mut().table.push(Arc::clone(&cx))?;
                let cx_res = cx_res.try_into_resource_any(&mut *store)?;
                let contract = contract.instantiate(&mut *store).await?;
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
                inputs.push(input);
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
    let execution = tracing.and_then(|templates| {
        let mut handles = Vec::with_capacity(input_locals.len());
        if !input_locals.is_empty() {
            // The root coordinator is instantiated before input UTXOs;
            // the registry iterates in instance-index order.
            let (.., coordinator) = store
                .data()
                .traces
                .instances()
                .context("failed to capture trace")?
                .next()
                .context("missing coordinator trace")?;
            let fref = templates
                .export_fref(export.name())
                .context("missing coordinator export")?;
            let entry = coordinator
                .steps()
                .iter()
                .find(|step| step.current_function_ref == Some(fref))
                .context("missing coordinator entry arguments")?;
            for local in input_locals {
                let &(handle, ..) = entry
                    .locals_words
                    .get(local)
                    .context("missing input handle local")?;
                handles.push(starstream_proving_runtime::ResourceHandle(handle));
            }
        }
        check_captured_transaction(&store.data().traces, handles, &input_methods)
    });
    Ok((
        Transaction {
            inputs,
            outputs: tx_outputs,
            events,
            proof: Box::default(), // TODO: add proof
        },
        execution,
    ))
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
