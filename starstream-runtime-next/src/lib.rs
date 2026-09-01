use std::pin::Pin;
use std::sync::Arc;

use tracing::{debug, instrument};
use wasmtime::component::{
    Component, ComponentExportIndex, ExportLookup, HasSelf, Instance, InstancePre, Linker,
    LinkerInstance, Resource, ResourceAny, ResourceTable, ResourceType, Type, Val, types,
};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut, Engine, StoreContextMut, bail, ensure};

pub mod bindings {
    // NOTE: `starstream:std/{builtin,utxo-context}` bindings are hand-written
    wasmtime::component::bindgen!({
        path: "../starstream-to-wasm/wit",
        inline: "
            package starstream:host;

            world host {
                import starstream:std/cardano;
            }
        ",
        imports: {
            default: tracing | trappable,
        }
    });
}

pub trait Host: bindings::starstream::std::cardano::Host + Send + Sized + 'static {
    type UtxoContext: Clone + Send;
    type Token;

    fn table(&mut self) -> &mut ResourceTable;

    fn contract(store: StoreContextMut<Self>) -> Contract<Self>;

    fn call_utxo_main(
        store: StoreContextMut<Self>,
        f: impl for<'a> FnOnce(
            StoreContextMut<'a, Self>,
            Self::UtxoContext,
        ) -> Pin<
            Box<dyn Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send + 'a>,
        > + Send,
    ) -> impl Future<Output = wasmtime::Result<Utxo<Self::UtxoContext>>> + Send;

    fn has_method(
        store: StoreContextMut<Self>,
        cx: Resource<Utxo<Self::UtxoContext>>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<bool>;

    fn drop_utxo(
        store: StoreContextMut<Self>,
        cx: Resource<Utxo<Self::UtxoContext>>,
    ) -> wasmtime::Result<()>;

    fn implements_method(
        store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
        hash: (u64, u64, u64, u64),
    ) -> wasmtime::Result<()>;

    fn resume(
        store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()>;

    fn drop_utxo_context(
        store: StoreContextMut<Self>,
        cx: Resource<Self::UtxoContext>,
    ) -> wasmtime::Result<()>;

    fn drop_token(store: StoreContextMut<Self>, cx: Resource<Self::Token>) -> wasmtime::Result<()>;

    fn emit_event(
        store: StoreContextMut<Self>,
        abi_name: &Arc<str>,
        name: &Arc<str>,
        params: &[Val],
    ) -> wasmtime::Result<()>;
}

pub fn componentize(wasm: impl AsRef<[u8]>) -> anyhow::Result<Vec<u8>> {
    use anyhow::Context as _;

    wit_component::ComponentEncoder::default()
        .validate(true)
        .module(wasm.as_ref())
        .context("failed to set core component module")?
        .encode()
        .context("failed to encode a component")
}

#[instrument(level = "trace", skip_all)]
fn load_component(engine: &Engine, wasm: impl AsRef<[u8]>) -> wasmtime::Result<Component> {
    let wasm = wasm.as_ref();
    if wasmparser::Parser::is_core_wasm(wasm) {
        let wasm = componentize(wasm).map_err(wasmtime::Error::from_anyhow)?;
        Component::from_binary(engine, &wasm)
    } else {
        Component::from_binary(engine, wasm)
    }
}

enum ContractImportTarget<'a, T: 'static> {
    Component(&'a Component),
    Contract(Contract<T>),
}

impl<T> ContractImportTarget<'_, T> {
    fn component(&self) -> &Component {
        match self {
            Self::Component(component) => component,
            Self::Contract(contract) => contract.pre.component(),
        }
    }

    fn contract(&self) -> Option<&Contract<T>> {
        if let Self::Contract(contract) = self {
            Some(contract)
        } else {
            None
        }
    }
}

/// Link ABI event [`types::ComponentFunc`] in a [`LinkerInstance`]
#[instrument(level = "trace", skip_all)]
fn link_event_function<T: Host>(
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    abi_name: &str,
    name: &str,
) -> wasmtime::Result<()> {
    let abi_name = Arc::<str>::from(abi_name);
    let name = Arc::<str>::from(name);
    ensure!(ty.results().len() == 0);
    linker.func_new(&Arc::clone(&name), move |store, _ty, params, _results| {
        T::emit_event(store, &abi_name, &name, params)
    })
}

/// Link dynamic ABI instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
fn link_event_instance<T: Host>(
    engine: &Engine,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
    abi_name: &str,
) -> wasmtime::Result<()> {
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(engine) {
        debug!(name, "linking event instance item");
        match ty {
            types::ComponentItem::ComponentFunc(ty) => {
                link_event_function(linker, ty, abi_name, name)?;
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("event instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => bail!("event instance module imports unsupported"),
            types::ComponentItem::Component(..) => {
                bail!("event instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("event instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) => {
                bail!("event instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

/// Link UTXO downcast.
#[instrument(level = "trace", skip_all)]
fn link_utxo_downcast<T: Host>(
    component: Component,
    linker: &mut LinkerInstance<T>,
    instance_idx: ComponentExportIndex,
) -> wasmtime::Result<()> {
    linker.func_wrap("downcast", move |mut store, (utxo,)| {
        let utxo = store
            .data_mut()
            .table()
            .get::<Utxo<T::UtxoContext>>(&utxo)?;
        if !Component::same(&utxo.component, &component) {
            return Ok((None,));
        }
        if utxo.instance_idx != instance_idx {
            return Ok((None,));
        }
        let utxo = utxo.clone();
        let utxo = store.data_mut().table().push(utxo)?;
        Ok((Some(utxo),))
    })
}

/// Link UTXO upcast.
#[instrument(level = "trace", skip_all)]
fn link_utxo_upcast<T: Host>(linker: &mut LinkerInstance<T>) -> wasmtime::Result<()> {
    linker.func_wrap("upcast", move |mut store, (utxo,)| {
        let utxo = store
            .data_mut()
            .table()
            .get::<Utxo<T::UtxoContext>>(&utxo)
            .cloned()?;
        let utxo = store.data_mut().table().push(utxo)?;
        Ok((utxo,))
    })
}

/// Link typed UTXO main in a [`LinkerInstance`]
#[instrument(level = "trace", skip_all)]
fn link_typed_utxo_main<T: Host>(
    target: &ContractImportTarget<'_, T>,
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    instance_idx: ComponentExportIndex,
    idx: ComponentExportIndex,
    name: &str,
) -> wasmtime::Result<()> {
    let mut result_tys = ty.results();
    let (Some(Type::Own(..)), None) = (result_tys.next(), result_tys.next()) else {
        bail!("`main fn` import does not return a single resource value")
    };
    let contract = target.contract().cloned();
    linker.func_new_async(name, move |mut store, _ty, params, results| {
        let contract = contract.clone();
        Box::new(async move {
            let contract = contract.unwrap_or_else(|| T::contract(store.as_context_mut()));
            let instance = contract.instantiate(&mut store).await?;

            let mut params = {
                let mut ps = Vec::with_capacity(params.len().saturating_add(1));
                ps.push(Val::Bool(false));
                for p in params {
                    ps.push(p.clone());
                }
                ps
            };
            let utxo = T::call_utxo_main(store.as_context_mut(), move |mut store, cx| {
                Box::pin(async move {
                    let cx_res = store.data_mut().table().push(cx.clone())?;
                    let cx_res = cx_res.try_into_resource_any(&mut store)?;
                    params[0] = Val::Resource(cx_res);
                    instance
                        .construct_utxo(&mut store, instance_idx, idx, params, cx)
                        .await
                })
            })
            .await?;
            let utxo = store.data_mut().table().push(utxo)?;
            let utxo = utxo.try_into_resource_any(store.as_context_mut())?;
            results[0] = Val::Resource(utxo);
            Ok(())
        })
    })
}

/// Link typed UTXO method in a [`LinkerInstance`]
#[instrument(level = "trace", skip_all)]
fn link_typed_utxo_method<T: Host>(
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    idx: ComponentExportIndex,
    name: &str,
) -> wasmtime::Result<()> {
    let Some((_, Type::Borrow(..))) = ty.params().next() else {
        bail!("function does not take borrowed resource type as first parameter");
    };
    linker.func_new_async(name, move |mut store, _ty, params, results| {
        Box::new(async move {
            let Some(Val::Resource(utxo)) = params.first() else {
                bail!("first parameter is not a resource")
            };
            let utxo = utxo.try_into_resource::<Utxo<T::UtxoContext>>(&mut store)?;
            let &Utxo {
                instance, resource, ..
            } = store.data_mut().table().get(&utxo)?;
            let f = instance
                .get_func(&mut store, idx)
                .context("method export not found")?;
            let params = {
                let mut ps = Vec::with_capacity(params.len());
                ps.push(Val::Resource(resource));
                for p in &params[1..] {
                    ps.push(p.clone());
                }
                ps
            };
            f.call_async(&mut store, &params, results).await?;
            Ok(())
        })
    })
}

/// Link typed UTXO [`types::ComponentFunc`] in a [`LinkerInstance`]
#[instrument(level = "trace", skip(target, linker, ty, instance_idx))]
fn link_typed_utxo_function<T: Host>(
    target: &ContractImportTarget<'_, T>,
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    instance_idx: &ComponentExportIndex,
    name: &str,
) -> wasmtime::Result<()> {
    let idx = target
        .component()
        .get_export_index(Some(instance_idx), name)
        .with_context(|| format!("`{name}` export was not found"))?;
    match name.split_once(']') {
        Some(("[static", ..)) => link_typed_utxo_main(target, linker, ty, *instance_idx, idx, name),
        Some(("[method", ..)) => link_typed_utxo_method(linker, ty, idx, name),
        _ => bail!("unexpected typed UTXO instance function import `{name}`"),
    }
}

/// Link typed UTXO instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
fn link_typed_utxo_instance<T: Host>(
    target: &ContractImportTarget<'_, T>,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
    name: &str,
) -> wasmtime::Result<()> {
    let component = target.component();
    let instance_idx = component
        .get_export_index(None, name)
        .with_context(|| format!("`{name}` export was not found"))?;
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(component.engine()) {
        debug!(name, "linking typed UTXO instance item");
        match ty {
            types::ComponentItem::ComponentFunc(..) if name == "downcast" => {
                link_utxo_downcast(component.clone(), linker, instance_idx)?;
            }
            types::ComponentItem::ComponentFunc(..) if name == "upcast" => {
                link_utxo_upcast(linker)?;
            }
            types::ComponentItem::ComponentFunc(ty) => {
                link_typed_utxo_function(target, linker, ty, &instance_idx, name)?;
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("typed UTXO instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => {
                bail!("typed UTXO instance module imports unsupported")
            }
            types::ComponentItem::Component(..) => {
                bail!("typed UTXO instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("typed UTXO instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) if name == "utxo" => {
                linker.resource(
                    "utxo",
                    ResourceType::host::<Utxo<T::UtxoContext>>(),
                    |mut store, rep| {
                        store
                            .data_mut()
                            .table()
                            .delete::<Utxo<T::UtxoContext>>(Resource::new_own(rep))?;
                        Ok(())
                    },
                )?;
            }
            types::ComponentItem::Resource(..) => {
                bail!("typed UTXO instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

/// Link dynamic UTXO [`types::ComponentFunc`] in a [`LinkerInstance`]
#[instrument(level = "trace", skip(linker, ty))]
fn link_dynamic_utxo_function<T: Host>(
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    name: &str,
) -> wasmtime::Result<()> {
    let Some((_, Type::Borrow(..))) = ty.params().next() else {
        bail!("function does not take borrowed resource type as first parameter");
    };
    let export_name = Arc::from(format!("[method]utxo.{name}"));
    linker.func_new_async(name, move |mut store, _ty, params, results| {
        let export_name = Arc::clone(&export_name);
        Box::new(async move {
            let Some(Val::Resource(utxo)) = params.first() else {
                bail!("first parameter is not a resource")
            };
            let utxo = utxo.try_into_resource::<Utxo<T::UtxoContext>>(&mut store)?;
            let &Utxo {
                instance,
                instance_idx,
                resource,
                ..
            } = store.data_mut().table().get(&utxo)?;
            let idx = instance
                .get_export_index(&mut store, Some(&instance_idx), &export_name)
                .context("method export index not found")?;
            let f = instance
                .get_func(&mut store, idx)
                .context("method export not found")?;
            let params = {
                let mut ps = Vec::with_capacity(params.len());
                ps.push(Val::Resource(resource));
                for p in &params[1..] {
                    ps.push(p.clone());
                }
                ps
            };
            f.call_async(&mut store, &params, results).await?;
            Ok(())
        })
    })
}

/// Link dynamic UTXO instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
fn link_dynamic_utxo_instance<T: Host>(
    engine: &Engine,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
) -> wasmtime::Result<()> {
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(engine) {
        debug!(name, "linking dynamic UTXO instance item");
        match ty {
            types::ComponentItem::ComponentFunc(ty) => {
                link_dynamic_utxo_function(linker, ty, name)?
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("dynamic UTXO instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => {
                bail!("dynamic UTXO instance module imports unsupported")
            }
            types::ComponentItem::Component(..) => {
                bail!("dynamic UTXO instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("dynamic UTXO instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) | types::ComponentItem::Resource(..) => {}
        }
    }
    Ok(())
}

/// Link typed token instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
fn link_typed_token_instance<T: Host>(
    engine: &Engine,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
) -> wasmtime::Result<()> {
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(engine) {
        debug!(name, "linking typed token instance item");
        match ty {
            types::ComponentItem::ComponentFunc(..) => {
                bail!("typed token instance function imports unsupported")
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("typed token instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => {
                bail!("typed token instance module imports unsupported")
            }
            types::ComponentItem::Component(..) => {
                bail!("typed token instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("typed token instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) if name == "token" => {
                linker.resource("token", ResourceType::host::<T::Token>(), |store, rep| {
                    T::drop_token(store, Resource::new_own(rep))
                })?;
            }
            types::ComponentItem::Resource(..) => {
                bail!("typed token instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

/// Link coordination script [`types::ComponentFunc`] in a [`LinkerInstance`]
#[instrument(level = "trace", skip(contract, linker))]
fn link_coordination_script_function<T: Host>(
    contract: Contract<T>,
    linker: &mut LinkerInstance<T>,
    name: &str,
) -> wasmtime::Result<()> {
    let export = contract.get_coordination_script(name)?;
    linker.func_new_async(name, move |mut store, _ty, params, results| {
        let export = export.clone();
        let contract = contract.clone();
        Box::new(async move {
            let instance = contract.instantiate(&mut store).await?;
            instance
                .call_coordination_script(&mut store, &export, params, results)
                .await
        })
    })
}

/// Link coordination script instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip(engine, linker, contracts, ty))]
fn link_coordination_script_instance<T: Host>(
    engine: &Engine,
    linker: &mut LinkerInstance<T>,
    contracts: &impl ContractLookup<T>,
    ty: &types::ComponentInstance,
) -> wasmtime::Result<()> {
    for (
        name,
        types::ComponentExtern {
            ty, external_id, ..
        },
    ) in ty.exports(engine)
    {
        debug!(name, "linking coordination script instance item");
        match ty {
            types::ComponentItem::ComponentFunc(..) => {
                let external_id = external_id.with_context(|| {
                    format!("`external-id` missing for coordination script import `{name}`")
                })?;
                let contract = contracts.get_contract(external_id).with_context(|| {
                    format!("failed to get contract for coordination script import `{name}`")
                })?;
                link_coordination_script_function(contract, linker, name)?;
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("coordination script instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => {
                bail!("coordination script instance module imports unsupported")
            }
            types::ComponentItem::Component(..) => {
                bail!("coordination script instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("coordination script instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) => {
                bail!("coordination script instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

/// Link non-std instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip(component, linker, contracts, ty))]
fn link_instance<T: Host>(
    component: &Component,
    linker: &mut LinkerInstance<T>,
    contracts: &impl ContractLookup<T>,
    ty: &types::ComponentInstance,
    name: &str,
    external_id: Option<&str>,
) -> wasmtime::Result<()> {
    debug_assert!(!name.starts_with("starstream:std"));

    let engine = component.engine();
    match (
        name.split_once('/'),
        ty.get_export(engine, "utxo"),
        ty.get_export(engine, "token"),
    ) {
        (Some(("starstream:utxo", name)), ..) => {
            let external_id = external_id
                .with_context(|| format!("`external-id` missing for typed UTXO import `{name}`"))?;
            let contract = contracts.get_contract(external_id).with_context(|| {
                format!("failed to get contract for typed UTXO import `{name}`")
            })?;
            link_typed_utxo_instance(&ContractImportTarget::Contract(contract), linker, ty, name)
        }

        (
            Some(("starstream:self", name)),
            Some(types::ComponentExtern {
                ty: types::ComponentItem::Resource(..),
                ..
            }),
            None,
        ) => link_typed_utxo_instance(
            &ContractImportTarget::Component(component),
            linker,
            ty,
            name,
        ),

        (
            Some(("starstream:self", ..)),
            None,
            Some(types::ComponentExtern {
                ty: types::ComponentItem::Resource(..),
                ..
            }),
        ) => link_typed_token_instance(engine, linker, ty),

        (
            Some(("starstream:self", ..)),
            Some(types::ComponentExtern {
                ty: types::ComponentItem::Resource(..),
                ..
            }),
            Some(types::ComponentExtern {
                ty: types::ComponentItem::Resource(..),
                ..
            }),
        ) => bail!("both `utxo` and `token` resources exported by instance `{name}` import"),

        (Some(("starstream:self", ..)), ..) => {
            bail!("failed to classify `{name}` instance import")
        }

        (Some(("starstream:events", name)), ..) => link_event_instance(engine, linker, ty, name),

        (Some(("starstream:effects", ..)), ..) => {
            bail!("effect imports unsupported")
        }

        (Some(("starstream:contract", "dynamic-utxo")), ..) => {
            link_dynamic_utxo_instance(engine, linker, ty)
        }

        (Some(("starstream:contract", "scripts")), ..) => {
            link_coordination_script_instance(engine, linker, contracts, ty)
        }

        _ => bail!("unexpected instance import `{name}`"),
    }
}

/// Link imports of the contract.
#[instrument(level = "trace", skip_all)]
fn link_imports<T: Host>(
    component: &Component,
    linker: &mut Linker<T>,
    contracts: &impl ContractLookup<T>,
) -> wasmtime::Result<()> {
    for (
        name,
        types::ComponentExtern {
            ty, external_id, ..
        },
    ) in component.component_type().imports(component.engine())
    {
        match ty {
            types::ComponentItem::ComponentFunc(..) => {
                bail!("root instance function imports unsupported")
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("core function imports unsupported")
            }
            types::ComponentItem::Module(..) => bail!("module imports unsupported"),
            types::ComponentItem::Component(..) => bail!("component imports unsupported"),
            types::ComponentItem::ComponentInstance(ty) => {
                if name.starts_with("starstream:std") {
                    debug!(?name, "skipping `starstream:std` import");
                    continue;
                }
                let mut linker = linker
                    .instance(name)
                    .with_context(|| format!("failed to instantiate `{name}` in the linker"))?;
                debug!(?name, "linking root instance");
                link_instance(component, &mut linker, contracts, &ty, name, external_id)?;
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) => {
                debug!(?name, "skip root instance resource import")
            }
        }
    }
    Ok(())
}

/// Compiled, pre-instantiated Starstream contract
pub struct Contract<T: 'static> {
    pre: InstancePre<T>,
    ty: types::Component,
}

impl<T: 'static> Clone for Contract<T> {
    fn clone(&self) -> Self {
        Self {
            pre: self.pre.clone(),
            ty: self.ty.clone(),
        }
    }
}

fn link_builtin<T: Host>(linker: &mut Linker<T>) -> wasmtime::Result<()> {
    let mut linker = linker
        .instance("starstream:std/builtin")
        .context("failed to instantiate `starstream:std/builtin` in the linker")?;

    linker.resource(
        "utxo",
        ResourceType::host::<Utxo<T::UtxoContext>>(),
        |store, cx| T::drop_utxo(store, Resource::new_own(cx)),
    )?;
    linker.func_wrap("[method]utxo.has-method", |store, (cx, hash)| {
        let ret = T::has_method(store, cx, hash)?;
        Ok((ret,))
    })?;

    linker.resource("token", ResourceType::host::<T::Token>(), |store, cx| {
        T::drop_token(store, Resource::new_own(cx))
    })?;
    Ok(())
}

fn link_utxo_context<T: Host>(linker: &mut Linker<T>) -> wasmtime::Result<()> {
    let mut linker = linker
        .instance("starstream:std/utxo-context")
        .context("failed to instantiate `starstream:std/utxo-context` in the linker")?;

    linker.resource(
        "utxo-context",
        ResourceType::host::<T::UtxoContext>(),
        |store, cx| T::drop_utxo_context(store, Resource::new_own(cx)),
    )?;
    linker.func_wrap(
        "[method]utxo-context.implements-method",
        |store, (cx, hash)| T::implements_method(store, cx, hash),
    )?;
    linker.func_wrap("[method]utxo-context.resume", |store, (cx,)| {
        T::resume(store, cx)
    })?;
    Ok(())
}

pub trait ContractLookup<T> {
    /// Lookup a contract by `external_id`
    fn get_contract(&self, external_id: &str) -> wasmtime::Result<Contract<T>>;
}

fn lookup_get_storage_export(
    component: &Component,
    instance: ComponentExportIndex,
    resource_ty: ResourceType,
    storage_ty: &types::Record,
) -> wasmtime::Result<ComponentExportIndex> {
    let (ty, idx) = component
        .get_export(Some(&instance), "get-storage")
        .context("`get-storage` export not found")?;
    let types::ComponentItem::ComponentFunc(ty) = ty else {
        bail!("`get-storage` export is not a function")
    };
    let mut params = ty.params();
    let (Some((_, Type::Borrow(param_resource_ty))), None) = (params.next(), params.next()) else {
        bail!("`get-storage` does not take borrowed resource type as the only parameter");
    };
    if param_resource_ty != resource_ty {
        bail!("`get-storage` resource type does not match expected resource type");
    }
    let mut results = ty.results();
    let (Some(Type::Record(record_ty)), None) = (results.next(), results.next()) else {
        bail!("`get-storage` does not return a record as the only return value");
    };
    if record_ty != *storage_ty {
        bail!("`get-storage` record type does not match storage type");
    }
    Ok(idx)
}

impl<T: Host> Contract<T> {
    /// Compile and pre-instantiate a Starstream [Contract]
    #[instrument(level = "trace", skip_all)]
    pub fn new(
        engine: &Engine,
        contracts: impl ContractLookup<T>,
        wasm: impl AsRef<[u8]>,
    ) -> wasmtime::Result<Self> {
        let wasm = wasm.as_ref();

        debug!("loading component");
        let component = load_component(engine, wasm)?;

        let mut linker = Linker::new(engine);

        debug!("linking component imports");
        bindings::Host_::add_to_linker::<_, HasSelf<_>>(&mut linker, |cx| cx)
            .context("failed to link generated bindings")?;
        link_builtin(&mut linker).context("failed to link `starstream:std/builtin`")?;
        link_utxo_context(&mut linker).context("failed to link `starstream:std/utxo-context`")?;
        link_imports(&component, &mut linker, &contracts)?;

        let ty = linker
            .substituted_component_type(&component)
            .context("failed to derive component type")?;

        debug!("pre-instantiating component");
        let pre = linker
            .instantiate_pre(&component)
            .context("failed to pre-instantiate component")?;

        Ok(Self { pre, ty })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_utxo_typed(
        &self,
        name: &str,
        instance_ty: types::ComponentInstance,
    ) -> wasmtime::Result<UtxoExport> {
        fn set_storage(
            component: &Component,
            instance_ty: &types::ComponentInstance,
            instance_idx: ComponentExportIndex,
            utxo_ty: ResourceType,
            utxo_cx_ty: ResourceType,
            storage_ty: &types::Record,
        ) -> wasmtime::Result<ComponentExportIndex> {
            let idx = component
                .get_export_index(Some(&instance_idx), "set-storage")
                .context("`set-storage` export index not found")?;
            let ty = instance_ty
                .get_export(component.engine(), "set-storage")
                .context("`set-storage` export not found")?;
            let types::ComponentExtern {
                ty: types::ComponentItem::ComponentFunc(ty),
                ..
            } = ty
            else {
                bail!("`set-storage` export is not a function")
            };
            let mut params = ty.params();
            let (Some((_, Type::Own(resource_ty))), Some((_, Type::Record(record_ty))), None) =
                (params.next(), params.next(), params.next())
            else {
                bail!("`set-storage` does not take owned resource and a record as parameters");
            };
            if resource_ty != utxo_cx_ty {
                bail!("`set-storage` resource type does not match UTXO context type");
            }
            if record_ty != *storage_ty {
                bail!("`set-storage` record type does not match storage type");
            }
            let mut results = ty.results();
            let (Some(Type::Own(resource_ty)), None) = (results.next(), results.next()) else {
                bail!("`set-storage` does not return an owned resource as the only return value");
            };
            if resource_ty != utxo_ty {
                bail!("`set-storage` resource type does not match UTXO resource type");
            }
            Ok(idx)
        }

        let engine = self.pre.engine();
        let component = self.pre.component();
        let instance_idx = component
            .get_export_index(None, name)
            .context("export not found")?;
        let Some(types::ComponentExtern {
            ty: types::ComponentItem::Resource(utxo_ty),
            ..
        }) = instance_ty.get_export(engine, "utxo")
        else {
            bail!("instance does not export `utxo` resource")
        };
        let storage = instance_ty
            .get_export(engine, "storage")
            .map(|types::ComponentExtern { ty, .. }| {
                let types::ComponentItem::Type(Type::Record(storage_ty)) = ty else {
                    bail!("`storage` export is not a record")
                };
                let get = lookup_get_storage_export(component, instance_idx, utxo_ty, &storage_ty)?;
                let set = set_storage(
                    component,
                    &instance_ty,
                    instance_idx,
                    utxo_ty,
                    ResourceType::host::<T::UtxoContext>(),
                    &storage_ty,
                )?;
                Ok(StorageExport {
                    ty: storage_ty,
                    get,
                    set,
                })
            })
            .transpose()?;
        Ok(UtxoExport {
            resource_ty: utxo_ty,
            instance_ty,
            instance_idx,
            storage,
        })
    }

    /// Get UTXO export by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_utxo(&self, name: &str) -> wasmtime::Result<UtxoExport> {
        let types::ComponentExtern { ty, .. } = self
            .ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentInstance(ty) = ty else {
            bail!("export is not an instance")
        };
        self.get_utxo_typed(name, ty)
    }

    /// Iterate over exported UTXOs along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn utxos(&self) -> impl Iterator<Item = (&str, wasmtime::Result<UtxoExport>)> {
        let engine = self.pre.engine();
        self.ty
            .exports(engine)
            .filter_map(move |(name, ty)| match ty {
                types::ComponentExtern {
                    ty: types::ComponentItem::ComponentInstance(ty),
                    ..
                } if ty.get_export(engine, "utxo").is_some() => {
                    Some((name, self.get_utxo_typed(name, ty)))
                }
                _ => None,
            })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_token_typed(
        &self,
        name: &str,
        instance_ty: types::ComponentInstance,
    ) -> wasmtime::Result<TokenExport> {
        fn set_storage(
            component: &Component,
            instance: ComponentExportIndex,
            token_ty: ResourceType,
            storage_ty: &types::Record,
        ) -> wasmtime::Result<ComponentExportIndex> {
            let (ty, idx) = component
                .get_export(Some(&instance), "set-storage")
                .context("`set-storage` export not found")?;
            let types::ComponentItem::ComponentFunc(ty) = ty else {
                bail!("`set-storage` export is not a function")
            };
            let mut params = ty.params();
            let (Some((_, Type::Record(record_ty))), None) = (params.next(), params.next()) else {
                bail!("`set-storage` does not take owned resource and a record as parameters");
            };
            if record_ty != *storage_ty {
                bail!("`set-storage` record type does not match storage type");
            }
            let mut results = ty.results();
            let (Some(Type::Own(resource_ty)), None) = (results.next(), results.next()) else {
                bail!("`set-storage` does not return an owned resource as the only return value");
            };
            if resource_ty != token_ty {
                bail!("`set-storage` resource type does not match token resource type");
            }
            Ok(idx)
        }

        fn attach(
            component: &Component,
            instance_ty: &types::ComponentInstance,
            instance: ComponentExportIndex,
            token_ty: ResourceType,
            utxo_ty: ResourceType,
        ) -> wasmtime::Result<ComponentExportIndex> {
            let idx = component
                .get_export_index(Some(&instance), "[method]token.attach")
                .context("`[method]token.attach` export index not found")?;
            let types::ComponentExtern { ty, .. } = instance_ty
                .get_export(component.engine(), "[method]token.attach")
                .context("`[method]token.attach` export not found")?;
            let types::ComponentItem::ComponentFunc(ty) = ty else {
                bail!("`[method]token.attach` export is not a function")
            };
            let mut params = ty.params();
            let (Some((_, Type::Borrow(param_0_ty))), Some((_, Type::Borrow(param_1_ty))), None) =
                (params.next(), params.next(), params.next())
            else {
                bail!(
                    "`[method]token.attach` does not take two borrowed resources as the only parameters"
                );
            };
            ensure!(
                param_0_ty == token_ty,
                "`[method]token.attach` first parameter does does not match token resource type"
            );
            ensure!(
                param_1_ty == utxo_ty,
                "`[method]token.attach` second parameter does does not match builtin UTXO resource type"
            );
            ensure!(
                ty.results().len() == 0,
                "`[method]token.attach` should not return anything"
            );
            Ok(idx)
        }

        fn detach(
            component: &Component,
            instance_ty: &types::ComponentInstance,
            instance: ComponentExportIndex,
            token_ty: ResourceType,
            utxo_ty: ResourceType,
        ) -> wasmtime::Result<ComponentExportIndex> {
            let idx = component
                .get_export_index(Some(&instance), "[method]token.detach")
                .context("`[method]token.detach` export index not found")?;
            let types::ComponentExtern { ty, .. } = instance_ty
                .get_export(component.engine(), "[method]token.detach")
                .context("`[method]token.detach` export not found")?;
            let types::ComponentItem::ComponentFunc(ty) = ty else {
                bail!("`[method]token.detach` export is not a function")
            };
            let mut params = ty.params();
            let (Some((_, Type::Borrow(param_0_ty))), Some((_, Type::Borrow(param_1_ty))), None) =
                (params.next(), params.next(), params.next())
            else {
                bail!(
                    "`[method]token.detach` does not take two borrowed resources as the only parameters"
                );
            };
            ensure!(
                param_0_ty == token_ty,
                "`[method]token.detach` first parameter does does not match token resource type"
            );
            ensure!(
                param_1_ty == utxo_ty,
                "`[method]token.detach` second parameter does does not match builtin UTXO resource type"
            );
            ensure!(
                ty.results().len() == 0,
                "`[method]token.detach` should not return anything"
            );
            Ok(idx)
        }

        let engine = self.pre.engine();
        let component = self.pre.component();
        let instance_idx = component
            .get_export_index(None, name)
            .context("export not found")?;
        let Some(types::ComponentExtern {
            ty: types::ComponentItem::Resource(token_ty),
            ..
        }) = instance_ty.get_export(engine, "token")
        else {
            bail!("instance does not export `token` resource")
        };
        let types::ComponentExtern { ty: storage_ty, .. } = instance_ty
            .get_export(engine, "storage")
            .context("instance does export `storage`")?;
        let types::ComponentItem::Type(Type::Record(storage_ty)) = storage_ty else {
            bail!("`storage` export is not a record")
        };

        let get_storage =
            lookup_get_storage_export(component, instance_idx, token_ty, &storage_ty)?;
        let set_storage = set_storage(component, instance_idx, token_ty, &storage_ty)?;

        let utxo_ty = ResourceType::host::<Utxo<T::UtxoContext>>();
        let attach = attach(component, &instance_ty, instance_idx, token_ty, utxo_ty)?;
        let detach = detach(component, &instance_ty, instance_idx, token_ty, utxo_ty)?;
        Ok(TokenExport {
            instance_ty,
            instance_idx,
            storage: StorageExport {
                ty: storage_ty,
                get: get_storage,
                set: set_storage,
            },
            attach,
            detach,
        })
    }

    /// Get token export by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_token(&self, name: &str) -> wasmtime::Result<TokenExport> {
        let types::ComponentExtern { ty, .. } = self
            .ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentInstance(ty) = ty else {
            bail!("export is not an instance")
        };
        self.get_token_typed(name, ty)
    }

    /// Iterate over exported tokens along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn tokens(&self) -> impl Iterator<Item = (&str, wasmtime::Result<TokenExport>)> {
        let engine = self.pre.engine();
        self.ty
            .exports(engine)
            .filter_map(move |(name, ty)| match ty {
                types::ComponentExtern {
                    ty: types::ComponentItem::ComponentInstance(ty),
                    ..
                } if ty.get_export(engine, "token").is_some() => {
                    Some((name, self.get_token_typed(name, ty)))
                }
                _ => None,
            })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_utxo_main_typed(
        &self,
        utxo: &UtxoExport,
        name: &str,
        ty: types::ComponentFunc,
    ) -> wasmtime::Result<UtxoMainExport> {
        let idx = self
            .pre
            .component()
            .get_export_index(Some(&utxo.instance_idx), name)
            .context("export not found")?;

        let (Some(Type::Own(resource_ty)), None) = ({
            let mut result_tys = ty.results();
            (result_tys.next(), result_tys.next())
        }) else {
            bail!("function does not return a single resource value")
        };
        if resource_ty != utxo.resource_ty {
            bail!("function return value does not match UTXO resource type");
        }
        Ok(UtxoMainExport { ty, idx })
    }

    /// Get a `main fn` of an exported UTXO by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_utxo_main(&self, utxo: &UtxoExport, name: &str) -> wasmtime::Result<UtxoMainExport> {
        let types::ComponentExtern { ty, .. } = utxo
            .instance_ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentFunc(ty) = ty else {
            bail!("export is not a function")
        };
        self.get_utxo_main_typed(utxo, name, ty)
    }

    /// Iterate over exported UTXO `main fn`s along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn utxo_mains<'a>(
        &'a self,
        utxo: &'a UtxoExport,
    ) -> impl Iterator<Item = (&'a str, wasmtime::Result<UtxoMainExport>)> {
        utxo.instance_ty
            .exports(self.pre.engine())
            .filter_map(move |(name, ty)| match ty {
                types::ComponentExtern {
                    ty: types::ComponentItem::ComponentFunc(ty),
                    ..
                } if name.starts_with("[static]") => {
                    Some((name, self.get_utxo_main_typed(utxo, name, ty)))
                }
                _ => None,
            })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_utxo_method_typed(
        &self,
        utxo: &UtxoExport,
        name: &str,
        ty: types::ComponentFunc,
    ) -> wasmtime::Result<MethodExport> {
        let idx = self
            .pre
            .component()
            .get_export_index(Some(&utxo.instance_idx), name)
            .context("export not found")?;
        let Some((_, Type::Borrow(resource_ty))) = ty.params().next() else {
            bail!("function does not take borrowed resource type as first parameter");
        };
        if resource_ty != utxo.resource_ty {
            bail!("resource type does not match UTXO resource type");
        }
        Ok(MethodExport { ty, idx })
    }

    /// Get a method of an exported UTXO by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_utxo_method(&self, utxo: &UtxoExport, name: &str) -> wasmtime::Result<MethodExport> {
        let types::ComponentExtern { ty, .. } = utxo
            .instance_ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentFunc(ty) = ty else {
            bail!("export is not a function")
        };
        self.get_utxo_method_typed(utxo, name, ty)
    }

    /// Iterate over exported UTXO methods along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn utxo_methods<'a>(
        &'a self,
        utxo: &'a UtxoExport,
    ) -> impl Iterator<Item = (&'a str, wasmtime::Result<MethodExport>)> {
        utxo.instance_ty
            .exports(self.pre.engine())
            .filter_map(move |(name, ty)| match ty {
                types::ComponentExtern {
                    ty: types::ComponentItem::ComponentFunc(ty),
                    ..
                } if name.starts_with("[method]") => {
                    Some((name, self.get_utxo_method_typed(utxo, name, ty)))
                }
                _ => None,
            })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_token_function_typed(
        &self,
        token: &TokenExport,
        name: &str,
        ty: types::ComponentFunc,
    ) -> wasmtime::Result<TokenFunctionExport> {
        let idx = self
            .pre
            .component()
            .get_export_index(Some(&token.instance_idx), name)
            .context("export not found")?;
        Ok(TokenFunctionExport { ty, idx })
    }

    /// Get a `mint fn` or `burn fn` of an exported token by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_token_function(
        &self,
        token: &TokenExport,
        name: &str,
    ) -> wasmtime::Result<TokenFunctionExport> {
        let types::ComponentExtern { ty, .. } = token
            .instance_ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentFunc(ty) = ty else {
            bail!("export is not a function")
        };
        self.get_token_function_typed(token, name, ty)
    }

    /// Iterate over exported token `mint fn`s and `burn fn`s along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn token_functions<'a>(
        &'a self,
        token: &'a TokenExport,
    ) -> impl Iterator<Item = (&'a str, wasmtime::Result<TokenFunctionExport>)> {
        token
            .instance_ty
            .exports(self.pre.engine())
            .filter_map(move |(name, ty)| match ty {
                types::ComponentExtern {
                    ty: types::ComponentItem::ComponentFunc(ty),
                    ..
                } if name.starts_with("[static]") => {
                    Some((name, self.get_token_function_typed(token, name, ty)))
                }
                _ => None,
            })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_coordination_script_typed(
        &self,
        name: &str,
        ty: types::ComponentFunc,
    ) -> wasmtime::Result<CoordinationScriptExport> {
        let idx = self
            .pre
            .component()
            .get_export_index(None, name)
            .context("export not found")?;
        Ok(CoordinationScriptExport { ty, idx })
    }

    /// Get an exported coordination script by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_coordination_script(
        &self,
        name: &str,
    ) -> wasmtime::Result<CoordinationScriptExport> {
        let types::ComponentExtern { ty, .. } = self
            .ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentFunc(ty) = ty else {
            bail!("export is not a function")
        };
        self.get_coordination_script_typed(name, ty)
    }

    /// Iterate over exported coordination scripts along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn coordination_scripts(
        &self,
    ) -> impl Iterator<Item = (&str, wasmtime::Result<CoordinationScriptExport>)> {
        let engine = self.pre.engine();
        self.ty.exports(engine).filter_map(|(name, ty)| match ty {
            types::ComponentExtern {
                ty: types::ComponentItem::ComponentFunc(ty),
                ..
            } => Some((name, self.get_coordination_script_typed(name, ty))),
            _ => None,
        })
    }

    /// Instantiate the contract
    #[instrument(level = "trace", skip_all)]
    pub async fn instantiate(
        &self,
        store: impl AsContextMut<Data = T>,
    ) -> wasmtime::Result<ContractInstance> {
        debug!("instantiating component");
        let instance = self
            .pre
            .instantiate_async(store)
            .await
            .context("failed to instantiate component")?;
        Ok(ContractInstance {
            component: self.pre.component().clone(),
            instance,
        })
    }
}

/// Single instantiation of a [Contract]
#[derive(Debug, Clone)]
pub struct ContractInstance {
    component: Component,
    instance: Instance,
}

impl ContractInstance {
    #[must_use]
    pub fn component(&self) -> &Component {
        &self.component
    }

    #[must_use]
    pub fn instance(&self) -> Instance {
        self.instance
    }

    #[instrument(level = "trace", skip_all)]
    async fn construct_utxo<T>(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        instance_idx: ComponentExportIndex,
        name: impl ExportLookup,
        params: impl AsRef<[Val]>,
        cx: T,
    ) -> wasmtime::Result<Utxo<T>> {
        let f = self
            .instance
            .get_func(store.as_context_mut(), name)
            .context("constructor function export not found")?;
        debug!("calling constructor function");
        let mut results = [Val::Bool(false)];
        f.call_async(store, params.as_ref(), &mut results)
            .await
            .context("failed to call constructor function")?;
        let [Val::Resource(resource)] = results else {
            bail!("invalid return value")
        };
        Ok(Utxo {
            component: self.component.clone(),
            instance: self.instance,
            instance_idx,
            resource,
            cx,
        })
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_utxo_main<T>(
        &self,
        store: impl AsContextMut<Data: Send>,
        UtxoExport { instance_idx, .. }: &UtxoExport,
        UtxoMainExport { idx, .. }: &UtxoMainExport,
        cx: T,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo<T>> {
        self.construct_utxo(store, *instance_idx, idx, params, cx)
            .await
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn load_utxo<T>(
        &self,
        store: impl AsContextMut<Data: Send>,
        UtxoExport { instance_idx, .. }: &UtxoExport,
        StorageExport { set, .. }: &StorageExport,
        cx: T,
        fields: impl Into<Vec<(String, Val)>>,
    ) -> wasmtime::Result<Utxo<T>> {
        self.construct_utxo(store, *instance_idx, set, [Val::Record(fields.into())], cx)
            .await
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_token_function(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        TokenFunctionExport { idx, .. }: &TokenFunctionExport,
        params: impl AsRef<[Val]>,
        mut results: impl AsMut<[Val]>,
    ) -> wasmtime::Result<()> {
        let f = self
            .instance
            .get_func(&mut store, idx)
            .context("method export not found")?;
        f.call_async(store, params.as_ref(), results.as_mut())
            .await
            .context("failed to call method")?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn load_token(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        StorageExport { set, .. }: &StorageExport,
        fields: impl Into<Vec<(String, Val)>>,
    ) -> wasmtime::Result<Token> {
        let f = self
            .instance
            .get_func(store.as_context_mut(), set)
            .context("`set-storage` export not found")?;
        debug!("calling `set-storage`");
        let mut results = [Val::Bool(false)];
        f.call_async(store, &[Val::Record(fields.into())], &mut results)
            .await
            .context("failed to call `set-storage`")?;
        let [Val::Resource(resource)] = results else {
            bail!("invalid return value")
        };
        Ok(Token {
            instance: self.instance,
            resource,
        })
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_coordination_script(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        CoordinationScriptExport { idx, .. }: &CoordinationScriptExport,
        params: impl AsRef<[Val]>,
        mut results: impl AsMut<[Val]>,
    ) -> wasmtime::Result<()> {
        let f = self
            .instance
            .get_func(store.as_context_mut(), idx)
            .context("coordination script export not found")?;
        debug!("calling coordination script");
        f.call_async(store, params.as_ref(), results.as_mut())
            .await
            .context("failed to call coordination script")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct StorageExport {
    ty: types::Record,
    get: ComponentExportIndex,
    set: ComponentExportIndex,
}

impl StorageExport {
    #[must_use]
    pub fn ty(&self) -> &types::Record {
        &self.ty
    }
}

#[derive(Clone, Debug)]
pub struct UtxoExport {
    resource_ty: ResourceType,
    instance_ty: types::ComponentInstance,
    instance_idx: ComponentExportIndex,
    storage: Option<StorageExport>,
}

impl UtxoExport {
    #[must_use]
    pub fn storage(&self) -> Option<&StorageExport> {
        self.storage.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct TokenExport {
    instance_ty: types::ComponentInstance,
    instance_idx: ComponentExportIndex,
    storage: StorageExport,
    attach: ComponentExportIndex,
    detach: ComponentExportIndex,
}

impl TokenExport {
    #[must_use]
    pub fn storage(&self) -> &StorageExport {
        &self.storage
    }
}

#[derive(Clone, Debug)]
pub struct UtxoMainExport {
    ty: types::ComponentFunc,
    idx: ComponentExportIndex,
}

impl UtxoMainExport {
    #[must_use]
    pub fn ty(&self) -> &types::ComponentFunc {
        &self.ty
    }
}

#[derive(Clone, Debug)]
pub struct TokenFunctionExport {
    ty: types::ComponentFunc,
    idx: ComponentExportIndex,
}

impl TokenFunctionExport {
    #[must_use]
    pub fn ty(&self) -> &types::ComponentFunc {
        &self.ty
    }
}

#[derive(Clone, Debug)]
pub struct MethodExport {
    ty: types::ComponentFunc,
    idx: ComponentExportIndex,
}

impl MethodExport {
    #[must_use]
    pub fn ty(&self) -> &types::ComponentFunc {
        &self.ty
    }
}

#[derive(Clone, Debug)]
pub struct CoordinationScriptExport {
    ty: types::ComponentFunc,
    idx: ComponentExportIndex,
}

impl CoordinationScriptExport {
    #[must_use]
    pub fn ty(&self) -> &types::ComponentFunc {
        &self.ty
    }
}

#[derive(Debug, Clone)]
pub struct Utxo<T> {
    component: Component,
    instance: Instance,
    instance_idx: ComponentExportIndex,
    resource: ResourceAny,
    cx: T,
}

impl<T> Utxo<T> {
    #[must_use]
    pub fn instance(&self) -> Instance {
        self.instance
    }

    #[must_use]
    pub fn resource(&self) -> ResourceAny {
        self.resource
    }

    #[must_use]
    pub fn context(&self) -> &T {
        &self.cx
    }

    pub fn storage(&self, export: &StorageExport) -> Storage<'_> {
        Storage {
            instance: &self.instance,
            resource: self.resource,
            get: export.get,
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_method(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        MethodExport { idx, .. }: &MethodExport,
        params: impl AsRef<[Val]>,
        mut results: impl AsMut<[Val]>,
    ) -> wasmtime::Result<()> {
        let f = self
            .instance
            .get_func(&mut store, idx)
            .context("method export not found")?;
        f.call_async(store, params.as_ref(), results.as_mut())
            .await
            .context("failed to call method")?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn drop(self, mut store: impl AsContextMut<Data: Send>) -> wasmtime::Result<()> {
        self.resource.resource_drop_async(&mut store).await?;
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    instance: Instance,
    resource: ResourceAny,
}

impl Token {
    #[must_use]
    pub fn instance(&self) -> Instance {
        self.instance
    }

    #[must_use]
    pub fn resource(&self) -> ResourceAny {
        self.resource
    }

    pub fn storage(&self, export: &StorageExport) -> Storage<'_> {
        Storage {
            instance: &self.instance,
            resource: self.resource,
            get: export.get,
        }
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_attach(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        TokenExport { attach, .. }: &TokenExport,
        utxo: ResourceAny,
    ) -> wasmtime::Result<()> {
        let f = self
            .instance
            .get_func(&mut store, attach)
            .context("`attach` export not found")?;
        f.call_async(
            store,
            &[Val::Resource(self.resource), Val::Resource(utxo)],
            &mut [],
        )
        .await
        .context("failed to call `attach`")?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_detach(
        &self,
        mut store: impl AsContextMut<Data: Send>,
        TokenExport { detach, .. }: &TokenExport,
        utxo: ResourceAny,
    ) -> wasmtime::Result<()> {
        let f = self
            .instance
            .get_func(&mut store, detach)
            .context("`detach` export not found")?;
        f.call_async(
            store,
            &[Val::Resource(self.resource), Val::Resource(utxo)],
            &mut [],
        )
        .await
        .context("failed to call `detach`")?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn drop(self, mut store: impl AsContextMut<Data: Send>) -> wasmtime::Result<()> {
        self.resource.resource_drop_async(&mut store).await?;
        Ok(())
    }
}

pub struct Storage<'a> {
    instance: &'a Instance,
    resource: ResourceAny,
    get: ComponentExportIndex,
}

impl Storage<'_> {
    #[instrument(level = "trace", skip_all)]
    pub async fn call_get(
        &self,
        mut store: impl AsContextMut<Data: Send>,
    ) -> wasmtime::Result<Vec<(String, Val)>> {
        let f = self
            .instance
            .get_func(&mut store, self.get)
            .context("`get-storage` export not found")?;
        let mut results = [Val::Bool(false); 1];
        f.call_async(&mut store, &[Val::Resource(self.resource)], &mut results)
            .await
            .context("failed to call `get-storage`")?;
        let [Val::Record(vs)] = results else {
            bail!("invalid return value")
        };
        Ok(vs)
    }
}
