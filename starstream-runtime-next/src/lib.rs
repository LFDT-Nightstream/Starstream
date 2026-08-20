use std::sync::Arc;

use tracing::{debug, instrument};
use wasmtime::component::{
    Component, ComponentExportIndex, ExportLookup, Func, HasSelf, Instance, InstancePre, Linker,
    LinkerInstance, Resource, ResourceAny, ResourceTable, ResourceType, Type, Val, types,
};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut, Engine, StoreContextMut, bail, ensure};

pub mod bindings {
    // NOTE: `starstream:std/utxo-context` bindings are hand-written
    wasmtime::component::bindgen!({
        path: "../starstream-to-wasm/wit",
        inline: "
            package starstream:host;

            world host {
                import starstream:std/builtin;
                import starstream:std/cardano;
            }
        ",
        with: {
            "starstream:std/builtin.utxo": crate::Utxo,
        },
        imports: {
            default: tracing | trappable,
        }
    });
}

pub trait Host:
    bindings::starstream::std::builtin::Host
    + bindings::starstream::std::cardano::Host
    + Send
    + Sized
    + 'static
{
    type UtxoContext: Send;

    fn table(&mut self) -> &mut ResourceTable;

    fn contract(store: StoreContextMut<Self>) -> Contract<Self>;

    fn new_utxo_context(store: StoreContextMut<Self>) -> Self::UtxoContext;

    fn output(
        store: StoreContextMut<Self>,
        utxo: Utxo,
        cx: Resource<Self::UtxoContext>,
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

    fn emit_event(
        store: StoreContextMut<Self>,
        abi_name: &str,
        name: &str,
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

/// Link ABI event [`types::ComponentFunc`] in a [`LinkerInstance`]
#[instrument(level = "trace", skip_all)]
fn link_event_function<T: Host>(
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    abi_name: &str,
    name: &str,
) -> wasmtime::Result<()> {
    debug!(abi_name, name, "linking ABI instance event function");
    let abi_name = Arc::<str>::from(abi_name);
    let name = Arc::<str>::from(name);
    ensure!(ty.results().len() == 0);
    linker.func_new(&Arc::clone(&name), move |store, _ty, params, _results| {
        T::emit_event(store, &abi_name, &name, params)
    })
}

/// Link dynamic imported ABI instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
fn link_event_instance_import<T: Host>(
    engine: &Engine,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
    abi_name: &str,
) -> wasmtime::Result<()> {
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(engine) {
        debug!(name, "linking ABI instance item");
        match ty {
            types::ComponentItem::ComponentFunc(ty) => {
                link_event_function(linker, ty, abi_name, name)?;
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("ABI instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => bail!("ABI instance module imports unsupported"),
            types::ComponentItem::Component(..) => {
                bail!("ABI instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("ABI instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) => {
                bail!("ABI instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

/// Link UTXO [`types::ComponentFunc`] in a [`LinkerInstance`]
#[instrument(level = "trace", skip(component, contract, linker, ty, instance))]
fn link_utxo_function<T: Host>(
    contract: Option<Contract<T>>,
    component: &Component,
    linker: &mut LinkerInstance<T>,
    ty: types::ComponentFunc,
    instance: &ComponentExportIndex,
    name: &str,
) -> wasmtime::Result<()> {
    let idx = component
        .get_export_index(Some(instance), name)
        .with_context(|| format!("`{name}` export was not found"))?;
    match name.split_once(']') {
        Some(("[static", ..)) => {
            let (Some(Type::Own(..)), None) = ({
                let mut result_tys = ty.results();
                (result_tys.next(), result_tys.next())
            }) else {
                bail!("function does not return a single resource value")
            };
            linker.func_new_async(name, move |mut store, _ty, params, results| {
                let contract = contract.clone();
                Box::new(async move {
                    ensure!(results.len() == 1);

                    let contract = contract.unwrap_or_else(|| T::contract(store.as_context_mut()));
                    let cx = T::new_utxo_context(store.as_context_mut());
                    let cx = store.data_mut().table().push(cx)?;
                    let cx_rep = cx.rep();
                    let cx = cx.try_into_resource_any(&mut store)?;

                    let instance = contract.instantiate(&mut store).await?;

                    let params = {
                        let mut ps = Vec::with_capacity(params.len().saturating_add(1));
                        ps.push(Val::Resource(cx));
                        for p in params {
                            ps.push(p.clone());
                        }
                        ps
                    };
                    let utxo = instance.construct_utxo(&mut store, idx, params).await?;
                    {
                        let utxo = store.data_mut().table().push(utxo)?;
                        let utxo = utxo.try_into_resource_any(store.as_context_mut())?;
                        results[0] = Val::Resource(utxo);
                    }
                    T::output(store, utxo, Resource::new_borrow(cx_rep))
                })
            })
        }
        Some(("[method", ..)) => {
            let Some((_, Type::Borrow(..))) = ty.params().next() else {
                bail!("function does not take borrowed resource type as first parameter");
            };
            linker.func_new_async(name, move |mut store, _ty, params, results| {
                Box::new(async move {
                    let Some(Val::Resource(utxo)) = params.first() else {
                        bail!("first parameter is not a resource")
                    };
                    let utxo = utxo.try_into_resource::<Utxo>(&mut store)?;
                    let utxo = store.data_mut().table().get(&utxo).copied()?;
                    let f = utxo.get_function_export(&mut store, idx)?;
                    let params = {
                        let mut ps = Vec::with_capacity(params.len());
                        ps.push(Val::Resource(utxo.resource()));
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
        _ => bail!("unexpected UTXO instance function import `{name}`"),
    }
}

/// Link dynamic imported UTXO instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
fn link_utxo_instance_import<T: Host>(
    contract: Option<Contract<T>>,
    component: &Component,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
    name: &str,
) -> wasmtime::Result<()> {
    let idx = component
        .get_export_index(None, name)
        .with_context(|| format!("`{name}` export was not found"))?;
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(component.engine()) {
        debug!(name, "linking UTXO instance item");
        match ty {
            types::ComponentItem::ComponentFunc(ty) => {
                link_utxo_function(contract.clone(), component, linker, ty, &idx, name)?;
            }
            types::ComponentItem::CoreFunc(..) => {
                bail!("UTXO instance core function imports unsupported")
            }
            types::ComponentItem::Module(..) => bail!("UTXO instance module imports unsupported"),
            types::ComponentItem::Component(..) => {
                bail!("UTXO instance component imports unsupported")
            }
            types::ComponentItem::ComponentInstance(..) => {
                bail!("UTXO instance component instance imports unsupported")
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) if name == "utxo" => {
                linker.resource("utxo", ResourceType::host::<Utxo>(), |mut store, rep| {
                    store
                        .data_mut()
                        .table()
                        .delete::<Utxo>(Resource::new_own(rep))?;
                    Ok(())
                })?;
            }
            types::ComponentItem::Resource(..) => {
                bail!("UTXO instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

/// Link dynamic imported instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip(component, linker, ty))]
fn link_instance_import<T: Host>(
    component: &Component,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
    instance: &str,
) -> wasmtime::Result<()> {
    debug_assert!(!instance.starts_with("starstream:std"));

    let engine = component.engine();
    match (
        instance.split_once('/'),
        ty.get_export(engine, "utxo"),
        ty.get_export(engine, "token"),
    ) {
        (Some(("starstream:utxo", _name)), ..) => {
            // TODO: Use external-id to lookup the contract
            bail!("cross-contract UTXO imports unsupported")
        }

        (
            Some(("starstream:self", name)),
            Some(types::ComponentExtern {
                ty: types::ComponentItem::Resource(..),
                ..
            }),
            None,
        ) => link_utxo_instance_import(None, component, linker, ty, name),

        (
            Some(("starstream:self", ..)),
            None,
            Some(types::ComponentExtern {
                ty: types::ComponentItem::Resource(..),
                ..
            }),
        ) => bail!("token imports unsupported"),

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
        ) => bail!("both `utxo` and `token` resources exported by instance `{instance}` import"),

        (Some(("starstream:self", ..)), ..) => {
            bail!("failed to classify `starstream:self` instance import")
        }

        (Some(("starstream:events", name)), ..) => {
            link_event_instance_import(engine, linker, ty, name)
        }

        (Some(("starstream:effects", ..)), ..) => {
            bail!("effect imports unsupported")
        }

        (Some(("starstream:contract", "dynamic-utxo")), ..) => {
            bail!("dynamic UTXO imports unsupported")
        }

        (Some(("starstream:contract", "scripts")), ..) => {
            bail!("coordination script imports unsupported")
        }

        _ => bail!("unexpected instance import `{instance}`"),
    }
}

/// Link dynamic imports of the contract
#[instrument(level = "trace", skip_all)]
fn link_dynamic_imports<T: Host>(
    component: &Component,
    linker: &mut Linker<T>,
) -> wasmtime::Result<()> {
    for (name, types::ComponentExtern { ty, .. }) in
        component.component_type().imports(component.engine())
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
                link_instance_import(component, &mut linker, &ty, name)?;
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

fn link_utxo_context<T: Host>(linker: &mut Linker<T>) -> wasmtime::Result<()> {
    let mut linker = linker
        .instance("starstream:std/utxo-context")
        .context("failed to instantiate `utxo-context` in the linker")?;

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

impl<T: Host> Contract<T> {
    /// Compile and pre-instantiate a Starstream [Contract]
    #[instrument(level = "trace", skip_all)]
    pub fn new(engine: &Engine, wasm: impl AsRef<[u8]>) -> wasmtime::Result<Self> {
        let wasm = wasm.as_ref();

        debug!("loading component");
        let component = load_component(engine, wasm)?;

        let mut linker = Linker::new(engine);

        debug!("linking component imports");
        bindings::Host_::add_to_linker::<_, HasSelf<_>>(&mut linker, |cx| cx)
            .context("failed to link builtins")?;
        link_utxo_context(&mut linker).context("failed to link `utxo-context`")?;
        link_dynamic_imports(&component, &mut linker)?;

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
        fn get_storage(
            component: &Component,
            instance: &ComponentExportIndex,
            utxo_ty: ResourceType,
            storage_ty: &types::Record,
        ) -> wasmtime::Result<ComponentExportIndex> {
            let (ty, idx) = component
                .get_export(Some(instance), "get-storage")
                .context("`get-storage` export not found")?;
            let types::ComponentItem::ComponentFunc(ty) = ty else {
                bail!("`get-storage` export is not a function")
            };
            let mut params = ty.params();
            let (Some((_, Type::Borrow(resource_ty))), None) = (params.next(), params.next())
            else {
                bail!("`get-storage` does not take borrowed resource type as the only parameter");
            };
            if resource_ty != utxo_ty {
                bail!("`get-storage` resource type does not match UTXO resource type");
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

        fn set_storage(
            component: &Component,
            instance: &ComponentExportIndex,
            utxo_ty: ResourceType,
            storage_ty: &types::Record,
        ) -> wasmtime::Result<ComponentExportIndex> {
            let (ty, idx) = component
                .get_export(Some(instance), "set-storage")
                .context("`set-storage` export not found")?;
            let types::ComponentItem::ComponentFunc(ty) = ty else {
                bail!("`set-storage` export is not a function")
            };
            let mut params = ty.params();
            let (Some((_, Type::Record(record_ty))), None) = (params.next(), params.next()) else {
                bail!("`set-storage` does not take a storage record as the only parameter");
            };
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
                let get = get_storage(component, &instance_idx, utxo_ty, &storage_ty)?;
                let set = set_storage(component, &instance_idx, utxo_ty, &storage_ty)?;
                Ok(UtxoStorageExport {
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
        self.ty.exports(engine).filter_map(|(name, ty)| match ty {
            types::ComponentExtern {
                ty: types::ComponentItem::ComponentInstance(ty),
                ..
            } => Some((name, self.get_utxo_typed(name, ty))),
            _ => None,
        })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_utxo_constructor_typed(
        &self,
        utxo: &UtxoExport,
        name: &str,
        ty: types::ComponentFunc,
    ) -> wasmtime::Result<ConstructorExport> {
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
        Ok(ConstructorExport { ty, idx })
    }

    /// Get a constructor of an exported UTXO by name
    #[instrument(level = "trace", skip_all)]
    pub fn get_utxo_constructor(
        &self,
        utxo: &UtxoExport,
        name: &str,
    ) -> wasmtime::Result<ConstructorExport> {
        let types::ComponentExtern { ty, .. } = utxo
            .instance_ty
            .get_export(self.pre.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentFunc(ty) = ty else {
            bail!("export is not a function")
        };
        self.get_utxo_constructor_typed(utxo, name, ty)
    }

    /// Iterate over exported UTXO constructors along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn utxo_constructors<'a>(
        &'a self,
        utxo: &'a UtxoExport,
    ) -> impl Iterator<Item = (&'a str, wasmtime::Result<ConstructorExport>)> {
        utxo.instance_ty
            .exports(self.pre.engine())
            .filter_map(move |(name, ty)| match ty {
                types::ComponentExtern {
                    ty: types::ComponentItem::ComponentFunc(ty),
                    ..
                } if name.starts_with("[static]") => {
                    Some((name, self.get_utxo_constructor_typed(utxo, name, ty)))
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
    ) -> wasmtime::Result<ContractInstance>
    where
        T: Send,
    {
        debug!("instantiating component");
        let instance = self
            .pre
            .instantiate_async(store)
            .await
            .context("failed to instantiate component")?;
        Ok(ContractInstance(instance))
    }
}

/// Single instantiation of a [Contract]
#[derive(Debug, Copy, Clone)]
pub struct ContractInstance(Instance);

impl ContractInstance {
    #[must_use]
    pub fn instance(&self) -> Instance {
        self.0
    }

    #[instrument(level = "trace", skip_all)]
    async fn construct_utxo<T>(
        &self,
        mut store: impl AsContextMut<Data = T>,
        name: impl ExportLookup,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo>
    where
        T: Send + 'static,
    {
        let f = self
            .0
            .get_func(store.as_context_mut(), name)
            .context("failed to lookup constructor function export")?;
        debug!("calling constructor function");
        let mut results = [Val::Bool(false)];
        f.call_async(store, params.as_ref(), &mut results)
            .await
            .context("failed to call constructor function")?;
        let [Val::Resource(resource)] = results else {
            bail!("invalid return value")
        };
        Ok(Utxo {
            instance: self.0,
            resource,
        })
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn create_utxo<T>(
        &self,
        store: impl AsContextMut<Data = T>,
        ConstructorExport { idx, .. }: &ConstructorExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo>
    where
        T: Send + 'static,
    {
        self.construct_utxo(store, idx, params).await
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn load_utxo<T>(
        &self,
        store: impl AsContextMut<Data = T>,
        UtxoStorageExport { set, .. }: &UtxoStorageExport,
        fields: impl Into<Vec<(String, Val)>>,
    ) -> wasmtime::Result<Utxo>
    where
        T: Send + 'static,
    {
        self.construct_utxo(store, set, [Val::Record(fields.into())])
            .await
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call_coordination_script<T>(
        &self,
        mut store: impl AsContextMut<Data = T>,
        CoordinationScriptExport { idx, .. }: &CoordinationScriptExport,
        params: impl AsRef<[Val]>,
        mut results: impl AsMut<[Val]>,
    ) -> wasmtime::Result<()>
    where
        T: Send + 'static,
    {
        let f = self
            .0
            .get_func(store.as_context_mut(), idx)
            .context("failed to lookup coordination script export")?;
        debug!("calling coordination script");
        f.call_async(store, params.as_ref(), results.as_mut())
            .await
            .context("failed to call coordination script")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct UtxoStorageExport {
    ty: types::Record,
    get: ComponentExportIndex,
    set: ComponentExportIndex,
}

impl UtxoStorageExport {
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
    storage: Option<UtxoStorageExport>,
}

impl UtxoExport {
    #[must_use]
    pub fn storage(&self) -> Option<&UtxoStorageExport> {
        self.storage.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct ConstructorExport {
    ty: types::ComponentFunc,
    idx: ComponentExportIndex,
}

impl ConstructorExport {
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

#[derive(Debug, Copy, Clone)]
pub struct Utxo {
    instance: Instance,
    resource: ResourceAny,
}

impl Utxo {
    #[must_use]
    pub fn instance(&self) -> Instance {
        self.instance
    }

    #[must_use]
    pub fn resource(&self) -> ResourceAny {
        self.resource
    }

    pub fn storage(&self, export: &UtxoStorageExport) -> UtxoStorage<'_> {
        UtxoStorage {
            utxo: self,
            get: export.get,
        }
    }

    fn get_function_export(
        &self,
        store: impl AsContextMut,
        name: impl ExportLookup,
    ) -> wasmtime::Result<Func> {
        self.instance
            .get_func(store, name)
            .context("function export not found")
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn call<T: Send>(
        &self,
        mut store: impl AsContextMut<Data = T>,
        export: &MethodExport,
        params: impl AsRef<[Val]>,
        mut results: impl AsMut<[Val]>,
    ) -> wasmtime::Result<()> {
        let f = self.get_function_export(&mut store, export.idx)?;
        f.call_async(&mut store, params.as_ref(), results.as_mut())
            .await
            .context("failed to call method")?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    pub async fn drop<T: Send>(
        self,
        mut store: impl AsContextMut<Data = T>,
    ) -> wasmtime::Result<()> {
        self.resource.resource_drop_async(&mut store).await?;
        Ok(())
    }
}

pub struct UtxoStorage<'a> {
    utxo: &'a Utxo,
    get: ComponentExportIndex,
}

impl UtxoStorage<'_> {
    #[instrument(level = "trace", skip_all)]
    pub async fn get<T: Send>(
        &self,
        mut store: impl AsContextMut<Data = T>,
    ) -> wasmtime::Result<Vec<(String, Val)>> {
        let f = self.utxo.get_function_export(&mut store, self.get)?;
        let mut results = [Val::Bool(false); 1];
        f.call_async(
            &mut store,
            &[Val::Resource(self.utxo.resource)],
            &mut results,
        )
        .await
        .context("failed to call function")?;
        let [Val::Record(vs)] = results else {
            bail!("invalid return value")
        };
        Ok(vs)
    }
}
