use core::ops::Deref;

use std::sync::Arc;

use tracing::{debug, instrument};
use wasmtime::component::{
    Component, ComponentExportIndex, ExportLookup, Func, HasSelf, Instance, InstancePre, Linker,
    LinkerInstance, ResourceAny, ResourceType, Type, Val, types,
};
use wasmtime::error::Context as _;
use wasmtime::{AsContextMut, Engine, Store, bail};

pub mod bindings {
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
            "starstream:std/builtin.implements-method": tracing | trappable,
            default: tracing,
        }
    });
}

pub trait Host:
    bindings::starstream::std::builtin::Host
    + bindings::starstream::std::builtin::HostUtxo
    + bindings::starstream::std::cardano::Host
    + EventHandler
    + 'static
{
}

impl<T> Host for T where
    T: bindings::starstream::std::builtin::Host
        + bindings::starstream::std::builtin::HostUtxo
        + bindings::starstream::std::cardano::Host
        + EventHandler
        + 'static
{
}

/// ABI event handler
pub trait EventHandler {
    fn emit_event(&mut self, instance: &str, name: &str, params: &[Val]);
}

fn componentize(wasm: impl AsRef<[u8]>) -> anyhow::Result<Vec<u8>> {
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
pub fn link_abi_event_function<T: EventHandler>(
    linker: &mut LinkerInstance<T>,
    _ty: types::ComponentFunc,
    instance: &str,
    name: &str,
) -> wasmtime::Result<()> {
    debug!(instance, name, "linking ABI instance event function");
    let instance = Arc::<str>::from(instance);
    let name = Arc::<str>::from(name);
    linker.func_new(
        &Arc::clone(&name),
        move |mut store, _ty, params, _results| {
            store.data_mut().emit_event(&instance, &name, params);
            Ok(())
        },
    )
}

/// Link dynamic imported instance in a [`LinkerInstance`].
#[instrument(level = "trace", skip_all)]
pub fn link_instance<T: EventHandler>(
    engine: &Engine,
    linker: &mut LinkerInstance<T>,
    ty: &types::ComponentInstance,
    instance: &str,
) -> wasmtime::Result<()> {
    if let Some(_utxo) = ty.get_export(engine, "utxo") {
        bail!("coordination scripts not supported yet")
    }
    for (name, types::ComponentExtern { ty, .. }) in ty.exports(engine) {
        debug!(name, "linking ABI instance item");
        match ty {
            types::ComponentItem::ComponentFunc(ty) => {
                link_abi_event_function(linker, ty, instance, name)?;
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

/// Link dynamic imports of the contract
#[instrument(level = "trace", skip_all)]
pub fn link_dynamic_imports<T: EventHandler>(
    engine: &Engine,
    linker: &mut Linker<T>,
    ty: &types::Component,
) -> wasmtime::Result<()> {
    for (name, types::ComponentExtern { ty, .. }) in ty.imports(engine) {
        if let Some(("starstream:std", ..)) = name.split_once('/') {
            debug!(?name, "skipping builtin instance import");
            continue;
        }
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
                let mut linker = linker
                    .instance(name)
                    .with_context(|| format!("failed to instantiate `{name}` in the linker"))?;
                debug!(?name, "linking root instance");
                link_instance(engine, &mut linker, &ty, name)?;
            }
            types::ComponentItem::Type(..) => {}
            types::ComponentItem::Resource(..) => {
                bail!("root instance resource imports unsupported")
            }
        }
    }
    Ok(())
}

#[must_use]
pub fn new_wasmtime_config() -> wasmtime::Config {
    let mut config = wasmtime::Config::new();
    config.wasm_component_model(true);
    #[cfg(feature = "trace")]
    config.guest_debug(true);
    config
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

impl<T: 'static> Deref for Contract<T> {
    type Target = InstancePre<T>;

    fn deref(&self) -> &Self::Target {
        &self.pre
    }
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
        link_dynamic_imports(engine, &mut linker, &component.component_type())?;

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

        let engine = self.engine();
        let component = self.component();
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
            .get_export(self.engine(), name)
            .context("export not found")?;
        let types::ComponentItem::ComponentInstance(ty) = ty else {
            bail!("export is not an instance")
        };
        self.get_utxo_typed(name, ty)
    }

    /// Iterate over exported UTXOs along with their names
    #[instrument(level = "trace", skip_all)]
    pub fn utxos(&self) -> impl Iterator<Item = (&str, wasmtime::Result<UtxoExport>)> {
        let engine = self.engine();
        self.ty.exports(engine).filter_map(|(name, ty)| {
            let types::ComponentExtern {
                ty: types::ComponentItem::ComponentInstance(ty),
                ..
            } = ty
            else {
                return None;
            };
            Some((name, self.get_utxo_typed(name, ty)))
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
            .get_export(self.engine(), name)
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
            .exports(self.engine())
            .filter_map(move |(name, ty)| {
                let types::ComponentExtern {
                    ty: types::ComponentItem::ComponentFunc(ty),
                    ..
                } = ty
                else {
                    return None;
                };
                if !name.starts_with("[static]") {
                    return None;
                }
                Some((name, self.get_utxo_constructor_typed(utxo, name, ty)))
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
            .get_export(self.engine(), name)
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
            .exports(self.engine())
            .filter_map(move |(name, ty)| {
                let types::ComponentExtern {
                    ty: types::ComponentItem::ComponentFunc(ty),
                    ..
                } = ty
                else {
                    return None;
                };
                if !name.starts_with("[method]") {
                    return None;
                }
                Some((name, self.get_utxo_method_typed(utxo, name, ty)))
            })
    }

    #[instrument(level = "trace", skip_all)]
    fn get_coordination_script_typed(
        &self,
        name: &str,
        ty: types::ComponentFunc,
    ) -> wasmtime::Result<CoordinationScriptExport> {
        let idx = self
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
            .get_export(self.engine(), name)
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
        let engine = self.engine();
        self.ty.exports(engine).filter_map(|(name, ty)| {
            let types::ComponentExtern {
                ty: types::ComponentItem::ComponentFunc(ty),
                ..
            } = ty
            else {
                return None;
            };
            Some((name, self.get_coordination_script_typed(name, ty)))
        })
    }

    /// Instantiate the contract
    #[instrument(level = "trace", skip_all)]
    fn instantiate(&self, store: &mut Store<T>) -> wasmtime::Result<Instance> {
        debug!("instantiating component");
        self.pre
            .instantiate(store)
            .context("failed to instantiate component")
    }

    /// Same as [Self::instantiate], but async
    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    async fn instantiate_async(&self, store: &mut Store<T>) -> wasmtime::Result<Instance>
    where
        T: Send,
    {
        #[cfg(feature = "trace")]
        {
            use core::marker::PhantomData;

            use tracing::{error, trace};
            use wasmtime::{DebugEvent, StoreContextMut};

            struct DebugHandler<T>(PhantomData<fn() -> T>);

            impl<T> Clone for DebugHandler<T> {
                fn clone(&self) -> Self {
                    Self(PhantomData)
                }
            }

            impl<T: 'static + Send> wasmtime::DebugHandler for DebugHandler<T> {
                type Data = T;

                async fn handle(
                    &self,
                    mut store: StoreContextMut<'_, Self::Data>,
                    event: DebugEvent<'_>,
                ) {
                    match event {
                        DebugEvent::Breakpoint => {
                            let frames: Vec<_> = store.debug_exit_frames().collect();
                            for frame in frames {
                                match frame.wasm_function_index_and_pc(&mut store) {
                                    Ok(Some((f, pc))) => debug!(?f, ?pc, "frame"),
                                    Ok(None) => trace!("skip trampoline frame"),
                                    Err(err) => error!(?err),
                                }
                            }
                        }
                        DebugEvent::HostcallError(..)
                        | DebugEvent::Exception(..)
                        | DebugEvent::Trap(..)
                        | DebugEvent::EpochYield => {}
                    }
                }
            }
            store.set_debug_handler(DebugHandler::<T>(PhantomData));
            {
                let Some(mut bp) = store.edit_breakpoints() else {
                    bail!("invalid engine config")
                };
                bp.single_step(true)
                    .context("failed to enable single-step debugging")?;
            }
        }
        debug!("instantiating component");
        self.pre
            .instantiate_async(store)
            .await
            .context("failed to instantiate component")
    }

    #[instrument(level = "trace", skip_all)]
    fn construct_utxo(
        &self,
        store: &mut Store<T>,
        name: impl ExportLookup,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo> {
        let instance = self.instantiate(store)?;
        let f = instance
            .get_func(&mut *store, name)
            .context("failed to lookup constructor function export")?;
        debug!("calling constructor function");
        let mut results = [Val::Bool(false)];
        f.call(store, params.as_ref(), &mut results)
            .context("failed to call constructor function")?;
        let [Val::Resource(resource)] = results else {
            bail!("invalid return value")
        };
        Ok(Utxo { instance, resource })
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    async fn construct_utxo_async(
        &self,
        store: &mut Store<T>,
        name: impl ExportLookup,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo>
    where
        T: Send,
    {
        let instance = self.instantiate_async(store).await?;
        let f = instance
            .get_func(&mut *store, name)
            .context("failed to lookup constructor function export")?;
        debug!("calling constructor function");
        let mut results = [Val::Bool(false)];
        f.call_async(&mut *store, params.as_ref(), &mut results)
            .await
            .context("failed to call constructor function")?;
        let [Val::Resource(resource)] = results else {
            bail!("invalid return value")
        };
        Ok(Utxo { instance, resource })
    }

    #[instrument(level = "trace", skip_all)]
    pub fn create_utxo(
        &self,
        store: &mut Store<T>,
        ConstructorExport { idx, .. }: &ConstructorExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo> {
        self.construct_utxo(store, idx, params)
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    pub async fn create_utxo_async(
        &self,
        store: &mut Store<T>,
        ConstructorExport { idx, .. }: &ConstructorExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Utxo>
    where
        T: Send,
    {
        self.construct_utxo_async(store, idx, params).await
    }

    #[instrument(level = "trace", skip_all)]
    pub fn load_utxo(
        &self,
        store: &mut Store<T>,
        UtxoStorageExport { set, .. }: &UtxoStorageExport,
        fields: impl Into<Vec<(String, Val)>>,
    ) -> wasmtime::Result<Utxo> {
        self.construct_utxo(store, set, [Val::Record(fields.into())])
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    pub async fn load_utxo_async(
        &self,
        store: &mut Store<T>,
        UtxoStorageExport { set, .. }: &UtxoStorageExport,
        fields: impl Into<Vec<(String, Val)>>,
    ) -> wasmtime::Result<Utxo>
    where
        T: Send,
    {
        self.construct_utxo_async(store, set, [Val::Record(fields.into())])
            .await
    }

    #[instrument(level = "trace", skip_all)]
    pub fn call_coordination_script(
        &self,
        mut store: &mut Store<T>,
        CoordinationScriptExport { idx, .. }: &CoordinationScriptExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<()> {
        let instance = self.instantiate(store)?;
        let f = instance
            .get_func(&mut store, idx)
            .context("failed to lookup coordination script export")?;
        debug!("calling coordination script");
        f.call(&mut store, params.as_ref(), &mut [])
            .context("failed to call coordination script")?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    pub async fn call_coordination_script_async(
        &self,
        mut store: &mut Store<T>,
        CoordinationScriptExport { idx, .. }: &CoordinationScriptExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<()>
    where
        T: Send,
    {
        let instance = self.instantiate_async(store).await?;
        let f = instance
            .get_func(&mut store, idx)
            .context("failed to lookup coordination script export")?;
        debug!("calling coordination script");
        f.call_async(&mut store, params.as_ref(), &mut [])
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

pub struct Utxo {
    instance: Instance,
    resource: ResourceAny,
}

impl Utxo {
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
    pub fn call(
        &self,
        mut store: impl AsContextMut,
        export: &MethodExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Box<[Val]>> {
        let f = self.get_function_export(&mut store, export.idx)?;
        let mut results = vec![Val::Bool(false); export.ty.results().len()];
        f.call(&mut store, params.as_ref(), &mut results)
            .context("failed to call method")?;
        Ok(results.into_boxed_slice())
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    pub async fn call_async<T: Send>(
        &self,
        mut store: impl AsContextMut<Data = T>,
        export: &MethodExport,
        params: impl AsRef<[Val]>,
    ) -> wasmtime::Result<Box<[Val]>> {
        let f = self.get_function_export(&mut store, export.idx)?;
        let mut results = vec![Val::Bool(false); export.ty.results().len()];
        f.call_async(&mut store, params.as_ref(), &mut results)
            .await
            .context("failed to call method")?;
        Ok(results.into_boxed_slice())
    }

    #[instrument(level = "trace", skip_all)]
    pub fn drop(self, mut store: impl AsContextMut) -> wasmtime::Result<()> {
        self.resource.resource_drop(&mut store)?;
        Ok(())
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    pub async fn drop_async<T: Send>(
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
    pub fn get(&self, mut store: impl AsContextMut) -> wasmtime::Result<Vec<(String, Val)>> {
        let f = self.utxo.get_function_export(&mut store, self.get)?;
        let mut results = [Val::Bool(false); 1];
        f.call(
            &mut store,
            &[Val::Resource(self.utxo.resource)],
            &mut results,
        )
        .context("failed to call function")?;
        let [Val::Record(vs)] = results else {
            bail!("invalid return value")
        };
        Ok(vs)
    }

    #[instrument(level = "trace", skip_all)]
    #[cfg(feature = "async")]
    pub async fn get_async<T: Send>(
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
