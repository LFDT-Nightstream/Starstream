use bytes::{Bytes, BytesMut};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use core::pin::pin;
use futures::StreamExt;
use futures::TryStreamExt;
use starstream_ledger::encode::gen_ctx;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::{DuplexStream, ReadHalf, WriteHalf};
use tokio::join;
use tokio::sync::Mutex;
use tokio_util::codec::Encoder;
use tracing::debug;
use wasmtime::component::ResourceType;
use wrpc_runtime_wasmtime::RemoteResource;
use wrpc_runtime_wasmtime::ServeExt;
use wrpc_transport::Accept;
use wrpc_transport::Invoke;
use wrpc_transport::InvokeExt;
use wrpc_transport::frame::Oneshot;

use starstream_ledger::Chain;
use starstream_registry::Registry;
use wasmtime::{AsContextMut, Engine, Store};
use wrpc_runtime_wasmtime::{
    ValEncoder, collect_component_resource_exports, collect_component_resource_imports,
};

use crate::api::binding::bindings;

#[derive(Clone)]
pub struct Handler {
    // Arc instead of 'a
    // as we can't easily control lifetime logic for different transport systems we need to implement the API for
    chain: Arc<Chain>,
    registry: Arc<Registry>,
}

impl Handler {
    pub fn new(chain: Arc<Chain>, registry: Arc<Registry>) -> Self {
        Self { chain, registry }
    }

    /// Forward an invocation to the appropriate component
    pub(crate) async fn call(
        &self,
        contract_hash: String,
        function: String,
        params: Bytes,
    ) -> anyhow::Result<Vec<u8>> {
        debug!(contract_hash, function, "forwarding invocation");

        let (oneshot_clt, oneshot_srv) = Oneshot::duplex(1024);
        let srv: Arc<
            wrpc_transport::frame::Server<(), ReadHalf<DuplexStream>, WriteHalf<DuplexStream>>,
        > = Arc::new(wrpc_transport::frame::Server::default());

        // Look up the component handler
        let utxo = self
            .chain
            .get_utxo(contract_hash.clone(), 0)
            .await
            .map_err(|_| anyhow::anyhow!("component instance `{contract_hash}` not found"))?;

        let mut store = Store::new(&self.chain.engine(), gen_ctx(oneshot_clt, ()));
        let component = utxo.wasm_instance.component();
        let instance = utxo
            .wasm_instance
            .instantiate_async(&mut store)
            .await
            .context("failed to instantiate component")?;
        // TODO: set data

        // Get the function type from the component
        // For interface exports like `export test:foo/add`, we need to look up the function
        // type from the component type, not from the instance directly.
        let component_type = component.component_type();

        // Debug: List all exports from the component type
        debug!("Enumerating all exports from component type:");
        for (name, _) in component_type.exports(store.engine()) {
            debug!("  Export: {}", name);
        }

        // Parse the function name to get interface and function parts
        let (interface_name, func_name_for_lookup) = if let Some(hash_pos) = function.rfind('#') {
            let interface_path = &function[..hash_pos];
            let fn_name = &function[hash_pos + 1..];
            (Some(interface_path), fn_name)
        } else {
            (None, function.as_str())
        };

        // Look up the function type from the component type
        let fun_ty = if let Some(interface_path) = interface_name {
            debug!("Looking for function '{}' in interface '{}'", func_name_for_lookup, interface_path);

            // Find the interface export
            let mut found_ty = None;
            for (export_name, export_item) in component_type.exports(store.engine()) {
                if export_name == interface_path {
                    debug!("Found interface export: {}", export_name);
                    // For interface exports, the item is ComponentInstance
                    if let wasmtime::component::types::ComponentItem::ComponentInstance(inst_ty) = export_item {
                        // Look for the function within the interface
                        for (fn_export_name, fn_export_item) in inst_ty.exports(store.engine()) {
                            debug!("  Interface member: {} (type: {:?})", fn_export_name, std::mem::discriminant(&fn_export_item));
                            if fn_export_name == func_name_for_lookup {
                                if let wasmtime::component::types::ComponentItem::ComponentFunc(func_ty) = fn_export_item {
                                    debug!("Found function type for '{}'", func_name_for_lookup);
                                    found_ty = Some(func_ty);
                                    break;
                                }
                            }
                        }
                    }
                    break;
                }
            }
            found_ty.ok_or_else(|| {
                anyhow::anyhow!("function `{}` not found in interface `{}`", func_name_for_lookup, interface_path)
            })?
        } else {
            // Top-level function export
            debug!("Looking for top-level function '{}'", func_name_for_lookup);
            let func = instance
                .get_func(&mut store, func_name_for_lookup)
                .ok_or_else(|| {
                    anyhow::anyhow!("function `{function}` not found in component")
                })?;
            func.ty(&store)
        };

        let mut guest_resources_vec = Vec::new();
        collect_component_resource_exports(
            store.engine(),
            &component.component_type(),
            &mut guest_resources_vec,
        );

        let mut host_resources = BTreeMap::new();
        collect_component_resource_imports(
            store.engine(),
            &component.component_type(),
            &mut host_resources,
        );

        let host_resources = host_resources
            .into_iter()
            .map(|(name, instance)| {
                let instance = instance
                    .into_iter()
                    .map(|(name, ty)| (name, (ty, ResourceType::host::<RemoteResource>())))
                    .collect::<HashMap<_, _>>();
                (name, instance)
            })
            .collect::<HashMap<_, _>>();
        let host_resources = Arc::from(host_resources);

        // Parse the function name to handle interface exports
        // For qualified names like "test:foo/add#add", split into:
        // - instance_name = "test:foo/add" (the interface path)
        // - func_name = "add" (the function within the interface)
        let (instance_name, func_name) = if let Some(hash_pos) = function.rfind('#') {
            let interface_path = &function[..hash_pos];
            let fn_name = &function[hash_pos + 1..];
            debug!("Parsed interface function: instance='{}', func='{}'", interface_path, fn_name);
            (interface_path.to_string(), fn_name.to_string())
        } else {
            // Top-level function export
            debug!("Top-level function: func='{}'", function);
            (String::new(), function.clone())
        };

        let pretty_name = if instance_name.is_empty() {
            "root".to_string()
        } else {
            instance_name.clone()
        };

        let pretty_name_clone = pretty_name.clone();
        let pretty_name_for_server = pretty_name.clone();
        let func_name_clone = func_name.clone();
        let store_shared = Arc::new(Mutex::new(store));
        let store_shared_clone = store_shared.clone();
        let instance_name_clone = instance_name.clone();
        let invocations_stream = srv
            .serve_function_shared(
                store_shared,
                instance,
                Arc::from(guest_resources_vec.into_boxed_slice()),
                host_resources,
                fun_ty,
                &instance_name,
                &func_name,
            )
            .await
            .with_context(|| format!("failed to register handler for function `{function}`"))?;
        let (result, invocation_handle) = join!(
            // client side
            async move {
                let paths: &[&[Option<usize>]] = &[];
                // Lock the store only to get the wrpc client and invoke
                // Release the lock immediately after getting the streams
                let (mut outgoing, mut incoming) = {
                    let store = store_shared_clone.lock().await;
                    store
                        .data()
                        .wrpc
                        .wrpc
                        .invoke((), &instance_name_clone, &func_name_clone, params, paths)
                        .await
                        .expect(&format!("failed to invoke {}", func_name_clone))
                };
                // Lock is now released, allowing server to process the invocation
                outgoing.flush().await?;
                let mut buf = vec![];
                incoming.read_to_end(&mut buf).await.with_context(|| {
                    format!("failed to read result for {pretty_name_clone} function `{func_name_clone}`")
                })?;
                Ok(buf)
            },
            // server side
            async move {
                srv.accept(oneshot_srv)
                    .await
                    .expect("failed to accept connection");

                tokio::spawn(async move {
                    let mut invocations = pin!(invocations_stream);
                    while let Some(invocation) = invocations.as_mut().next().await {
                        match invocation {
                            Ok((_, fut)) => {
                                if let Err(err) = fut.await {
                                    eprintln!(
                                        "failed to serve invocation for {pretty_name_for_server} function `{func_name}`: {err:?}"
                                    );
                                }
                            }
                            Err(err) => {
                                eprintln!(
                                    "failed to accept invocation for {pretty_name_for_server} function `{func_name}`: {err:?}"
                                );
                            }
                        }
                    }
                })
            }
        );
        // Clean up the invocation handle since the oneshot connection is complete
        // The stream should naturally end, but we abort to ensure cleanup happens immediately
        invocation_handle.abort();
        result
    }

    pub(crate) async fn get_wit(
        &self,
        hash: String,
        resolve: bool,
    ) -> anyhow::Result<bindings::exports::starstream::registry::handler::ComponentInterface> {
        let result = self.registry.get_wit(hash, resolve).await?;
        Ok(
            bindings::exports::starstream::registry::handler::ComponentInterface {
                wit: result.wit,
                entrypoint: result.entrypoint,
            },
        )
    }
}
