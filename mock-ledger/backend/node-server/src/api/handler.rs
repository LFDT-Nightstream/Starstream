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
use wasmtime::{AsContextMut, Engine, Store};
use wrpc_runtime_wasmtime::{
    ValEncoder, collect_component_resource_exports, collect_component_resource_imports,
};

#[derive(Clone)]
pub struct Handler {
    // Arc instead of 'a
    // as we can't easily control lifetime logic for different transport systems we need to implement the API for
    chain: Arc<Chain>,
}

impl Handler {
    pub fn new(chain: Arc<Chain>) -> Self {
        Self { chain }
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

        // Get the typed function from the instance
        // This avoids the double borrow issue by getting a typed function handle
        // Func implements Copy, so we can just assign it directly
        let func = instance
            .get_func(&mut store, &function)
            .ok_or_else(|| anyhow::anyhow!("function `{function}` not found in component"))?;

        let fun_ty = func.ty(&store);

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

        let instance_name = "".to_string();
        let pretty_name = if instance_name.is_empty() {
            "root".to_string()
        } else {
            instance_name.clone()
        };

        let func_name = function.clone();
        let pretty_name = pretty_name.clone();
        let pretty_name_for_server = pretty_name.clone();
        let store_shared = Arc::new(Mutex::new(store));
        let store_shared_clone = store_shared.clone();
        let invocations_stream = srv
            .serve_function_shared(
                store_shared,
                instance,
                Arc::from(guest_resources_vec.into_boxed_slice()),
                host_resources,
                fun_ty,
                &instance_name,
                &function,
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
                        .invoke((), &instance_name, &function, params, paths)
                        .await
                        .expect(&format!("failed to invoke {}", function))
                };
                // Lock is now released, allowing server to process the invocation
                outgoing.flush().await?;
                let mut buf = vec![];
                incoming.read_to_end(&mut buf).await.with_context(|| {
                    format!("failed to read result for {pretty_name} function `{function}`")
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
}
