use std::sync::Arc;
use bytes::{Bytes, BytesMut};

use anyhow::Context;
use starstream_ledger::encode::gen_ctx;
use tracing::debug;
use futures::TryStreamExt;
use tokio::join;
use tokio_util::codec::Encoder;
// use tokio_stream::StreamExt;
use wrpc_transport::frame::Oneshot;
use wrpc_transport::Invoke;
use wrpc_transport::InvokeExt;
use wrpc_runtime_wasmtime::ServeExt;
use wrpc_transport::Accept;
use core::pin::pin;
use tokio::sync::Mutex;
use tokio::io::{DuplexStream, ReadHalf, WriteHalf};

use starstream_ledger::{Chain};
use wrpc_runtime_wasmtime::{ValEncoder, collect_component_resource_exports};
use wasmtime::{AsContextMut, Store, Engine};

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

        let (oneshot_clt, mut oneshot_srv) = Oneshot::duplex(1024);
        let srv: Arc<wrpc_transport::frame::Server<(), ReadHalf<DuplexStream>, WriteHalf<DuplexStream>>> = Arc::new(wrpc_transport::frame::Server::default());
        
        // Look up the component handler
        let utxo = self
            .chain
            .get_utxo(contract_hash.clone(), 0)
            .await
            .map_err(|_| anyhow::anyhow!("component instance `{contract_hash}` not found"))?;

        let mut store = Store::new(&self.chain.engine(), gen_ctx(oneshot_clt, ()));
        let component = utxo.wasm_instance.component();
        let instance = utxo.wasm_instance
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

        // let param_types = func.params(&store);
        let fun_ty = func.ty(&store);
        let result_len =  fun_ty.results().len();

        let mut guest_resources_vec = Vec::new();
        collect_component_resource_exports(
            store.engine(),
            &component.component_type(),
            &mut guest_resources_vec,
        );
        
        let store_shared = Arc::new(Mutex::new(store));
        let invocations_stream = srv
            .serve_function_shared(
                store_shared,
                instance,
                Arc::from(guest_resources_vec.into_boxed_slice()),
                std::collections::HashMap::default(),
                fun_ty,
                "",
                &function,
            )
            .await
            .with_context(|| {
                format!("failed to register handler for function `{function}`")
            })?;
        let (result, _) = join!(
            async {
                let paths: &[&[Option<usize>]] = &[];
                let result = oneshot_clt
                    .invoke((), "", &function, params, paths)
                    .await
                    .expect(&format!("failed to invoke {}", function));
                result
            },
            async {
                // TODO
            }
        );
        //         srv.accept(oneshot_srv)
        //             .await
        //             .expect("failed to accept connection");
        //         let ((), params, rx, tx) = pin!(invocations)
        //             .try_next()
        //             .await
        //             .expect("failed to accept invocation")
        //             .expect("unexpected end of stream");
        //         assert!(rx.is_none());

        //         let mut results = vec![wasmtime::component::Val::Bool(false); result_len];
        
        //         func.call_async(&mut store, &params, &mut results)
        //             .await
        //             .context("failed to call component function")?;
                
        //         // Encode the results Vec<Val> to bytes for transmission
        //         // The issue: tx expects a tuple that implements TupleEncode, but Vec<Val> doesn't.
        //         // Solution: Encode the results to bytes using Component Model binary encoding,
        //         // then send those bytes as a tuple (Bytes implements TupleEncode).
        //         //
        //         // Note: ValEncoder requires WrpcView context, but we're using Store<()>.
        //         // For now, we'll need to manually encode or use a different approach.
        //         // The proper encoding would follow the Component Model binary encoding spec:
        //         // https://github.com/WebAssembly/component-model/blob/main/design/mvp/Binary.md
        //         //
        //         // This is a placeholder - you'll need to implement proper encoding based on
        //         // the result_types. Each Val needs to be encoded according to its Type.
        //         let mut encoded = BytesMut::new();
        //         // TODO: Implement proper Component Model binary encoding for results
        //         // For each (val, ty) in results.iter().zip(result_types.iter()):
        //         //   - Encode val according to ty using Component Model binary format
        //         //   - Append encoded bytes to encoded buffer
                
        //         // Send the encoded bytes as a tuple - Bytes implements TupleEncode
        //         tx((Bytes::from(encoded),)).await.expect("failed to transmit response");
        //         Ok(())
        //     }
        // );

        // let result_bufs = result.0.into_bytes();
        // Ok(result_bufs)
        Ok(vec![])
    }
}
