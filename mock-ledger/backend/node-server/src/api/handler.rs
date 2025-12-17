use std::sync::Arc;
use bytes::BytesMut;

use anyhow::Context;
use tracing::debug;
use tokio_util::codec::Encoder;

use starstream_ledger::{Chain, encode::ChainContext};
use wrpc_runtime_wasmtime::{ValEncoder, collect_component_resource_exports};
use wasmtime::{AsContextMut, Engine};

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
        _params: Vec<u8>,
    ) -> anyhow::Result<Vec<u8>> {
        debug!(contract_hash, function, "forwarding invocation");

        // Look up the component handler
        let utxo = self
            .chain
            .get_utxo(contract_hash.clone(), 0)
            .await
            .map_err(|_| anyhow::anyhow!("component instance `{contract_hash}` not found"))?;

        // Get the component instance and store
        let instance_guard = utxo
            .wasm_instance
            .lock()
            .map_err(|e| anyhow::anyhow!("failed to lock instance: {e}"))?;
        let component_guard = utxo
            .wasm_component;
        let mut store_guard = utxo
            .wasm_store
            .lock()
            .map_err(|e| anyhow::anyhow!("failed to lock store: {e}"))?;

        // Get the typed function from the instance
        // This avoids the double borrow issue by getting a typed function handle
        // Func implements Copy, so we can just assign it directly
        let func = {
            instance_guard
                .get_func(&mut *store_guard, &function)
                .ok_or_else(|| anyhow::anyhow!("function `{function}` not found in component"))?
        };

        // Get the function type to determine the number of return values
        // let func_ty = func.ty(&*store_guard);
        // let return_count = func_ty.results().len();

        // Allocate results array dynamically based on the function's return type
        // let mut results = vec![wasmtime::component::Val::Bool(false); return_count];
        
        // Call the function with empty params and dynamic results
        
        
        // debug!("Component function returned {} value(s)", results.len());

        // Encode the results using Component Model value encoding
        // let encoded = encode_values(&results)
        //     .context("failed to encode component function results")?;

        // debug!("Component function returned: {}", result_value);
        
        // Now call the function (exports is dropped, so we can use store_guard again)
        let mut results = [wasmtime::component::Val::S64(0)];
        func.call(&mut *store_guard, &[], &mut results)
            .context("failed to call component function")?;

        // Extract the result (s64)
        // TODO: make this more generic
        // let result_value = match results[0] {
        //     wasmtime::component::Val::S64(val) => val,
        //     _ => return Err(anyhow::anyhow!("unexpected return type from component function")),
        // };
        // let result_string = results[0];
        // Ok(result_string.into());

        // TODO: do we really need to initialize this here?
        let engine = Engine::default();

        let mut result_bufs = Vec::new();
        for (i, ty) in func.results(&*store_guard).iter().enumerate() {
            let mut buf = BytesMut::default();
            let context = (&mut *store_guard).as_context_mut();
            // TODO: get "resources" from the store_guard in a way that makes sense
            let mut guest_resources_vec = Vec::new();
            collect_component_resource_exports(&engine, &(*component_guard).component_type(), &mut guest_resources_vec);
            let mut enc = ValEncoder::new(context, ty, guest_resources_vec.as_slice());
            enc.encode(&results[i], &mut buf)
                .with_context(|| format!("failed to encode result value {i}"))?;
            // TODO: enc.deferred
            result_bufs.extend_from_slice(buf.freeze().as_ref());
        }

        Ok(result_bufs)
    }
}
