use std::sync::Arc;

use anyhow::Context;
use tracing::debug;

use crate::ledger::Chain;

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

        // Now call the function (exports is dropped, so we can use store_guard again)
        let mut results = [wasmtime::component::Val::S64(0)];
        func.call(&mut *store_guard, &[], &mut results)
            .context("failed to call component function")?;

        // Extract the result (s64)
        // TODO: make this more generic
        let result_value = match results[0] {
            wasmtime::component::Val::S64(val) => val,
            _ => return Err(anyhow::anyhow!("unexpected return type from component function")),
        };

        debug!("Component function returned: {}", result_value);

        // For now, encode as a string representation so the client can display it
        // In a real implementation, this should use Component Model value encoding
        let result_string = result_value.to_string();
        Ok(result_string.into_bytes())
    }
}
