use core::net::SocketAddr;

use std::collections::HashMap;
use std::sync::Arc;
use std::path::PathBuf;
use std::sync::Mutex;

use tracing::debug;
use anyhow::Context;
use wasmtime::*;
use wasmtime::component::{Component, Instance, Linker};
use p3_field::PrimeField64;
use crate::hash::poseidon2_hash_bytes;

pub mod bindings {
    wit_bindgen_wrpc::generate!({
        with: {
            "starstream:node-rpc/handler": generate,
        }
    });
}

/// A registry of component handlers that can be invoked dynamically
type ComponentRegistry = Arc<tokio::sync::RwLock<HashMap<String, ComponentHandler>>>;

/// Handler for a specific component instance
struct ComponentHandler {
    /// The component instance that can be invoked
    instance: Arc<Mutex<Instance>>,
    /// The store for the component instance (protected by Mutex for async safety)
    store: Arc<Mutex<Store<()>>>,
}

#[derive(Clone)]
pub struct Handler {
    /// Registry of available components
    components: ComponentRegistry,
}

impl Handler {
    pub fn new() -> anyhow::Result<Self> {
        let mut components = HashMap::new();
        
        // Load the no-args component dynamically
        let engine = Engine::default();
        let component_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("genesis")
            .join("no-args.wasm");
        
        let component_bytes = std::fs::read(&component_path)
            .with_context(|| format!("failed to read component from {:?}", component_path))?;
        
        let component = Component::new(&engine, &component_bytes)
            .context("failed to parse component")?;
        
        let linker = Linker::new(&engine);
        let mut store = Store::new(&engine, ());
        
        let instance = linker
            .instantiate(&mut store, &component)
            .context("failed to instantiate component")?;
        
        let digest = poseidon2_hash_bytes(&component_bytes);
        let contract_hash = format!("0x{:016X}", digest[0].as_canonical_u64());

        // Register the component handler
        components.insert(
            contract_hash,
            ComponentHandler {
                instance: Arc::new(Mutex::new(instance)),
                store: Arc::new(Mutex::new(store)),
            },
        );

        Ok(Self {
            components: Arc::new(tokio::sync::RwLock::new(components)),
        })
    }

    /// Forward an invocation to the appropriate component
    async fn forward_invocation(
        &self,
        contract_hash: String,
        function: String,
        _params: Vec<u8>,
    ) -> anyhow::Result<Vec<u8>> {
        debug!(contract_hash, function, "forwarding invocation");

        // Look up the component handler
        let components = self.components.read().await;
        let handler = components
            .get(&contract_hash)
            .ok_or_else(|| anyhow::anyhow!("component instance `{contract_hash}` not found"))?;

        // Get the component instance and store
        let instance_guard = handler.instance.lock()
            .map_err(|e| anyhow::anyhow!("failed to lock instance: {e}"))?;
        let mut store_guard = handler.store.lock()
            .map_err(|e| anyhow::anyhow!("failed to lock store: {e}"))?;

        // Get the typed function from the instance
        // This avoids the double borrow issue by getting a typed function handle
        // Func implements Copy, so we can just assign it directly
        let func = {
            instance_guard.get_func(&mut *store_guard, &function)
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

impl bindings::exports::starstream::node_rpc::handler::Handler<SocketAddr> for Handler {
    async fn call(
        &self,
        _: SocketAddr,
        contract_hash: String,
        function: String,
        params: wit_bindgen_wrpc::bytes::Bytes,
    ) -> anyhow::Result<wit_bindgen_wrpc::bytes::Bytes> {
        self.forward_invocation(contract_hash, function, params.to_vec())
            .await
            .map(|v| v.into())
    }
}
