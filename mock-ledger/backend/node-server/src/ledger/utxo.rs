use std::sync::Mutex;
use std::sync::Arc;
use std::collections::HashMap;
use wasmtime::Store;
use wasmtime::component::{Instance};

#[derive(Clone)]
pub struct UtxoInstance {
  /// The component instance that can be invoked
  pub wasm_instance: Arc<Mutex<Instance>>,
  /// The store for the component instance
  pub wasm_store: Arc<Mutex<Store<()>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UtxoId {
  /// The contract hash
  contract_hash: String,
  /// nth occurrence of the contract_hash on chain
  serial: u64,
}
impl UtxoId {
  pub fn new(contract_hash: String, serial: u64) -> Self {
    Self { contract_hash, serial }
  }
  pub fn contract_hash(&self) -> &String {
    &self.contract_hash
  }
  pub fn serial(&self) -> u64 {
    self.serial
  }
}

/// Mapping from UtxoId -> WASM Component
pub type UtxoRegistry = tokio::sync::RwLock<HashMap<UtxoId, UtxoInstance>>;
