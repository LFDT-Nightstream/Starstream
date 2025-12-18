use std::sync::Mutex;
use std::sync::Arc;
use std::collections::HashMap;
use wasmtime::Store;
use wasmtime::component::{Instance, Component};

use crate::encode::ChainContext;


#[derive(Clone)]
pub struct UtxoInstance {
  /// The component that was instantiated
  /// TODO: does this need a Mutex? Can this be derived from other data?
  pub wasm_component: Arc<Component>,
  /// The component instance that can be invoked
  pub wasm_instance: Arc<Mutex<Instance>>,
  /// The store for the component instance.
  /// Contains the ChainContext which holds host state (ledger context, etc.)
  pub wasm_store: Arc<Mutex<Store<ChainContext>>>,
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
