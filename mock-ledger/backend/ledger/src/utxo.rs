use std::collections::HashMap;
use std::sync::Arc;
use wasmtime::component::InstancePre;

use crate::encode::{Ctx, InMemoryTransport};

#[derive(Clone)]
pub struct UtxoInstance {
    /// Cached setup for hydrating a UTXO. Combine it with the datum to initialize
    /// InstancePre is immutable and cloneable, so Arc is sufficient for shared ownership
    pub wasm_instance: Arc<InstancePre<Ctx<InMemoryTransport>>>,
    /// UTXO datum encoded using the WASM Component value encoding
    pub datum: Vec<u8>,
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
        Self {
            contract_hash,
            serial,
        }
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
