use std::{collections::HashMap};
use std::sync::Arc;
use anyhow::{Context, anyhow};

use wasmtime::*;
use wasmtime::component::{Component, Linker};
use p3_field::PrimeField64;

use crate::utxo::{UtxoId, UtxoInstance, UtxoRegistry};
use crate::utils::hash::poseidon2_hash_bytes;

pub struct Chain {
    utxos: UtxoRegistry,
    engine: Arc<Engine>,
}

pub type WasmComponent = Vec<u8>;

impl Chain {
  pub fn new(genesis_block: &Vec<WasmComponent>) -> anyhow::Result<Self> {
      let mut utxos = HashMap::new();

      let mut config = wasmtime::Config::default();
      config.async_support(true);
      let engine = Engine::new(&config)?;

      // load genesis block
      {
        for component_bytes in genesis_block {
          let component = Component::new(&engine, component_bytes)
              .context("failed to parse component")?;

          // TODO: inject context imports into linker: inject_context_imports
          let linker = Linker::new(&engine);
          let instance_pre = linker.instantiate_pre(&component)?;
          
          let digest = poseidon2_hash_bytes(component_bytes);
          let contract_hash = format!("0x{:016X}{:016X}{:016X}{:016X}", digest[0].as_canonical_u64(), digest[1].as_canonical_u64(), digest[2].as_canonical_u64(), digest[3].as_canonical_u64());

          // Register the utxo
          utxos.insert(
              UtxoId::new(contract_hash, 0),
              UtxoInstance {
                  wasm_instance: Arc::new(instance_pre),
                  datum: vec![] // TODO: maybe some genesis UTXOs require storage initialization
              },
          );
        }
      }

      Ok(Self {
        engine: Arc::new(engine),
        utxos: tokio::sync::RwLock::new(utxos),
    })
  }

  pub fn engine(&self) -> Arc<Engine> {
    self.engine.clone()
  }

  pub async fn get_utxo(&self, contract_hash: String, serial: u64) -> anyhow::Result<UtxoInstance> {
    self.get_utxo_by_id(&UtxoId::new(contract_hash, serial)).await
  }

  pub async fn get_utxo_by_id(&self, id: &UtxoId) -> anyhow::Result<UtxoInstance> {
    let utxos = self.utxos.read().await;
    let utxo = utxos
        .get(id)
        .cloned()
        .ok_or_else(|| anyhow!("UTXO not found"))?;
    Ok(utxo)
  }
}


/// Host any context that can be fetched from Starstream contracts through an effect handler
///
/// Think of this similar to a React Context: Starstream programs can raise an effect to receive context from the runtime
/// This is represented as an interface (in the WIT definition of the word) that the host provides when it calls into the UTXO
/// (pseudocode as the Starstream syntax isn't decided yet): `raise Ctx.Caller()`
/// 
/// Here, the ledger is the "host" in the WASM sense
/// and it exposes this data to guests via host functions added to the Linker
/// 
/// Note: blockchain execution doesn't involve stateful handles like file descriptors or network connections
///       so it can be an interface, and not a "resource"
fn inject_context_imports() {
  // TODO: Add ledger context fields as needed:
  // pub block_range: (u64, u64), // validity interval of tx
  // pub timestamp_range: (u64, u64), // validity interval of tx
  // pub caller_address: String
}