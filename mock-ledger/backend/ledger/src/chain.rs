use std::{collections::HashMap};
use std::sync::Arc;
use std::sync::Mutex;
use anyhow::{Context, anyhow};

use wasmtime::*;
use wasmtime::component::{Component, Linker};
use p3_field::PrimeField64;

use crate::encode::ChainContext;
use crate::utxo::{UtxoId, UtxoInstance, UtxoRegistry};
use crate::utils::hash::poseidon2_hash_bytes;

pub struct Chain {
    utxos: UtxoRegistry,
}

type WasmComponent = Vec<u8>;

impl Chain {
  pub fn new(genesis_block: &Vec<WasmComponent>) -> anyhow::Result<Self> {
      let mut utxos = HashMap::new();

      // load genesis block
      {
        // Load the no-args component dynamically
        let engine = Engine::default();
        
        for component_bytes in genesis_block {
          let component = Component::new(&engine, component_bytes)
              .context("failed to parse component")?;

          let linker = Linker::new(&engine);
          let mut store = Store::new(&engine, ChainContext::default());
          
          let instance = linker
              .instantiate(&mut store, &component)
              .context("failed to instantiate component")?;
          
          let digest = poseidon2_hash_bytes(component_bytes);
          let contract_hash = format!("0x{:016X}{:016X}{:016X}{:016X}", digest[0].as_canonical_u64(), digest[1].as_canonical_u64(), digest[2].as_canonical_u64(), digest[3].as_canonical_u64());

          // Register the utxo
          utxos.insert(
              UtxoId::new(contract_hash, 0),
              UtxoInstance {
                  wasm_component: Arc::new(component),
                  wasm_instance: Arc::new(Mutex::new(instance)),
                  wasm_store: Arc::new(Mutex::new(store)),
              },
          );
        }
      }

      Ok(Self {
        utxos: tokio::sync::RwLock::new(utxos),
    })
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