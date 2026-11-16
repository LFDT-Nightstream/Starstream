use std::collections::HashMap;
use std::sync::Arc;

use bytes::Bytes;
use ed25519_dalek::VerifyingKey;

use crate::CardanoCtx;
use crate::genesis::{Genesis, build_genesis};
use crate::server::{Ledger, configure_wasmtime};

impl Ledger {
    /// Create a new ledger whose genesis outputs are created by the scripts
    /// of `genesis`, resolving contracts in `imported` by their digest.
    ///
    /// The engines running the genesis scripts and the ledger are built from
    /// `config`, configured with [`crate::configure_wasmtime`] and
    /// [`configure_wasmtime`] respectively.
    #[allow(clippy::too_many_arguments)]
    pub async fn with_genesis(
        mut config: wasmtime::Config,
        max_requests: u32,
        fuel: u64,
        network: impl Into<Arc<str>>,
        admin: VerifyingKey,
        genesis: Genesis,
        imported: &HashMap<[u8; 32], Bytes>,
        cardano: CardanoCtx,
    ) -> anyhow::Result<Self> {
        let genesis = if genesis.scripts.is_empty() {
            Vec::default()
        } else {
            let mut genesis_config = config.clone();
            crate::configure_wasmtime(&mut genesis_config);
            let engine = wasmtime::Engine::new(&genesis_config)?;
            build_genesis(&engine, genesis, imported, cardano).await?
        };
        configure_wasmtime(&mut config);
        let engine = wasmtime::Engine::new(&config)?;
        Ok(Self::new(
            engine,
            max_requests,
            fuel,
            network,
            admin,
            genesis,
            cardano,
        ))
    }
}
