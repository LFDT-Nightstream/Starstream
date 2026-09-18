//! Starstream ledger server.

use core::sync::atomic::AtomicU64;

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Weak};

use bytes::Bytes;
use ed25519_dalek::VerifyingKey;
use sha2::{Digest as _, Sha256};
use starstream_runtime_next::{
    CoordinationScriptImport, UtxoImport, get_coordination_script_instance_import, utxo_imports,
};
use tokio::sync::{RwLock, Semaphore};
use tracing::error;
use wasmtime::component::{Component, ResourceTable};
use wasmtime::error::Context as _;

use crate::{Block, TransactionOutput, parse_digest};

mod host;
mod http;

/// The ledger-side state of a publishing account, identified by its Ed25519
/// public key.
///
/// Publishing charges the balance one unit per byte of envelope and must carry a
/// nonce strictly greater than `last_nonce`.
#[derive(Debug, Default)]
struct Account {
    /// Account balance.
    pub balance: AtomicU64,
    /// Last nonce used by the account.
    pub last_nonce: AtomicU64,
}

#[derive(Debug)]
struct AdminAccount {
    key: VerifyingKey,
    /// Last nonce used by admin account.
    /// This nonce is not taken into account for publish actions.
    last_nonce: AtomicU64,
}

#[derive(Debug, Default)]
struct Ctx {
    table: ResourceTable,
}

#[derive(Debug)]
struct UtxoCtx {
    methods: HashSet<(u64, u64, u64, u64)>,
}

struct Contract {
    wasm: Bytes,
    envelope: Bytes,
}

#[derive(Clone, Debug)]
struct Transaction {
    outputs: Vec<Option<Arc<Bytes>>>,
    envelope: Bytes,
}

struct Genesis {
    outputs: RwLock<Box<[Option<Arc<Bytes>>]>>,
    tx_outputs: Box<[TransactionOutput]>,
}

/// Starstream ledger
pub struct Ledger {
    engine: wasmtime::Engine,
    blocks: RwLock<Vec<Block>>,
    contracts: RwLock<HashMap<[u8; 32], Arc<Contract>>>,
    accounts: RwLock<HashMap<VerifyingKey, Account>>,
    transactions: RwLock<HashMap<[u8; 32], Transaction>>,
    utxos: RwLock<HashMap<[u8; 32], Weak<Bytes>>>,
    admin: AdminAccount,
    genesis: Genesis,
    network: Arc<str>,
    max_requests: u32,
    permits: Semaphore,
}

impl Ledger {
    pub fn new(
        engine: wasmtime::Engine,
        max_requests: u32,
        network: impl Into<Arc<str>>,
        admin: VerifyingKey,
        genesis: impl Into<Box<[TransactionOutput]>>,
    ) -> Self {
        let max_requests = usize::try_from(max_requests)
            .unwrap_or(Semaphore::MAX_PERMITS)
            .min(Semaphore::MAX_PERMITS);
        let admin = AdminAccount {
            key: admin,
            last_nonce: AtomicU64::default(),
        };
        let genesis = genesis.into();
        let mut utxos = HashMap::with_capacity(genesis.len());
        let mut outputs = Vec::with_capacity(genesis.len());
        for TransactionOutput { wasm, .. } in &genesis {
            let digest: [u8; 32] = Sha256::digest(wasm).into();
            let utxo = if let Some(utxo) = utxos.get(&digest).and_then(Weak::upgrade) {
                utxo
            } else {
                let utxo = Arc::new(Bytes::copy_from_slice(wasm));
                utxos.insert(digest, Arc::downgrade(&utxo));
                utxo
            };
            outputs.push(Some(utxo));
        }
        Self {
            engine,
            blocks: RwLock::default(),
            contracts: RwLock::default(),
            accounts: RwLock::default(),
            transactions: RwLock::default(),
            utxos: RwLock::new(utxos),
            admin,
            genesis: Genesis {
                outputs: RwLock::new(outputs.into()),
                tx_outputs: genesis,
            },
            network: network.into(),
            max_requests: max_requests as _,
            permits: Semaphore::new(max_requests),
        }
    }

    async fn compile(
        &self,
        imports: &mut HashMap<Box<str>, starstream_runtime_next::Contract<Ctx>>,
        external_id: Option<&str>,
        wasm: &[u8],
    ) -> wasmtime::Result<starstream_runtime_next::Contract<Ctx>> {
        struct ContractLookup<'a>(
            &'a mut HashMap<Box<str>, starstream_runtime_next::Contract<Ctx>>,
        );
        impl starstream_runtime_next::ContractLookup<Ctx> for ContractLookup<'_> {
            fn get_contract(
                &self,
                external_id: &str,
            ) -> wasmtime::Result<starstream_runtime_next::Contract<Ctx>> {
                let contract = self.0.get(external_id).with_context(|| {
                    error!(external_id, "unresolved contract import");
                    format!("contract identified by `external-id` `{external_id}` not found")
                })?;
                Ok(contract.clone())
            }
        }

        let component =
            Component::from_binary(&self.engine, wasm).context("failed to compile component")?;
        let ty = component.component_type();
        // TODO: Ignore script imports
        let script_instance = get_coordination_script_instance_import(&self.engine, &ty);
        let script_external_ids = script_instance.as_ref().map(|instance| {
            instance.coordination_scripts().map(|import| {
                import.map(|CoordinationScriptImport { external_id, .. }| external_id)
            })
        });
        let utxo_external_ids = utxo_imports(&self.engine, &ty)
            .map(|import| import.map(|UtxoImport { external_id, .. }| external_id));
        let external_ids: HashSet<_> = utxo_external_ids
            .chain(script_external_ids.into_iter().flatten())
            .collect::<wasmtime::Result<_>>()?;
        for external_id in external_ids {
            if imports.contains_key(external_id) {
                continue;
            }
            let digest = parse_digest(external_id).with_context(|| {
                format!("failed to parse `external-id` `{external_id}` as multibase multihash")
            })?;
            let wasm = {
                let contracts = self.contracts.read().await;
                let contract = contracts.get(&digest).with_context(|| {
                    format!("contract identifed by `external-id` {external_id} not found")
                })?;
                contract.wasm.clone()
            };
            let contract = Box::pin(self.compile(imports, Some(external_id), &wasm)).await?;
            imports.insert(external_id.into(), contract);
        }
        starstream_runtime_next::Contract::new(&component, external_id, ContractLookup(imports))
    }
}
