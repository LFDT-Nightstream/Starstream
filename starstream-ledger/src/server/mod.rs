//! Starstream ledger server.

use core::ops::{Deref, DerefMut};
use core::sync::atomic::AtomicU64;

use std::collections::HashMap;
use std::sync::Arc;

use bytes::Bytes;
use ed25519_dalek::VerifyingKey;
use starstream_runtime_next::{CoordinationScriptExport, UtxoExport};
use tokio::sync::{RwLock, Semaphore};
use wasmtime::component::ResourceTable;
use wasmtime_wizer::Wizer;

use crate::Block;

mod host;
mod http;
mod lookup;

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

struct Ctx {
    table: ResourceTable,
}

struct Contract {
    contract: starstream_runtime_next::Contract<Ctx>,
    #[expect(unused, reason = "TODO")]
    contract_wasm: Bytes,
    #[expect(unused, reason = "TODO")]
    scripts: HashMap<Box<str>, CoordinationScriptExport>,
    #[expect(unused, reason = "TODO")]
    utxos: HashMap<Box<str>, UtxoExport>,
    wasm: Bytes,
    envelope: Bytes,
}

impl Deref for Contract {
    type Target = starstream_runtime_next::Contract<Ctx>;

    fn deref(&self) -> &Self::Target {
        &self.contract
    }
}

impl DerefMut for Contract {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.contract
    }
}

/// Starstream ledger
pub struct Ledger {
    engine: wasmtime::Engine,
    wizer: Wizer,
    blocks: Arc<RwLock<Vec<Block>>>,
    contracts: Arc<RwLock<HashMap<[u8; 32], Arc<Contract>>>>,
    accounts: Arc<RwLock<HashMap<VerifyingKey, Account>>>,
    admin: AdminAccount,
    network: Arc<str>,
    max_requests: u32,
    permits: Arc<Semaphore>,
}

impl Ledger {
    pub fn new(
        engine: wasmtime::Engine,
        max_requests: u32,
        network: impl Into<Arc<str>>,
        admin: VerifyingKey,
    ) -> Self {
        let max_requests = usize::try_from(max_requests)
            .unwrap_or(Semaphore::MAX_PERMITS)
            .min(Semaphore::MAX_PERMITS);
        Self {
            engine,
            wizer: Wizer::new(),
            blocks: Arc::default(),
            contracts: Arc::default(),
            accounts: Arc::default(),
            admin: AdminAccount {
                key: admin,
                last_nonce: AtomicU64::default(),
            },
            network: network.into(),
            max_requests: max_requests as _,
            permits: Arc::new(Semaphore::new(max_requests)),
        }
    }
}
