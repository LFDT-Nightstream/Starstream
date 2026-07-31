//! A standalone Starstream ledger server built on `starstream-runtime-next`.
//!
//! [`Ledger::handle_http`] serves the HTTP API specified in
//! `docs/ledger.md`:
//!
//! - `PUT`/`GET /contracts/<digest>` — publish and fetch contracts,
//!   content-addressed by the SHA-256 digest of the component Wasm,
//!   encoded as a multibase multihash (see [`encode_digest`]).
//!   Publishing is a signed `COSE_Sign1` envelope over the CBOR
//!   transaction `[context, network, nonce, wasm]` (see
//!   [`PUBLISH_CONTEXT`]), charged to an [`Account`] identified by the
//!   signer's Ed25519 public key.
//! - `POST /accounts/<account>` — fund an account, identified by the
//!   lowercase-hex encoding of its Ed25519 public key, with a transfer
//!   from the admin account's balance. The body is a signed `COSE_Sign1`
//!   envelope over the CBOR transaction
//!   `[context, network, nonce, account, amount]` (see
//!   [`FUND_ACCOUNT_CONTEXT`]) and must be signed by the admin key.
//! - `GET /contracts/<digest>/rpc` — fetch a contract's
//!   coordination-script ABI as WIT.
//! - `GET /utxos/<digest>/rpc` — fetch a persisted UTXO's ABI as WIT,
//!   content-addressed by the SHA-256 digest of its snapshot Wasm.
//! - `POST /rpc` — invoke over wRPC framing, dispatched on the wRPC
//!   instance: `starstream:contract/<digest>` invokes a coordination
//!   script, with UTXO imports resolved via [`X_STARSTREAM_UTXO`] request
//!   headers and the UTXOs the script constructs snapshotted and
//!   persisted as a transaction; `starstream:utxo/<digest>` invokes a
//!   method on a persisted UTXO restored from its snapshot.
//!
//! Running contracts observe the ledger's [`CardanoCtx`] through the
//! `starstream:std/cardano` host interface.

use core::ops::{Deref, DerefMut};

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use bytes::Bytes;
use sha2::{Digest as _, Sha256};
use starstream_runtime_next::{CoordinationScriptExport, UtxoExport};
use thiserror::Error;
use wasmtime::component::ResourceTable;

pub mod codec;
mod host;
mod ledger;

pub use ledger::*;

/// The domain-separation context every publish transaction must carry as its
/// first element, binding the signature to this protocol.
pub const PUBLISH_CONTEXT: &str = "starstream:publish";

/// The domain-separation context every fund-account transaction must carry as
/// its first element, binding the signature to this protocol.
pub const FUND_ACCOUNT_CONTEXT: &str = "starstream:fund-account";

/// The header mapping a UTXO import instance to the digest of the contract
/// providing it on coordination-script invocation requests
/// (`<instance>=<contract-digest>`, repeatable), and carrying the digest
/// addressing each persisted UTXO (`<utxo-digest>`) — in output order — on
/// the response.
pub const X_STARSTREAM_UTXO: &str = "X-Starstream-Utxo";

/// The header carrying the zero-based index of the transaction recorded for a
/// coordination-script invocation on the response.
pub const X_STARSTREAM_TRANSACTION: &str = "X-Starstream-Transaction";

/// The header carrying the height of the block recording the transaction of a
/// coordination-script invocation on the response.
pub const X_STARSTREAM_BLOCK: &str = "X-Starstream-Block";

/// The [multihash] code of sha2-256.
///
/// [multihash]: https://github.com/multiformats/multihash
const MULTIHASH_SHA2_256: u64 = 0x12;

/// Encode a raw SHA-256 contract digest in the canonical ledger form: the
/// multibase base32-lower encoding of its sha2-256 multihash. The result is
/// always a 56-character lowercase alphanumeric string starting with `b` — a
/// valid component-model label and URL path segment.
pub fn encode_digest(digest: &[u8; 32]) -> String {
    let Ok(digest) = multihash::Multihash::<32>::wrap(MULTIHASH_SHA2_256, digest) else {
        unreachable!();
    };
    multibase::encode(multibase::Base::Base32Lower, digest.to_bytes())
}

#[derive(Debug, Error)]
pub enum DigestParseError {
    #[error(transparent)]
    Base(multibase::Error),
    #[error(transparent)]
    Hash(multihash::Error),
    #[error("unexpected multihash code `{0}`")]
    Code(u64),
    #[error("unexpected multihash size {0}")]
    Size(u8),
}

pub fn parse_digest(s: &str) -> Result<[u8; 32], DigestParseError> {
    let (_, buf) = multibase::decode(s).map_err(DigestParseError::Base)?;
    let mh = multihash::Multihash::<32>::from_bytes(&buf).map_err(DigestParseError::Hash)?;
    let (code, digest, size) = mh.into_inner();
    if code != MULTIHASH_SHA2_256 {
        return Err(DigestParseError::Code(code));
    }
    if size != 32 {
        return Err(DigestParseError::Size(size));
    }
    Ok(digest)
}

pub enum Action {
    ContractUpload(Bytes),
    FundAccount { key: Box<str>, amount: u64 },
    Transaction(Arc<Transaction>),
}

pub struct Block {
    pub height: usize,
    pub actions: Box<[Action]>,
    // TODO: Add proofs
}

/// The Cardano context a contract can observe via the `starstream:std/cardano`
/// host functions.
#[derive(Clone, Copy, Debug, Default)]
pub struct CardanoCtx {
    /// Block height reported to the guest via `cardano#block-height`.
    pub block_height: i64,
    /// Current slot reported to the guest via `cardano#current-slot`.
    pub current_slot: i64,
}

#[derive(Clone, Debug)]
struct UtxoOutput {
    utxo: starstream_runtime_next::Utxo,
    instance: Arc<str>,
    implemented: HashSet<(u64, u64, u64, u64)>,
}

#[derive(Default)]
struct Ctx {
    table: ResourceTable,
    cardano: CardanoCtx,
    implemented: HashSet<(u64, u64, u64, u64)>,
    imports: HashMap<Box<str>, UtxoImport>,
    outputs: Vec<UtxoOutput>,
}

struct Contract {
    contract: starstream_runtime_next::Contract<Ctx>,
    scripts: HashMap<Box<str>, CoordinationScriptExport>,
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

#[derive(Clone, Debug, Default)]
pub struct Utxo {
    pub contract_digest: [u8; 32],
    pub instance: Arc<str>,
    pub wasm: Bytes,
    pub implemented: HashSet<(u64, u64, u64, u64)>,
    pub storage: Vec<(String, wasmtime::component::Val)>,
}

impl Utxo {
    /// Returns true if this UTXO reported `name` as implemented.
    ///
    /// Note that this method assumes Starstream names.
    fn implements_method(&self, name: &str) -> bool {
        let d = Sha256::digest(name.as_bytes());
        self.implemented.contains(&(
            u64::from_le_bytes([d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7]]),
            u64::from_le_bytes([d[8], d[9], d[10], d[11], d[12], d[13], d[14], d[15]]),
            u64::from_le_bytes([d[16], d[17], d[18], d[19], d[20], d[21], d[22], d[23]]),
            u64::from_le_bytes([d[24], d[25], d[26], d[27], d[28], d[29], d[30], d[31]]),
        ))
    }
}

#[derive(Clone)]
struct UtxoImport {
    contract: starstream_runtime_next::Contract<Ctx>,
    export: UtxoExport,
}

/// The ledger-side state of a publishing account, identified by the
/// lowercase-hex encoding of its Ed25519 public key.
///
/// Publishing charges the balance one unit per byte of Wasm and must carry a
/// nonce strictly greater than `last_nonce`.
#[derive(Clone, Debug, Default)]
pub struct Account {
    /// Account balance.
    pub balance: u64,
    /// Last nonce used by the account.
    pub last_nonce: u64,
}

pub struct Transaction {
    pub coordination_script_digest: [u8; 32],
    pub inputs: Box<[(usize, usize)]>,
    pub outputs: Box<[Arc<Utxo>]>,
    // TODO: Add proof
}
