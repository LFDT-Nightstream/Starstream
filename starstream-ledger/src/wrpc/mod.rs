//! Starstream ledger wRPC protocol definitions.

use wasmtime::component::{Type, Val};
use wrpc::wasmtime::ValDecoder;

#[cfg(any(feature = "client", feature = "server"))]
pub mod bindings;

pub const LEDGER_ADMIN_INSTANCE: &str = "starstream:ledger/admin";
pub const LEDGER_ACCOUNT_INSTANCE: &str = "starstream:ledger/account";
pub const LEDGER_BLOCK_INSTANCE: &str = "starstream:ledger/block";
pub const LEDGER_CONTRACT_INSTANCE: &str = "starstream:ledger/contract";
pub const LEDGER_TRANSACTION_INSTANCE: &str = "starstream:ledger/transaction";
pub const LEDGER_COROUTINE_INSTANCE: &str = "starstream:ledger/coroutine";
pub const LEDGER_GENESIS_INSTANCE: &str = "starstream:ledger/genesis";
pub const LEDGER_NETWORK_INSTANCE: &str = "starstream:ledger/network";
pub const LEDGER_UTXO_INSTANCE: &str = "starstream:ledger/utxo";

/// Unpack a value of type `ty`, which contains no resources, futures or streams, from `buf`
pub fn unpack_val(ty: Type, buf: &[u8]) -> wasmtime::Result<Val> {
    let val = wrpc::unpack_with(ValDecoder::new(ty).with_max_len(u32::MAX), buf)?;
    Val::try_from(val)
}
