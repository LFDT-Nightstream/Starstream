//! Starstream ledger wRPC protocol definitions.

#[cfg(any(feature = "client", feature = "server"))]
pub mod bindings;
#[cfg(any(feature = "client", feature = "server"))]
pub mod codec;

pub const LEDGER_BLOCK_INSTANCE: &str = "starstream:ledger/block";
pub const LEDGER_CONTRACT_INSTANCE: &str = "starstream:ledger/contract";
pub const LEDGER_TRANSACTION_INSTANCE: &str = "starstream:ledger/transaction";
pub const LEDGER_GENESIS_INSTANCE: &str = "starstream:ledger/genesis";
pub const LEDGER_UTXO_INSTANCE: &str = "starstream:ledger/utxo";
