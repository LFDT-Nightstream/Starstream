//! Starstream ledger wRPC protocol definitions.

#[cfg(any(feature = "client", feature = "server"))]
pub mod codec;

/// The package name used for the ledger.
pub const LEDGER_PACKAGE: &str = "starstream:ledger";

/// The package name used for UTXOs.
pub const UTXO_PACKAGE: &str = "starstream:utxo";
