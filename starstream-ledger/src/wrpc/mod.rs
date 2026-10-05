//! Starstream ledger wRPC protocol definitions.

#[cfg(any(feature = "client", feature = "server"))]
pub mod bindings;
#[cfg(any(feature = "client", feature = "server"))]
pub mod codec;

/// The package name used for the ledger.
pub const LEDGER_PACKAGE: &str = "starstream:ledger";
