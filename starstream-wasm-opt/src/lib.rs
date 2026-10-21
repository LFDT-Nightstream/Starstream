//! Wasm-to-Wasm tooling for the core module that `starstream-to-wasm` emits,
//! targeting the Neo-Wasm prover.
//!
//! Neo-Wasm proves an execution by single-stepping it in wasmtime: every
//! executed operator, structural ones included, becomes one trace row of the
//! same width whatever its opcode. Optimization passes here therefore aim to
//! execute fewer operators, and must only ever emit what Neo-Wasm can trace,
//! which [`validate`] checks.

mod validate;

pub use validate::{ValidationError, validate};
