//! Starstream DSL - A compiler and interpreter for the Starstream virtual machine
//! 
//! This crate provides:
//! - A compiler that translates IMP-like programs to stack machine opcodes
//! - A streaming interpreter that executes opcodes with a plugin architecture
//! - Support for running in the browser via WASM

// Re-export everything from the sub-crates for backward compatibility
pub use starstream_types::*;
pub use starstream_compiler::*;
pub use starstream_interpreter::*;
