//! Streaming interpreter for the Starstream DSL
//! 
//! This crate provides:
//! - A streaming interpreter that executes opcodes one at a time
//! - A plugin architecture for handling different opcode types
//! - Execution context management

pub mod interpreter;
pub mod context;

pub use interpreter::*;
