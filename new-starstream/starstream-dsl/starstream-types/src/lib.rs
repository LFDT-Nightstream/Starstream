//! Shared types for the Starstream DSL
//! 
//! This crate contains the core types used across the Starstream DSL ecosystem:
//! - Abstract Syntax Tree (AST) definitions
//! - Stack machine opcodes
//! - Error types

mod typechecking;
mod symbols;
pub mod ast;
pub mod opcodes;
pub mod error;

pub use ast::*;
pub use opcodes::*;
pub use error::*;
