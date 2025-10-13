//! Shared types for the Starstream DSL
//!
//! This crate contains the core types used across the Starstream DSL ecosystem:
//! - Abstract Syntax Tree (AST) definitions
//! - Stack machine opcodes
//! - Error types

pub mod ast;
pub mod error;
mod symbols;
mod typechecking;

pub use ast::*;
pub use error::*;
