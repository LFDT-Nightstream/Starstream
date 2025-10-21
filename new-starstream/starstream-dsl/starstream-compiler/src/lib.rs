//! Compiler for the Starstream DSL
//!
//! This crate provides:
//! - A parser that translates source code to AST

pub mod formatter;
pub mod parser;

pub use parser::*;

pub const FILE_EXTENSION: &str = "star";
