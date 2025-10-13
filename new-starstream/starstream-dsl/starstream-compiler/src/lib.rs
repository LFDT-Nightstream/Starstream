//! Compiler for the Starstream DSL
//!
//! This crate provides:
//! - A parser that translates source code to AST
//! - A compiler that translates AST to stack machine opcodes

pub mod parser;

pub use parser::*;
