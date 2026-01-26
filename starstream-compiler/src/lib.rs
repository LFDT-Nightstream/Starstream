//! Compiler for the Starstream DSL
//!
//! This crate provides:
//! - A parser that translates source code to AST

pub mod docs;
pub mod formatter;
pub mod parser;
pub mod typecheck;

pub use docs::{DocsOutput, generate_docs};
pub use parser::parse_program;
pub use typecheck::{TypecheckOptions, typecheck_program};

pub const FILE_EXTENSION: &str = "star";
