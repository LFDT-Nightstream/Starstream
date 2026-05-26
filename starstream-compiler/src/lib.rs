//! Compiler for the Starstream DSL
//!
//! This crate provides:
//! - A parser that translates source code to AST

pub mod docs;
pub mod formatter;
pub mod module_graph;
pub mod parser;
pub mod typecheck;

pub use docs::{DocsOutput, generate_docs};
pub use module_graph::{Module, ModuleGraph, ModuleGraphError, ModuleId, PathImport};
pub use parser::parse_program;
pub use typecheck::{
    TypeWarning, TypeWarningKind, TypecheckFailure, TypecheckModulesFailure, TypecheckOptions,
    TypedModule, TypedModuleGraph, typecheck_modules, typecheck_program,
};

pub const FILE_EXTENSION: &str = "star";
