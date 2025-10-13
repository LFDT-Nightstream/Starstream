//! Tree-walking interpreter scaffold for the pared-down Starstream DSL.
#![allow(dead_code)]

use starstream_types::ast::{Block, Expr, Program, Statement};

/// Placeholder runtime value. Real semantics will replace this later.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Integer(i64),
    Boolean(bool),
}

/// Execute a program starting at the supplied entry point.
pub fn run_program(_program: &Program, _entry_point: &str, _args: Vec<Value>) -> Value {
    todo!("interpreter not implemented yet")
}

/// Evaluate a top-level statement.
pub fn eval_statement(_statement: &Statement) -> Value {
    todo!("statement evaluation not implemented yet")
}

/// Evaluate a block of statements.
pub fn eval_block(_block: &Block) -> Value {
    todo!("block evaluation not implemented yet")
}

/// Evaluate an expression.
pub fn eval_expr(_expr: &Expr) -> Value {
    todo!("expression evaluation not implemented yet")
}
