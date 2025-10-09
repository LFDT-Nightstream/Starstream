//! Error types for the Starstream DSL

use thiserror::Error;
use serde::{Deserialize, Serialize};

#[derive(Error, Debug, Serialize, Deserialize)]
pub enum StarstreamError {
    #[error("Parse error: {0}")]
    ParseError(String),
    
    #[error("Compilation error: {0}")]
    CompilationError(String),
    
    #[error("Runtime error: {0}")]
    RuntimeError(String),
    
    #[error("Interpreter error: {0}")]
    InterpreterError(String),
    
    #[error("Invalid opcode: {0}")]
    InvalidOpcode(String),
    
    #[error("Stack underflow")]
    StackUnderflow,
    
    #[error("Stack overflow")]
    StackOverflow,
    
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    
    #[error("Type error: {0}")]
    TypeError(String),
}

pub type Result<T> = std::result::Result<T, StarstreamError>;
