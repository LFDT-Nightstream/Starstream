//! Error types for the Starstream DSL

use serde::{Deserialize, Serialize};
use thiserror::Error;

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

/// An error code with both a name and embedded documentation attached.
#[derive(Clone, Copy)]
pub struct ErrorCode {
    pub name: &'static str,
    pub docs: &'static str,
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

inventory::collect!(ErrorCode);

impl ErrorCode {
    /// Iterate over all error codes registered with [error_code].
    pub fn iter() -> impl Iterator<Item = &'static ErrorCode> {
        inventory::iter::<ErrorCode>()
    }
}

/// Create an error code, attaching its docs from the associated file in the
/// `docs` folder and registering it with [ErrorCode::iter].
#[macro_export]
macro_rules! error_code {
    ($id:ident) => {{
        static $id: ErrorCode = ErrorCode {
            name: stringify!($id),
            docs: include_str!(concat!(
                // TODO: assumes all crates are at the top level
                env!("CARGO_MANIFEST_DIR"),
                "/../docs/errors/",
                stringify!($id),
                ".md",
            )),
        };
        $crate::__inventory::submit!($id);
        &$id
    }};
}
