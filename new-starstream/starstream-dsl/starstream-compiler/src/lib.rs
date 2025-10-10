//! Compiler for the Starstream DSL
//!
//! This crate provides:
//! - A parser that translates source code to AST
//! - A compiler that translates AST to stack machine opcodes

pub mod parser;

use chumsky::Parser;
pub use parser::*;
use starstream_types::StarstreamProgram;

pub fn parse_program(source: &str) -> Result<StarstreamProgram, String> {
    parser::starstream_program().parse(source).into_result().map_err(|_| "Broken".to_string())
}
