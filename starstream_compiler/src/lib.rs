//! Compiler from the Starstream language to WASM modules for the Starstream runtime.

pub mod ast;
mod codegen;
mod parser;

use ariadne::Source;
use ast::StarstreamProgram;
use chumsky::Parser as _;
pub use codegen::compile;
pub use parser::starstream_program;

pub fn parse(source_code: &str) -> (StarstreamProgram, String) {
    let (ast, errors) = starstream_program().parse(source_code).into_output_errors();

    let mut report = Vec::new();
    for e in errors {
        parser::error_to_report(e)
            .write(Source::from(&source_code), &mut report)
            .unwrap()
    }

    (
        ast.unwrap_or_default(),
        String::from_utf8_lossy(&report).into_owned(),
    )
}

/// Highest-level one-shot compiler from Starstream source code to WASM binary.
pub fn starstream_to_wasm(source_code: &str) -> Result<Vec<u8>, String> {
    let (ast, errors) = starstream_program().parse(source_code).into_output_errors();

    for e in errors {
        parser::error_to_report(e)
            .eprint(Source::from(&source_code))
            .unwrap();
    }

    dbg!(&ast);

    codegen::compile(&ast.unwrap())
}
