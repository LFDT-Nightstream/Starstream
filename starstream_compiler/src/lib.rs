//! Compiler from the Starstream language to WASM modules for the Starstream runtime.

pub mod ast;
mod codegen;
mod parser;
mod scope_resolution;
mod typechecking;

use self::ast::StarstreamProgram;
pub use self::codegen::compile;
pub use self::parser::starstream_program;
use ariadne::{Report, Source};
use chumsky::Parser as _;
pub use scope_resolution::do_scope_analysis;
pub use typechecking::do_type_inference;

pub fn write_errors(output: &mut Vec<u8>, source_code: &str, errors: &[Report]) {
    for report in errors {
        report
            .write(Source::from(source_code), &mut *output)
            .unwrap()
    }
}

pub fn format_errors(source_code: &str, errors: &[Report]) -> String {
    let mut output = Vec::new();
    write_errors(&mut output, source_code, errors);
    String::from_utf8_lossy(&output).into_owned()
}

pub fn parse(source_code: &str) -> (Option<StarstreamProgram>, Vec<Report>) {
    let (ast, errors) = starstream_program().parse(source_code).into_output_errors();
    let errors = errors.into_iter().map(parser::error_to_report).collect();
    (ast, errors)
}

/// Highest-level one-shot compiler from Starstream source code to WASM binary.
pub fn starstream_to_wasm(source_code: &str) -> Result<Vec<u8>, String> {
    let ast = match parse(source_code) {
        (Some(ast), _) => ast,
        (None, errors) => return Err(format_errors(source_code, &errors)),
    };

    let (ast, mut symbols) = match do_scope_analysis(ast) {
        Ok(ast) => ast,
        Err(errors) => return Err(format_errors(source_code, &errors)),
    };

    let ast = match do_type_inference(ast, &mut symbols) {
        Ok((ast, warnings)) => {
            format_errors(source_code, &warnings);

            ast
        }
        Err(errors) => return Err(format_errors(source_code, &errors)),
    };

    let module = match compile(&ast) {
        (Some(module), _) => module,
        (None, errors) => return Err(format_errors(source_code, &errors)),
    };
    Ok(module)
}
