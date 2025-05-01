pub mod parser;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser as _;
use parser::{StarstreamProgram, starstream_program};

pub fn parse(source_code: &str) -> (StarstreamProgram, String) {
    let (ast, errors) = starstream_program().parse(source_code).into_output_errors();

    let mut report = Vec::new();
    errors.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, ((), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(e.to_string())
            .with_label(
                Label::new(((), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .write(Source::from(&source_code), &mut report)
            .unwrap()
    });

    (
        ast.unwrap_or_default(),
        String::from_utf8_lossy(&report).into_owned(),
    )
}

/// Highest-level one-shot compiler from Starstream source code to WASM binary.
pub fn starstream_to_wasm(source_code: &str) -> Result<Vec<u8>, String> {
    let (ast, errors) = starstream_program().parse(source_code).into_output_errors();

    errors.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, ((), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(e.to_string())
            .with_label(
                Label::new(((), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(&source_code))
            .unwrap()
    });

    dbg!(ast);

    todo!()
}
