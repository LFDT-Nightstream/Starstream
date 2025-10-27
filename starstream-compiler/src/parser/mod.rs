mod context;
mod expression;
mod primitives;
mod program;
mod statement;

pub use expression::parser as expression;
pub use program::parser as program;
pub use statement::parser as statement;

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use starstream_types::ast::Program;

pub fn error_to_report(e: Rich<char>) -> Report {
    Report::build(ReportKind::Error, e.span().into_range())
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(e.to_string())
        .with_label(
            Label::new(e.span().into_range())
                .with_message(e.reason().to_string())
                .with_color(Color::Red),
        )
        .finish()
}

pub fn parse_program<'a>(source: &'a str) -> ParseResult<Program, Rich<'a, char>> {
    let mut state = context::State::new();

    program().parse_with_state(source, &mut state)
}
