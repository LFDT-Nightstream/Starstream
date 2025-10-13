use chumsky::prelude::*;
use starstream_types::ast::Program;

use super::{context::Extra, statement};

/// Get a parser for a Starstream source file.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Program, Extra<'a>> {
    statement::parser()
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|statements| Program { statements })
}
