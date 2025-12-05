use chumsky::prelude::*;
use starstream_types::ast::Program;

use super::{context::Extra, definition};

/// Get a parser for a Starstream source file.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Program, Extra<'a>> {
    definition::parser()
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|definitions| Program { definitions })
}

// NOTE: The test cases for the root program parser are included in the
// compiler integration tests, rather than separated out here.
