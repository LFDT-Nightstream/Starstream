use chumsky::prelude::*;
use starstream_types::ast::Program;

use crate::parser::ParserExt;

use super::{
    comment,
    context::{Extra, ParserInput},
    definition,
};

/// Get a parser for a Starstream source file.
pub fn parser<'a>() -> impl Parser<'a, ParserInput<'a>, Program, Extra<'a>> {
    comment::shebang()
        .or_not()
        .then(
            definition()
                .spanned()
                .padded()
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(end())
        .map(|(shebang, definitions)| Program {
            shebang,
            definitions,
        })
}

// NOTE: The test cases for the root program parser are included in the
// compiler integration tests, rather than separated out here.
