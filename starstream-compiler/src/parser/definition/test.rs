use chumsky::prelude::*;
use starstream_types::{Block, TestDef};

use crate::parser::{
    context::{Extra, ParserInput},
    primitives,
};

pub fn test<'a>(
    block: impl Parser<'a, ParserInput<'a>, Block, Extra<'a>>,
) -> impl Parser<'a, ParserInput<'a>, TestDef, Extra<'a>> {
    text::keyword("test")
        .padded()
        .ignore_then(primitives::string_literal().or_not())
        .then(block)
        .map(|(description, body)| TestDef { description, body })
}
