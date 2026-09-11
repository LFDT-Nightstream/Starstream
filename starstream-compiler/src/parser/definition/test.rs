use chumsky::prelude::*;
use starstream_types::{Block, TestDef};

use crate::parser::{context::Extra, primitives};

pub fn test<'a>(
    block: impl Parser<'a, &'a str, Block, Extra<'a>>,
) -> impl Parser<'a, &'a str, TestDef, Extra<'a>> {
    just("test")
        .padded()
        .ignore_then(primitives::string_literal().or_not())
        .then(block)
        .map(|(description, body)| TestDef { description, body })
}
