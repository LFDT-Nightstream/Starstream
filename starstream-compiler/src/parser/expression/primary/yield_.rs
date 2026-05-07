use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives::identifier};

pub fn yield_<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("yield")
        .padded()
        .ignore_then(
            identifier()
                .separated_by(just(",").padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .map(|abis| Expr::Yield { abis })
        .spanned()
}
