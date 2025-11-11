use chumsky::prelude::*;
use starstream_types::ast::{BinaryOp, Expr, Spanned};

use crate::parser::context::Extra;

use super::chain;

pub fn parser<'a>(
    lower: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let operator = choice((
        just("==").to(BinaryOp::Equal),
        just("!=").to(BinaryOp::NotEqual),
    ))
    .padded();

    chain(lower, operator)
}
