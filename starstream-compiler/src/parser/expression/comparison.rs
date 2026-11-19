use chumsky::prelude::*;
use starstream_types::ast::{BinaryOp, Expr, Spanned};

use crate::parser::context::{Extra, ParserInput};

use super::chain;

pub fn parser<'a>(
    lower: impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> {
    let operator = choice((
        just("<=").to(BinaryOp::LessEqual),
        just('<').to(BinaryOp::Less),
        just(">=").to(BinaryOp::GreaterEqual),
        just('>').to(BinaryOp::Greater),
    ))
    .padded();

    chain(lower, operator)
}
