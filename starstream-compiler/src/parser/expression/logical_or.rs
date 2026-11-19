use chumsky::prelude::*;
use starstream_types::ast::{BinaryOp, Expr, Spanned};

use crate::parser::context::{Extra, ParserInput};

use super::chain;

pub fn parser<'a>(
    lower: impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> {
    let operator = just("||").to(BinaryOp::Or).padded();

    chain(lower, operator)
}
