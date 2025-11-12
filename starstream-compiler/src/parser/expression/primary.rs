use chumsky::prelude::*;
use starstream_types::ast::{Expr, Literal, Spanned};

use crate::parser::{context::Extra, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>>,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let integer = text::int(10).map_with(|digits: &str, extra| {
        let value = digits.parse::<i64>().expect("integer literal");

        Spanned::new(Expr::Literal(Literal::Integer(value)), extra.span())
    });

    let boolean = choice((just("true").to(true), just("false").to(false)))
        .padded()
        .map_with(|value, extra| {
            Spanned::new(Expr::Literal(Literal::Boolean(value)), extra.span())
        });

    let identifier = primitives::identifier()
        .map_with(|ident, extra| Spanned::new(Expr::Identifier(ident), extra.span()));

    let grouping = expression
        .delimited_by(just('(').padded(), just(')').padded())
        .map_with(|inner, extra| Spanned::new(Expr::Grouping(Box::new(inner)), extra.span()));

    choice((grouping, integer, boolean, identifier))
}
