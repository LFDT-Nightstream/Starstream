use chumsky::{prelude::*, span::SimpleSpan};
use starstream_types::ast::{BinaryOp, Expr, Spanned};

use super::context::Extra;

mod additive;
mod comparison;
mod equality;
mod logical_and;
mod logical_or;
mod multiplicative;
mod primary;
mod unary;

pub fn parser<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    recursive(|expression| {
        let primary = primary::parser(expression.clone()).boxed();
        let unary = unary::parser(primary).boxed();
        let multiplicative = multiplicative::parser(unary).boxed();
        let additive = additive::parser(multiplicative).boxed();
        let comparison = comparison::parser(additive).boxed();
        let equality = equality::parser(comparison).boxed();
        let logical_and = logical_and::parser(equality).boxed();
        logical_or::parser(logical_and).boxed()
    })
    .boxed()
}

pub(super) fn chain<'a>(
    lower: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    operator: impl Parser<'a, &'a str, BinaryOp, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let tail = operator.then(lower.clone());

    lower
        .clone()
        .foldl(tail.repeated(), |lhs, (op, rhs)| combine(lhs, op, rhs))
}

fn combine(lhs: Spanned<Expr>, op: BinaryOp, rhs: Spanned<Expr>) -> Spanned<Expr> {
    let span = span_union(lhs.span, rhs.span);
    Spanned::new(
        Expr::Binary {
            op,
            left: Box::new(lhs),
            right: Box::new(rhs),
        },
        span,
    )
}

fn span_union(a: SimpleSpan, b: SimpleSpan) -> SimpleSpan {
    let start = a.start.min(b.start);
    let end = a.end.max(b.end);
    SimpleSpan::new((), start..end)
}
