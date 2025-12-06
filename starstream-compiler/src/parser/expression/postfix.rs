use chumsky::{prelude::*, span::SimpleSpan};
use starstream_types::ast::{Expr, Identifier, Spanned};

use crate::parser::{context::Extra, primitives};

enum Suffix {
    FieldAccess(Identifier),
    Call(Vec<Spanned<Expr>>),
}

pub fn parser<'a>(
    lower: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let field_access = just('.')
        .padded()
        .ignore_then(primitives::identifier())
        .map(Suffix::FieldAccess);

    let call = expression
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(Suffix::Call);

    let suffix = choice((field_access, call));

    lower
        .clone()
        .foldl_with(suffix.repeated(), |target, suffix, extra| {
            let start = target.span.start;
            let end = extra.span().end;
            let span = SimpleSpan::new((), start..end);

            match suffix {
                Suffix::FieldAccess(field) => Spanned::new(
                    Expr::FieldAccess {
                        target: Box::new(target),
                        field,
                    },
                    span,
                ),
                Suffix::Call(args) => Spanned::new(
                    Expr::Call {
                        callee: Box::new(target),
                        args,
                    },
                    span,
                ),
            }
        })
}
