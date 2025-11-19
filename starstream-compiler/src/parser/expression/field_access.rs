use chumsky::{prelude::*, span::SimpleSpan};
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{context::Extra, primitives};

pub fn parser<'a>(
    lower: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let field = just('.').padded().ignore_then(primitives::identifier());

    lower.foldl(field.repeated(), |target, field| {
        let start = target.span.start;
        let end = field.span.map(|span| span.end).unwrap_or(target.span.end);
        let span = SimpleSpan::new((), start..end);

        Spanned::new(
            Expr::FieldAccess {
                target: Box::new(target),
                field,
            },
            span,
        )
    })
}
