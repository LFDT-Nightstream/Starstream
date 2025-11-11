use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned, UnaryOp};

use crate::parser::context::Extra;

pub fn parser<'a>(
    base: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    recursive(|unary_parser| {
        let negate = just('-')
            .padded()
            .ignore_then(unary_parser.clone())
            .map_with(|expr, extra| {
                Spanned::new(
                    Expr::Unary {
                        op: UnaryOp::Negate,
                        expr: Box::new(expr),
                    },
                    extra.span(),
                )
            });

        let not = just('!')
            .padded()
            .ignore_then(unary_parser)
            .map_with(|expr, extra| {
                Spanned::new(
                    Expr::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                    extra.span(),
                )
            });

        choice((negate, not, base))
    })
}
