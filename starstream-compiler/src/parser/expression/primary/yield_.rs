use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives::identifier};

pub fn yield_<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("yield")
        .padded()
        .ignore_then(
            identifier()
                .separated_by(just(",").padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .map(|abis| Expr::Yield { abis })
        .spanned()
}

#[cfg(test)]
mod tests {
    #[test]
    fn yield_no_abis() {
        assert_expression_snapshot!("yield()");
    }

    #[test]
    fn yield_trailing_comma() {
        assert_expression_snapshot!("yield(AbiA, AbiB,)");
    }
}
