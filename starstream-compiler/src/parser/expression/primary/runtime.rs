use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("runtime")
        .padded()
        .ignore_then(expression)
        .map(|expr| Expr::Runtime {
            expr: Box::new(expr),
        })
        .spanned()
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::assert_expression_snapshot;

    #[test]
    fn runtime_call() {
        assert_expression_snapshot!("runtime blockHeight()");
    }

    #[test]
    fn runtime_namespaced_call() {
        assert_expression_snapshot!("runtime context::blockHeight()");
    }
}
