use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, expression::postfix::arguments};

pub fn emit<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("emit")
        .padded()
        // TODO: spec says any primary expression, we support only scoped names here
        .ignore_then(super::scoped_name_expr())
        .then(arguments(expression))
        .map(|(expr, args)| Expr::Emit {
            callee: Box::new(expr),
            args,
        })
        .spanned()
}

pub fn raise<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("raise")
        .padded()
        // TODO: spec says any primary expression, we support only scoped names here
        .ignore_then(super::scoped_name_expr())
        .then(arguments(expression))
        .map(|(expr, args)| Expr::Raise {
            callee: Box::new(expr),
            args,
        })
        .spanned()
}

pub fn runtime<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("runtime")
        .padded()
        // TODO: spec says any primary expression, we support only scoped names here
        .ignore_then(super::scoped_name_expr())
        .then(arguments(expression))
        .map(|(expr, args)| Expr::Runtime {
            callee: Box::new(expr),
            args,
        })
        .spanned()
}

#[cfg(test)]
mod tests {
    #[test]
    fn emit_no_args() {
        assert_expression_snapshot!(
            r#"
            // Emit ping
            emit Ping()
            "#
        );
    }

    #[test]
    fn emit_with_args() {
        assert_expression_snapshot!(
            r#"
            // Transfer event
            emit Transfer(sender, to, amount)
            "#
        );
    }

    #[test]
    fn raise_call() {
        assert_expression_snapshot!("raise blockHeight()");
    }

    #[test]
    fn raise_namespaced_call() {
        assert_expression_snapshot!("raise context::blockHeight()");
    }

    #[test]
    fn runtime_call() {
        assert_expression_snapshot!("runtime blockHeight()");
    }

    #[test]
    fn runtime_namespaced_call() {
        assert_expression_snapshot!("runtime context::blockHeight()");
    }
}
