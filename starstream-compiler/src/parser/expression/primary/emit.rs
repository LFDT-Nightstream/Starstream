use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("emit")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            expression
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .map(|(event, args)| Expr::Emit { event, args })
        .spanned()
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::assert_expression_snapshot;

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
}
