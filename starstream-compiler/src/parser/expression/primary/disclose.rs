use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("disclose")
        .padded()
        .ignore_then(expression.delimited_by(just('(').padded(), just(')').padded()))
        .map(|expr| Expr::Disclose {
            expr: Box::new(expr),
        })
        .spanned()
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::assert_expression_snapshot;

    #[test]
    fn disclose_identifier() {
        assert_expression_snapshot!("disclose(x)");
    }

    #[test]
    fn disclose_binary_expression() {
        assert_expression_snapshot!("disclose(a + b)");
    }
}
