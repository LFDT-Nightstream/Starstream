use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives};

pub fn integer<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    primitives::integer_literal().map(Expr::Literal).spanned()
}

pub fn boolean<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    primitives::boolean_literal().map(Expr::Literal).spanned()
}

pub fn unit<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    primitives::unit_literal().map(Expr::Literal).spanned()
}

pub fn identifier<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    primitives::identifier().map(Expr::Identifier).spanned()
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::assert_expression_snapshot;

    #[test]
    fn integer_literal() {
        assert_expression_snapshot!("42");
    }

    #[test]
    fn boolean_literal() {
        assert_expression_snapshot!("true");
    }

    #[test]
    fn unit_literal() {
        assert_expression_snapshot!("()");
    }

    #[test]
    fn identifier() {
        assert_expression_snapshot!("foo");
    }
}
