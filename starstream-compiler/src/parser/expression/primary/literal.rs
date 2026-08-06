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

#[cfg(test)]
mod tests {
    #[test]
    fn integer_literal() {
        assert_expression_snapshot!("42");
    }

    #[test]
    fn integer_literal_hex() {
        assert_expression_snapshot!("0xDeadBeef");
    }

    #[test]
    fn integer_literal_octal() {
        assert_expression_snapshot!("0o755");
    }

    #[test]
    fn integer_literal_binary() {
        assert_expression_snapshot!("0b1010");
    }

    #[test]
    fn integer_literal_huge_does_not_panic() {
        // Digits overflowing `i128` must parse; range checking is the type
        // checker's job.
        assert_expression_snapshot!("170141183460469231731687303715884105728");
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
