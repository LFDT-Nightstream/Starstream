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
    use crate::parser::expression;
    use chumsky::prelude::*;
    use indoc::indoc;

    macro_rules! assert_expression_snapshot {
        ($code:expr) => {{
            let parsed = expression::parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("expression should parse");

            insta::with_settings!({
                description => format!("Code:\n\n{}", indoc! { $code }),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_debug_snapshot!(parsed);
            });
        }};
    }

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
