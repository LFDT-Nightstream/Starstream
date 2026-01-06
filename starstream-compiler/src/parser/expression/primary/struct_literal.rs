use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned, StructLiteralField};

use crate::parser::{ParserExt, context::Extra, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    primitives::identifier()
        .then(
            field_initializer(expression)
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, fields)| Expr::StructLiteral { name, fields })
        .spanned()
}

pub fn field_initializer<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, StructLiteralField, Extra<'a>> {
    primitives::identifier()
        .then_ignore(just(':').padded())
        .then(expression)
        .map(|(name, value)| StructLiteralField { name, value })
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
    fn struct_literal_expression() {
        assert_expression_snapshot!(
            r#"
            Point {
                x: 10,
                y: value + 1,
            }
            "#
        );
    }
}
