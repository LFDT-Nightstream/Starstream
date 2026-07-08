use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned, StructFieldInitializer};

use crate::parser::{ParserExt, context::Extra, expression::primary::scoped_name, primitives};

pub fn struct_constructor<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    scoped_name()
        .then(
            field_initializer(expression)
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, fields)| Expr::StructConstructor { name, fields })
        .spanned()
}

pub fn field_initializer<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, StructFieldInitializer, Extra<'a>> {
    primitives::identifier()
        .then_ignore(just(':').padded())
        .then(expression)
        .map(|(name, value)| StructFieldInitializer { name, value })
}

#[cfg(test)]
mod tests {
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
