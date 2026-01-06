use chumsky::prelude::*;
use starstream_types::ast::{EnumConstructorPayload, Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives};

use super::struct_literal::field_initializer;

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let tuple_payload = expression
        .clone()
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(EnumConstructorPayload::Tuple);

    let struct_payload = field_initializer(expression)
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map(EnumConstructorPayload::Struct);

    primitives::identifier()
        .then_ignore(just("::").padded())
        .then(primitives::identifier())
        .then(
            choice((struct_payload, tuple_payload))
                .or_not()
                .map(|payload| payload.unwrap_or(EnumConstructorPayload::Unit)),
        )
        .map(|((enum_name, variant), payload)| Expr::EnumConstructor {
            enum_name,
            variant,
            payload,
        })
        .spanned()
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::assert_expression_snapshot;

    #[test]
    fn enum_constructor_expression() {
        assert_expression_snapshot!("Result::Ok(answer)");
    }

    #[test]
    fn enum_constructor_unit() {
        assert_expression_snapshot!("Option::None");
    }
}
