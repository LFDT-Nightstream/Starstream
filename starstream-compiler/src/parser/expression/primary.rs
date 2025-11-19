use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, Literal, MatchArm, Spanned, StructLiteralField};

use crate::parser::{context::Extra, pattern, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block_parser: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let integer = text::int(10).map_with(|digits: &str, extra| {
        let value = digits.parse::<i64>().expect("integer literal");

        Spanned::new(Expr::Literal(Literal::Integer(value)), extra.span())
    });

    let boolean = choice((just("true").to(true), just("false").to(false)))
        .padded()
        .map_with(|value, extra| {
            Spanned::new(Expr::Literal(Literal::Boolean(value)), extra.span())
        });

    let identifier = primitives::identifier()
        .map_with(|ident, extra| Spanned::new(Expr::Identifier(ident), extra.span()));

    let grouping = expression
        .clone()
        .delimited_by(just('(').padded(), just(')').padded())
        .map_with(|inner, extra| Spanned::new(Expr::Grouping(Box::new(inner)), extra.span()));

    let struct_literal = primitives::identifier()
        .then(
            struct_field_initializer(expression.clone())
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map_with(|(name, fields), extra| {
            Spanned::new(Expr::StructLiteral { name, fields }, extra.span())
        });

    let enum_constructor = primitives::identifier()
        .then_ignore(just("::").padded())
        .then(primitives::identifier())
        .then(
            expression
                .clone()
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded())
                .or_not(),
        )
        .map_with(|((enum_name, variant), payload), extra| {
            Spanned::new(
                Expr::EnumConstructor {
                    enum_name,
                    variant,
                    payload: payload.unwrap_or_default(),
                },
                extra.span(),
            )
        });

    let pattern_parser = pattern::parser();
    let match_arm = pattern_parser
        .then_ignore(just("=>").padded())
        .then(block_parser)
        .map(|(pattern, body)| MatchArm { pattern, body });

    let match_expression = just("match")
        .padded()
        .ignore_then(expression.clone())
        .then(
            match_arm
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map_with(|(scrutinee, arms), extra| {
            Spanned::new(
                Expr::Match {
                    scrutinee: Box::new(scrutinee),
                    arms,
                },
                extra.span(),
            )
        });

    choice((
        match_expression,
        struct_literal,
        enum_constructor,
        grouping,
        integer,
        boolean,
        identifier,
    ))
}

fn struct_field_initializer<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, StructLiteralField, Extra<'a>> {
    primitives::identifier()
        .then_ignore(just(':').padded())
        .then(expression)
        .map(|(name, value)| StructLiteralField { name, value })
}
