use chumsky::prelude::*;
use starstream_types::ast::{
    Block, EnumConstructorPayload, Expr, MatchArm, Spanned, StructLiteralField,
};

use crate::parser::{context::Extra, pattern, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let integer_literal = primitives::integer_literal()
        .map_with(|lit, extra| Spanned::new(Expr::Literal(lit), extra.span()));

    let boolean_literal = primitives::boolean_literal()
        .map_with(|lit, extra| Spanned::new(Expr::Literal(lit), extra.span()));

    let unit_literal = primitives::unit_literal()
        .map_with(|lit, extra| Spanned::new(Expr::Literal(lit), extra.span()));

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

    let tuple_constructor_payload = expression
        .clone()
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(EnumConstructorPayload::Tuple);

    let struct_constructor_payload = struct_field_initializer(expression.clone())
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map(EnumConstructorPayload::Struct);

    let enum_constructor = primitives::identifier()
        .then_ignore(just("::").padded())
        .then(primitives::identifier())
        .then(
            choice((struct_constructor_payload, tuple_constructor_payload))
                .or_not()
                .map(|payload| payload.unwrap_or(EnumConstructorPayload::Unit)),
        )
        .map_with(|((enum_name, variant), payload), extra| {
            Spanned::new(
                Expr::EnumConstructor {
                    enum_name,
                    variant,
                    payload,
                },
                extra.span(),
            )
        });

    let if_expression = just("if")
        .padded()
        .ignore_then(
            expression
                .clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(block.clone())
        .then(
            just("else")
                .padded()
                .then(just("if"))
                .padded()
                .ignore_then(
                    expression
                        .clone()
                        .delimited_by(just('(').padded(), just(')').padded()),
                )
                .then(block.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then(just("else").padded().ignore_then(block.clone()).or_not())
        .map_with(|((first, mut rest), else_branch), extra| {
            Spanned::new(
                Expr::If {
                    branches: {
                        rest.insert(0, first);
                        rest
                    },
                    else_branch: else_branch.map(Box::new),
                },
                extra.span(),
            )
        });

    let pattern_parser = pattern::parser();
    let match_arm = pattern_parser
        .then_ignore(just("=>").padded())
        .then(block.clone())
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

    let block =
        block.map_with(|block, extra| Spanned::new(Expr::Block(Box::new(block)), extra.span()));

    choice((
        grouping,
        integer_literal,
        boolean_literal,
        unit_literal,
        struct_literal,
        enum_constructor,
        block,
        if_expression,
        match_expression,
        // Identifier last to prefer struct literals if possible?
        // TODO: `match x {` ambiguity resolution might not be what we want.
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
