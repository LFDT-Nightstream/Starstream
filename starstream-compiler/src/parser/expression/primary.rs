use chumsky::prelude::*;
use starstream_types::ast::{
    Block, EnumConstructorPayload, Expr, MatchArm, Spanned, StructLiteralField,
};

use crate::parser::{ParserExt, context::Extra, pattern, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let integer_literal = primitives::integer_literal().map(Expr::Literal).spanned();

    let boolean_literal = primitives::boolean_literal().map(Expr::Literal).spanned();

    let unit_literal = primitives::unit_literal().map(Expr::Literal).spanned();

    let identifier = primitives::identifier().map(Expr::Identifier).spanned();

    let grouping = expression
        .clone()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(Box::new)
        .map(Expr::Grouping)
        .spanned();

    let struct_literal = primitives::identifier()
        .then(
            struct_field_initializer(expression.clone())
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, fields)| Expr::StructLiteral { name, fields })
        .spanned();

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
        .map(|((enum_name, variant), payload)| Expr::EnumConstructor {
            enum_name,
            variant,
            payload,
        })
        .spanned();

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
        .map(|((first, mut rest), else_branch)| Expr::If {
            branches: {
                rest.insert(0, first);
                rest
            },
            else_branch: else_branch.map(Box::new),
        })
        .spanned();

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
        .map(|(scrutinee, arms)| Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
        .spanned();

    let block = block.map(Box::new).map(Expr::Block).spanned();

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
