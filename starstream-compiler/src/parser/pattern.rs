use chumsky::{prelude::*, recursive::recursive};
use starstream_types::ast::{EnumPatternPayload, Literal, Pattern, StructPatternField};

use super::{context::Extra, primitives};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Pattern, Extra<'a>> {
    recursive(|pattern| {
        let identifier = primitives::identifier().boxed();

        // Wildcard pattern: `_`
        let wildcard = just('_')
            .map_with(|_, extra| Pattern::Wildcard { span: extra.span() })
            .padded();

        // Integer literal pattern: `0`, `42`, etc.
        let integer_literal = text::int(10).map_with(|digits: &str, extra| {
            let value = digits.parse::<i64>().expect("integer literal");
            Pattern::Literal {
                value: Literal::Integer(value),
                span: extra.span(),
            }
        });

        // Boolean literal pattern: `true`, `false`
        let boolean_literal = choice((just("true").to(true), just("false").to(false)))
            .padded()
            .map_with(|value, extra| Pattern::Literal {
                value: Literal::Boolean(value),
                span: extra.span(),
            });

        // Unit literal pattern: `()`
        let unit_literal = just("()")
            .padded()
            .map_with(|_, extra| Pattern::Literal {
                value: Literal::Unit,
                span: extra.span(),
            });

        let struct_field = identifier
            .clone()
            .then(just(':').padded().ignore_then(pattern.clone()).or_not())
            .map(|(name, pattern)| {
                let fallback = Pattern::Binding(name.clone());

                StructPatternField {
                    name,
                    pattern: Box::new(pattern.unwrap_or(fallback)),
                }
            });

        let tuple_payload = pattern
            .clone()
            .separated_by(just(',').padded())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('(').padded(), just(')').padded())
            .map(EnumPatternPayload::Tuple);

        let struct_payload = struct_field
            .clone()
            .separated_by(just(',').padded())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('{').padded(), just('}').padded())
            .map(EnumPatternPayload::Struct);

        let enum_variant = identifier
            .clone()
            .then_ignore(just("::").padded())
            .then(identifier.clone())
            .then(
                choice((struct_payload, tuple_payload))
                    .or_not()
                    .map(|payload| payload.unwrap_or(EnumPatternPayload::Unit)),
            )
            .map(|((enum_name, variant), payload)| Pattern::EnumVariant {
                enum_name,
                variant,
                payload,
            });

        let struct_pattern = identifier
            .clone()
            .then(
                struct_field
                    .separated_by(just(',').padded())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just('{').padded(), just('}').padded()),
            )
            .map(|(name, fields)| Pattern::Struct { name, fields });

        let binding = identifier.map(Pattern::Binding);

        choice((
            wildcard,
            integer_literal,
            boolean_literal,
            unit_literal,
            enum_variant,
            struct_pattern,
            binding,
        ))
        .padded()
    })
}
