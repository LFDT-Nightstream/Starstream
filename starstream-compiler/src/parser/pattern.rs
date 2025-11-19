use chumsky::{prelude::*, recursive::recursive};
use starstream_types::ast::{Pattern, StructPatternField};

use super::{context::Extra, primitives};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Pattern, Extra<'a>> {
    recursive(|pattern| {
        let identifier = primitives::identifier().boxed();

        let enum_variant = identifier
            .clone()
            .then_ignore(just("::").padded())
            .then(identifier.clone())
            .then(
                pattern
                    .clone()
                    .separated_by(just(',').padded())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just('(').padded(), just(')').padded())
                    .or_not(),
            )
            .map(|((enum_name, variant), payload)| Pattern::EnumVariant {
                enum_name,
                variant,
                payload: payload.unwrap_or_default(),
            });

        let struct_field = identifier
            .clone()
            .then_ignore(just(':').padded())
            .then(pattern.clone())
            .map(|(name, pattern)| StructPatternField {
                name,
                pattern: Box::new(pattern),
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

        choice((enum_variant, struct_pattern, binding)).padded()
    })
}
