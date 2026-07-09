use chumsky::{prelude::*, recursive::recursive};
use starstream_types::ast::{Pattern, StructPatternField};

use super::{context::Extra, primitives};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Pattern, Extra<'a>> {
    recursive(|pattern| {
        let identifier = primitives::identifier().boxed();

        // Wildcard pattern: `_`
        let wildcard = just('_')
            .map_with(|_, extra| Pattern::Wildcard { span: extra.span() })
            .padded();

        let integer_literal =
            primitives::integer_literal().map_with(|value, extra| Pattern::Literal {
                value,
                span: extra.span(),
            });

        let boolean_literal =
            primitives::boolean_literal().map_with(|value, extra| Pattern::Literal {
                value,
                span: extra.span(),
            });

        let unit_literal = primitives::unit_literal().map_with(|value, extra| Pattern::Literal {
            value,
            span: extra.span(),
        });

        let struct_field = identifier
            .clone()
            .then(just(':').padded().ignore_then(pattern.clone()).or_not())
            .map(|(name, pattern)| {
                let fallback = Pattern::Name(vec![name.clone()]);

                StructPatternField {
                    name,
                    pattern: Box::new(pattern.unwrap_or(fallback)),
                }
            });

        let struct_pattern = primitives::scoped_name()
            .clone()
            .then(
                struct_field
                    .separated_by(just(',').padded())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just('{').padded(), just('}').padded()),
            )
            .map(|(name, fields)| Pattern::Struct { name, fields });

        let tuple_pattern = primitives::scoped_name()
            .clone()
            .then(
                pattern
                    .separated_by(just(',').padded())
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just('(').padded(), just(')').padded()),
            )
            .map(|(name, fields)| Pattern::Tuple { name, fields });

        let binding = primitives::scoped_name().map(Pattern::Name);

        choice((
            wildcard,
            integer_literal,
            boolean_literal,
            unit_literal,
            struct_pattern,
            tuple_pattern,
            binding,
        ))
        .padded()
    })
}
