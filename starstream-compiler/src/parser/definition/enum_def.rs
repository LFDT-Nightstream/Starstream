use chumsky::prelude::*;
use starstream_types::ast::{EnumDef, EnumVariant, EnumVariantPayload, StructField};

use crate::parser::{context::Extra, primitives, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, EnumDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    let tuple_payload = type_parser
        .clone()
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(EnumVariantPayload::Tuple);

    let struct_payload = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map(|(name, ty)| StructField { name, ty })
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map(EnumVariantPayload::Struct);

    let payload = choice((struct_payload, tuple_payload))
        .or_not()
        .map(|payload| payload.unwrap_or(EnumVariantPayload::Unit));

    let variant = primitives::identifier()
        .then(payload)
        .map(|(name, payload)| EnumVariant { name, payload });

    just("enum")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            variant
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, variants)| EnumDef { name, variants })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_enum_snapshot {
        ($code:expr) => {{
            let parsed = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("enum should parse");

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
    fn enum_unit_variants() {
        assert_enum_snapshot!("enum Color { Red, Green, Blue }");
    }

    #[test]
    fn enum_tuple_variants() {
        assert_enum_snapshot!("enum Option { Some(i64), None }");
    }

    #[test]
    fn enum_struct_variants() {
        assert_enum_snapshot!("enum Result { Ok { value: i64 }, Err { msg: i64 } }");
    }
}
