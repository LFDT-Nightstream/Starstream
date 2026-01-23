use chumsky::prelude::*;
use starstream_types::ast::{EnumDef, EnumVariant, EnumVariantPayload, StructField};

use crate::parser::{comment, context::Extra, primitives, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, EnumDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    let tuple_payload = type_parser
        .clone()
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(EnumVariantPayload::Tuple);

    let struct_field = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map_with(|(name, ty), extra| StructField {
            name,
            ty,
            span: extra.span(),
        });

    let struct_payload = struct_field
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map(EnumVariantPayload::Struct);

    let payload = choice((struct_payload, tuple_payload))
        .or_not()
        .map(|p| p.unwrap_or(EnumVariantPayload::Unit))
        .boxed();

    // Collect comments for CommentMap.
    // WARNING: Must use .then() not .ignore_then() - chumsky optimizes away side effects in ignored parsers.
    let comments_padding = comment::comment_collecting()
        .padded()
        .repeated()
        .collect::<Vec<_>>();

    // Parse variant body: name with optional payload (boxed to allow cloning)
    let variant_body = primitives::identifier()
        .then(payload)
        .map_with(|(name, payload), extra| EnumVariant {
            name,
            payload,
            span: extra.span(),
        })
        .boxed();

    // Variant with mandatory comma (handles inline comments after comma)
    let variant_with_comma = comments_padding
        .clone()
        .then(variant_body.clone())
        .then(just(',').padded().then(comments_padding.clone()))
        .map(|((_, variant), _)| variant);

    // Last variant without trailing comma
    let last_variant = comments_padding
        .clone()
        .then(variant_body)
        .then(comments_padding)
        .map(|((_, variant), _)| variant);

    // Variants: zero or more with commas, then optionally one without
    let variants = variant_with_comma
        .repeated()
        .collect::<Vec<_>>()
        .then(last_variant.or_not())
        .map(|(mut variants, last)| {
            variants.extend(last);
            variants
        });

    just("enum")
        .padded()
        .ignore_then(primitives::identifier())
        .then(variants.delimited_by(just('{').padded(), just('}').padded()))
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
