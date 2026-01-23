use chumsky::prelude::*;
use starstream_types::ast::{StructDef, StructField};

use crate::parser::{comment, context::Extra, primitives, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, StructDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    // Collect comments for CommentMap.
    // WARNING: Must use .then() not .ignore_then() - chumsky optimizes away side effects in ignored parsers.
    let comments_padding = comment::comment_collecting()
        .padded()
        .repeated()
        .collect::<Vec<_>>();

    // Parse field body: name: type (boxed to allow cloning)
    let field_body = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map_with(|(name, ty), extra| StructField {
            name,
            ty,
            span: extra.span(),
        })
        .boxed();

    // Field with mandatory comma (handles inline comments after comma)
    let field_with_comma = comments_padding
        .clone()
        .then(field_body.clone())
        .then(just(',').padded().then(comments_padding.clone()))
        .map(|((_, field), _)| field);

    // Last field without trailing comma
    let last_field = comments_padding
        .clone()
        .then(field_body)
        .then(comments_padding)
        .map(|((_, field), _)| field);

    // Fields: zero or more with commas, then optionally one without
    let fields = field_with_comma
        .repeated()
        .collect::<Vec<_>>()
        .then(last_field.or_not())
        .map(|(mut fields, last)| {
            fields.extend(last);
            fields
        });

    just("struct")
        .padded()
        .ignore_then(primitives::identifier())
        .then(fields.delimited_by(just('{').padded(), just('}').padded()))
        .map(|(name, fields)| StructDef { name, fields })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_struct_snapshot {
        ($code:expr) => {{
            let parsed = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("struct should parse");

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
    fn struct_single_field() {
        assert_struct_snapshot!("struct Point { x: i64 }");
    }

    #[test]
    fn struct_multiple_fields() {
        assert_struct_snapshot!("struct Point { x: i64, y: i64 }");
    }
}
