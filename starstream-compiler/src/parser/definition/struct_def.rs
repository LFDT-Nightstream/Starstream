use chumsky::prelude::*;
use starstream_types::ast::{StructDef, StructField};

use crate::parser::{context::Extra, primitives, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, StructDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    let field = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map(|(name, ty)| StructField { name, ty });

    just("struct")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            field
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
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
