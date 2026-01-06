use chumsky::prelude::*;
use starstream_types::{FunctionExport, ast::{FunctionDef, FunctionParam}};

use crate::parser::{context::Extra, primitives, statement, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, FunctionDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();
    let block_parser = statement::block_parser();

    let parameter = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser.clone())
        .map(|(name, ty)| FunctionParam { name, ty });

    function_export()
        .padded()
        .or_not()
        .then_ignore(just("fn"))
        .padded()
        .then(primitives::identifier())
        .then(
            parameter
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just("->").padded().ignore_then(type_parser).or_not())
        .then(block_parser)
        .map(
            |((((export, name), params), return_type), body)| FunctionDef {
                export,
                name,
                params,
                return_type,
                body,
            },
        )
}

fn function_export<'a>() -> impl Parser<'a, &'a str, FunctionExport, Extra<'a>> {
    choice((just("script").map(|_| FunctionExport::Script),))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_function_snapshot {
        ($code:expr) => {{
            let parsed = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("function should parse");

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
    fn function_simple() {
        assert_function_snapshot!("fn foo() {}");
    }

    #[test]
    fn function_with_params() {
        assert_function_snapshot!("fn add(a: i64, b: i64) -> i64 { a }");
    }

    #[test]
    fn function_script_export() {
        assert_function_snapshot!("script fn main() {}");
    }
}
