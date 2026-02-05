use chumsky::prelude::*;
use starstream_types::{
    FunctionExport, TypeAnnotation,
    ast::{FunctionDef, FunctionParam},
};

use crate::parser::{context::Extra, primitives, statement, type_annotation};

fn parameter<'a>() -> impl Parser<'a, &'a str, FunctionParam, Extra<'a>> {
    primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_annotation::parser().boxed())
        .map(|(name, ty)| FunctionParam { name, ty })
}

fn parameter_list<'a>() -> impl Parser<'a, &'a str, Vec<FunctionParam>, Extra<'a>> {
    parameter()
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
}

fn return_type<'a>() -> impl Parser<'a, &'a str, Option<TypeAnnotation>, Extra<'a>> {
    just("->")
        .padded()
        .ignore_then(type_annotation::parser().boxed())
        .or_not()
}

// Note: FunctionDef's export is not parsed, always None.
// Maybe splitting another struct out is cleaner long-term?
pub fn function_with_body<'a>() -> impl Parser<'a, &'a str, FunctionDef, Extra<'a>> {
    just("fn")
        .padded()
        .ignore_then(primitives::identifier())
        .then(parameter_list())
        .then(return_type())
        .then(statement::block_parser())
        .map(|(((name, params), return_type), body)| FunctionDef {
            export: None,
            name,
            params,
            return_type,
            body,
        })
}

/// Parse `function_definition` grammar node.
///
/// Like `script fn foo(bar: i32) -> i32 { bar }`.
pub fn parser<'a>() -> impl Parser<'a, &'a str, FunctionDef, Extra<'a>> {
    function_export()
        .padded()
        .or_not()
        .then(function_with_body())
        .map(|(export, func)| FunctionDef { export, ..func })
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
