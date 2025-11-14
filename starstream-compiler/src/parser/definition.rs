use chumsky::prelude::*;
use starstream_types::{
    FunctionExport,
    ast::{Definition, FunctionDef, FunctionParam},
};

use super::{context::Extra, primitives, statement, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Definition, Extra<'a>> {
    choice((function_definition().map(Definition::Function),))
}

fn function_definition<'a>() -> impl Parser<'a, &'a str, FunctionDef, Extra<'a>> {
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
