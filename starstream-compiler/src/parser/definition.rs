use chumsky::prelude::*;
use starstream_types::{
    FunctionExport,
    ast::{
        ContractDef, Definition, EnumDef, EnumVariant, EnumVariantPayload, FunctionDef,
        FunctionParam, StructDef, StructField,
    },
};

use super::{context::Extra, primitives, statement, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Definition, Extra<'a>> {
    recursive(|def| {
        choice((
            function_definition().map(Definition::Function),
            struct_definition().map(Definition::Struct),
            enum_definition().map(Definition::Enum),
            contract_definition(def).map(Definition::Contract),
        ))
    })
}

fn contract_definition<'a>(
    definition_parser: impl Parser<'a, &'a str, Definition, Extra<'a>> + Clone,
) -> impl Parser<'a, &'a str, ContractDef, Extra<'a>> + Clone {
    just("contract")
        .padded()
        .ignore_then(
            definition_parser
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|definitions| ContractDef { definitions })
}

fn function_definition<'a>() -> impl Parser<'a, &'a str, FunctionDef, Extra<'a>> + Clone {
    let type_parser = type_annotation::parser().boxed();
    let block_parser = statement::block_parser().boxed();
    let identifier = primitives::identifier().boxed();

    let parameter = identifier
        .clone()
        .then_ignore(just(':').padded())
        .then(type_parser.clone())
        .map(|(name, ty)| FunctionParam { name, ty });

    function_export()
        .padded()
        .or_not()
        .then_ignore(just("fn"))
        .padded()
        .then(identifier)
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

fn function_export<'a>() -> impl Parser<'a, &'a str, FunctionExport, Extra<'a>> + Clone {
    choice((just("script").map(|_| FunctionExport::Script),))
}

fn struct_definition<'a>() -> impl Parser<'a, &'a str, StructDef, Extra<'a>> + Clone {
    let type_parser = type_annotation::parser().boxed();
    let identifier = primitives::identifier().boxed();

    let field = identifier
        .clone()
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map(|(name, ty)| StructField { name, ty });

    just("struct")
        .padded()
        .ignore_then(identifier)
        .then(
            field
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, fields)| StructDef { name, fields })
}

fn enum_definition<'a>() -> impl Parser<'a, &'a str, EnumDef, Extra<'a>> + Clone {
    let type_parser = type_annotation::parser().boxed();
    let identifier = primitives::identifier().boxed();

    let tuple_payload = type_parser
        .clone()
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(EnumVariantPayload::Tuple);

    let struct_payload = identifier
        .clone()
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

    let variant = identifier
        .clone()
        .then(payload)
        .map(|(name, payload)| EnumVariant { name, payload });

    just("enum")
        .padded()
        .ignore_then(identifier)
        .then(
            variant
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, variants)| EnumDef { name, variants })
}
