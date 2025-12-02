use chumsky::prelude::*;
use starstream_types::{
    FunctionExport, UtxoDef, UtxoGlobal, UtxoPart,
    ast::{
        Definition, EnumDef, EnumVariant, EnumVariantPayload, FunctionDef, FunctionParam,
        StructDef, StructField,
    },
};

use super::{context::Extra, primitives, statement, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Definition, Extra<'a>> {
    choice((
        function_definition().map(Definition::Function),
        struct_definition().map(Definition::Struct),
        enum_definition().map(Definition::Enum),
        utxo_definition().map(Definition::Utxo),
    ))
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

fn struct_definition<'a>() -> impl Parser<'a, &'a str, StructDef, Extra<'a>> {
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

fn enum_definition<'a>() -> impl Parser<'a, &'a str, EnumDef, Extra<'a>> {
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

fn utxo_definition<'a>() -> impl Parser<'a, &'a str, UtxoDef, Extra<'a>> {
    let utxo_global = just("let")
        .padded()
        .ignore_then(primitives::identifier())
        .then(just(":").ignore_then(type_annotation::parser()))
        .then_ignore(just(';').padded())
        .map(|(name, ty)| UtxoGlobal { name, ty });

    let part = utxo_global
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(
            just("storage").padded().then(just('{')).padded(),
            just('}').padded(),
        )
        .map(|vars| UtxoPart::Storage(vars));

    just("utxo")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            part.repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, parts)| UtxoDef { name, parts })
}
