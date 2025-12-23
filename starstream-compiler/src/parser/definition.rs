use chumsky::prelude::*;
use starstream_types::{
    AbiDef, AbiPart, EventDef, FunctionExport, UtxoDef, UtxoGlobal, UtxoPart,
    ast::{
        Definition, EnumDef, EnumVariant, EnumVariantPayload, FunctionDef, FunctionParam,
        ImportDef, ImportItems, ImportNamedItem, ImportSource, StructDef, StructField,
    },
};

use super::{context::Extra, primitives, statement, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, Definition, Extra<'a>> {
    choice((
        import_definition().map(Definition::Import),
        function_definition().map(Definition::Function),
        struct_definition().map(Definition::Struct),
        enum_definition().map(Definition::Enum),
        utxo_definition().map(Definition::Utxo),
        abi_definition().map(Definition::Abi),
    ))
}

fn import_definition<'a>() -> impl Parser<'a, &'a str, ImportDef, Extra<'a>> {
    let named_item = primitives::identifier()
        .then(
            just("as")
                .padded()
                .ignore_then(primitives::identifier())
                .or_not(),
        )
        .map(|(imported, alias)| ImportNamedItem {
            local: alias.unwrap_or_else(|| imported.clone()),
            imported,
        });

    let named_items = named_item
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map(ImportItems::Named);

    let namespace_import = primitives::identifier().map(ImportItems::Namespace);

    let import_source = primitives::identifier()
        .then_ignore(just(':'))
        .then(primitives::identifier())
        .then(just('/').ignore_then(primitives::identifier()).or_not())
        .map(|((namespace, package), interface)| ImportSource {
            namespace,
            package,
            interface,
        });

    just("import")
        .padded()
        .ignore_then(choice((named_items, namespace_import)))
        .then_ignore(just("from").padded())
        .then(import_source)
        .then_ignore(just(';').padded())
        .map(|(items, from)| ImportDef { items, from })
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
        .then(just("mut"))
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
        .map(UtxoPart::Storage);

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

fn abi_definition<'a>() -> impl Parser<'a, &'a str, AbiDef, Extra<'a>> {
    let abi_part = event_definition().map(AbiPart::Event);

    just("abi")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            abi_part
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, parts)| AbiDef { name, parts })
}

fn event_definition<'a>() -> impl Parser<'a, &'a str, EventDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    let parameter = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map(|(name, ty)| FunctionParam { name, ty });

    just("event")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            parameter
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then_ignore(just(';').padded())
        .map(|(name, params)| EventDef { name, params })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_definition_snapshot {
        ($code:expr) => {{
            let def = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("definition should parse");

            insta::with_settings!({
                description => format!("Code:\n\n{}", indoc! { $code }),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_debug_snapshot!(def);
            });
        }};
    }

    #[test]
    fn abi_empty() {
        assert_definition_snapshot!("abi Events {}");
    }

    #[test]
    fn abi_single_event() {
        assert_definition_snapshot!(
            r#"
            abi Events {
                event Transfer(from: i64, to: i64);
            }
            "#
        );
    }

    #[test]
    fn abi_multiple_events() {
        assert_definition_snapshot!(
            r#"
            abi Events {
                event Transfer(from: i64, to: i64, amount: i64);
                event Log(message: i64);
            }
            "#
        );
    }

    #[test]
    fn abi_event_no_params() {
        assert_definition_snapshot!(
            r#"
            abi Events {
                event Ping();
            }
            "#
        );
    }

    #[test]
    fn import_named() {
        assert_definition_snapshot!("import { blockHeight } from starstream:std/cardano;");
    }

    #[test]
    fn import_named_multiple() {
        assert_definition_snapshot!("import { blockHeight, now } from starstream:std/cardano;");
    }

    #[test]
    fn import_named_with_alias() {
        assert_definition_snapshot!(
            "import { blockHeight as height } from starstream:std/cardano;"
        );
    }

    #[test]
    fn import_namespace() {
        assert_definition_snapshot!("import context from starstream:std;");
    }
}
