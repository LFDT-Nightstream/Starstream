use chumsky::prelude::*;
use starstream_types::{Block, FunctionDef, FunctionExport, UtxoDef, UtxoGlobal, UtxoPart};

use crate::parser::{
    context::Extra,
    definition::function::function,
    primitives::{self, identifier},
    type_annotation,
};

pub fn utxo<'a>(
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone,
) -> impl Parser<'a, &'a str, UtxoDef, Extra<'a>> {
    let utxo_global = just("let")
        .padded()
        .then(just("mut"))
        .padded()
        .ignore_then(primitives::identifier())
        .then(just(":").ignore_then(type_annotation::parser()))
        .then_ignore(just(';').padded())
        .map(|(name, ty)| UtxoGlobal { name, ty });

    let storage_part = utxo_global
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(
            just("storage").padded().then(just('{')).padded(),
            just('}').padded(),
        )
        .map(UtxoPart::Storage);

    let fn_part = just("main")
        .padded()
        .or_not()
        .then(function(block.clone()))
        .map(|(main, def)| {
            UtxoPart::Function(
                FunctionDef {
                    export: main.map(|_| FunctionExport::UtxoMain),
                    ..def
                }
                .into(),
            )
        });

    let abi_impl_part = just("impl")
        .padded()
        .ignore_then(identifier())
        .padded()
        .then(
            function(block)
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(abi, parts)| UtxoPart::AbiImpl { abi, parts });

    let part = choice((storage_part, fn_part, abi_impl_part));

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

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_utxo_snapshot {
        ($code:expr) => {{
            let (_, block, _) = $crate::parser::recursives();
            let parsed = utxo(block)
                .parse(indoc! { $code })
                .into_result()
                .expect("utxo should parse");

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
    fn utxo_empty() {
        assert_utxo_snapshot!("utxo Counter {}");
    }

    #[test]
    fn utxo_with_storage() {
        assert_utxo_snapshot!(
            r#"
            utxo Counter {
                storage {
                    let mut count: i64;
                }
            }
            "#
        );
    }
}
