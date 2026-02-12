use chumsky::prelude::*;
use starstream_types::{FunctionDef, FunctionExport, UtxoDef, UtxoGlobal, UtxoPart};

use crate::parser::{
    context::Extra, definition::function::function_with_body, primitives, type_annotation,
};

pub fn parser<'a>() -> impl Parser<'a, &'a str, UtxoDef, Extra<'a>> {
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

    let main_fn_part = just("main")
        .padded()
        .ignore_then(function_with_body())
        .map(|def| {
            UtxoPart::MainFn(FunctionDef {
                export: Some(FunctionExport::UtxoMain),
                ..def
            })
        });

    let part = choice((storage_part, main_fn_part));

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
            let parsed = parser()
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
