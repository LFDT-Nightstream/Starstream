use chumsky::prelude::*;
use starstream_types::{Block, FunctionDef, FunctionExport, TokenDef, TokenGlobal, TokenPart};

use crate::parser::{
    context::Extra,
    definition::function::function,
    primitives::{self, identifier},
    type_annotation,
};

pub fn token<'a>(
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone,
) -> impl Parser<'a, &'a str, TokenDef, Extra<'a>> {
    let token_global = just("indexed")
        .padded()
        .or_not()
        .then(primitives::identifier())
        .then(just(":").ignore_then(type_annotation::parser()))
        .map(|((indexed, name), ty)| TokenGlobal {
            indexed: indexed.is_some(),
            name,
            ty,
        });

    let storage_part = token_global
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(
            just("storage").padded().then(just('{')).padded(),
            just('}').padded(),
        )
        .map(TokenPart::Storage);

    let fn_part = choice((
        just("mint").to(FunctionExport::TokenMint),
        just("burn").to(FunctionExport::TokenBurn),
    ))
    .padded()
    .or_not()
    .then(function(block.clone()))
    .map(|(modifier, def)| {
        TokenPart::Function(
            FunctionDef {
                export: modifier,
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
        .map(|(abi, parts)| TokenPart::AbiImpl { abi, parts });

    let part = choice((storage_part, fn_part, abi_impl_part));

    just("token")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            part.repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, parts)| TokenDef { name, parts })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_token_snapshot {
        ($code:expr) => {{
            let (_, block, _) = $crate::parser::recursives();
            let parsed = token(block)
                .parse(indoc! { $code })
                .into_result()
                .expect("token should parse");

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
    fn token_empty() {
        assert_token_snapshot!("token MyToken {}");
    }

    #[test]
    fn token_with_indexed_storage() {
        assert_token_snapshot!(
            r#"
            token MyToken {
                storage {
                    indexed my_arbitrary_variable: u32,
                    plain_variable: u8,
                }
            }
            "#
        );
    }

    #[test]
    fn token_with_mint_burn() {
        assert_token_snapshot!(
            r#"
            token MyToken {
                mint fn my_mint_fn() {}
                burn fn my_burn_fn() {}
            }
            "#
        );
    }

    #[test]
    fn token_with_impl() {
        assert_token_snapshot!(
            r#"
            token MyToken {
                impl Token {
                    fn attach(to: Utxo) {}
                    fn detach(source: Utxo) {}
                }
            }
            "#
        );
    }

    #[test]
    fn token_full() {
        assert_token_snapshot!(
            r#"
            token MyToken {
                storage {
                    indexed my_arbitrary_variable: u32,
                }
                mint fn my_mint_fn() {}
                burn fn my_burn_fn() {}
                impl Token {
                    fn attach(to: Utxo) {}
                    fn detach(source: Utxo) {}
                }
                fn helper() {}
            }
            "#
        );
    }
}
