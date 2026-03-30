use chumsky::prelude::*;
use starstream_types::{AbiDef, AbiMethodDecl, AbiPart, EventDef, ast::FunctionParam};

use crate::parser::{comment, context::Extra, primitives, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, AbiDef, Extra<'a>> {
    // WARNING: Must use .then() not .ignore_then() — chumsky optimizes away side effects in ignored parsers.
    let comments_padding = comment::comment_collecting()
        .padded()
        .repeated()
        .collect::<Vec<_>>();

    let abi_part = comments_padding
        .clone()
        .then(choice((
            event_definition().map(AbiPart::Event),
            fn_decl_definition().map(AbiPart::FnDecl),
        )))
        .map(|(_, part)| part);

    just("abi")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            abi_part
                .repeated()
                .collect::<Vec<_>>()
                .then(comments_padding)
                .map(|(parts, _)| parts)
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, parts)| AbiDef { name, parts })
}

fn event_definition<'a>() -> impl Parser<'a, &'a str, EventDef, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    let parameter = just("pub")
        .padded()
        .or_not()
        .then(primitives::identifier())
        .then_ignore(just(':').padded())
        .then(type_parser)
        .map(|((public, name), ty)| FunctionParam {
            public: public.is_some(),
            name,
            ty,
        });

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
        .map_with(|(name, params), extra| EventDef {
            name,
            params,
            span: extra.span(),
        })
}

fn fn_decl_definition<'a>() -> impl Parser<'a, &'a str, AbiMethodDecl, Extra<'a>> {
    let type_parser = type_annotation::parser().boxed();

    let parameter = primitives::identifier()
        .then_ignore(just(':').padded())
        .then(type_parser.clone())
        .map(|(name, ty)| FunctionParam {
            public: false,
            name,
            ty,
        });

    just("fn")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            parameter
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(just("->").padded().ignore_then(type_parser).or_not())
        .then_ignore(just(';').padded())
        .map_with(|((name, params), return_type), extra| AbiMethodDecl {
            name,
            params,
            return_type,
            span: extra.span(),
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_abi_snapshot {
        ($code:expr) => {{
            let parsed = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("abi should parse");

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
    fn abi_empty() {
        assert_abi_snapshot!("abi Events {}");
    }

    #[test]
    fn abi_single_event() {
        assert_abi_snapshot!(
            r#"
            abi Events {
                event Transfer(sender: i64, to: i64);
            }
            "#
        );
    }

    #[test]
    fn abi_multiple_events() {
        assert_abi_snapshot!(
            r#"
            abi Events {
                event Transfer(sender: i64, to: i64, amount: i64);
                event Log(message: i64);
            }
            "#
        );
    }

    #[test]
    fn abi_event_no_params() {
        assert_abi_snapshot!(
            r#"
            abi Events {
                event Ping();
            }
            "#
        );
    }

    #[test]
    fn abi_event_with_public_param() {
        assert_abi_snapshot!(
            r#"
            abi Events {
                event Transfer(pub sender: i64, to: i64);
            }
            "#
        );
    }

    #[test]
    fn abi_with_method() {
        assert_abi_snapshot!(
            r#"
            abi TokenAbi {
                fn transfer(amount: i64) -> bool;
            }
            "#
        );
    }

    #[test]
    fn abi_mixed_events_and_methods() {
        assert_abi_snapshot!(
            r#"
            abi TokenAbi {
                event Transfer(sender: i64, to: i64, amount: i64);
                fn transfer(amount: i64) -> bool;
                fn balance() -> i64;
            }
            "#
        );
    }

    #[test]
    fn abi_method_no_return_type() {
        assert_abi_snapshot!(
            r#"
            abi MyAbi {
                fn doSomething(x: i64);
            }
            "#
        );
    }
}
