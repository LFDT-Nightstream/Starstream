use chumsky::prelude::*;
use starstream_types::{AbiDef, AbiPart, EventDef, ast::FunctionParam};

use crate::parser::{context::Extra, primitives, type_annotation};

pub fn parser<'a>() -> impl Parser<'a, &'a str, AbiDef, Extra<'a>> {
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
}
