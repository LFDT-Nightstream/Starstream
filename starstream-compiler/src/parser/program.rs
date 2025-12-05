use chumsky::prelude::*;
use starstream_types::ast::Program;

use super::{context::Extra, definition};

/// Get a parser for a Starstream source file.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Program, Extra<'a>> {
    definition::parser()
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|definitions| Program { definitions })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_program_snapshot {
        ($code:expr) => {{
            let program = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("program should parse");

            insta::with_settings!({
                description => format!("Code:\n\n{}", indoc! { $code }),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_debug_snapshot!(program);
            });
        }};
    }

    #[test]
    fn single_function() {
        assert_program_snapshot!(
            r#"
            fn add(a: i64, b: i64) -> i64 {
                a + b
            }
            "#
        );
    }

    #[test]
    fn multiple_functions() {
        assert_program_snapshot!(
            r#"
            fn main() {
                let flag = true;
                if (flag) { return; } else { return; }
            }

            fn compute() -> i64 {
                let base = 41;
                base + 1
            }
            "#
        );
    }

    #[test]
    fn struct_definition() {
        assert_program_snapshot!(
            r#"
            struct Point {
                x: i64,
                y: i64,
            }
            "#
        );
    }

    #[test]
    fn enum_definition() {
        assert_program_snapshot!(
            r#"
            enum Message {
                Ping,
                Pong(i64),
            }
            "#
        );
    }

    #[test]
    fn utxo_storage() {
        assert_program_snapshot!(
            r#"
            utxo MyUtxo {
                storage {
                    let mut data: i64;
                }
            }
            "#
        );
    }
}
