use chumsky::prelude::*;
use starstream_types::ast::Program;

use super::{context::Extra, statement};

/// Get a parser for a Starstream source file.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Program, Extra<'a>> {
    statement::parser()
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|statements| Program { statements })
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
    fn sequential_statements() {
        assert_program_snapshot!(
            r#"
            let count = 0;
            count = count + 1;
            false;
            "#
        );
    }

    #[test]
    fn conditional_program() {
        assert_program_snapshot!(
            r#"
            let flag = true;
            if (flag) { let answer = 42; } else { answer = 0; }
            "#
        );
    }
}
