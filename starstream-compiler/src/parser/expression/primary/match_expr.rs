use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, MatchArm, Spanned};

use crate::parser::{ParserExt, context::Extra, pattern};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let pattern_parser = pattern::parser();
    let match_arm = pattern_parser
        .then_ignore(just("=>").padded())
        .then(block)
        .map(|(pattern, body)| MatchArm { pattern, body });

    just("match")
        .padded()
        .ignore_then(expression)
        .then(
            match_arm
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(scrutinee, arms)| Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
        .spanned()
}

#[cfg(test)]
mod tests {
    use crate::parser::expression;
    use chumsky::prelude::*;
    use indoc::indoc;

    macro_rules! assert_expression_snapshot {
        ($code:expr) => {{
            let parsed = expression::parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("expression should parse");

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
    fn match_expression() {
        assert_expression_snapshot!(
            r#"
            match value {
                Result::Ok(inner) => {
                    inner
                },
                Result::Err(reason) => {
                    reason
                },
            }
            "#
        );
    }
}
