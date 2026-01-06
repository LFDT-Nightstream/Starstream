use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, Spanned};

use crate::parser::{ParserExt, context::Extra};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("if")
        .padded()
        .ignore_then(
            expression
                .clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(block.clone())
        .then(
            just("else")
                .padded()
                .then(just("if"))
                .padded()
                .ignore_then(
                    expression
                        .clone()
                        .delimited_by(just('(').padded(), just(')').padded()),
                )
                .then(block.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then(just("else").padded().ignore_then(block).or_not())
        .map(|((first, mut rest), else_branch)| Expr::If {
            branches: {
                rest.insert(0, first);
                rest
            },
            else_branch: else_branch.map(Box::new),
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
    fn if_else() {
        assert_expression_snapshot!(
            r#"
            if (flag) { let a = 1; } else { value = value + 1; }
            "#
        );
    }

    #[test]
    fn if_else_if() {
        assert_expression_snapshot!(
            r#"
            if (a) { 1 } else if (b) { 2 } else { 3 }
            "#
        );
    }
}
