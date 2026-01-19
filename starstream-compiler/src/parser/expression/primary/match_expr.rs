use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, MatchArm, Spanned};

use crate::parser::{ParserExt, comment, context::Extra, pattern};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let pattern_parser = pattern::parser();
    // Collect comments before each match arm and capture span
    // Use .then() instead of .ignore_then() to ensure side-effects happen
    let match_arm = comment::comment_collecting()
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then(
            pattern_parser
                .then_ignore(just("=>").padded())
                .then(block)
                .map_with(|(pattern, body), extra| MatchArm {
                    pattern,
                    body,
                    span: extra.span(),
                }),
        )
        .map(|(_, arm)| arm);

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
    use crate::parser::expression::assert_expression_snapshot;

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
