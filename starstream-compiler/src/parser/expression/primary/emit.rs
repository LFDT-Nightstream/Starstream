use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("emit")
        .padded()
        .ignore_then(primitives::identifier())
        .then(
            expression
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .map(|(event, args)| Expr::Emit { event, args })
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
    fn emit_no_args() {
        assert_expression_snapshot!(
            r#"
            // Emit ping
            emit Ping()
            "#
        );
    }

    #[test]
    fn emit_with_args() {
        assert_expression_snapshot!(
            r#"
            // Transfer event
            emit Transfer(sender, to, amount)
            "#
        );
    }
}
