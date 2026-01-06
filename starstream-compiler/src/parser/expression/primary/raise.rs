use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("raise")
        .padded()
        .ignore_then(expression)
        .map(|expr| Expr::Raise {
            expr: Box::new(expr),
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
    fn raise_call() {
        assert_expression_snapshot!("raise blockHeight()");
    }

    #[test]
    fn raise_namespaced_call() {
        assert_expression_snapshot!("raise context::blockHeight()");
    }
}
