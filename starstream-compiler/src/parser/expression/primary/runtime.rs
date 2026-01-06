use chumsky::prelude::*;
use starstream_types::ast::{Expr, Spanned};

use crate::parser::{ParserExt, context::Extra};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    just("runtime")
        .padded()
        .ignore_then(expression)
        .map(|expr| Expr::Runtime {
            expr: Box::new(expr),
        })
        .spanned()
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;
    use crate::parser::expression;
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
    fn runtime_call() {
        assert_expression_snapshot!("runtime blockHeight()");
    }

    #[test]
    fn runtime_namespaced_call() {
        assert_expression_snapshot!("runtime context::blockHeight()");
    }
}
