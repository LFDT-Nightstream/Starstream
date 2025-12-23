use chumsky::{prelude::*, span::SimpleSpan};
use starstream_types::ast::{BinaryOp, Expr, Spanned};

use super::context::Extra;
use crate::parser::statement;

mod additive;
mod comparison;
mod equality;
mod logical_and;
mod logical_or;
mod multiplicative;
mod postfix;
mod primary;
mod unary;

pub fn parser<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    recursive(|expression| {
        let block_parser = statement::block_parser_with_expr(expression.clone()).boxed();
        let primary = primary::parser(expression.clone(), block_parser).boxed();
        let postfix = postfix::parser(primary, expression.clone()).boxed();
        let unary = unary::parser(postfix).boxed();
        let multiplicative = multiplicative::parser(unary).boxed();
        let additive = additive::parser(multiplicative).boxed();
        let comparison = comparison::parser(additive).boxed();
        let equality = equality::parser(comparison).boxed();
        let logical_and = logical_and::parser(equality).boxed();

        logical_or::parser(logical_and).boxed()
    })
}

pub(super) fn chain<'a>(
    lower: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    operator: impl Parser<'a, &'a str, BinaryOp, Extra<'a>>,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let tail = operator.then(lower.clone());

    lower.foldl(tail.repeated(), |lhs, (op, rhs)| combine(lhs, op, rhs))
}

fn combine(lhs: Spanned<Expr>, op: BinaryOp, rhs: Spanned<Expr>) -> Spanned<Expr> {
    let span = span_union(lhs.span, rhs.span);

    Spanned::new(
        Expr::Binary {
            op,
            left: Box::new(lhs),
            right: Box::new(rhs),
        },
        span,
    )
}

fn span_union(a: SimpleSpan, b: SimpleSpan) -> SimpleSpan {
    let start = a.start.min(b.start);
    let end = a.end.max(b.end);

    SimpleSpan::from(start..end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_expression_snapshot {
        ($code:expr) => {{
            let parsed = parser()
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
    fn integer_literal() {
        assert_expression_snapshot!("42");
    }

    #[test]
    fn arithmetic_precedence() {
        assert_expression_snapshot!("1 + 2 * 3 - 4");
    }

    #[test]
    fn logical_chaining() {
        assert_expression_snapshot!("(1 < 2) && false || true");
    }

    #[test]
    fn struct_literal_expression() {
        assert_expression_snapshot!(
            r#"
            Point {
                x: 10,
                y: value + 1,
            }
            "#
        );
    }

    #[test]
    fn enum_constructor_expression() {
        assert_expression_snapshot!("Result::Ok(answer)");
    }

    #[test]
    fn field_access_chain() {
        assert_expression_snapshot!("state.position.x");
    }

    #[test]
    fn block() {
        assert_expression_snapshot!(
            r#"
            { let mut a = 1; a = a + 1; }
            "#
        );
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

    #[test]
    fn function_call_no_args() {
        assert_expression_snapshot!("foo()");
    }

    #[test]
    fn function_call_with_args() {
        assert_expression_snapshot!("add(1, 2)");
    }

    #[test]
    fn function_call_chained_with_field() {
        assert_expression_snapshot!("foo().bar");
    }

    #[test]
    fn field_access_then_call() {
        assert_expression_snapshot!("obj.method(x)");
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
            emit Transfer(from, to, amount)
            "#
        );
    }

    #[test]
    fn raise_call() {
        assert_expression_snapshot!("raise blockHeight()");
    }

    #[test]
    fn raise_namespaced_call() {
        assert_expression_snapshot!("raise context::blockHeight()");
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
