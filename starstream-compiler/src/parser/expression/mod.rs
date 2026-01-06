use chumsky::{prelude::*, span::SimpleSpan};
use starstream_types::ast::{BinaryOp, Expr, Spanned};

use super::context::Extra;
use crate::parser::statement;

/// Test helper macro for expression snapshot tests.
/// Defined here so all expression submodules can use it.
#[cfg(test)]
macro_rules! assert_expression_snapshot {
    ($code:expr) => {{
        use chumsky::prelude::*;
        use indoc::indoc;

        let parsed = $crate::parser::expression::parser()
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

#[cfg(test)]
pub(crate) use assert_expression_snapshot;

mod additive;
mod comparison;
mod equality;
mod logical_and;
mod logical_or;
mod multiplicative;
mod postfix;
mod primary;
mod unary;

pub use primary::parser as primary;

pub fn parser<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    recursive(|expression| {
        let block_parser = statement::block_parser_with_expr(expression.clone()).boxed();
        let primary_parser = primary(expression.clone(), block_parser).boxed();
        let postfix = postfix::parser(primary_parser, expression.clone()).boxed();
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
    use super::assert_expression_snapshot;

    #[test]
    fn arithmetic_precedence() {
        assert_expression_snapshot!("1 + 2 * 3 - 4");
    }

    #[test]
    fn logical_chaining() {
        assert_expression_snapshot!("(1 < 2) && false || true");
    }

    #[test]
    fn field_access_chain() {
        assert_expression_snapshot!("state.position.x");
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
}
