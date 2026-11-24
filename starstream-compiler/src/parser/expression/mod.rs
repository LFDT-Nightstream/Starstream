use chumsky::prelude::*;
use starstream_types::{
    Block, Span,
    ast::{BinaryOp, Expr, Spanned},
};

use super::context::{Extra, ParserInput};

/// Test helper macro for expression snapshot tests.
/// Defined here so all expression submodules can use it.
#[cfg(test)]
macro_rules! assert_expression_snapshot {
    ($code:expr) => {{
        use chumsky::prelude::*;
        use indoc::indoc;

        let (_, _, expr) = $crate::parser::recursives();
        let parsed = expr
            .parse(indoc! { $code }.with_context(starstream_types::ModuleId::ENTRY))
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

mod additive;
mod comparison;
mod equality;
mod logical_and;
mod logical_or;
mod multiplicative;
mod postfix;
mod primary;
mod unary;

pub use primary::primary;

pub fn expr<'a>(
    expression: impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, ParserInput<'a>, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> + Clone {
    let primary_parser = primary(expression.clone(), block).boxed();
    let postfix = postfix::parser(primary_parser, expression.clone()).boxed();
    let unary = unary::parser(postfix).boxed();
    let multiplicative = multiplicative::parser(unary).boxed();
    let additive = additive::parser(multiplicative).boxed();
    let comparison = comparison::parser(additive).boxed();
    let equality = equality::parser(comparison).boxed();
    let logical_and = logical_and::parser(equality).boxed();

    logical_or::parser(logical_and).boxed()
}

pub(super) fn chain<'a>(
    lower: impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    operator: impl Parser<'a, ParserInput<'a>, BinaryOp, Extra<'a>>,
) -> impl Parser<'a, ParserInput<'a>, Spanned<Expr>, Extra<'a>> {
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

fn span_union(a: Span, b: Span) -> Span {
    let start = a.start.min(b.start);
    let end = a.end.max(b.end);

    Span::new(a.context, start..end)
}

#[cfg(test)]
mod tests {
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
