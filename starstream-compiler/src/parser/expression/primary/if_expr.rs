use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, IfCondition, Spanned};

use crate::parser::{ParserExt, context::Extra, primitives};

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let is_condition = primitives::identifier()
        .then_ignore(just("is").padded())
        .then(primitives::identifier())
        .map(|(name, abi_name)| IfCondition::Is { name, abi_name });

    let bool_condition = expression
        .clone()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(IfCondition::Bool);

    let condition = choice((is_condition, bool_condition)).boxed();

    just("if")
        .padded()
        .ignore_then(condition.clone())
        .then(block.clone())
        .then(
            just("else")
                .padded()
                .then(just("if"))
                .padded()
                .ignore_then(condition.clone())
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
    use crate::parser::expression::assert_expression_snapshot;

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

    #[test]
    fn if_is_basic() {
        assert_expression_snapshot!(
            r#"
            if x is MyAbi { 1 } else { 2 }
            "#
        );
    }

    #[test]
    fn if_is_chain() {
        assert_expression_snapshot!(
            r#"
            if x is AbiA { 1 } else if x is AbiB { 2 } else { 3 }
            "#
        );
    }

    #[test]
    fn if_is_mixed() {
        assert_expression_snapshot!(
            r#"
            if x is AbiA { 1 } else if (flag) { 2 } else { 3 }
            "#
        );
    }
}
