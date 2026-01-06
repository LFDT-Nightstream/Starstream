use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, Spanned};

use crate::parser::{ParserExt, context::Extra};

mod emit;
mod enum_constructor;
mod if_expr;
mod literal;
mod match_expr;
mod raise;
mod runtime;
mod struct_literal;

pub use emit::parser as emit;
pub use enum_constructor::parser as enum_constructor;
pub use if_expr::parser as if_expr;
pub use literal::{boolean, identifier, integer, unit};
pub use match_expr::parser as match_expr;
pub use raise::parser as raise;
pub use runtime::parser as runtime;
pub use struct_literal::parser as struct_literal;

pub fn parser<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let grouping = expression
        .clone()
        .delimited_by(just('(').padded(), just(')').padded())
        .map(Box::new)
        .map(Expr::Grouping)
        .spanned();

    let block_expr = block.clone().map(Box::new).map(Expr::Block).spanned();

    choice((
        grouping,
        integer(),
        boolean(),
        unit(),
        struct_literal(expression.clone()),
        enum_constructor(expression.clone()),
        emit(expression.clone()),
        raise(expression.clone()),
        runtime(expression.clone()),
        block_expr,
        if_expr(expression.clone(), block.clone()),
        match_expr(expression, block),
        // Identifier last to prefer struct literals if possible
        identifier(),
    ))
}

#[cfg(test)]
mod tests {
    use crate::parser::expression::assert_expression_snapshot;

    #[test]
    fn grouping() {
        assert_expression_snapshot!("(1 + 2)");
    }

    #[test]
    fn block() {
        assert_expression_snapshot!(
            r#"
            { let mut a = 1; a = a + 1; }
            "#
        );
    }
}
