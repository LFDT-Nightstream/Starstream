use chumsky::prelude::*;
use starstream_types::{
    ScopedName,
    ast::{Block, Expr, Spanned},
};

use crate::parser::{ParserExt, context::Extra, primitives};

mod disclose;
mod emit_raise_runtime;
mod if_expr;
mod literal;
mod match_expr;
mod struct_constructor;
mod yield_;

pub use disclose::parser as disclose;
pub use emit_raise_runtime::{emit, raise, runtime};
pub use if_expr::parser as if_expr;
pub use literal::{boolean, integer, unit};
pub use match_expr::parser as match_expr;
pub use struct_constructor::struct_constructor;
pub use yield_::yield_;

pub fn primary<'a>(
    expression: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone {
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
        struct_constructor(expression.clone()),
        disclose(expression.clone()),
        emit(expression.clone()),
        raise(expression.clone()),
        runtime(expression.clone()),
        block_expr,
        yield_(),
        if_expr(expression.clone(), block.clone()),
        match_expr(expression.clone(), block.clone()),
        // Identifier last to prefer struct literals if possible
        scoped_name_expr(),
    ))
    .boxed()
}

fn scoped_name_expr<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    scoped_name().map(|x| Expr::ScopedName(x)).spanned()
}

pub fn scoped_name<'a>() -> impl Parser<'a, &'a str, ScopedName, Extra<'a>> + Clone {
    primitives::identifier()
        .separated_by(just("::"))
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
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

    #[test]
    fn enum_constructor_expression() {
        assert_expression_snapshot!("Result::Ok(answer)");
    }

    #[test]
    fn enum_constructor_unit() {
        assert_expression_snapshot!("Option::None");
    }
}
