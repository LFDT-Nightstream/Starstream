use chumsky::{prelude::*, recursive::Recursive};
use starstream_types::ast::{Block, Expr, Spanned, Statement};

use crate::parser::type_annotation;

use super::{context::Extra, expression, primitives};

/// Parser for individual statements.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Statement, Extra<'a>> {
    let (statement_parser, _) = build_parsers();

    statement_parser
}

/// Parser for `{ ... }` blocks, including optional tail expressions.
pub fn block_parser<'a>() -> impl Parser<'a, &'a str, Block, Extra<'a>> {
    let (_, block_parser) = build_parsers();

    block_parser
}

pub(super) fn parser_with_block<'a>(
    block_parser: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Statement, Extra<'a>> {
    let variable_declaration = just("let")
        .padded()
        .ignore_then(just("mut").padded().or_not())
        .then(primitives::identifier())
        .then(just(":").ignore_then(type_annotation::parser()).or_not())
        .then_ignore(just('=').padded())
        .then(expr.clone())
        .then_ignore(just(';').padded())
        .map(
            |(((mutable, name), ty), value)| Statement::VariableDeclaration {
                mutable: mutable.is_some(),
                name,
                ty,
                value,
            },
        );

    let assignment = primitives::identifier()
        .then_ignore(just('=').padded())
        .then(expr.clone())
        .then_ignore(just(';').padded())
        .map(|(target, value)| Statement::Assignment { target, value });

    let if_statement = just("if")
        .padded()
        .ignore_then(
            expr.clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(block_parser.clone())
        .then(
            just("else")
                .padded()
                .then(just("if"))
                .padded()
                .ignore_then(
                    expr.clone()
                        .delimited_by(just('(').padded(), just(')').padded()),
                )
                .then(block_parser.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then(
            just("else")
                .padded()
                .ignore_then(block_parser.clone())
                .or_not(),
        )
        .map(|((first, mut rest), else_branch)| Statement::If {
            branches: {
                rest.insert(0, first);
                rest
            },
            else_branch,
        });

    let while_statement = just("while")
        .padded()
        .ignore_then(
            expr.clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(block_parser.clone())
        .map(|(condition, body)| Statement::While { condition, body });

    let block_statement = block_parser.clone().map(Statement::Block).padded();

    let return_statement = just("return")
        .padded()
        .ignore_then(
            expr.clone()
                .then_ignore(just(';').padded())
                .map(Some)
                .or(just(';').padded().to(None)),
        )
        .map(Statement::Return);

    let expression_statement = expr
        .then_ignore(just(';').padded())
        .map(Statement::Expression);

    choice((
        variable_declaration,
        assignment,
        if_statement,
        while_statement,
        block_statement,
        return_statement,
        expression_statement,
    ))
}

fn build_parsers<'a>() -> (
    impl Parser<'a, &'a str, Statement, Extra<'a>>,
    impl Parser<'a, &'a str, Block, Extra<'a>>,
) {
    let mut statement_parser = Recursive::declare();
    let mut block_parser = Recursive::declare();
    let expr = expression::parser().boxed();

    let block = statement_parser
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.clone().padded().or_not())
        .delimited_by(just('{').padded(), just('}').padded())
        .map(|(statements, tail_expression)| Block {
            statements,
            tail_expression,
        })
        .boxed();

    block_parser.define(block);
    let stmt = parser_with_block(block_parser.clone(), expr).boxed();
    statement_parser.define(stmt);

    (statement_parser, block_parser)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_statement_snapshot {
        ($code:expr) => {{
            let stmt = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("statement should parse");

            insta::with_settings!({
                description => format!("Code:\n\n{}", indoc! { $code }),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_debug_snapshot!(stmt);
            });
        }};
    }

    #[test]
    fn let_binding() {
        assert_statement_snapshot!("let answer = 42;");
    }

    #[test]
    fn let_mut_binding() {
        assert_statement_snapshot!("let mut answer = 42;");
    }

    #[test]
    fn assignment() {
        assert_statement_snapshot!("value = value + 1;");
    }

    #[test]
    fn if_else() {
        assert_statement_snapshot!(
            r#"
            if (flag) { let a = 1; } else { value = value + 1; }
            "#
        );
    }

    #[test]
    fn while_loop() {
        assert_statement_snapshot!(
            r#"
            while (count < 10) { count = count + 1; }
            "#
        );
    }

    #[test]
    fn block() {
        assert_statement_snapshot!(
            r#"
            { let mut a = 1; a = a + 1; }
            "#
        );
    }

    #[test]
    fn expression_statement() {
        assert_statement_snapshot!("value + 1;");
    }

    #[test]
    fn return_without_value() {
        assert_statement_snapshot!("return;");
    }

    #[test]
    fn return_with_value() {
        assert_statement_snapshot!("return flag;");
    }
}
