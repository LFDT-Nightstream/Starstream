use chumsky::{prelude::*, recursive::recursive};
use starstream_types::ast::{Block, Statement};

use super::{context::Extra, expression, primitives};

/// Parser for individual statements.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Statement, Extra<'a>> {
    recursive(|statement_parser| {
        let block_parser = statement_parser
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just('{').padded(), just('}').padded())
            .map(|statements| Block { statements })
            .boxed();

        parser_with_block(block_parser).boxed()
    })
}

pub(super) fn parser_with_block<'a>(
    block_parser: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Statement, Extra<'a>> {
    let expr = expression::parser().boxed();

    let variable_declaration = just("let")
        .padded()
        .ignore_then(primitives::identifier())
        .then_ignore(just('=').padded())
        .then(expr.clone())
        .then_ignore(just(';').padded())
        .map(|(name, value)| Statement::VariableDeclaration { name, value });

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

    let expression_statement = expr
        .then_ignore(just(';').padded())
        .map(Statement::Expression);

    choice((
        variable_declaration,
        assignment,
        if_statement,
        while_statement,
        block_statement,
        expression_statement,
    ))
    .boxed()
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
            { let a = 1; a = a + 1; }
            "#
        );
    }

    #[test]
    fn expression_statement() {
        assert_statement_snapshot!("value + 1;");
    }
}
