use chumsky::prelude::*;
use starstream_types::ast::{Block, Expr, Spanned, Statement};

use super::{ParserExt, context::Extra, primitives, type_annotation};

pub fn block<'a>(
    statement: impl Parser<'a, &'a str, Statement, Extra<'a>> + 'a,
    expr: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + 'a,
) -> impl Parser<'a, &'a str, Block, Extra<'a>> + Clone {
    statement
        .spanned()
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.padded().or_not())
        .delimited_by(just('{').padded(), just('}').padded())
        .map_with(|(statements, tail_expression), extra| Block {
            statements,
            tail_expression,
            span: extra.span(),
        })
        .boxed()
}

pub fn statement<'a>(
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
    expr: impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Statement, Extra<'a>> {
    let variable_declaration = just("let")
        .padded()
        .ignore_then(just("pub").padded().or_not())
        .then(just("mut").padded().or_not())
        .then(primitives::identifier())
        .then(just(":").ignore_then(type_annotation::parser()).or_not())
        .then_ignore(just('=').padded())
        .then(expr.clone())
        .then_ignore(just(';').padded())
        .map(
            |((((public, mutable), name), ty), value)| Statement::VariableDeclaration {
                public: public.is_some(),
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

    let while_statement = just("while")
        .padded()
        .ignore_then(
            expr.clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(block.clone())
        .map(|(condition, body)| Statement::While { condition, body });

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
        .clone()
        .then_ignore(just(';').padded())
        .map(Statement::Expression);

    // Block-bodied expressions (if, match, block) can be used as statements
    // without a trailing semicolon, since they unambiguously end with `}`.
    // The lookahead ensures we don't consume the tail expression of a block:
    // only match when the next non-whitespace token is NOT `}`.
    let block_expression_statement = expr
        .try_map(|e, span| match &e.node {
            Expr::If { .. } | Expr::Match { .. } | Expr::Block(_) => Ok(Statement::Expression(e)),
            _ => Err(Rich::custom(span, "expected ';'")),
        })
        .then_ignore(just('}').padded().not().rewind());

    choice((
        variable_declaration,
        assignment,
        while_statement,
        return_statement,
        expression_statement,
        block_expression_statement,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_statement_snapshot {
        ($code:expr) => {{
            let (statement, _, _) = $crate::parser::recursives();
            let stmt = statement
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
    fn let_pub_binding() {
        assert_statement_snapshot!("let pub answer = 42;");
    }

    #[test]
    fn let_pub_mut_binding() {
        assert_statement_snapshot!("let pub mut answer = 42;");
    }
    #[test]
    fn assignment() {
        assert_statement_snapshot!("value = value + 1;");
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
