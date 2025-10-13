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
                .ignore_then(block_parser.clone())
                .or_not(),
        )
        .map(|((condition, then_branch), else_branch)| Statement::If {
            condition,
            then_branch,
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
