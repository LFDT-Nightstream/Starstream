mod comment;
mod context;
mod definition;
mod error;
mod expression;
mod pattern;
mod primitives;
mod program;
mod statement;
mod type_annotation;

pub use definition::parser as definition;
pub use error::ParseError;
pub use expression::expr as expression;
pub use program::parser as program;
pub use statement::statement;

use chumsky::prelude::*;
use starstream_types::{Block, CommentMap, Expr, Spanned, Statement, ast::Program};

use crate::parser::context::{Extra, State};

pub struct ParseOutput {
    pub program: Option<Program>,
    pub errors: Vec<ParseError>,
    pub extra: State,
}

impl ParseOutput {
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    pub fn into_program(self) -> Option<Program> {
        self.program
    }

    pub fn program(&self) -> Option<&Program> {
        self.program.as_ref()
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn into_output_errors(self) -> (Option<Program>, Vec<ParseError>) {
        (self.program, self.errors)
    }

    /// Get the CommentMap containing all comments from the source.
    pub fn comment_map(&self) -> CommentMap {
        CommentMap::from_comments(self.extra.comments.clone())
    }
}

pub fn parse_program(source: &str) -> ParseOutput {
    let mut state = context::State::new();
    let (program, errors) = program()
        .parse_with_state(source, &mut state)
        .into_output_errors();

    let errors = errors
        .into_iter()
        .map(error::ParseError::from_rich)
        .collect();

    ParseOutput {
        program,
        errors,
        extra: state.0,
    }
}

fn recursives<'a>() -> (
    impl Parser<'a, &'a str, Statement, Extra<'a>> + Clone,
    impl Parser<'a, &'a str, Block, Extra<'a>> + Clone,
    impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> + Clone,
) {
    let mut statement_recursive = Recursive::declare();
    let mut block_recursive = Recursive::declare();
    let mut expr_recursive = Recursive::declare();

    statement_recursive
        .define(statement::statement(block_recursive.clone(), expr_recursive.clone()).boxed());
    block_recursive.define(statement::block(
        statement_recursive.clone(),
        expr_recursive.clone(),
    ));
    expr_recursive.define(expression::expr(
        expr_recursive.clone(),
        block_recursive.clone(),
    ));

    (statement_recursive, block_recursive, expr_recursive)
}

trait ParserExt<'a, T>: Sized {
    /// Surround parse result in [Spanned].
    fn spanned(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>>;
    /// Surround parse result in [Spanned].
    fn spanned_clone(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>> + Clone
    where
        Self: Clone;
}

/// Creates the parser chain for collecting comments and wrapping in Spanned.
///
/// Comments are collected into State for the CommentMap. We use `.collect()` to force
/// side-effects to happen (chumsky optimization can skip them otherwise), and `.padded()`
/// between comments to handle whitespace between consecutive comments.
macro_rules! spanned_with_comments {
    ($parser:expr) => {{
        let comments = comment::comment_collecting()
            .padded()
            .repeated()
            .collect::<Vec<_>>();
        comments
            .clone()
            .then(
                $parser.map_with(|node, extra: &mut context::MapExtra| Spanned {
                    node,
                    span: extra.span(),
                }),
            )
            .then(comments)
            .map(|((_, spanned), _)| spanned)
    }};
}

impl<'a, T, U: Parser<'a, &'a str, T, Extra<'a>>> ParserExt<'a, T> for U {
    fn spanned(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>> {
        spanned_with_comments!(self)
    }

    fn spanned_clone(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>> + Clone
    where
        Self: Clone,
    {
        spanned_with_comments!(self)
    }
}
