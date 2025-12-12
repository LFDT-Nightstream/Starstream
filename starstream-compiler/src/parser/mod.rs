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

pub use error::ParseError;
pub use expression::parser as expression;
pub use program::parser as program;
pub use statement::parser as statement;

use chumsky::prelude::*;
use starstream_types::{Spanned, ast::Program};

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

trait ParserExt<'a, T>: Sized {
    /// Surround parse result in [Spanned].
    fn spanned(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>>;
    /// Surround parse result in [Spanned].
    fn spanned_clone(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>> + Clone
    where
        Self: Clone;
}

impl<'a, T, U: Parser<'a, &'a str, T, Extra<'a>>> ParserExt<'a, T> for U {
    fn spanned(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>> {
        comment::comment()
            .repeated()
            .collect::<Vec<_>>()
            .then(self)
            .map_with(|(comments, node), extra: &mut context::MapExtra| Spanned {
                node,
                span: extra.span(),
                comments,
            })
    }

    fn spanned_clone(self) -> impl Parser<'a, &'a str, Spanned<T>, Extra<'a>> + Clone
    where
        Self: Clone,
    {
        comment::comment()
            .repeated()
            .collect::<Vec<_>>()
            .then(self)
            .map_with(|(comments, node), extra: &mut context::MapExtra| Spanned {
                node,
                span: extra.span(),
                comments,
            })
    }
}
