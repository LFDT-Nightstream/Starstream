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
use starstream_types::ast::Program;

pub struct ParseOutput {
    program: Option<Program>,
    errors: Vec<ParseError>,
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

    ParseOutput { program, errors }
}
