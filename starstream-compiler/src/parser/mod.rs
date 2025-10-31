mod context;
mod error;
mod expression;
mod primitives;
mod program;
mod statement;

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
}

pub fn parse_program(source: &str) -> ParseOutput {
    let mut state = context::State::new();
    let result = program().parse_with_state(source, &mut state);

    let errors = result
        .errors()
        .cloned()
        .map(error::ParseError::from_rich)
        .collect();

    ParseOutput {
        program: result.into_output(),
        errors,
    }
}
