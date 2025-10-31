//! State tracking for an open text document.
use std::sync::Arc;

use ropey::Rope;
use tower_lsp_server::lsp_types::Uri;

use starstream_compiler::{
    formatter, parse_program,
    parser::ParseError,
    typecheck::typecheck_program,
    typecheck::{TypeError, TypecheckOptions, TypecheckSuccess},
};
use starstream_types::ast::Program;

use crate::diagnostics::diagnostic_to_lsp;

/// Analysis data cached for an open document.
#[derive(Debug)]
pub struct DocumentState {
    rope: Rope,
    version: Option<i32>,
    program: Option<Arc<Program>>,
    typed: Option<TypecheckSuccess>,
    diagnostics: Vec<tower_lsp_server::lsp_types::Diagnostic>,
}

impl DocumentState {
    /// Create initial document state from raw text.
    pub fn from_text(uri: &Uri, text: &str, version: Option<i32>) -> Self {
        let mut state = Self {
            rope: Rope::from_str(text),
            version,
            program: None,
            typed: None,
            diagnostics: Vec::new(),
        };

        state.reanalyse(uri, text);
        state
    }

    /// Update the stored text + version and recompute analysis artifacts.
    pub fn update(&mut self, uri: &Uri, text: &str, version: Option<i32>) {
        self.rope = Rope::from_str(text);
        self.version = version;
        self.reanalyse(uri, text);
    }

    /// Most recent diagnostics derived from parsing and typechecking.
    pub fn diagnostics(&self) -> &[tower_lsp_server::lsp_types::Diagnostic] {
        &self.diagnostics
    }

    /// Document version reflected in the currently cached state.
    pub fn version(&self) -> Option<i32> {
        self.version
    }

    /// Typed AST produced by the typechecker, if type-checking succeeded.
    #[allow(dead_code)]
    pub fn typed(&self) -> Option<&TypecheckSuccess> {
        self.typed.as_ref()
    }

    /// Untyped AST produced by the parser, if parsing succeeded.
    pub fn program(&self) -> Option<&Program> {
        self.program.as_deref()
    }

    /// Rope representation covering the latest snapshot of the document.
    #[allow(dead_code)]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    fn reanalyse(&mut self, uri: &Uri, text: &str) {
        self.diagnostics.clear();
        self.program = None;
        self.typed = None;

        let parse_output = parse_program(text);

        for error in parse_output.errors() {
            self.push_parse_error(uri, error);
        }

        let program = parse_output.program().cloned().map(Arc::new);
        self.program = program;

        if let Some(program) = self.program.as_ref() {
            match typecheck_program(program.as_ref(), TypecheckOptions::default()) {
                Ok(typed) => {
                    self.typed = Some(typed);
                }
                Err(type_errors) => {
                    for error in type_errors {
                        self.push_type_error(uri, error);
                    }
                }
            }
        }
    }

    fn push_parse_error(&mut self, uri: &Uri, error: &ParseError) {
        let diag = diagnostic_to_lsp(&self.rope, uri, error);
        self.diagnostics.push(diag);
    }

    fn push_type_error(&mut self, uri: &Uri, error: TypeError) {
        let diag = diagnostic_to_lsp(&self.rope, uri, &error);
        self.diagnostics.push(diag);
    }

    /// Format the cached AST. Returns `None` if parsing failed.
    pub fn format(&self) -> Result<Option<String>, std::fmt::Error> {
        let program = match self.program() {
            Some(program) => program,
            None => return Ok(None),
        };

        formatter::program(program).map(Some)
    }
}
