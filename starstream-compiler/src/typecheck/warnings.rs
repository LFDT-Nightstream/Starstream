use std::fmt;

use miette::{Diagnostic, LabeledSpan, Severity};
use starstream_types::{Span, WarningCode, warning_code};
use thiserror::Error;

use super::diagnostic::DiagnosticCore;

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct TypeWarning {
    pub kind: TypeWarningKind,
    core: DiagnosticCore,
}

impl TypeWarning {
    pub fn new(kind: TypeWarningKind, span: Span) -> Self {
        Self {
            kind,
            core: DiagnosticCore::new(span),
        }
    }

    pub fn with_secondary(mut self, span: Span, message: impl Into<String>) -> Self {
        self.core = self.core.with_secondary(span, message);
        self
    }

    pub fn with_primary_message(mut self, message: impl Into<String>) -> Self {
        self.core = self.core.with_primary_message(message);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.core = self.core.with_help(help);
        self
    }
}

impl Diagnostic for TypeWarning {
    fn code(&self) -> Option<Box<dyn fmt::Display + '_>> {
        Some(Box::new(self.kind.warning_code()))
    }

    fn severity(&self) -> Option<Severity> {
        Some(Severity::Warning)
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(Box::new(format!(
            "https://starstream.nightstream.dev/warnings/{}",
            self.kind.warning_code()
        )))
    }

    fn help(&self) -> Option<Box<dyn fmt::Display + '_>> {
        self.core
            .help()
            .map(|help| Box::new(help) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.core.labels().into_iter()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeWarningKind {
    UnnecessaryDisclose,
}

impl TypeWarningKind {
    pub fn warning_code(&self) -> &'static WarningCode {
        match self {
            TypeWarningKind::UnnecessaryDisclose => warning_code!(W0001),
        }
    }
}

impl fmt::Display for TypeWarningKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeWarningKind::UnnecessaryDisclose => {
                write!(
                    f,
                    "`disclose(...)` is unnecessary because the wrapped value is already public"
                )
            }
        }
    }
}
