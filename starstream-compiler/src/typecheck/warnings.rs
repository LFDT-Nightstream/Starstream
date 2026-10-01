use std::fmt;

use miette::{Diagnostic, Severity};
use starstream_types::{StarError, WarningCode, warning_code};

pub type TypeWarning = StarError<TypeWarningKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeWarningKind {
    UnnecessaryDisclose,
    /// A path import was skipped due to an error in module resolution.
    PathImportNotResolved {
        path: String,
    },
}

impl TypeWarningKind {
    pub fn warning_code(&self) -> &'static WarningCode {
        match self {
            TypeWarningKind::UnnecessaryDisclose => warning_code!(W0001),
            TypeWarningKind::PathImportNotResolved { .. } => warning_code!(W0002),
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
            TypeWarningKind::PathImportNotResolved { path } => {
                write!(f, "path import `{path}` was not resolved")
            }
        }
    }
}

impl std::error::Error for TypeWarningKind {}

impl Diagnostic for TypeWarningKind {
    fn code(&self) -> Option<Box<dyn fmt::Display + '_>> {
        Some(Box::new(self.warning_code()))
    }

    fn severity(&self) -> Option<Severity> {
        Some(Severity::Warning)
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(Box::new(format!(
            "https://starstream.nightstream.dev/warnings/{}",
            self.warning_code()
        )))
    }
}
