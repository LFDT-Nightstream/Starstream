//! Error types for the Starstream DSL

use std::{fmt::Display, sync::Arc};

use miette::{Diagnostic, LabeledSpan, Severity, SourceCode, SourceSpan};

use crate::Span;

// ----------------------------------------------------------------------------
/// Span extensions for Miette compatibility.
pub trait SpanExt {
    /// Convert [Span] to [SourceSpan].
    fn miette(self) -> SourceSpan;
    /// Create a primary label from this span.
    fn primary(self, message: impl Display) -> LabeledSpan;
    /// Create a secondary label from this span.
    fn secondary(self, message: impl Display) -> LabeledSpan;
}

impl SpanExt for Span {
    fn miette(self) -> SourceSpan {
        let start = self.start.into();
        let len = self.end.saturating_sub(self.start);
        SourceSpan::new(start, len)
    }

    fn primary(self, message: impl Display) -> LabeledSpan {
        LabeledSpan::new_primary_with_span(Some(message.to_string()), self.miette())
    }

    fn secondary(self, message: impl Display) -> LabeledSpan {
        LabeledSpan::new_with_span(Some(message.to_string()), self.miette())
    }
}

// ----------------------------------------------------------------------------
/// Convenience to add extra Miette information to error kind enums.
#[derive(Debug, Clone)]
pub struct StarError<T> {
    kind: Box<T>,
    core: Box<StarErrorCore>,
}

#[derive(Default, Clone)]
struct StarErrorCore {
    code: Option<String>,
    severity: Option<Severity>,
    help: Option<String>,
    url: Option<String>,
    source_code: Option<Arc<dyn SourceCode>>,
    labels: Vec<LabeledSpan>,
    cause: Option<Arc<dyn Diagnostic + Send + Sync>>,
    related: Vec<Arc<dyn Diagnostic + Send + Sync>>,
}

// impl<T> From<T> for StarError<T>

impl<T> StarError<T> {
    pub fn new(kind: T, span: Span) -> Self {
        StarError {
            kind: Box::new(kind),
            core: Box::new(StarErrorCore {
                labels: vec![LabeledSpan::new_primary_with_span(None, span.miette())],
                ..Default::default()
            }),
        }
    }

    pub fn with_code(mut self, code: impl Display) -> Self {
        self.core.code = Some(code.to_string());
        self
    }

    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.core.severity = Some(severity);
        self
    }

    pub fn with_help(mut self, help: impl Display) -> Self {
        self.core.help = Some(help.to_string());
        self
    }

    pub fn with_url(mut self, url: impl Display) -> Self {
        self.core.url = Some(url.to_string());
        self
    }

    pub fn with_source_code(mut self, code: impl SourceCode + 'static) -> Self {
        self.core.source_code = Some(Arc::new(code));
        self
    }

    // with_label
    // with_labels

    pub fn and_label(mut self, label: impl Into<LabeledSpan>) -> Self {
        self.core.labels.push(label.into());
        self
    }

    // and_labels

    pub fn with_primary_message(mut self, message: impl Display) -> Self {
        for each in &mut self.core.labels {
            if each.primary() {
                each.set_label(Some(message.to_string()));
                break;
            }
        }
        self
    }

    pub fn with_secondary(mut self, span: Span, message: impl Display) -> Self {
        self.core.labels.push(span.secondary(message));
        self
    }

    pub fn with_cause(mut self, cause: impl Diagnostic + Send + Sync + 'static) -> Self {
        self.core.cause = Some(Arc::new(cause));
        self
    }

    pub fn and_related(mut self, related: impl Diagnostic + Send + Sync + 'static) -> Self {
        self.core.related.push(Arc::new(related));
        self
    }
}

impl<T> std::ops::Deref for StarError<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T: Display> Display for StarError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl<T: std::error::Error> std::error::Error for StarError<T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.core
            .cause
            .as_deref()
            .map(|x| x as &dyn std::error::Error)
            .or_else(|| self.kind.source())
    }
}

impl<T: Diagnostic> Diagnostic for StarError<T> {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.core
            .code
            .as_deref()
            .map(|c| Box::new(c) as Box<dyn Display>)
            .or_else(|| self.kind.code())
    }

    fn severity(&self) -> Option<Severity> {
        self.core.severity.or_else(|| self.kind.severity())
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.core
            .help
            .as_deref()
            .map(|c| Box::new(c) as Box<dyn Display>)
            .or_else(|| self.kind.help())
    }

    fn url<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.core
            .url
            .as_deref()
            .map(|c| Box::new(c) as Box<dyn Display>)
            .or_else(|| self.kind.url())
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.core
            .source_code
            .as_deref()
            .or_else(|| self.kind.source_code())
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let us = if self.core.labels.is_empty() {
            None
        } else {
            Some(self.core.labels.iter().cloned())
        };
        let them = self.kind.labels();
        if us.is_some() || them.is_some() {
            Some(Box::new(
                us.into_iter().flatten().chain(them.into_iter().flatten()),
            ))
        } else {
            None
        }
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        let us = if self.core.related.is_empty() {
            None
        } else {
            Some(
                self.core
                    .related
                    .iter()
                    .map(|arc| &**arc as &dyn Diagnostic),
            )
        };
        let them = self.kind.related();
        if us.is_some() || them.is_some() {
            Some(Box::new(
                us.into_iter().flatten().chain(them.into_iter().flatten()),
            ))
        } else {
            None
        }
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.core
            .cause
            .as_deref()
            .map(|x| x as &dyn Diagnostic)
            .or_else(|| self.kind.diagnostic_source())
    }
}

impl std::fmt::Debug for StarErrorCore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StarErrorCore")
            .field("severity", &self.severity)
            .field("code", &self.code)
            .field("url", &self.url)
            .field("cause", &self.cause)
            .field("labels", &self.labels)
            .field("help", &self.help)
            .field("related", &self.related)
            .finish()
    }
}

// ----------------------------------------------------------------------------
// Error and warning codes

/// An error code with both a name and embedded documentation attached.
#[derive(Clone, Copy)]
pub struct ErrorCode {
    pub name: &'static str,
    pub docs: &'static str,
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

inventory::collect!(ErrorCode);

impl ErrorCode {
    /// Iterate over all error codes registered with [`error_code`].
    pub fn iter() -> impl Iterator<Item = &'static ErrorCode> {
        inventory::iter::<ErrorCode>()
    }
}

/// A warning code with both a name and embedded documentation attached.
#[derive(Clone, Copy)]
pub struct WarningCode {
    pub name: &'static str,
    pub docs: &'static str,
}

impl Display for WarningCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

inventory::collect!(WarningCode);

impl WarningCode {
    /// Iterate over all warning codes registered with [`warning_code`].
    pub fn iter() -> impl Iterator<Item = &'static WarningCode> {
        inventory::iter::<WarningCode>()
    }
}

/// Create an error code, attaching its docs from the associated file in the
/// `docs` folder and registering it with [`ErrorCode::iter`].
#[macro_export]
macro_rules! error_code {
    ($id:ident) => {{
        static $id: ErrorCode = ErrorCode {
            name: stringify!($id),
            docs: include_str!(concat!(
                // TODO: assumes all crates are at the top level
                env!("CARGO_MANIFEST_DIR"),
                "/../docs/errors/",
                stringify!($id),
                ".md",
            )),
        };
        $crate::__inventory::submit!($id);
        &$id
    }};
}

/// Create a warning code, attaching its docs from the associated file in the
/// `docs` folder and registering it with [`WarningCode::iter`].
#[macro_export]
macro_rules! warning_code {
    ($id:ident) => {{
        static $id: WarningCode = WarningCode {
            name: stringify!($id),
            docs: include_str!(concat!(
                // TODO: assumes all crates are at the top level
                env!("CARGO_MANIFEST_DIR"),
                "/../docs/warnings/",
                stringify!($id),
                ".md",
            )),
        };
        $crate::__inventory::submit!($id);
        &$id
    }};
}
