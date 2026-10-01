use chumsky::error::{Rich, RichReason};
use miette::{Diagnostic, LabeledSpan};
use starstream_types::{Span, SpanExt};

#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    label: Option<String>,
    span: Span,
    help: Option<String>,
}

impl From<Rich<'_, char>> for ParseError {
    fn from(error: Rich<'_, char>) -> Self {
        let message = error.to_string();
        let span = *error.span();

        let label = match error.into_reason() {
            RichReason::Custom(msg) => msg,
            RichReason::ExpectedFound { found, .. } => match found {
                Some(found) => format!("found {found:?}"),
                None => "found end of input".to_string(),
            },
        };

        Self {
            message,
            label: Some(label),
            span,
            // Could put "expected ..." into `help` but it's redundant with the message from to_string().
            help: None,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ParseError {}

impl Diagnostic for ParseError {
    fn help(&self) -> Option<Box<dyn std::fmt::Display + '_>> {
        self.help
            .as_ref()
            .map(|help| Box::new(help.as_str()) as Box<dyn std::fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let span = self.span.miette();
        let label = match &self.label {
            Some(text) => LabeledSpan::new_primary_with_span(Some(text.clone()), span),
            None => LabeledSpan::new_primary_with_span(None, span),
        };

        Some(Box::new(std::iter::once(label)))
    }
}
