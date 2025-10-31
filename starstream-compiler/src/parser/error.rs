use chumsky::error::{Rich, RichReason};
use miette::{Diagnostic, LabeledSpan, SourceSpan};
use starstream_types::Span;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{message}")]
pub struct ParseError {
    message: String,
    label: Option<String>,
    span: Span,
    help: Option<String>,
}

impl ParseError {
    pub fn from_rich(error: Rich<'_, char>) -> Self {
        let owned = error.into_owned();
        let message = owned.to_string();
        let span = *owned.span();

        let label = match owned.reason() {
            RichReason::Custom(msg) => Some(msg.to_string()),
            RichReason::ExpectedFound { found, .. } => Some(match found {
                Some(found) => format!("found `{found:?}`"),
                None => "found end of input".to_string(),
            }),
        };

        let mut expected_items = Vec::new();
        for expected in owned.expected() {
            let text = expected.to_string();
            if !text.is_empty() {
                expected_items.push(text);
            }
        }

        let help = if expected_items.is_empty() {
            None
        } else {
            Some(format!("expected {}", expected_items.join(", ")))
        };

        Self {
            message,
            label,
            span,
            help,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl Diagnostic for ParseError {
    fn code(&self) -> Option<Box<dyn std::fmt::Display + '_>> {
        Some(Box::new("starstream::parse"))
    }

    fn help(&self) -> Option<Box<dyn std::fmt::Display + '_>> {
        self.help
            .as_ref()
            .map(|help| Box::new(help.as_str()) as Box<dyn std::fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let span = to_source_span(self.span);
        let label = match &self.label {
            Some(text) => LabeledSpan::new_primary_with_span(Some(text.clone()), span),
            None => LabeledSpan::new_primary_with_span(None, span),
        };

        Some(Box::new(std::iter::once(label)))
    }
}

fn to_source_span(span: Span) -> SourceSpan {
    let len = span.end.saturating_sub(span.start);
    SourceSpan::new(span.start.into(), len)
}
