use miette::{LabeledSpan, SourceSpan};
use starstream_types::Span;

#[derive(Debug, Clone)]
pub(super) struct DiagnosticCore {
    primary: Label,
    secondary: Vec<Label>,
    help: Option<String>,
}

impl DiagnosticCore {
    pub(super) fn new(span: Span) -> Self {
        Self {
            primary: Label::primary(span),
            secondary: Vec::new(),
            help: None,
        }
    }

    pub(super) fn with_secondary(mut self, span: Span, message: impl Into<String>) -> Self {
        self.secondary.push(Label::secondary(span, message));
        self
    }

    pub(super) fn with_primary_message(mut self, message: impl Into<String>) -> Self {
        self.primary.message = Some(message.into());
        self
    }

    pub(super) fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub(super) fn primary_span(&self) -> Span {
        self.primary.span
    }

    pub(super) fn help(&self) -> Option<&str> {
        self.help.as_deref()
    }

    pub(super) fn labels(&self) -> Vec<LabeledSpan> {
        let total = 1 + self.secondary.len();
        let mut labels = Vec::with_capacity(total);

        labels.push(self.primary.to_labeled_span(None));
        for secondary in &self.secondary {
            labels.push(secondary.to_labeled_span(Some(false)));
        }

        labels
    }
}

#[derive(Debug, Clone)]
struct Label {
    span: Span,
    message: Option<String>,
    is_primary: bool,
}

impl Label {
    fn primary(span: Span) -> Self {
        Self {
            span,
            message: None,
            is_primary: true,
        }
    }

    fn secondary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: Some(message.into()),
            is_primary: false,
        }
    }

    fn to_labeled_span(&self, force_primary: Option<bool>) -> LabeledSpan {
        let span = to_source_span(self.span);
        let message = self.message.clone();
        let is_primary = force_primary.unwrap_or(self.is_primary);

        match message {
            Some(msg) => {
                if is_primary {
                    LabeledSpan::new_primary_with_span(Some(msg), span)
                } else {
                    LabeledSpan::new_with_span(Some(msg), span)
                }
            }
            None => {
                if is_primary {
                    LabeledSpan::new_primary_with_span(None, span)
                } else {
                    LabeledSpan::new_with_span(None, span)
                }
            }
        }
    }
}

pub(super) fn to_source_span(span: Span) -> SourceSpan {
    let start = span.start.into();
    let len = span.end.saturating_sub(span.start);
    SourceSpan::new(start, len)
}
