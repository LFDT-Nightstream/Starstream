use std::fmt;

use miette::{Diagnostic, LabeledSpan, SourceSpan};
use starstream_types::{
    Span, Type,
    ast::{BinaryOp, UnaryOp},
};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct TypeError {
    pub kind: TypeErrorKind,
    primary: Label,
    secondary: Vec<Label>,
    help: Option<String>,
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, span: Span) -> Self {
        Self {
            kind,
            primary: Label {
                span,
                message: None,
                is_primary: true,
            },
            secondary: Vec::new(),
            help: None,
        }
    }

    pub fn with_secondary(mut self, span: Span, message: impl Into<String>) -> Self {
        self.secondary.push(Label {
            span,
            message: Some(message.into()),
            is_primary: false,
        });
        self
    }

    pub fn with_primary_message(mut self, message: impl Into<String>) -> Self {
        self.primary.message = Some(message.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn primary_span(&self) -> Span {
        self.primary.span
    }
}

impl Diagnostic for TypeError {
    fn code(&self) -> Option<Box<dyn fmt::Display + '_>> {
        let code = match &self.kind {
            TypeErrorKind::UnknownVariable { .. } => "starstream::type::unknown_variable",
            TypeErrorKind::Redeclaration { .. } => "starstream::type::redeclaration",
            TypeErrorKind::AssignmentMismatch { .. } => "starstream::type::assignment_mismatch",
            TypeErrorKind::AssignmentToImmutable { .. } => {
                "starstream::type::assignment_to_immutable"
            }
            TypeErrorKind::UnaryMismatch { .. } => "starstream::type::unary_mismatch",
            TypeErrorKind::BinaryOperandMismatch { .. } => "starstream::type::binary_operands",
            TypeErrorKind::ConditionNotBool { .. } => "starstream::type::condition_not_bool",
            TypeErrorKind::GeneralMismatch { .. } => "starstream::type::mismatch",
            TypeErrorKind::ReturnMismatch { .. } => "starstream::type::return_mismatch",
            TypeErrorKind::MissingReturn { .. } => "starstream::type::missing_return",
            TypeErrorKind::UnknownTypeAnnotation { .. } => {
                "starstream::type::unknown_type_annotation"
            }
            TypeErrorKind::UnsupportedTypeFeature { .. } => {
                "starstream::type::unsupported_type_feature"
            }
        };
        Some(Box::new(code))
    }

    fn help(&self) -> Option<Box<dyn fmt::Display + '_>> {
        self.help
            .as_ref()
            .map(|help| Box::new(help.as_str()) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let total = 1 + self.secondary.len();
        let mut labels = Vec::with_capacity(total);

        labels.push(self.primary.to_labeled_span(None));
        for secondary in &self.secondary {
            labels.push(secondary.to_labeled_span(Some(false)));
        }

        Some(Box::new(labels.into_iter()))
    }
}

#[derive(Debug)]
struct Label {
    span: Span,
    message: Option<String>,
    is_primary: bool,
}

impl Label {
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

fn to_source_span(span: Span) -> SourceSpan {
    let start = span.start.into();
    let len = span.end.saturating_sub(span.start);
    SourceSpan::new(start, len)
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    UnknownVariable {
        name: String,
    },
    Redeclaration {
        name: String,
    },
    AssignmentMismatch {
        name: String,
        expected: Type,
        found: Type,
    },
    AssignmentToImmutable {
        name: String,
    },
    UnaryMismatch {
        op: UnaryOp,
        expected: Type,
        found: Type,
    },
    BinaryOperandMismatch {
        op: BinaryOp,
        left: Type,
        right: Type,
    },
    ConditionNotBool {
        context: ConditionContext,
        found: Type,
    },
    GeneralMismatch {
        expected: Type,
        found: Type,
    },
    ReturnMismatch {
        expected: Type,
        found: Type,
    },
    MissingReturn {
        expected: Type,
    },
    UnknownTypeAnnotation {
        name: String,
    },
    UnsupportedTypeFeature {
        description: String,
    },
}

#[derive(Debug, Clone)]
pub enum ConditionContext {
    If,
    While,
}

impl fmt::Display for ConditionContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConditionContext::If => write!(f, "if condition"),
            ConditionContext::While => write!(f, "while condition"),
        }
    }
}

impl fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeErrorKind::UnknownVariable { name } => write!(f, "unknown variable `{name}`"),
            TypeErrorKind::Redeclaration { name } => {
                write!(f, "variable `{name}` is already defined in this scope")
            }
            TypeErrorKind::AssignmentMismatch {
                name,
                expected,
                found,
            } => write!(
                f,
                "cannot assign value of type `{found}` to variable `{name}` of type `{expected}`"
            ),
            TypeErrorKind::AssignmentToImmutable { name } => {
                write!(f, "cannot assign to immutable variable `{name}`")
            }
            TypeErrorKind::UnaryMismatch {
                op,
                expected,
                found,
            } => write!(
                f,
                "unary `{}` expects type `{expected}` but found `{found}`",
                display_unary_op(*op)
            ),
            TypeErrorKind::BinaryOperandMismatch { op, left, right } => write!(
                f,
                "binary `{}` operands must match; found `{}` and `{}`",
                display_binary_op(*op),
                left,
                right
            ),
            TypeErrorKind::ConditionNotBool { context, found } => {
                write!(f, "the {} must have type `bool`, found `{found}`", context)
            }
            TypeErrorKind::GeneralMismatch { expected, found } => {
                write!(f, "expected type `{expected}`, found `{found}`")
            }
            TypeErrorKind::ReturnMismatch { expected, found } => {
                write!(
                    f,
                    "return type `{found}` does not match function signature `{expected}`"
                )
            }
            TypeErrorKind::MissingReturn { expected } => {
                write!(f, "missing return of type `{expected}`")
            }
            TypeErrorKind::UnknownTypeAnnotation { name } => {
                write!(f, "unknown type annotation `{name}`")
            }
            TypeErrorKind::UnsupportedTypeFeature { description } => {
                write!(f, "unsupported type feature: {description}")
            }
        }
    }
}

fn display_binary_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Remainder => "%",
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
    }
}

fn display_unary_op(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Negate => "-",
        UnaryOp::Not => "!",
    }
}
