use crate::{ast::Identifier, typechecking::ComparableType};
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::span::SimpleSpan;
use std::collections::HashSet;

const ARIADNE_CONFIG: ariadne::Config =
    ariadne::Config::new().with_index_type(ariadne::IndexType::Byte);

#[repr(u32)]
pub enum Code {
    NameResolution = 200,
    TypeError = 300,
}

pub trait DiagnosticError {
    fn error_code(&self) -> u32;

    fn main_location(&self) -> SimpleSpan;

    fn message(&self) -> String;

    fn related_info(&self) -> Vec<DiagnosticAnnotation>;

    fn severity(&self) -> ReportKind<'static> {
        ReportKind::Error
    }
}

#[derive(Debug)]
pub enum NameResolutionError {
    NotFound {
        span: SimpleSpan,
    },
    RedeclarationError {
        ident: Identifier,
        previous: SimpleSpan,
    },
    AbiMismatch {
        def_span: SimpleSpan,
        abi_span: SimpleSpan,
    },
}

#[derive(Debug)]
pub enum TypeError {
    FieldNotFound {
        span: SimpleSpan,
        expected: String,
    },
    TypeMismatch {
        span: SimpleSpan,
        expected: ComparableType,
        found: ComparableType,
    },
    NonSigned {
        span: SimpleSpan,
        found: ComparableType,
    },
    LinearVariableUsedMoreThanOnce {
        var_span: SimpleSpan,
        span1: SimpleSpan,
        span2: SimpleSpan,
    },
    UnusedVariable {
        var_span: SimpleSpan,
        is_error: bool,
    },
    LinearVariableAffine {
        var_span: SimpleSpan,
        branch_span: SimpleSpan,
    },
    UtxoMainInvalidType {
        span: SimpleSpan,
    },
    EffectTypeMismatch {
        span: SimpleSpan,
        expected: HashSet<String>,
        found: HashSet<String>,
    },
    MissingEffectHandler {
        span: SimpleSpan,
        effect_name: String,
        interface_name: String,
    },
}

pub struct DiagnosticAnnotation {
    pub location: SimpleSpan,
    pub message: String,
    pub color: Color,
}

impl DiagnosticError for NameResolutionError {
    fn error_code(&self) -> u32 {
        let offset = match self {
            NameResolutionError::NotFound { span: _ } => 0,
            NameResolutionError::RedeclarationError {
                ident: _,
                previous: _,
            } => 1,
            NameResolutionError::AbiMismatch {
                def_span: _,
                abi_span: _,
            } => 2,
        };

        Code::NameResolution as u32 + offset
    }

    fn main_location(&self) -> SimpleSpan {
        match self {
            NameResolutionError::NotFound { span } => *span,
            NameResolutionError::RedeclarationError { ident, previous: _ } => {
                // TODO: this uselessly points to the start of the file if span isn't available.
                ident.span.unwrap_or(SimpleSpan::from(0..0))
            }
            NameResolutionError::AbiMismatch {
                def_span,
                abi_span: _,
            } => *def_span,
        }
    }

    fn message(&self) -> String {
        match self {
            NameResolutionError::NotFound { span: _ } => "not found in this scope".to_string(),
            NameResolutionError::RedeclarationError { ident, previous: _ } => {
                format!("function {} already declared", &ident.raw)
            }
            NameResolutionError::AbiMismatch {
                def_span: _,
                abi_span: _,
            } => "function doesn't match abi".to_string(),
        }
    }

    fn related_info(&self) -> Vec<DiagnosticAnnotation> {
        match self {
            NameResolutionError::NotFound { span: _ } => vec![],
            NameResolutionError::RedeclarationError { ident: _, previous } => {
                vec![DiagnosticAnnotation {
                    location: *previous,
                    message: "previous declaration".to_string(),
                    color: Color::BrightRed,
                }]
            }
            NameResolutionError::AbiMismatch {
                def_span: _,
                abi_span,
            } => vec![DiagnosticAnnotation {
                location: *abi_span,
                message: "defined here".to_string(),
                color: Color::Green,
            }],
        }
    }
}

impl DiagnosticError for TypeError {
    fn error_code(&self) -> u32 {
        let offset = match self {
            TypeError::FieldNotFound {
                span: _,
                expected: _,
            } => 0,
            TypeError::TypeMismatch {
                span: _,
                expected: _,
                found: _,
            } => 1,
            TypeError::NonSigned { span: _, found: _ } => 2,
            TypeError::LinearVariableUsedMoreThanOnce {
                var_span: _,
                span1: _,
                span2: _,
            } => 3,
            TypeError::UnusedVariable {
                var_span: _,
                is_error: _,
            } => 4,
            TypeError::LinearVariableAffine {
                var_span: _,
                branch_span: _,
            } => 5,
            TypeError::UtxoMainInvalidType { span: _ } => 6,
            TypeError::EffectTypeMismatch {
                span: _,
                expected: _,
                found: _,
            } => 7,
            TypeError::MissingEffectHandler {
                span: _,
                effect_name: _,
                interface_name: _,
            } => 8,
        };
        Code::TypeError as u32 + offset
    }

    fn main_location(&self) -> SimpleSpan {
        match self {
            TypeError::FieldNotFound { span, expected: _ } => *span,
            TypeError::TypeMismatch {
                span,
                expected: _,
                found: _,
            } => *span,
            TypeError::NonSigned { span, found: _ } => *span,
            TypeError::LinearVariableUsedMoreThanOnce {
                var_span,
                span1: _,
                span2: _,
            } => *var_span,
            TypeError::UnusedVariable { var_span, .. } => *var_span,
            TypeError::LinearVariableAffine {
                var_span,
                branch_span: _,
            } => *var_span,
            TypeError::UtxoMainInvalidType { span } => *span,
            TypeError::EffectTypeMismatch {
                span,
                expected: _,
                found: _,
            } => *span,
            TypeError::MissingEffectHandler {
                span,
                effect_name: _,
                interface_name: _,
            } => *span,
        }
    }

    fn message(&self) -> String {
        match self {
            TypeError::FieldNotFound { span: _, expected } => {
                format!("method or field {} not found", expected)
            }
            TypeError::TypeMismatch {
                span: _,
                expected,
                found,
            } => {
                format!("type mismatch: expected {}, found {}", expected, found)
            }
            TypeError::NonSigned { span: _, found } => {
                format!("expected a signed numeric type, found {}", found)
            }
            TypeError::LinearVariableUsedMoreThanOnce {
                var_span: _,
                span1: _,
                span2: _,
            } => "linear variable used more than once".to_string(),
            TypeError::UnusedVariable { var_span: _, .. } => "unused variable".to_string(),
            TypeError::LinearVariableAffine {
                var_span: _,
                branch_span: _,
            } => "linear variable consumed partially".to_string(),
            TypeError::UtxoMainInvalidType { span: _ } => {
                "main block in utxo should not return values".to_string()
            }
            TypeError::EffectTypeMismatch {
                span: _,
                expected,
                found,
            } => {
                format!(
                    "effect type mismatch: expected {:?}, found {:?}",
                    expected, found
                )
            }
            TypeError::MissingEffectHandler {
                span: _,
                effect_name,
                interface_name,
            } => {
                format!(
                    "missing handler for {} from {} interface",
                    effect_name, interface_name
                )
            }
        }
    }

    fn related_info(&self) -> Vec<DiagnosticAnnotation> {
        match self {
            TypeError::FieldNotFound {
                span: _,
                expected: _,
            } => vec![],
            TypeError::TypeMismatch {
                span: _,
                expected: _,
                found: _,
            } => vec![],
            TypeError::NonSigned { span: _, found: _ } => vec![],
            TypeError::LinearVariableUsedMoreThanOnce {
                var_span: _,
                span1,
                span2,
            } => {
                vec![
                    DiagnosticAnnotation {
                        location: *span1,
                        message: "first used here".to_string(),
                        color: Color::BrightRed,
                    },
                    DiagnosticAnnotation {
                        location: *span2,
                        message: "then used here".to_string(),
                        color: Color::BrightRed,
                    },
                ]
            }
            TypeError::UnusedVariable { var_span: _, .. } => vec![],
            TypeError::LinearVariableAffine {
                var_span: _,
                branch_span,
            } => {
                vec![DiagnosticAnnotation {
                    location: *branch_span,
                    message: "unused variable".to_string(),
                    color: Color::BrightRed,
                }]
            }
            TypeError::UtxoMainInvalidType { span: _ } => vec![],
            TypeError::EffectTypeMismatch {
                span: _,
                expected: _,
                found: _,
            } => vec![],
            TypeError::MissingEffectHandler {
                span: _,
                effect_name: _,
                interface_name: _,
            } => vec![],
        }
    }

    fn severity(&self) -> ReportKind<'static> {
        match self {
            TypeError::UnusedVariable {
                var_span: _,
                is_error,
            } => {
                if *is_error {
                    ReportKind::Error
                } else {
                    ReportKind::Warning
                }
            }
            _ => ReportKind::Error,
        }
    }
}

impl From<&TypeError> for Report<'static> {
    fn from(value: &TypeError) -> Self {
        to_report(value)
    }
}

impl From<&NameResolutionError> for Report<'static> {
    fn from(value: &NameResolutionError) -> Self {
        to_report(value)
    }
}

fn to_report<T>(value: &T) -> Report<'static>
where
    T: DiagnosticError,
{
    let code = value.error_code();
    let location = value.main_location();
    let message = value.message();
    let severity = value.severity();

    let mut report_builder = Report::build(severity, location.into_range())
        .with_config(ARIADNE_CONFIG)
        .with_code(code)
        .with_label(
            Label::new(location.into_range())
                .with_message(message)
                .with_color(Color::Red),
        );

    for annotation in value.related_info() {
        report_builder = report_builder.with_label(
            Label::new(annotation.location.into_range())
                .with_message(annotation.message)
                .with_color(annotation.color),
        );
    }

    report_builder.finish()
}
