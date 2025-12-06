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
        Some(Box::new(self.kind.error_code()))
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(Box::new(format!(
            "https://starstream.nightstream.dev/errors/{}",
            self.kind.error_code()
        )))
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

        if let TypeErrorKind::ArgumentTypeMismatch {
            expected,
            param_span: Some(span),
            ..
        } = &self.kind
        {
            labels.push(LabeledSpan::new_with_span(
                Some(format!("parameter expects `{}`", expected.to_compact_string())),
                to_source_span(*span),
            ));
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
    TypeAlreadyDefined {
        name: String,
    },
    FunctionAlreadyDefined {
        name: String,
    },
    DuplicateStructField {
        struct_name: String,
        field_name: String,
    },
    DuplicateEnumVariant {
        enum_name: String,
        variant_name: String,
    },
    UnknownStruct {
        name: String,
    },
    UnknownEnum {
        name: String,
    },
    UnknownStructField {
        struct_name: String,
        field_name: String,
    },
    DuplicateStructLiteralField {
        field_name: String,
    },
    MissingStructField {
        struct_name: String,
        field_name: String,
    },
    FieldAccessNotStruct {
        found: Type,
    },
    FieldAccessUnknownField {
        field_name: String,
        ty: Type,
    },
    UnknownEnumVariant {
        enum_name: String,
        variant_name: String,
    },
    EnumPayloadMismatch {
        enum_name: String,
        variant_name: String,
        expected: EnumPayloadKind,
        found: EnumPayloadKind,
    },
    MatchNotEnum {
        found: Type,
    },
    PatternEnumMismatch {
        enum_name: String,
        found: Type,
    },
    UnsupportedTypeFeature {
        description: String,
    },
    /// Pattern matching is not exhaustive; some cases are not covered.
    NonExhaustiveMatch {
        missing_patterns: Vec<String>,
    },
    /// A pattern in a match is unreachable because previous patterns already cover all its cases.
    UnreachablePattern,
    /// Attempted to call a value that is not a function.
    NotAFunction {
        found: Type,
    },
    /// Function called with wrong number of arguments.
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    /// Argument type does not match expected parameter type.
    ArgumentTypeMismatch {
        expected: Type,
        found: Type,
        position: usize,
        param_span: Option<Span>,
    },
}

impl TypeErrorKind {
    /// Returns the numeric error code for this error kind.
    pub fn error_code(&self) -> &'static str {
        match self {
            TypeErrorKind::UnknownVariable { .. } => "E0001",
            TypeErrorKind::Redeclaration { .. } => "E0002",
            TypeErrorKind::AssignmentMismatch { .. } => "E0003",
            TypeErrorKind::AssignmentToImmutable { .. } => "E0004",
            TypeErrorKind::UnaryMismatch { .. } => "E0005",
            TypeErrorKind::BinaryOperandMismatch { .. } => "E0006",
            TypeErrorKind::ConditionNotBool { .. } => "E0007",
            TypeErrorKind::GeneralMismatch { .. } => "E0008",
            TypeErrorKind::ReturnMismatch { .. } => "E0009",
            TypeErrorKind::MissingReturn { .. } => "E0010",
            TypeErrorKind::UnknownTypeAnnotation { .. } => "E0011",
            TypeErrorKind::TypeAlreadyDefined { .. } => "E0012",
            TypeErrorKind::FunctionAlreadyDefined { .. } => "E0013",
            TypeErrorKind::DuplicateStructField { .. } => "E0014",
            TypeErrorKind::DuplicateEnumVariant { .. } => "E0015",
            TypeErrorKind::UnknownStruct { .. } => "E0016",
            TypeErrorKind::UnknownEnum { .. } => "E0017",
            TypeErrorKind::UnknownStructField { .. } => "E0018",
            TypeErrorKind::DuplicateStructLiteralField { .. } => "E0019",
            TypeErrorKind::MissingStructField { .. } => "E0020",
            TypeErrorKind::FieldAccessNotStruct { .. } => "E0021",
            TypeErrorKind::FieldAccessUnknownField { .. } => "E0022",
            TypeErrorKind::UnknownEnumVariant { .. } => "E0023",
            TypeErrorKind::EnumPayloadMismatch { .. } => "E0024",
            TypeErrorKind::MatchNotEnum { .. } => "E0025",
            TypeErrorKind::PatternEnumMismatch { .. } => "E0026",
            TypeErrorKind::UnsupportedTypeFeature { .. } => "E0027",
            TypeErrorKind::NonExhaustiveMatch { .. } => "E0028",
            TypeErrorKind::UnreachablePattern => "E0029",
            TypeErrorKind::NotAFunction { .. } => "E0030",
            TypeErrorKind::ArityMismatch { .. } => "E0031",
            TypeErrorKind::ArgumentTypeMismatch { .. } => "E0032",
        }
    }
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

#[derive(Debug, Clone)]
pub enum EnumPayloadKind {
    Unit,
    Tuple(usize),
    Struct(usize),
}

impl EnumPayloadKind {
    pub fn unit() -> Self {
        EnumPayloadKind::Unit
    }

    pub fn tuple(len: usize) -> Self {
        EnumPayloadKind::Tuple(len)
    }

    pub fn struct_payload(len: usize) -> Self {
        EnumPayloadKind::Struct(len)
    }
}

impl fmt::Display for EnumPayloadKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnumPayloadKind::Unit => write!(f, "no payload"),
            EnumPayloadKind::Tuple(len) => {
                if *len == 1 {
                    write!(f, "a tuple with 1 value")
                } else {
                    write!(f, "a tuple with {len} values")
                }
            }
            EnumPayloadKind::Struct(len) => {
                if *len == 1 {
                    write!(f, "a struct with 1 field")
                } else {
                    write!(f, "a struct with {len} fields")
                }
            }
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
                "cannot assign value of type `{}` to variable `{name}` of type `{}`",
                found.to_compact_string(),
                expected.to_compact_string()
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
                "unary `{}` expects type `{}` but found `{}`",
                display_unary_op(*op),
                expected.to_compact_string(),
                found.to_compact_string()
            ),
            TypeErrorKind::BinaryOperandMismatch { op, left, right } => write!(
                f,
                "binary `{}` operands must match; found `{}` and `{}`",
                display_binary_op(*op),
                left.to_compact_string(),
                right.to_compact_string()
            ),
            TypeErrorKind::ConditionNotBool { context, found } => {
                write!(
                    f,
                    "the {} must have type `bool`, found `{}`",
                    context,
                    found.to_compact_string()
                )
            }
            TypeErrorKind::GeneralMismatch { expected, found } => {
                write!(
                    f,
                    "expected type `{}`, found `{}`",
                    expected.to_compact_string(),
                    found.to_compact_string()
                )
            }
            TypeErrorKind::ReturnMismatch { expected, found } => {
                write!(
                    f,
                    "return type `{}` does not match function signature `{}`",
                    found.to_compact_string(),
                    expected.to_compact_string()
                )
            }
            TypeErrorKind::MissingReturn { expected } => {
                write!(f, "missing return of type `{}`", expected.to_compact_string())
            }
            TypeErrorKind::UnknownTypeAnnotation { name } => {
                write!(f, "unknown type annotation `{name}`")
            }
            TypeErrorKind::TypeAlreadyDefined { name } => {
                write!(f, "type `{name}` is already defined in this module")
            }
            TypeErrorKind::FunctionAlreadyDefined { name } => {
                write!(f, "function `{name}` is already defined in this module")
            }
            TypeErrorKind::DuplicateStructField {
                struct_name,
                field_name,
            } => write!(
                f,
                "field `{field_name}` appears multiple times in struct `{struct_name}`"
            ),
            TypeErrorKind::DuplicateEnumVariant {
                enum_name,
                variant_name,
            } => write!(
                f,
                "variant `{variant_name}` appears multiple times in enum `{enum_name}`"
            ),
            TypeErrorKind::UnknownStruct { name } => write!(f, "unknown struct `{name}`"),
            TypeErrorKind::UnknownEnum { name } => write!(f, "unknown enum `{name}`"),
            TypeErrorKind::UnknownStructField {
                struct_name,
                field_name,
            } => write!(
                f,
                "struct `{struct_name}` has no field named `{field_name}`"
            ),
            TypeErrorKind::DuplicateStructLiteralField { field_name } => {
                write!(f, "field `{field_name}` is specified more than once")
            }
            TypeErrorKind::MissingStructField {
                struct_name,
                field_name,
            } => write!(
                f,
                "struct `{struct_name}` literal is missing field `{field_name}`"
            ),
            TypeErrorKind::FieldAccessNotStruct { found } => {
                write!(
                    f,
                    "cannot access fields on value of type `{}`",
                    found.to_compact_string()
                )
            }
            TypeErrorKind::FieldAccessUnknownField { field_name, ty } => {
                write!(
                    f,
                    "{} has no field named `{field_name}`",
                    field_owner_label(ty)
                )
            }
            TypeErrorKind::UnknownEnumVariant {
                enum_name,
                variant_name,
            } => write!(
                f,
                "enum `{enum_name}` has no variant named `{variant_name}`"
            ),
            TypeErrorKind::EnumPayloadMismatch {
                enum_name,
                variant_name,
                expected,
                found,
            } => write!(
                f,
                "variant `{enum_name}::{variant_name}` expects {expected} but {found} provided"
            ),
            TypeErrorKind::MatchNotEnum { found } => {
                write!(
                    f,
                    "match scrutinee must be an enum, found `{}`",
                    found.to_compact_string()
                )
            }
            TypeErrorKind::PatternEnumMismatch { enum_name, found } => write!(
                f,
                "pattern references enum `{enum_name}` but scrutinee has type `{}`",
                found.to_compact_string()
            ),
            TypeErrorKind::UnsupportedTypeFeature { description } => {
                write!(f, "unsupported type feature: {description}")
            }
            TypeErrorKind::NonExhaustiveMatch { .. } => {
                write!(f, "non-exhaustive match")
            }
            TypeErrorKind::UnreachablePattern => {
                write!(f, "unreachable pattern")
            }
            TypeErrorKind::NotAFunction { found } => {
                write!(
                    f,
                    "cannot call value of type `{}` as a function",
                    found.to_compact_string()
                )
            }
            TypeErrorKind::ArityMismatch { expected, found } => {
                let args = if *expected == 1 {
                    "argument"
                } else {
                    "arguments"
                };

                write!(
                    f,
                    "function expects {expected} {args} but {found} were provided"
                )
            }
            TypeErrorKind::ArgumentTypeMismatch {
                expected,
                found,
                position,
                ..
            } => {
                write!(
                    f,
                    "argument {position} has type `{}` but `{}` was expected",
                    found.to_compact_string(),
                    expected.to_compact_string()
                )
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

fn field_owner_label(ty: &Type) -> String {
    match ty {
        Type::Record(record) => format!("struct `{}`", record.name),
        Type::Enum(enum_type) => format!("enum `{}`", enum_type.name),
        _ => format!("type `{}`", ty.to_compact_string()),
    }
}
