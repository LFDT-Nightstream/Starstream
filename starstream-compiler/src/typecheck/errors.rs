use std::fmt;

use miette::{Diagnostic, LabeledSpan};
use starstream_types::{
    ErrorCode, FunctionKind, Span, Type,
    ast::{BinaryOp, UnaryOp},
    error_code,
};
use thiserror::Error;

use super::diagnostic::{DiagnosticCore, to_source_span};

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct TypeError {
    pub kind: TypeErrorKind,
    core: DiagnosticCore,
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, span: Span) -> Self {
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

    pub fn primary_span(&self) -> Span {
        self.core.primary_span()
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
        self.core
            .help()
            .map(|help| Box::new(help) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let mut labels = self.core.labels();

        if let TypeErrorKind::ArgumentTypeMismatch {
            expected,
            param_span: Some(span),
            ..
        } = &self.kind
        {
            labels.push(LabeledSpan::new_with_span(
                Some(format!(
                    "parameter expects `{}`",
                    expected.to_compact_string()
                )),
                to_source_span(*span),
            ));
        }

        Some(Box::new(labels.into_iter()))
    }
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    UnknownName {
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
    UnknownNamespace {
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
    /// Attempted to emit an event that doesn't exist.
    UnknownEvent {
        name: String,
    },
    /// Event called with wrong number of arguments.
    EventArityMismatch {
        event_name: String,
        expected: usize,
        found: usize,
    },
    /// Event argument type does not match expected parameter type.
    EventArgumentTypeMismatch {
        event_name: String,
        expected: Type,
        found: Type,
        position: usize,
        param_span: Option<Span>,
    },
    /// Unknown package in import.
    UnknownImportPackage {
        namespace: String,
        package: String,
    },
    /// Unknown interface in import.
    UnknownImportInterface {
        namespace: String,
        package: String,
        interface: String,
    },
    /// Unknown function in import.
    UnknownImportFunction {
        path: String,
        name: String,
    },
    /// Keyword `raise`, `emit`, or `runtime` used to call a normal function.
    EmitRaiseRuntimeUnneeded {
        function_name: String,
        unneeded_keyword: FunctionKind,
    },
    /// Keyword `raise`, `emit`, or `runtime` needed to call a function.
    EmitRaiseRuntimeNeeded {
        function_name: String,
        needed_keyword: FunctionKind,
    },
    /// Function requires keyword, but the wrong one was used.
    EmitRaiseRuntimeMismatch {
        function_name: String,
        needed_keyword: FunctionKind,
        wrong_keyword: FunctionKind,
    },
    // E0041 was RuntimeWithoutKeyword, replaced with EmitRaiseRuntimeNeeded
    /// Writing or initializing a public binding requires explicit disclosure of private data.
    ExplicitDisclosureRequiredForPublicBinding {
        variable_name: String,
    },
    /// Generic type used with wrong number of type arguments.
    WrongGenericArity {
        type_name: String,
        expected: usize,
        found: usize,
    },
    /// A function was declared with a return type in a position where it shouldn't have one.
    ReturnTypeNotAllowed,
    /// An integer literal is out of range for its resolved type.
    LiteralOutOfRange {
        value: i128,
        ty: Type,
    },
    /// `if x is Abi` used on a non-Utxo variable.
    IsCheckRequiresUtxo {
        name: String,
        found: Type,
    },
    /// Unknown ABI name in `if x is UnknownAbi`.
    UnknownAbi {
        name: String,
    },
    /// ABI has no method with the given name.
    AbiMethodNotFound {
        abi_name: String,
        method_name: String,
    },
    /// More than one method call on a narrowed ABI variable in the same block.
    LinearMethodCallViolation {
        var_name: String,
        abi_name: String,
    },
    /// A path import names an item that the target module does not export.
    UnknownPathExport {
        /// The raw path text from the `from "…"` clause.
        path: String,
        /// The name that wasn't found.
        name: String,
    },
    /// `yield` appears outside Utxo `main fn`.
    YieldOutsideMainFn,
    /// A `token` definition has no `mint fn`.
    TokenMissingMintFn {
        name: String,
    },
    /// A `token` definition has no `impl Token { ... }` block.
    TokenMissingImpl {
        name: String,
    },
    /// A `token` definition has more than one `impl Token { ... }` block.
    TokenDuplicateImpl {
        name: String,
    },
    /// User code tried to declare an `abi` with a reserved built-in name (e.g. `Token`).
    ReservedAbiName {
        name: String,
    },
}

impl TypeErrorKind {
    /// Returns the numeric error code for this error kind.
    pub fn error_code(&self) -> &'static ErrorCode {
        match self {
            TypeErrorKind::UnknownName { .. } => error_code!(E0001),
            TypeErrorKind::Redeclaration { .. } => error_code!(E0002),
            TypeErrorKind::AssignmentMismatch { .. } => error_code!(E0003),
            TypeErrorKind::AssignmentToImmutable { .. } => error_code!(E0004),
            TypeErrorKind::UnaryMismatch { .. } => error_code!(E0005),
            TypeErrorKind::BinaryOperandMismatch { .. } => error_code!(E0006),
            TypeErrorKind::ConditionNotBool { .. } => error_code!(E0007),
            TypeErrorKind::GeneralMismatch { .. } => error_code!(E0008),
            TypeErrorKind::ReturnMismatch { .. } => error_code!(E0009),
            TypeErrorKind::MissingReturn { .. } => error_code!(E0010),
            TypeErrorKind::UnknownTypeAnnotation { .. } => error_code!(E0011),
            TypeErrorKind::TypeAlreadyDefined { .. } => error_code!(E0012),
            TypeErrorKind::FunctionAlreadyDefined { .. } => error_code!(E0013),
            TypeErrorKind::DuplicateStructField { .. } => error_code!(E0014),
            TypeErrorKind::DuplicateEnumVariant { .. } => error_code!(E0015),
            TypeErrorKind::UnknownStruct { .. } => error_code!(E0016),
            TypeErrorKind::UnknownNamespace { .. } => error_code!(E0017),
            TypeErrorKind::UnknownStructField { .. } => error_code!(E0018),
            TypeErrorKind::DuplicateStructLiteralField { .. } => error_code!(E0019),
            TypeErrorKind::MissingStructField { .. } => error_code!(E0020),
            TypeErrorKind::FieldAccessNotStruct { .. } => error_code!(E0021),
            TypeErrorKind::FieldAccessUnknownField { .. } => error_code!(E0022),
            TypeErrorKind::UnknownEnumVariant { .. } => error_code!(E0023),
            TypeErrorKind::EnumPayloadMismatch { .. } => error_code!(E0024),
            TypeErrorKind::PatternEnumMismatch { .. } => error_code!(E0025),
            TypeErrorKind::UnsupportedTypeFeature { .. } => error_code!(E0026),
            TypeErrorKind::NonExhaustiveMatch { .. } => error_code!(E0027),
            TypeErrorKind::UnreachablePattern => error_code!(E0028),
            TypeErrorKind::NotAFunction { .. } => error_code!(E0029),
            TypeErrorKind::ArityMismatch { .. } => error_code!(E0030),
            TypeErrorKind::ArgumentTypeMismatch { .. } => error_code!(E0031),
            TypeErrorKind::UnknownEvent { .. } => error_code!(E0032),
            TypeErrorKind::EventArityMismatch { .. } => error_code!(E0033),
            TypeErrorKind::EventArgumentTypeMismatch { .. } => error_code!(E0034),
            TypeErrorKind::UnknownImportPackage { .. } => error_code!(E0035),
            TypeErrorKind::UnknownImportInterface { .. } => error_code!(E0036),
            TypeErrorKind::UnknownImportFunction { .. } => error_code!(E0037),
            TypeErrorKind::EmitRaiseRuntimeUnneeded { .. } => error_code!(E0038),
            TypeErrorKind::EmitRaiseRuntimeNeeded { .. } => error_code!(E0039),
            TypeErrorKind::EmitRaiseRuntimeMismatch { .. } => error_code!(E0040),
            // E0041 removed
            TypeErrorKind::WrongGenericArity { .. } => error_code!(E0042),
            TypeErrorKind::ReturnTypeNotAllowed => error_code!(E0043),
            TypeErrorKind::LiteralOutOfRange { .. } => error_code!(E0044),
            TypeErrorKind::ExplicitDisclosureRequiredForPublicBinding { .. } => error_code!(E0045),
            TypeErrorKind::IsCheckRequiresUtxo { .. } => error_code!(E0046),
            TypeErrorKind::UnknownAbi { .. } => error_code!(E0047),
            TypeErrorKind::AbiMethodNotFound { .. } => error_code!(E0048),
            TypeErrorKind::LinearMethodCallViolation { .. } => error_code!(E0049),
            TypeErrorKind::UnknownPathExport { .. } => error_code!(E0050),
            TypeErrorKind::YieldOutsideMainFn => error_code!(E0051),
            TypeErrorKind::TokenMissingMintFn { .. } => error_code!(E0052),
            TypeErrorKind::TokenMissingImpl { .. } => error_code!(E0053),
            TypeErrorKind::TokenDuplicateImpl { .. } => error_code!(E0054),
            TypeErrorKind::ReservedAbiName { .. } => error_code!(E0055),
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
            TypeErrorKind::UnknownName { name } => write!(f, "unknown name `{name}`"),
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
                write!(
                    f,
                    "missing return of type `{}`",
                    expected.to_compact_string()
                )
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
            TypeErrorKind::UnknownNamespace { name } => write!(f, "unknown namespace `{name}`"),
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
                write!(
                    f,
                    "function expects {expected} argument{} but {found} {} provided",
                    if *expected == 1 { "" } else { "s" },
                    if *found == 1 { "was" } else { "were" },
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
            TypeErrorKind::UnknownEvent { name } => write!(f, "unknown event `{name}`"),
            TypeErrorKind::EventArityMismatch {
                event_name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "event `{event_name}` expects {expected} argument{} but {found} {} provided",
                    if *expected == 1 { "" } else { "s" },
                    if *found == 1 { "was" } else { "were" },
                )
            }
            TypeErrorKind::EventArgumentTypeMismatch {
                event_name,
                expected,
                found,
                position,
                ..
            } => {
                write!(
                    f,
                    "event `{event_name}` argument {position} has type `{}` but `{}` was expected",
                    found.to_compact_string(),
                    expected.to_compact_string()
                )
            }
            TypeErrorKind::UnknownImportPackage { namespace, package } => {
                write!(f, "unknown package `{namespace}:{package}`")
            }
            TypeErrorKind::UnknownImportInterface {
                namespace,
                package,
                interface,
            } => {
                write!(f, "unknown interface `{namespace}:{package}/{interface}`")
            }
            TypeErrorKind::UnknownImportFunction { path, name } => {
                write!(f, "unknown function `{name}` in `{path}`")
            }
            TypeErrorKind::EmitRaiseRuntimeUnneeded {
                function_name,
                unneeded_keyword,
            } => {
                write!(
                    f,
                    "`{}` keyword not required to call normal function `{}`",
                    unneeded_keyword.call_keyword(),
                    function_name,
                )
            }
            TypeErrorKind::EmitRaiseRuntimeNeeded {
                function_name,
                needed_keyword,
            } => {
                write!(
                    f,
                    "`{}` keyword required to call `{} {}`",
                    needed_keyword.call_keyword(),
                    needed_keyword.declaration_keyword(),
                    function_name,
                )
            }
            TypeErrorKind::EmitRaiseRuntimeMismatch {
                function_name,
                needed_keyword,
                wrong_keyword,
            } => {
                write!(
                    f,
                    "`{}` keyword used instead of `{}` required to call `{} {}`",
                    wrong_keyword.call_keyword(),
                    needed_keyword.call_keyword(),
                    needed_keyword.declaration_keyword(),
                    function_name,
                )
            }
            // E0041 removed
            TypeErrorKind::ExplicitDisclosureRequiredForPublicBinding { variable_name } => {
                write!(
                    f,
                    "public binding `{variable_name}` requires a public RHS; wrap private values with `disclose(...)`"
                )
            }
            TypeErrorKind::WrongGenericArity {
                type_name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "type `{type_name}` expects {expected} generic argument{} but {found} {} provided",
                    if *expected == 1 { "" } else { "s" },
                    if *found == 1 { "was" } else { "were" },
                )
            }
            TypeErrorKind::ReturnTypeNotAllowed => {
                write!(f, "return type not allowed on this function")
            }
            TypeErrorKind::LiteralOutOfRange { value, ty } => {
                write!(
                    f,
                    "integer literal `{value}` does not fit in type `{}`",
                    ty.to_compact_string()
                )
            }
            TypeErrorKind::IsCheckRequiresUtxo { name, found } => {
                write!(
                    f,
                    "`if {name} is ...` requires a Utxo type, but `{name}` has type `{}`",
                    found.to_compact_string()
                )
            }
            TypeErrorKind::UnknownAbi { name } => {
                write!(f, "unknown ABI type `{name}`")
            }
            TypeErrorKind::AbiMethodNotFound {
                abi_name,
                method_name,
            } => {
                write!(f, "ABI `{abi_name}` has no method named `{method_name}`")
            }
            TypeErrorKind::LinearMethodCallViolation { var_name, abi_name } => {
                write!(
                    f,
                    "only one method call is allowed on narrowed variable `{var_name}` (ABI `{abi_name}`)"
                )
            }
            TypeErrorKind::UnknownPathExport { path, name } => {
                write!(f, "module `{path}` does not export `{name}`")
            }
            TypeErrorKind::YieldOutsideMainFn => {
                write!(f, "`yield` can only be used inside a `main fn`")
            }
            TypeErrorKind::TokenMissingMintFn { name } => {
                write!(f, "token `{name}` must have at least one `mint fn`")
            }
            TypeErrorKind::TokenMissingImpl { name } => {
                write!(
                    f,
                    "token `{name}` must have an `impl Token {{ ... }}` block"
                )
            }
            TypeErrorKind::TokenDuplicateImpl { name } => {
                write!(
                    f,
                    "token `{name}` may have only one `impl Token {{ ... }}` block"
                )
            }
            TypeErrorKind::ReservedAbiName { name } => {
                write!(f, "`{name}` is a built-in ABI and cannot be redeclared")
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
