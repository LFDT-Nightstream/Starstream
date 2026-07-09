//! Typed abstract syntax tree produced after type inference.
//!
//! The parser builds the untyped AST in [`crate::ast`]. Once we run inference
//! we convert that tree into these typed counterparts so downstream stages
//! (formatting, codegen, LSP features) can rely on explicit type information
//! without re-running inference.

use std::sync::Arc;

use crate::{
    Abi, FunctionExport, ScopedName, Span, Spanned,
    ast::{BinaryOp, Identifier, Literal, UnaryOp},
    types::Type,
};

/// Entire program with types attached.
#[derive(Clone, Debug, Default)]
pub struct TypedProgram {
    /// True if the program uses the `yield` expression anywhere.
    pub has_yields: bool,
    /// List of elements in the program.
    pub definitions: Vec<TypedDefinition>,
}

#[derive(Clone, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum TypedDefinition {
    Import(TypedImportDef),
    Function(TypedFunctionDef),
    Struct(TypedStructDef),
    Enum(TypedEnumDef),
    Utxo(TypedUtxoDef),
    Token(TypedTokenDef),
    Abi(TypedAbiDef),
    /// `contract;` marker carried through from the AST.
    Contract,
}

#[derive(Clone, Debug)]
pub struct TypedImportDef {
    pub items: TypedImportItems,
    pub from: TypedImportSource,
}

#[derive(Clone, Debug)]
pub enum TypedImportItems {
    Named(Vec<TypedImportNamedItem>),
    /// Namespace import with alias and the functions available in that namespace.
    Namespace {
        alias: Identifier,
        functions: Vec<TypedImportNamedItem>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedImportNamedItem {
    pub imported: Identifier,
    pub local: Identifier,
    /// The type of the imported item (typically a function type).
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum TypedImportSource {
    Wit {
        namespace: Identifier,
        package: Identifier,
        interface: Option<Identifier>,
    },
    Path {
        /// Raw path text as written in the source.
        value: String,
        /// Canonical absolute path of the resolved module, if known.
        canonical: Option<std::path::PathBuf>,
    },
}

impl std::fmt::Display for TypedImportSource {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypedImportSource::Wit {
                namespace,
                package,
                interface,
            } => {
                write!(f, "{namespace}:{package}")?;
                if let Some(interface) = interface {
                    write!(f, "/{interface}")?;
                }
                Ok(())
            }
            TypedImportSource::Path { value, .. } => write!(f, "\"{value}\""),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypedFunctionDef {
    pub export: Option<FunctionExport>,
    pub name: Identifier,
    pub params: Vec<TypedFunctionParam>,
    pub return_type: Type,
    pub body: TypedBlock,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedFunctionParam {
    pub public: bool,
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedStructDef {
    pub name: Identifier,
    pub fields: Vec<TypedStructField>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedStructField {
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedEnumDef {
    pub name: Identifier,
    pub variants: Vec<TypedEnumVariant>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedEnumVariant {
    pub name: Identifier,
    pub payload: TypedEnumVariantPayload,
}

#[derive(Clone, Debug)]
pub struct TypedUtxoDef {
    pub name: Identifier,
    pub parts: Vec<TypedUtxoPart>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum TypedUtxoPart {
    Storage(Vec<TypedUtxoGlobal>),
    Function(Box<TypedFunctionDef>),
    AbiImpl {
        span: Span,
        abi: Type,
        parts: Vec<TypedFunctionDef>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedUtxoGlobal {
    pub name: Identifier,
    pub ty: Type,
}

/// Shallow typed marker for a `token` definition. Token bodies are not yet
/// type-checked; this carries just the name/span so the definition survives
/// lowering. Fleshed out alongside the global `Token` type in a follow-up.
#[derive(Clone, Debug)]
pub struct TypedTokenDef {
    pub name: Identifier,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypedAbiDef {
    pub name: Identifier,
    pub parts: Vec<TypedAbiPart>,
}

#[derive(Clone, Debug)]
pub enum TypedAbiPart {
    Event(TypedEventDef),
    Effect(TypedEffectDef),
    FnDecl(TypedAbiMethodDecl),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedAbiMethodDecl {
    pub name: Identifier,
    pub params: Vec<TypedFunctionParam>,
    pub return_type: Type,
    pub span: Span,
}

impl TypedAbiMethodDecl {
    /// Get the stable hashable identity of this method type.
    #[must_use]
    pub fn identity(&self) -> &str {
        // TODO: specify hashing for types and include real type signature here
        self.name.as_str()
    }
}

#[derive(Clone, Debug)]
pub struct TypedEventDef {
    pub name: Identifier,
    pub params: Vec<TypedFunctionParam>,
}

#[derive(Clone, Debug)]
pub struct TypedEffectDef {
    pub name: Identifier,
    pub params: Vec<TypedFunctionParam>,
    pub return_type: Type,
}

/// Typed statements.
#[derive(Clone, Debug)]
pub enum TypedStatement {
    VariableDeclaration {
        public: bool,
        mutable: bool,
        name: Identifier,
        value: Spanned<TypedExpr>,
    },
    Assignment {
        target: Identifier,
        value: Spanned<TypedExpr>,
    },
    While {
        condition: Spanned<TypedExpr>,
        body: TypedBlock,
    },
    Expression(Spanned<TypedExpr>),
    Return(Option<Spanned<TypedExpr>>),
    Resume,
}

/// `{ statement* }`
#[derive(Clone, Debug, Default)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub tail_expression: Option<Spanned<TypedExpr>>,
}

impl From<Vec<TypedStatement>> for TypedBlock {
    fn from(value: Vec<TypedStatement>) -> Self {
        TypedBlock {
            statements: value,
            tail_expression: None,
        }
    }
}

impl From<Spanned<TypedExpr>> for TypedBlock {
    fn from(value: Spanned<TypedExpr>) -> Self {
        TypedBlock {
            statements: Vec::new(),
            tail_expression: Some(value),
        }
    }
}

/// The condition part of a typed `if` branch.
#[derive(Clone, Debug)]
pub enum TypedIfCondition {
    /// A normal boolean expression: `if (expr) { ... }`
    Bool(Spanned<TypedExpr>),
    /// A type-narrowing test: `if ident is AbiType { ... }`
    Is {
        name: Identifier,
        abi_name: Identifier,
        /// The original type of the variable before narrowing.
        original_type: Type,
    },
}

/// Typed expression node. The [`kind`](TypedExpr::kind) mirrors the untyped
/// AST while [`ty`](TypedExpr::ty) carries the finalized type for quick access.
#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: TypedExprKind,
}

pub type TypedArguments = Vec<Spanned<TypedExpr>>;

#[derive(Clone, Debug)]
pub enum TypedExprKind {
    // Primary expressions ----------
    Grouping(Box<Spanned<TypedExpr>>),
    ScopedName(ScopedName),
    Literal(Literal),
    StructConstructor {
        name: ScopedName,
        fields: Vec<TypedStructFieldInitializer>,
        /// Discriminant, or 0 if not an enum.
        enum_variant: usize,
    },
    /// Visibility-lifting expression: `disclose(expr)`
    Disclose {
        expr: Box<Spanned<TypedExpr>>,
    },
    /// Event emission expression: `emit some_emit_fn(args...)`
    Emit {
        callee: Box<Spanned<TypedExpr>>,
        args: TypedArguments,
    },
    /// Effectful call: `raise some_effect_fn(...)`
    Raise {
        callee: Box<Spanned<TypedExpr>>,
        args: TypedArguments,
    },
    /// Runtime call: `runtime some_runtime_fn(...)`
    Runtime {
        callee: Box<Spanned<TypedExpr>>,
        args: TypedArguments,
    },
    // Control-flow primary expressions ----------
    /// `yield` and `yield(AbiName, ...)`
    Yield {
        /// Empty for bare `yield`, or list of abi infos
        abis: Vec<Arc<Abi>>,
    },
    Block(Box<TypedBlock>),
    If {
        branches: Vec<(TypedIfCondition, TypedBlock)>,
        else_branch: Option<Box<TypedBlock>>,
    },
    Match {
        scrutinee: Box<Spanned<TypedExpr>>,
        arms: Vec<TypedMatchArm>,
    },
    // Postfix expressions ----------
    Call {
        callee: Box<Spanned<TypedExpr>>,
        args: TypedArguments,
    },
    FieldAccess {
        target: Box<Spanned<TypedExpr>>,
        field: Identifier,
    },
    // Operators ----------
    Unary {
        op: UnaryOp,
        expr: Box<Spanned<TypedExpr>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Spanned<TypedExpr>>,
        right: Box<Spanned<TypedExpr>>,
    },
}

impl From<Identifier> for TypedExprKind {
    fn from(value: Identifier) -> Self {
        TypedExprKind::ScopedName(vec![value])
    }
}

impl From<Identifier> for Spanned<TypedExprKind> {
    fn from(value: Identifier) -> Self {
        let span = value.span;
        Spanned::new(TypedExprKind::from(value), span)
    }
}

#[derive(Clone, Debug)]
pub struct TypedStructFieldInitializer {
    pub name: Identifier,
    pub value: Spanned<TypedExpr>,
}

#[derive(Clone, Debug)]
pub struct TypedMatchArm {
    pub pattern: TypedPattern,
    pub body: TypedBlock,
}

#[derive(Clone, Debug)]
pub enum TypedPattern {
    /// A binding pattern that captures the matched value.
    Binding(Identifier),
    /// A wildcard pattern that matches anything but doesn't bind.
    Wildcard,
    /// A literal pattern that matches a specific value.
    Literal(Literal),
    Struct {
        name: ScopedName,
        fields: Vec<TypedStructPatternField>,
    },
    Tuple {
        name: ScopedName,
        fields: Vec<TypedPattern>,
    },
    /// `Enum::UnitVariant` constant.
    Constant { name: ScopedName, variant: usize },
}

#[derive(Clone, Debug)]
pub enum TypedEnumVariantPayload {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<TypedStructField>),
}

#[derive(Clone, Debug)]
pub struct TypedStructPatternField {
    pub name: Identifier,
    pub pattern: Box<TypedPattern>,
}

impl TypedExpr {
    #[must_use]
    pub fn new(ty: Type, kind: TypedExprKind) -> Self {
        Self { ty, kind }
    }
}

impl TypedBlock {
    #[must_use]
    pub fn new(
        statements: Vec<TypedStatement>,
        tail_expression: Option<Spanned<TypedExpr>>,
    ) -> Self {
        Self {
            statements,
            tail_expression,
        }
    }
}
