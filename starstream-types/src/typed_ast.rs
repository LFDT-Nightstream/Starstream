//! Typed abstract syntax tree produced after type inference.
//!
//! The parser builds the untyped AST in [`crate::ast`]. Once we run inference
//! we convert that tree into these typed counterparts so downstream stages
//! (formatting, codegen, LSP features) can rely on explicit type information
//! without re-running inference.

use std::sync::Arc;

use crate::{
    AbiType, DUMMY_SPAN, EnumType, FunctionExport, FunctionType, ImportSource, RecordType,
    ScopedName, Span, Spanned, TokenType, TypedAbiMethodDecl, UtxoType,
    ast::{BinaryOp, Identifier, Literal, UnaryOp},
    types::Type,
};

/// Entire program with types attached.
#[derive(Clone, Debug, Default)]
pub struct TypedProgram {
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
    /// All items imported, whether as a namespace or one-by-one.
    pub items: Vec<TypedImportItem>,
    pub from: ImportSource,
}

#[derive(Clone, Debug)]
pub struct TypedImportItem {
    pub imported: Identifier,
    pub local: Identifier,
    /// The type of the imported item (typically a function type).
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedFunctionDef {
    pub export: Option<FunctionExport>,
    pub name: Identifier,
    pub id: NameId,
    pub ty: Arc<FunctionType>,
    pub body: TypedBlock,
}

#[derive(Clone, Debug)]
pub struct TypedStructDef {
    pub ty: Arc<RecordType>,
}

#[derive(Clone, Debug)]
pub struct TypedEnumDef {
    pub ty: Arc<EnumType>,
}

#[derive(Clone, Debug)]
pub struct TypedUtxoDef {
    pub name: Identifier,
    pub parts: Vec<TypedUtxoPart>,
    pub ty: Arc<UtxoType>,
}

#[derive(Clone, Debug)]
pub enum TypedUtxoPart {
    Storage(Vec<TypedUtxoGlobal>),
    Function(Box<TypedFunctionDef>),
    AbiImpl {
        span: Span,
        abi: Arc<AbiType>,
        parts: Vec<TypedFunctionDef>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedUtxoGlobal {
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedTokenDef {
    pub name: Identifier,
    pub parts: Vec<TypedTokenPart>,
    pub ty: Arc<TokenType>,
}

#[derive(Clone, Debug)]
pub enum TypedTokenPart {
    Storage(Vec<TypedTokenGlobal>),
    Function(Box<TypedFunctionDef>),
    AbiImpl {
        span: Span,
        abi: Arc<AbiType>,
        parts: Vec<TypedFunctionDef>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedTokenGlobal {
    /// Whether the field carries the `indexed` modifier.
    pub indexed: bool,
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypedAbiDef {
    pub ty: Arc<AbiType>,
    /// All functions. If you only want methods, use [AbiType::methods].
    pub functions: Vec<TypedAbiMethodDecl>,
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
    TryWith {
        subject: TypedBlock,
        effects: Vec<(ScopedName, Vec<TypedPattern>, TypedBlock)>,
    },
}

/// `{ statement* }`
#[derive(Clone, Debug, Default)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub tail_expression: Option<Spanned<TypedExpr>>,
}

impl TypedBlock {
    pub fn ty(&self) -> &Type {
        match &self.tail_expression {
            None => &Type::Unit,
            Some(tail) => &tail.node.ty,
        }
    }

    pub fn tail_span(&self) -> Span {
        match &self.tail_expression {
            None => DUMMY_SPAN,
            Some(tail) => tail.span,
        }
    }
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
        /// The variable being narrowed.
        name: Identifier,
        /// The original type of the variable before narrowing.
        original_type: Type,
        /// The type being narrowed to.
        abi: Arc<AbiType>,
        /// The span of the right-hand side of the `is`.
        abi_name_span: Span,
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
    ScopedName {
        name: ScopedName,
        /// Enum variant ID if constant.
        constant: Option<usize>,
    },
    Literal(Literal),
    /// An anonymous tuple: `(a, b)`. Always at least two elements.
    Tuple(Vec<Spanned<TypedExpr>>),
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
        abis: Vec<Arc<AbiType>>,
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
        TypedExprKind::ScopedName {
            name: vec![value],
            constant: None,
        }
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
    /// An anonymous tuple pattern: `(a, b)`. Always at least two elements.
    AnonTuple { fields: Vec<TypedPattern> },
    /// `Enum::UnitVariant` constant.
    Constant { name: ScopedName, variant: usize },
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

/// Numeric identifier for a resolved string identifier.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NameId(pub u32);

impl NameId {
    pub fn fresh(&mut self) -> NameId {
        let id = NameId(self.0);
        self.0 += 1;
        id
    }
}

impl std::fmt::Debug for NameId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NameId({})", self.0)
    }
}

impl std::fmt::Display for NameId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
