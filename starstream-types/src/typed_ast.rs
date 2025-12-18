//! Typed abstract syntax tree produced after type inference.
//!
//! The parser builds the untyped AST in [`crate::ast`]. Once we run inference
//! we convert that tree into these typed counterparts so downstream stages
//! (formatting, codegen, LSP features) can rely on explicit type information
//! without re-running inference.

use crate::{
    FunctionExport, Spanned,
    ast::{BinaryOp, Identifier, Literal, UnaryOp},
    types::Type,
};

/// Entire program with types attached.
#[derive(Clone, Debug)]
pub struct TypedProgram {
    pub definitions: Vec<TypedDefinition>,
}

#[derive(Clone, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum TypedDefinition {
    Function(TypedFunctionDef),
    Struct(TypedStructDef),
    Enum(TypedEnumDef),
    Utxo(TypedUtxoDef),
}

#[derive(Clone, Debug)]
pub struct TypedFunctionDef {
    pub export: Option<FunctionExport>,
    pub name: Identifier,
    pub params: Vec<TypedFunctionParam>,
    pub return_type: Type,
    pub body: TypedBlock,
}

#[derive(Clone, Debug)]
pub struct TypedFunctionParam {
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
}

#[derive(Clone, Debug)]
pub enum TypedUtxoPart {
    Storage(Vec<TypedUtxoGlobal>),
}

#[derive(Clone, Debug)]
pub struct TypedUtxoGlobal {
    pub name: Identifier,
    pub ty: Type,
}

/// Typed statements.
#[derive(Clone, Debug)]
pub enum TypedStatement {
    VariableDeclaration {
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

/// Typed expression node. The [`kind`](TypedExpr::kind) mirrors the untyped
/// AST while [`ty`](TypedExpr::ty) carries the finalized type for quick access.
#[derive(Clone, Debug)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: TypedExprKind,
}

#[derive(Clone, Debug)]
pub enum TypedExprKind {
    Literal(Literal),
    Identifier(Identifier),
    Unary {
        op: UnaryOp,
        expr: Box<Spanned<TypedExpr>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Spanned<TypedExpr>>,
        right: Box<Spanned<TypedExpr>>,
    },
    Grouping(Box<Spanned<TypedExpr>>),
    StructLiteral {
        name: Identifier,
        fields: Vec<TypedStructLiteralField>,
    },
    FieldAccess {
        target: Box<Spanned<TypedExpr>>,
        field: Identifier,
    },
    EnumConstructor {
        enum_name: Identifier,
        variant: Identifier,
        payload: TypedEnumConstructorPayload,
    },
    Block(Box<TypedBlock>),
    If {
        branches: Vec<(Spanned<TypedExpr>, TypedBlock)>,
        else_branch: Option<Box<TypedBlock>>,
    },
    Match {
        scrutinee: Box<Spanned<TypedExpr>>,
        arms: Vec<TypedMatchArm>,
    },
    Call {
        callee: Box<Spanned<TypedExpr>>,
        args: Vec<Spanned<TypedExpr>>,
    },
}

#[derive(Clone, Debug)]
pub struct TypedStructLiteralField {
    pub name: Identifier,
    pub value: Spanned<TypedExpr>,
}

#[derive(Clone, Debug)]
pub enum TypedEnumConstructorPayload {
    Unit,
    Tuple(Vec<Spanned<TypedExpr>>),
    Struct(Vec<TypedStructLiteralField>),
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
        name: Identifier,
        fields: Vec<TypedStructPatternField>,
    },
    EnumVariant {
        enum_name: Identifier,
        variant: Identifier,
        payload: TypedEnumPatternPayload,
    },
}

#[derive(Clone, Debug)]
pub enum TypedEnumVariantPayload {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<TypedStructField>),
}

#[derive(Clone, Debug)]
pub enum TypedEnumPatternPayload {
    Unit,
    Tuple(Vec<TypedPattern>),
    Struct(Vec<TypedStructPatternField>),
}

#[derive(Clone, Debug)]
pub struct TypedStructPatternField {
    pub name: Identifier,
    pub pattern: Box<TypedPattern>,
}

impl TypedExpr {
    pub fn new(ty: Type, kind: TypedExprKind) -> Self {
        Self { ty, kind }
    }
}

impl TypedBlock {
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

impl TypedProgram {
    pub fn new(definitions: Vec<TypedDefinition>) -> Self {
        Self { definitions }
    }
}
