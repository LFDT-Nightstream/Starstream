//! Typed abstract syntax tree produced after type inference.
//!
//! The parser builds the untyped AST in [`crate::ast`]. Once we run inference
//! we convert that tree into these typed counterparts so downstream stages
//! (formatting, codegen, LSP features) can rely on explicit type information
//! without re-running inference.

use crate::{
    Spanned,
    ast::{BinaryOp, Identifier, Literal, UnaryOp},
    types::Type,
};

/// Entire program with types attached.
#[derive(Clone, Debug)]
pub struct TypedProgram {
    pub statements: Vec<TypedStatement>,
}

/// Typed statements.
#[derive(Clone, Debug)]
pub enum TypedStatement {
    VariableDeclaration {
        name: Identifier,
        value: Spanned<TypedExpr>,
    },
    Assignment {
        target: Identifier,
        value: Spanned<TypedExpr>,
    },
    If {
        condition: Spanned<TypedExpr>,
        then_branch: TypedBlock,
        else_branch: Option<TypedBlock>,
    },
    While {
        condition: Spanned<TypedExpr>,
        body: TypedBlock,
    },
    Block(TypedBlock),
    Expression(Spanned<TypedExpr>),
}

/// `{ statement* }`
#[derive(Clone, Debug)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
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
}

impl TypedExpr {
    pub fn new(ty: Type, kind: TypedExprKind) -> Self {
        Self { ty, kind }
    }
}

impl TypedBlock {
    pub fn new(statements: Vec<TypedStatement>) -> Self {
        Self { statements }
    }
}

impl TypedProgram {
    pub fn new(statements: Vec<TypedStatement>) -> Self {
        Self { statements }
    }
}
