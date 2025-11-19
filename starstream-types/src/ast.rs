//! Abstract syntax tree for the pared-down Starstream DSL.

use chumsky::span::SimpleSpan;
use serde::Serialize;

pub type Span = SimpleSpan;

/// Entire program: a sequence of definitions.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

/// Top-level items.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Definition {
    Function(FunctionDef),
    Struct(StructDef),
    Enum(EnumDef),
}

/// Statements allowed by the current grammar.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Statement {
    VariableDeclaration {
        mutable: bool,
        name: Identifier,
        ty: Option<TypeAnnotation>,
        value: Spanned<Expr>,
    },
    Assignment {
        target: Identifier,
        value: Spanned<Expr>,
    },
    If {
        branches: Vec<(Spanned<Expr>, Block)>,
        else_branch: Option<Block>,
    },
    While {
        condition: Spanned<Expr>,
        body: Block,
    },
    Block(Block),
    Expression(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
}

/// `{ statement* }`
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail_expression: Option<Spanned<Expr>>,
}

/// Expressions with precedence encoded via parser, not the AST shape.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Unary {
        op: UnaryOp,
        expr: Box<Spanned<Expr>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Grouping(Box<Spanned<Expr>>),
    StructLiteral {
        name: Identifier,
        fields: Vec<StructLiteralField>,
    },
    FieldAccess {
        target: Box<Spanned<Expr>>,
        field: Identifier,
    },
    EnumConstructor {
        enum_name: Identifier,
        variant: Identifier,
        payload: Vec<Spanned<Expr>>,
    },
    Match {
        scrutinee: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    Unit,
}

#[derive(Copy, Clone, Debug, Serialize, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Copy, Clone, Debug, Serialize, PartialEq, Eq)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Remainder,
    Add,
    Subtract,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct StructLiteralField {
    pub name: Identifier,
    pub value: Spanned<Expr>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Pattern {
    Binding(Identifier),
    Struct {
        name: Identifier,
        fields: Vec<StructPatternField>,
    },
    EnumVariant {
        enum_name: Identifier,
        variant: Identifier,
        payload: Vec<Pattern>,
    },
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct StructPatternField {
    pub name: Identifier,
    pub pattern: Box<Pattern>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
    #[serde(skip)]
    pub span: Option<Span>,
}

impl Identifier {
    pub fn new(name: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

/// Function definition declared at module scope.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct FunctionDef {
    pub export: Option<FunctionExport>,
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Block,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum FunctionExport {
    Script,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct FunctionParam {
    pub name: Identifier,
    pub ty: TypeAnnotation,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct StructDef {
    pub name: Identifier,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct StructField {
    pub name: Identifier,
    pub ty: TypeAnnotation,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub payload: Vec<TypeAnnotation>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct TypeAnnotation {
    pub name: Identifier,
    pub generics: Vec<TypeAnnotation>,
}

/// Helper for attaching source spans to AST nodes.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    #[serde(skip)]
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    /// Map the contained value while keeping the original span.
    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: map(self.node),
            span: self.span,
        }
    }

    pub fn none(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span {
                start: 0,
                end: 0,
                context: (),
            },
        }
    }
}
