//! Abstract syntax tree for the pared-down Starstream DSL.

use chumsky::span::SimpleSpan;
use serde::Serialize;

pub type Span = SimpleSpan;

/// Entire program: a sequence of statements.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

/// Statements allowed by the current grammar.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Statement {
    VariableDeclaration {
        name: Identifier,
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
}

/// `{ statement* }`
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
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
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
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
