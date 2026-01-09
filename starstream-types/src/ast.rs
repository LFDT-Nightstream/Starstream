//! Abstract syntax tree for the pared-down Starstream DSL.

use chumsky::span::SimpleSpan;
use serde::Serialize;

// ----------------------------------------------------------------------------
// Basics

pub type Span = SimpleSpan;

/// AST node with a source span attached.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    #[serde(skip)]
    pub span: Span,
    #[serde(skip)]
    pub comments_before: Vec<Comment>,
    #[serde(skip)]
    pub comments_after: Vec<Comment>,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self {
            node,
            span,
            comments_before: Vec::new(),
            comments_after: Vec::new(),
        }
    }

    /// Map the contained value while keeping the original span.
    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: map(self.node),
            span: self.span,
            comments_before: self.comments_before,
            comments_after: self.comments_after,
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
            comments_before: Vec::new(),
            comments_after: Vec::new(),
        }
    }
}

/// Identifier text with a source span attached.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
    #[serde(skip)]
    pub span: Option<Span>,
}

impl Identifier {
    /// Construct an Identifier without an associated span.
    pub fn anon(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            span: None,
        }
    }

    /// Construct an Identifier with an associated span.
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span: Some(span),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

// ----------------------------------------------------------------------------
// Top-level definitions

#[derive(Clone, Debug, PartialEq)]
pub struct Comment(pub Span);

impl Comment {
    /// Extracts doc comments (lines starting with `///`) from a slice of comments.
    /// Consecutive `///` lines are joined with newlines. Returns None if no doc comments.
    pub fn extract_doc(comments: &[Comment], source: &str) -> Option<String> {
        let doc_lines: Vec<&str> = comments
            .iter()
            .filter_map(|c| {
                let text = source.get(c.0.start..c.0.end)?;
                if text.starts_with("///") {
                    // Strip "///" and optional leading space
                    let content = text.strip_prefix("///").unwrap_or("");
                    let content = content.strip_prefix(' ').unwrap_or(content);
                    Some(content)
                } else {
                    None
                }
            })
            .collect();

        if doc_lines.is_empty() {
            None
        } else {
            Some(doc_lines.join("\n"))
        }
    }
}

/// Entire program: a sequence of definitions.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Program {
    #[serde(skip)]
    pub shebang: Option<Comment>,
    pub definitions: Vec<Spanned<Definition>>,
}

/// Top-level definition items.
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Definition {
    Import(ImportDef),
    Function(FunctionDef),
    Struct(StructDef),
    Enum(EnumDef),
    Utxo(UtxoDef),
    Abi(AbiDef),
}

/// `import { blockHeight } from starstream:std/cardano;`
/// `import context from starstream:std;`
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct ImportDef {
    pub items: ImportItems,
    pub from: ImportSource,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum ImportItems {
    /// `import { blockHeight, foo as bar } from ...;`
    Named(Vec<ImportNamedItem>),
    /// `import context from starstream:std;` — brings in a namespace alias.
    Namespace(Identifier),
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct ImportNamedItem {
    /// Original name in the interface.
    pub imported: Identifier,
    /// Local binding name (equals `imported` if no `as`).
    pub local: Identifier,
}

/// WIT-style source path: `namespace:package/interface`
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct ImportSource {
    /// e.g. `starstream`
    pub namespace: Identifier,
    /// e.g. `std`
    pub package: Identifier,
    /// e.g. `Some("cardano")` for `starstream:std/cardano`, `None` for `starstream:std`
    pub interface: Option<Identifier>,
}

/// `fn` definition.
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

/// `struct` definition.
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

/// `enum` definition.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<EnumVariant>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EnumVariant {
    pub name: Identifier,
    pub payload: EnumVariantPayload,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum EnumVariantPayload {
    Unit,
    Tuple(Vec<TypeAnnotation>),
    Struct(Vec<StructField>),
}

/// `utxo` definition.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct UtxoDef {
    pub name: Identifier,
    pub parts: Vec<UtxoPart>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum UtxoPart {
    Storage(Vec<UtxoGlobal>),
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct UtxoGlobal {
    pub name: Identifier,
    pub ty: TypeAnnotation,
}

/// `abi` definition containing event and function declarations.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct AbiDef {
    pub name: Identifier,
    pub parts: Vec<AbiPart>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum AbiPart {
    Event(EventDef),
}

/// `event` declaration inside an `abi` block.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EventDef {
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
}

// ----------------------------------------------------------------------------
// Type syntax

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct TypeAnnotation {
    pub name: Identifier,
    pub generics: Vec<TypeAnnotation>,
}

// ----------------------------------------------------------------------------
// Statements

/// A new scope containing a group of statements and an optional trailing expression.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
    pub tail_expression: Option<Spanned<Expr>>,
}

/// A statement that produces no value.
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
    While {
        condition: Spanned<Expr>,
        body: Block,
    },
    Expression(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
}

// ----------------------------------------------------------------------------
// Expressions

/// An expression that evaluates to a value of a particular type.
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
        payload: EnumConstructorPayload,
    },
    Block(Box<Block>),
    If {
        branches: Vec<(Spanned<Expr>, Block)>,
        else_branch: Option<Box<Block>>,
    },
    Match {
        scrutinee: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },
    Call {
        callee: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    /// Event emission expression: `emit EventName(args...)`
    Emit {
        event: Identifier,
        args: Vec<Spanned<Expr>>,
    },
    /// Effectful call: `raise some_effectful_fn(...)`
    Raise {
        expr: Box<Spanned<Expr>>,
    },
    /// Runtime call: `runtime some_runtime_fn(...)`
    Runtime {
        expr: Box<Spanned<Expr>>,
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
pub enum EnumConstructorPayload {
    Unit,
    Tuple(Vec<Spanned<Expr>>),
    Struct(Vec<StructLiteralField>),
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Pattern {
    /// A binding pattern that captures the matched value (e.g., `x`).
    Binding(Identifier),
    /// A wildcard pattern that matches anything but doesn't bind (e.g., `_`).
    Wildcard {
        #[serde(skip)]
        span: Span,
    },
    /// A literal pattern that matches a specific value (e.g., `0`, `true`).
    Literal {
        value: Literal,
        #[serde(skip)]
        span: Span,
    },
    Struct {
        name: Identifier,
        fields: Vec<StructPatternField>,
    },
    EnumVariant {
        enum_name: Identifier,
        variant: Identifier,
        payload: EnumPatternPayload,
    },
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct StructPatternField {
    pub name: Identifier,
    pub pattern: Box<Pattern>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum EnumPatternPayload {
    Unit,
    Tuple(Vec<Pattern>),
    Struct(Vec<StructPatternField>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::span::Span as SpanTrait;

    fn span(start: usize, end: usize) -> Span {
        Span::new((), start..end)
    }

    #[test]
    fn extract_doc_single_line() {
        let source = "/// This is a doc comment";
        let comments = vec![Comment(span(0, source.len()))];
        let doc = Comment::extract_doc(&comments, source);
        assert_eq!(doc, Some("This is a doc comment".to_string()));
    }

    #[test]
    fn extract_doc_multiple_lines() {
        let source = "/// First line\n/// Second line\n";
        let comments = vec![
            Comment(span(0, 14)),  // "/// First line"
            Comment(span(15, 30)), // "/// Second line"
        ];
        let doc = Comment::extract_doc(&comments, source);
        assert_eq!(doc, Some("First line\nSecond line".to_string()));
    }

    #[test]
    fn extract_doc_no_doc_comments() {
        let source = "// Regular comment";
        let comments = vec![Comment(span(0, 18))];
        let doc = Comment::extract_doc(&comments, source);
        assert_eq!(doc, None);
    }

    #[test]
    fn extract_doc_mixed_comments() {
        let source = "// Regular\n/// Doc comment";
        let comments = vec![
            Comment(span(0, 10)),  // "// Regular"
            Comment(span(11, 26)), // "/// Doc comment"
        ];
        let doc = Comment::extract_doc(&comments, source);
        assert_eq!(doc, Some("Doc comment".to_string()));
    }

    #[test]
    fn extract_doc_no_space_after_slashes() {
        let source = "///No space";
        let comments = vec![Comment(span(0, 11))];
        let doc = Comment::extract_doc(&comments, source);
        assert_eq!(doc, Some("No space".to_string()));
    }
}
