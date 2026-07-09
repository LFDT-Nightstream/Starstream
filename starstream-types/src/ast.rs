//! Abstract syntax tree for the pared-down Starstream DSL.

use chumsky::span::SimpleSpan;
use chumsky::span::Span as SpanTrait;
use serde::Serialize;

// ----------------------------------------------------------------------------
// Basics

pub type Span = SimpleSpan;

pub const DUMMY_SPAN: Span = Span {
    start: 0,
    end: 0,
    context: (),
};

/// Create a span from start and end offsets.
///
/// This is a convenience function that avoids needing to import the chumsky Span trait.
#[must_use]
pub fn span(start: usize, end: usize) -> Span {
    Span::new((), start..end)
}

/// AST node with a source span attached.
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

/// Identifier text with a source span attached.
#[derive(Clone, Debug, Serialize, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
    #[serde(skip)]
    pub span: Span,
}

impl Identifier {
    /// Construct an Identifier without an associated span.
    pub fn anon(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            span: DUMMY_SPAN,
        }
    }

    /// Construct an Identifier with an associated span.
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }

    /// Get the identifier's text.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.name
    }

    /// Get the identifier's source span if available, or `DUMMY_SPAN` if the identifier was hardcoded.
    #[must_use]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Get the identifier's source span if available, or `default` if the identifier was hardcoded.
    #[must_use]
    pub fn span_or(&self, default: Span) -> Span {
        if self.span == DUMMY_SPAN {
            default
        } else {
            self.span
        }
    }

    /// Get the identifier's source span if available, or `None` if the identifier was hardcoded.
    #[must_use]
    pub fn opt_span(&self) -> Option<Span> {
        if self.span == DUMMY_SPAN {
            None
        } else {
            Some(self.span)
        }
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
    #[must_use]
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
    Token(TokenDef),
    Abi(AbiDef),
    /// `contract;` — marks the file as a contract entry point. May appear
    /// anywhere at the top level; the formatter moves it to the top. The
    /// enclosing `Spanned` carries the span of the `contract` keyword.
    Contract,
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

/// Where an `import` statement pulls items from.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum ImportSource {
    /// WIT-style source path: `namespace:package/interface`.
    Wit {
        /// e.g. `starstream`
        namespace: Identifier,
        /// e.g. `std`
        package: Identifier,
        /// e.g. `Some("cardano")` for `starstream:std/cardano`, `None` for `starstream:std`
        interface: Option<Identifier>,
    },
    /// Relative path to another `.star` file: `"./helpers/math.star"`.
    Path(ImportPath),
}

/// A quoted relative path to another `.star` file.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct ImportPath {
    /// The raw path text as written in the source (without surrounding quotes).
    pub value: String,
    /// Span of the literal including surrounding quotes.
    #[serde(skip)]
    pub span: Span,
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
    /// `script fn`
    Script,
    /// `utxo { main fn }`
    UtxoMain,
    /// `token { mint fn }`
    TokenMint,
    /// `token { burn fn }`
    TokenBurn,
}

impl FunctionExport {
    pub fn keyword(&self) -> &'static str {
        match self {
            FunctionExport::Script => "script",
            FunctionExport::UtxoMain => "main",
            FunctionExport::TokenMint => "mint",
            FunctionExport::TokenBurn => "burn",
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct FunctionParam {
    pub public: bool,
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
    /// The span covering the entire field.
    #[serde(skip)]
    pub span: Span,
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
    /// The span covering the entire variant.
    #[serde(skip)]
    pub span: Span,
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
    Function(Box<FunctionDef>),
    AbiImpl {
        abi: Identifier,
        parts: Vec<AbiImplPart>,
    },
}

pub type AbiImplPart = FunctionDef;

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct UtxoGlobal {
    pub name: Identifier,
    pub ty: TypeAnnotation,
}

/// `token` definition.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct TokenDef {
    pub name: Identifier,
    pub parts: Vec<TokenPart>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum TokenPart {
    Storage(Vec<TokenGlobal>),
    Function(Box<FunctionDef>),
    AbiImpl {
        abi: Identifier,
        parts: Vec<AbiImplPart>,
    },
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct TokenGlobal {
    /// Whether the field carries the `indexed` modifier.
    pub indexed: bool,
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
    Effect(EffectDef),
    FnDecl(AbiMethodDecl),
}

impl AbiPart {
    pub fn span(&self) -> Span {
        match self {
            AbiPart::Event(e) => e.span,
            AbiPart::Effect(e) => e.span,
            AbiPart::FnDecl(m) => m.span,
        }
    }
}

/// Method declaration inside an `abi` block (signature only, no body).
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct AbiMethodDecl {
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeAnnotation>,
    /// The span covering the entire declaration from `fn` to `;`.
    #[serde(skip)]
    pub span: Span,
}

/// `event` declaration inside an `abi` block.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EventDef {
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    /// The span covering the entire declaration from `event` to `;`.
    #[serde(skip)]
    pub span: Span,
}

/// Method declaration inside an `abi` block (signature only, no body).
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct EffectDef {
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeAnnotation>,
    /// The span covering the entire declaration from `fn` to `;`.
    #[serde(skip)]
    pub span: Span,
}

// ----------------------------------------------------------------------------
// Type syntax

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct TypeAnnotation {
    pub name: Identifier,
    pub generics: Vec<TypeAnnotation>,
}

impl From<Identifier> for TypeAnnotation {
    fn from(name: Identifier) -> Self {
        TypeAnnotation {
            name,
            generics: Default::default(),
        }
    }
}

// ----------------------------------------------------------------------------
// Statements

/// A new scope containing a group of statements and an optional trailing expression.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
    pub tail_expression: Option<Spanned<Expr>>,
    /// The span covering the entire block from `{` to `}`.
    #[serde(skip)]
    pub span: Span,
}

/// A statement that produces no value.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Statement {
    VariableDeclaration {
        public: bool,
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
    Resume,
}

// ----------------------------------------------------------------------------
// Expressions

/// The condition part of an `if` branch.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum IfCondition {
    /// A normal boolean expression: `if (expr) { ... }`
    Bool(Spanned<Expr>),
    /// A type-narrowing test: `if ident is AbiType { ... }`
    Is {
        name: Identifier,
        abi_name: Identifier,
    },
}

/// Identifiers separated by `::`. Non-empty.
pub type ScopedName = Vec<Identifier>;

/// Expressions within parentheses and separated by commas.
pub type Arguments = Vec<Spanned<Expr>>;

/// An expression that evaluates to a value of a particular type.
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Expr {
    // Primary expressions ----------
    Grouping(Box<Spanned<Expr>>),
    ScopedName(ScopedName),
    Literal(Literal),
    StructConstructor {
        name: ScopedName,
        fields: Vec<StructFieldInitializer>,
    },
    /// Visibility-lifting expression: `disclose(expr)`
    Disclose {
        expr: Box<Spanned<Expr>>,
    },
    /// Event emission expression: `emit some_emit_fn(args...)`
    Emit {
        callee: Box<Spanned<Expr>>,
        args: Arguments,
    },
    /// Effectful call: `raise some_effect_fn(...)`
    Raise {
        callee: Box<Spanned<Expr>>,
        args: Arguments,
    },
    /// Runtime call: `runtime some_runtime_fn(...)`
    Runtime {
        callee: Box<Spanned<Expr>>,
        args: Arguments,
    },
    // Control-flow primary expressions ----------
    /// `yield` and `yield(AbiName, ...)`
    Yield {
        /// Empty for bare `yield`, or list of abi names
        abis: Vec<Identifier>,
    },
    Block(Box<Block>),
    If {
        branches: Vec<(IfCondition, Block)>,
        else_branch: Option<Box<Block>>,
    },
    Match {
        scrutinee: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },
    // Postfix expressions ----------
    Call {
        callee: Box<Spanned<Expr>>,
        args: Arguments,
    },
    FieldAccess {
        target: Box<Spanned<Expr>>,
        field: Identifier,
    },
    // Operators ----------
    Unary {
        op: UnaryOp,
        expr: Box<Spanned<Expr>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
}

impl Expr {
    pub fn name(&self) -> Option<&str> {
        match self {
            Expr::Grouping(inner) => inner.node.name(),
            Expr::ScopedName(name) => Some(name.last().unwrap().as_str()),
            Expr::FieldAccess { target: _, field } => Some(field.as_str()),
            _ => None,
        }
    }
}

impl From<Identifier> for Expr {
    fn from(value: Identifier) -> Self {
        Expr::ScopedName(vec![value])
    }
}

impl From<Identifier> for Spanned<Expr> {
    fn from(value: Identifier) -> Self {
        let span = value.span;
        Spanned::new(Expr::from(value), span)
    }
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Literal {
    Integer(i128),
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
pub struct StructFieldInitializer {
    pub name: Identifier,
    pub value: Spanned<Expr>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
    /// The span covering the entire match arm from pattern to end of body.
    #[serde(skip)]
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum Pattern {
    /// A binding using a name. Might be a lone name that captures the matched
    /// value, or might be an `EnumName::UnitVariant` constant.
    Name(ScopedName),
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
        name: ScopedName,
        fields: Vec<StructPatternField>,
    },
    Tuple {
        name: ScopedName,
        fields: Vec<Pattern>,
    },
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct StructPatternField {
    pub name: Identifier,
    pub pattern: Box<Pattern>,
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
