//! AST types describing a Starstream source file.

use crate::scope_resolution::SymbolId;
use chumsky::span::SimpleSpan;

/// The root type of a Starstream source file.
#[derive(Clone, Debug, Default)]
pub struct StarstreamProgram {
    pub items: Vec<ProgramItem>,
}

/// A coordination script, UTXO, or token definition block.
#[derive(Clone, Debug)]
pub enum ProgramItem {
    // TODO: Import
    Abi(Abi),
    Script(Script),
    Utxo(Utxo),
    Token(Token),
    TypeDef(TypeDef),
    Constant { name: Identifier, value: f64 },
}

/// `utxo Name { ... }`
#[derive(Clone, Debug)]
pub struct Utxo {
    pub name: Identifier,
    pub items: Vec<UtxoItem>,
}

#[derive(Clone, Debug)]
pub enum UtxoItem {
    Main(Main),
    Impl(Impl),
    Storage(Storage),
    Yield(TypeArg),
    Resume(TypeArg),
}

#[derive(Clone, Debug)]
pub struct Main {
    pub type_sig: Option<TypedBindings>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub name: Identifier,
    pub items: Vec<TokenItem>,
}

#[derive(Clone, Debug)]
pub enum TokenItem {
    Bind(Bind),
    Unbind(Unbind),
    Mint(Mint),
}

#[derive(Clone, Debug)]
pub struct Bind(pub Block);

#[derive(Clone, Debug)]
pub struct Unbind(pub Block);

#[derive(Clone, Debug)]
pub struct Mint(pub Block);

#[derive(Clone, Debug)]
pub struct Impl {
    pub name: Identifier,
    pub definitions: Vec<FnDef>,
}

#[derive(Clone, Debug)]
pub struct Script {
    pub definitions: Vec<FnDef>,
}

#[derive(Clone, Debug)]
pub struct Storage {
    pub bindings: TypedBindings,
}

#[derive(Clone, Debug)]
pub struct Sig {
    pub name: Identifier,
    pub input_types: Vec<TypeArg>,
    pub output_type: Option<TypeArg>,
}

#[derive(Clone, Debug)]
pub struct FnDecl(pub Sig);

#[derive(Clone, Debug)]
pub enum TypeOrSelf {
    Type(TypeArg),
    _Self,
}

#[derive(Clone, Debug)]
pub struct FnArgDeclaration {
    pub name: Identifier,
    pub ty: TypeOrSelf,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub ident: Identifier,
    pub inputs: Vec<FnArgDeclaration>,
    pub output: Option<TypeArg>,
    pub body: Block,
    pub effects: Vec<Identifier>,
}

#[derive(Clone, Debug)]
pub enum EffectDecl {
    EffectSig(Sig),
    EventSig(Sig),
    ErrorSig(Sig),
}

#[derive(Clone, Debug)]
pub enum AbiElem {
    FnDecl(FnDecl),
    EffectDecl(EffectDecl),
}

#[derive(Clone, Debug)]
pub struct Abi {
    pub name: Identifier,
    pub values: Vec<AbiElem>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    pub raw: String,
    pub uid: Option<SymbolId>,
    pub span: Option<SimpleSpan>,
}

impl Identifier {
    pub fn new(name: impl Into<String>, span: Option<SimpleSpan>) -> Self {
        Self {
            raw: name.into(),
            uid: None,
            span,
        }
    }
}

impl AsMut<Identifier> for Identifier {
    fn as_mut(&mut self) -> &mut Identifier {
        self
    }
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub name: Identifier,
    pub ty: TypeDefRhs,
}

#[derive(Clone, Debug)]
pub enum TypeDefRhs {
    TypeArg(TypeArg),
    Object(TypedBindings),
    Variant(Variant),
}

#[derive(Clone, Debug)]
pub struct Object(pub TypedBindings);

#[derive(Clone, Debug)]
pub struct Variant(pub Vec<(Identifier, TypedBindings)>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeRef(pub Identifier);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnType {
    pub inputs: TypedBindings,
    pub output: Option<Box<TypeArg>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeArg {
    Bool,
    F32,
    F64,
    U32,
    I32,
    U64,
    I64,
    String,
    Intermediate {
        abi: Box<TypeArg>,
        storage: Box<TypeArg>,
    },
    TypeRef(TypeRef),
    TypeApplication(TypeRef, Vec<TypeArg>),
    FnType(FnType),
    Ref(Box<TypeArg>),
}

#[derive(Clone, Debug)]
pub enum Statement {
    /// `let [mut] a = b;`
    BindVar {
        var: Identifier,
        mutable: bool,
        ty: Option<TypeArg>,
        value: Spanned<Expr>,
    },
    /// `return a;`
    Return(Option<Spanned<Expr>>),
    /// `resume a;`
    Resume(Option<Spanned<Expr>>),
    /// `a = b;`
    Assign {
        var: Identifier,
        expr: Spanned<Expr>,
    },
    /// `with { a... } catch (b) { c... } ...`
    With(Block, Vec<(EffectHandler, Block)>),
    /// `while (a) { b... }`
    While(Spanned<Expr>, LoopBody),
    /// `loop { a... }`
    Loop(LoopBody),
}

#[derive(Clone, Debug)]
pub enum LoopBody {
    Statement(Box<Statement>),
    Block(Block),
    Expr(Spanned<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    PrimaryExpr(FieldAccessExpression),
    BlockExpr(BlockExpr),
    // Comparison operators
    /// `a == b`
    Equals(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a != b`
    NotEquals(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a < b`
    LessThan(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a > b`
    GreaterThan(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a <= b`
    LessEq(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a >= b`
    GreaterEq(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    // Arithmetic operators
    /// `a + b`
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a - b`
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a * b`
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a / b`
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a % b`
    Mod(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `-a`
    Neg(Box<Spanned<Expr>>),
    // Bitwise operators
    /// `~a`
    BitNot(Box<Spanned<Expr>>),
    /// `a & b`
    BitAnd(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a | b`
    BitOr(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a ^ b`
    BitXor(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a << b`
    LShift(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a >> b`
    RShift(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    // Boolean operators
    /// `!a`
    Not(Box<Spanned<Expr>>),
    /// `a && b`
    And(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// `a || b`
    Or(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
}

#[derive(Clone, Debug)]
pub enum BlockExpr {
    /// `if (a) { b } else { c }`
    IfThenElse(Box<Spanned<Expr>>, Box<Block>, Option<Box<Block>>),
    /// `{ a... }`
    Block(Block),
}

#[derive(Clone, Debug)]
pub enum FieldAccessExpression {
    PrimaryExpr(PrimaryExpr),
    FieldAccess {
        base: Box<FieldAccessExpression>,
        field: IdentifierExpr,
    },
}

#[derive(Clone, Debug)]
pub struct IdentifierExpr {
    pub name: Identifier,
    pub args: Option<Arguments>,
}

#[derive(Clone, Debug)]
pub enum PrimaryExpr {
    Number(u32),
    /// `true` or `false` literal
    Bool(bool),
    /// `a`
    Ident(IdentifierExpr),
    /// `a::b::c`
    Namespace {
        namespaces: Vec<Identifier>,
        ident: IdentifierExpr,
    },
    /// `(a)`
    ParExpr(Box<Spanned<Expr>>),
    /// `yield a`
    Yield(Option<Box<Spanned<Expr>>>),
    /// `raise a`
    Raise {
        ident: IdentifierExpr,
    },
    /// `raise a::b(..)`
    RaiseNamespaced {
        namespaces: Vec<Identifier>,
        ident: IdentifierExpr,
    },
    /// `a { b: c, ... }`
    Object(TypeArg, Vec<(Identifier, Spanned<Expr>)>),
    StringLiteral(String),
}

#[derive(Clone, Debug)]
pub enum ExprOrStatement {
    Expr(Spanned<Expr>),
    Statement(Statement),
}

#[derive(Clone, Debug)]
pub enum Block {
    Chain {
        head: Box<ExprOrStatement>,
        tail: Box<Block>,
    },
    Close {
        semicolon: bool,
    },
}

#[derive(Clone, Debug)]
pub struct Arguments {
    pub xs: Vec<Spanned<Expr>>,
}

#[derive(Clone, Debug)]
pub struct OptionallyTypedBindings {
    pub values: Vec<(Identifier, Option<TypeArg>)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedBindings {
    pub values: Vec<(Identifier, TypeArg)>,
}

#[derive(Clone, Debug)]
pub struct EffectArgDeclaration {
    pub name: Identifier,
    pub ty: Option<TypeArg>,
}

#[derive(Clone, Debug)]
pub struct EffectHandler {
    pub interface: Identifier,
    pub ident: Identifier,
    pub args: Vec<EffectArgDeclaration>,
}

#[derive(Clone, Debug)]
pub struct Effect {
    pub ident: Identifier,
    pub type_sig: TypedBindings,
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub span: SimpleSpan,
}
