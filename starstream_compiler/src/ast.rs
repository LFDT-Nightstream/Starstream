//! AST types describing a Starstream source file.

#[derive(Clone, Debug, Default)]
pub struct StarstreamProgram {
    pub items: Vec<ProgramItem>,
}

#[derive(Clone, Debug)]
pub enum ProgramItem {
    Utxo(Utxo),
    Script(Script),
    Token(Token),
}

#[derive(Clone, Debug)]
pub struct Utxo {
    pub name: Identifier,
    pub items: Vec<UtxoItem>,
}

#[derive(Clone, Debug)]
pub enum UtxoItem {
    Abi(Abi),
    Main(Main),
    Impl(Impl),
    Storage(Storage),
}

#[derive(Clone, Debug)]
pub struct Main {
    pub type_sig: Option<OptionallyTypedBindings>,
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
    Abi(Abi),
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
    pub input_types: Vec<Type>,
    pub output_type: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct FnSig(pub Sig);

#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: Identifier,
    pub inputs: OptionallyTypedBindings,
    pub output: Option<Type>,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub enum EffectSig {
    EffectSig(Sig),
    EventSig(Sig),
    ErrorSig(Sig),
}

#[derive(Clone, Debug)]
pub enum AbiElem {
    FnSig(FnSig),
    EffectSig(EffectSig),
}

#[derive(Clone, Debug)]
pub struct Abi {
    pub values: Vec<AbiElem>,
}

#[derive(Clone, Debug)]
pub struct Identifier(pub String);

#[derive(Clone, Debug)]
pub enum Type {
    BaseType(Identifier, Option<Vec<Type>>),
    Object(TypedBindings),
    FnType(TypedBindings, Option<Box<Type>>),
}

#[derive(Clone, Debug)]
pub enum Statement {
    BindVar {
        var: Identifier,
        value: Expr,
        mutable: bool,
    },
    Return(Option<Expr>),
    Resume(Option<Expr>),
    Assign(Identifier, Expr),
    With(Block, Vec<(Effect, Block)>),
    While(Expr, LoopBody),
    Loop(LoopBody),
}

#[derive(Clone, Debug)]
pub enum LoopBody {
    Statement(Box<Statement>),
    Block(Block),
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Equals(Box<Self>, Box<Self>),
    NotEquals(Box<Self>, Box<Self>),
    LessThan(Box<Self>, Box<Self>),
    Add(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Pow(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Neg(Box<Self>),
    PrimaryExpr(
        PrimaryExpr,
        Option<Arguments>,
        Vec<(Identifier, Option<Arguments>)>,
    ),
    BlockExpr(BlockExpr),
}

#[derive(Clone, Debug)]
pub enum BlockExpr {
    IfThenElse(Box<Expr>, Box<Block>, Option<Box<Block>>),
    Block(Block),
}

#[derive(Clone, Debug)]
pub enum PrimaryExpr {
    Null,
    Number(f64),
    Bool(bool),
    Ident(Identifier),
    ParExpr(Box<Expr>),
    Yield(Box<Expr>),
    Raise(Box<Expr>),
    Object(Type, Vec<(Identifier, Expr)>),
}

#[derive(Clone, Debug)]
pub enum ExprOrStatement {
    Expr(Expr),
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
    pub xs: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct OptionallyTypedBindings {
    pub values: Vec<(Identifier, Option<Type>)>,
}

#[derive(Clone, Debug)]
pub struct TypedBindings {
    pub values: Vec<(Identifier, Type)>,
}

#[derive(Clone, Debug)]
pub struct Effect {
    pub ident: Identifier,
    pub type_sig: OptionallyTypedBindings,
}
