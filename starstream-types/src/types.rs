//! Core type representations for the Starstream DSL.
//!
//! The current language only needs a handful of primitive types, but the
//! structures in this module are intentionally flexible so we can layer in
//! features like generics, traits, linear resources, and effect sets without
//! discarding the API surface introduced here.

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use crate::{Identifier, NameId, Span};

mod display;

// ----------------------------------------------------------------------------
// Core `Type` data structure and its parts.

/// Starstream type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    /// An unknown type represented by a type variable.
    Var(TypeVarId),
    /// Integer type with width and signedness.
    Int(IntWidth),
    /// Boolean.
    Bool,
    /// Unit `()` value used for statement expressions and other places that
    /// conceptually return nothing.
    Unit,
    /// Function type `(params) -> result` with an optional kind.
    Function(Arc<FunctionType>),
    /// Tuple type `(T0, T1, …)`.
    Tuple(Arc<Vec<Type>>),
    /// Struct/record type with named fields.
    Record(Arc<RecordType>),
    /// Enum/sum type with named variants.
    Enum(Arc<EnumType>),
    /// The built-in `Utxo` type.
    UtxoAny,
    /// The type created by a `utxo` definition.
    Utxo(Arc<UtxoType>),
    /// The built-in `Token` type.
    TokenAny,
    /// The type created by a `token` definition.
    Token(Arc<TokenType>),
    /// The type created by an `abi` definition. Also the type of a Utxo
    /// narrowed via `if x is AbiName`.
    Abi(Arc<AbiType>),
}

// Keep Type small (and use Rc instead of Box) to make it cheap to clone.
const _: [(); 0 - !(std::mem::size_of::<Type>() <= 32) as usize] = [];

/// Identifier for a type variable.
///
/// During inference we generate fresh type variables to represent unknown
/// types. They are later unified with concrete types or quantified into
/// [`Scheme`]s. Using a small newtype keeps the representation compact while
/// still allowing us to attach formatting logic.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVarId(pub u32);

/// Integer width and signedness.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

/// Type of a `fn` item.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub kind: FunctionKind,
    pub name_span: Span,
    pub params: Vec<Type>,
    pub param_spans: Vec<Span>,
    pub result: Type,
    /// Optional statically-known callee. Otherwise it's a pointer.
    pub callee: Option<StaticFunction>,
}

/// Function kind: whether it can be called normally or requires a keyword prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub enum FunctionKind {
    /// Functions requiring no prefix keyword to call.
    #[default]
    Normal,
    /// Functions requiring `emit` keyword to call, generally declared using `event`.
    Emit,
    /// Functions requiring `raise` keyword to call, generally declared using `effect`.
    Raise,
    /// Functions requruing `runtime` keyword to call, generally imported host functions.
    Runtime,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StaticFunction {
    /// A specific function declared in the global namespace.
    Named(NameId),
    /// Tuple variant constructor for the given variant of the function's return type.
    Constructor { variant: usize },
}

/// Type of a `struct`.
/// Structural: interchangeable with other `struct`s and tuples with the same field type order.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordType {
    pub name: Identifier,
    pub fields: Vec<RecordFieldType>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordFieldType {
    pub name: Identifier,
    pub ty: Type,
}

/// Type of an `enum`.
/// Structural: interchangeable with other `enum`s with the same variant counts and types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub name: Identifier,
    pub variants: Vec<EnumVariantType>,
    /// Instantiated type arguments for generic enums (e.g., `[i64]` for `Option<i64>`).
    /// Empty for non-generic enums.
    pub type_args: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumVariantType {
    pub name: Identifier,
    pub kind: EnumVariantKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<RecordFieldType>),
}

/// Type of a `utxo`.
/// Nominal: encodes the source contract hash.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UtxoType {
    pub name: String,
    pub id: NameId,
    /// All [AbiType]s that this Utxo sometimes implements.
    pub possible_abis: Vec<Arc<AbiType>>,
    /// [AbiType]s that this Utxo implements at all of its yield points.
    pub always_abis: Vec<Arc<AbiType>>,
}

/// Type of a `token`.
/// Nominal: encodes the source contract hash.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenType {
    pub name: String,
    pub id: NameId,
}

/// Type of an `abi`.
/// Structural: compatible with similar interfaces.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AbiType {
    pub name: Identifier,
    pub methods: Vec<TypedAbiMethodDecl>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedAbiMethodDecl {
    pub name: Identifier,
    pub id: NameId,
    pub ty: Arc<FunctionType>,
}

impl TypedAbiMethodDecl {
    /// Get the stable hashable identity of this method type.
    #[must_use]
    pub fn identity(&self) -> &str {
        // TODO: specify hashing for types and include real type signature here
        self.name.as_str()
    }
}

// ----------------------------------------------------------------------------
// Impls for the above.

impl Type {
    #[must_use]
    pub fn unit() -> Self {
        Type::Unit
    }

    #[must_use]
    pub fn bool() -> Self {
        Type::Bool
    }

    #[must_use]
    pub fn int() -> Self {
        Type::Int(IntWidth::I64)
    }
}

impl From<IntWidth> for Type {
    fn from(value: IntWidth) -> Self {
        Type::Int(value)
    }
}

impl From<FunctionType> for Type {
    fn from(value: FunctionType) -> Self {
        Type::Function(Arc::new(value))
    }
}

impl From<Arc<FunctionType>> for Type {
    fn from(value: Arc<FunctionType>) -> Self {
        Type::Function(value)
    }
}

impl From<RecordType> for Type {
    fn from(value: RecordType) -> Self {
        Type::Record(Arc::new(value))
    }
}

impl From<Arc<RecordType>> for Type {
    fn from(value: Arc<RecordType>) -> Self {
        Type::Record(value)
    }
}

impl From<EnumType> for Type {
    fn from(value: EnumType) -> Self {
        Type::Enum(Arc::new(value))
    }
}

impl From<Arc<EnumType>> for Type {
    fn from(value: Arc<EnumType>) -> Self {
        Type::Enum(value)
    }
}

impl From<AbiType> for Type {
    fn from(value: AbiType) -> Self {
        Type::Abi(Arc::new(value))
    }
}

impl From<Arc<AbiType>> for Type {
    fn from(value: Arc<AbiType>) -> Self {
        Type::Abi(value)
    }
}

impl TypeVarId {
    pub fn fresh(&mut self) -> TypeVarId {
        let id = *self;
        self.0 += 1;
        id
    }
}

/// Always put TypeVarId on one line (ignore `{:#?}`) to keep snapshots readable.
impl fmt::Debug for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeVarId({})", self.0)
    }
}

impl IntWidth {
    #[must_use]
    pub fn is_signed(self) -> bool {
        matches!(
            self,
            IntWidth::I8 | IntWidth::I16 | IntWidth::I32 | IntWidth::I64
        )
    }

    #[must_use]
    pub fn bit_width(self) -> u32 {
        match self {
            IntWidth::I8 | IntWidth::U8 => 8,
            IntWidth::I16 | IntWidth::U16 => 16,
            IntWidth::I32 | IntWidth::U32 => 32,
            IntWidth::I64 | IntWidth::U64 => 64,
        }
    }

    #[must_use]
    pub fn display_name(self) -> &'static str {
        match self {
            IntWidth::I8 => "i8",
            IntWidth::I16 => "i16",
            IntWidth::I32 => "i32",
            IntWidth::I64 => "i64",
            IntWidth::U8 => "u8",
            IntWidth::U16 => "u16",
            IntWidth::U32 => "u32",
            IntWidth::U64 => "u64",
        }
    }

    #[must_use]
    pub fn min_value(self) -> i128 {
        match self {
            IntWidth::I8 => i128::from(i8::MIN),
            IntWidth::I16 => i128::from(i16::MIN),
            IntWidth::I32 => i128::from(i32::MIN),
            IntWidth::I64 => i128::from(i64::MIN),
            IntWidth::U8 | IntWidth::U16 | IntWidth::U32 | IntWidth::U64 => 0,
        }
    }

    #[must_use]
    pub fn max_value(self) -> i128 {
        match self {
            IntWidth::I8 => i128::from(i8::MAX),
            IntWidth::I16 => i128::from(i16::MAX),
            IntWidth::I32 => i128::from(i32::MAX),
            IntWidth::I64 => i128::from(i64::MAX),
            IntWidth::U8 => i128::from(u8::MAX),
            IntWidth::U16 => i128::from(u16::MAX),
            IntWidth::U32 => i128::from(u32::MAX),
            IntWidth::U64 => i128::from(u64::MAX),
        }
    }

    #[must_use]
    pub fn fits(self, value: i128) -> bool {
        value >= self.min_value() && value <= self.max_value()
    }

    #[must_use]
    pub fn is_64bit(self) -> bool {
        matches!(self, IntWidth::I64 | IntWidth::U64)
    }

    #[must_use]
    pub fn is_sub32(self) -> bool {
        matches!(
            self,
            IntWidth::I8 | IntWidth::I16 | IntWidth::U8 | IntWidth::U16
        )
    }
}

impl FunctionKind {
    pub fn declaration_keyword(&self) -> &'static str {
        match self {
            FunctionKind::Normal => "fn",
            FunctionKind::Emit => "event",
            FunctionKind::Raise => "effect",
            FunctionKind::Runtime => "runtime fn",
        }
    }

    pub fn call_keyword(&self) -> &'static str {
        match self {
            FunctionKind::Normal => "",
            FunctionKind::Emit => "emit",
            FunctionKind::Raise => "raise",
            FunctionKind::Runtime => "runtime",
        }
    }
}

impl RecordFieldType {
    pub fn new(name: Identifier, ty: Type) -> Self {
        Self { name, ty }
    }
}

impl EnumType {
    pub fn get_variant(&self, name: &str) -> Option<&EnumVariantType> {
        self.variants.iter().find(|v| v.name.as_str() == name)
    }
}

impl EnumVariantType {
    pub fn unit(name: Identifier) -> Self {
        Self {
            name,
            kind: EnumVariantKind::Unit,
        }
    }

    pub fn tuple(name: Identifier, payload: Vec<Type>) -> Self {
        Self {
            name,
            kind: EnumVariantKind::Tuple(payload),
        }
    }

    pub fn struct_variant(name: Identifier, fields: Vec<RecordFieldType>) -> Self {
        Self {
            name,
            kind: EnumVariantKind::Struct(fields),
        }
    }
}

// ----------------------------------------------------------------------------
// Helper types not required by `Type` itself.

/// A polymorphic type scheme `∀vars. ty`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub vars: Vec<TypeVarId>,
    pub ty: Type,
}

impl Scheme {
    #[must_use]
    pub fn monomorphic(ty: Type) -> Self {
        Scheme {
            vars: Vec::new(),
            ty,
        }
    }
}

/// A generic type definition with its template type and named parameters.
#[derive(Clone, Debug)]
pub struct GenericTypeDef {
    /// The generic type with `Type::Var` for each param
    /// (`type_args` set to the param vars so display works naturally).
    pub ty: Type,
    pub type_params: Vec<TypeParam>,
    pub doc: Option<String>,
    pub variant_docs: HashMap<String, String>,
}

/// A named type parameter in a generic type definition.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeParam {
    pub id: TypeVarId,
    pub name: String,
}

impl GenericTypeDef {
    #[must_use]
    pub fn param_name_map(&self) -> HashMap<TypeVarId, String> {
        self.type_params
            .iter()
            .map(|p| (p.id, p.name.clone()))
            .collect()
    }
}
