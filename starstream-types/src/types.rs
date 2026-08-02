//! Core type representations for the Starstream DSL.
//!
//! The current language only needs a handful of primitive types, but the
//! structures in this module are intentionally flexible so we can layer in
//! features like generics, traits, linear resources, and effect sets without
//! discarding the API surface introduced here.

use pretty::RcDoc;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::sync::Arc;

use crate::{Identifier, Span, TypedAbiMethodDecl};

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
    UtxoNamed(String),
    /// The built-in `Token` type.
    TokenAny,
    /// The type created by a `token` definition.
    TokenNamed(String),
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    Named(String),
    /// Tuple variant constructor for the given variant of the function's return type.
    Constructor { variant: usize },
}

/// Type of a `struct`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordType {
    pub name: String,
    pub fields: Vec<RecordFieldType>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordFieldType {
    pub name: Identifier,
    pub ty: Type,
}

/// Type of an `enum`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub name: String,
    pub variants: Vec<EnumVariantType>,
    /// Instantiated type arguments for generic enums (e.g., `[i64]` for `Option<i64>`).
    /// Empty for non-generic enums.
    pub type_args: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumVariantType {
    pub name: String,
    pub kind: EnumVariantKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EnumVariantKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<RecordFieldType>),
}

/// Type of an `abi`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AbiType {
    pub name: Identifier,
    pub methods: Vec<TypedAbiMethodDecl>,
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

impl EnumVariantType {
    pub fn unit(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            kind: EnumVariantKind::Unit,
        }
    }

    pub fn tuple(name: impl Into<String>, payload: Vec<Type>) -> Self {
        Self {
            name: name.into(),
            kind: EnumVariantKind::Tuple(payload),
        }
    }

    pub fn struct_variant(name: impl Into<String>, fields: Vec<RecordFieldType>) -> Self {
        Self {
            name: name.into(),
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

// ----------------------------------------------------------------------------
// Formatting.

const TYPE_FORMAT_WIDTH: usize = 80;

impl Display for TypeVarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "forall")?;
            for v in &self.vars {
                write!(f, " {v}")?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_doc(TypeDocMode::Expanded, &HashMap::new())
            .render_fmt(TYPE_FORMAT_WIDTH, f)
    }
}

impl Type {
    #[must_use]
    pub fn compact_display(&self) -> impl Display {
        RenderDoc(self.to_doc(TypeDocMode::Compact, &HashMap::new()))
    }

    /// Render expanded display using named type parameters for `Type::Var`.
    #[must_use]
    pub fn display_with_params(&self, params: &HashMap<TypeVarId, String>) -> impl Display {
        RenderDoc(self.to_doc(TypeDocMode::Expanded, params))
    }

    /// Render compact display using named type parameters for `Type::Var`.
    #[must_use]
    pub fn compact_display_with_params(&self, params: &HashMap<TypeVarId, String>) -> impl Display {
        RenderDoc(self.to_doc(TypeDocMode::Compact, params))
    }

    fn to_doc(&self, mode: TypeDocMode, params: &HashMap<TypeVarId, String>) -> RcDoc<'static, ()> {
        match self {
            Type::Var(id) => match params.get(id) {
                Some(ty) => RcDoc::text(ty.clone()),
                None => RcDoc::as_string(id),
            },
            Type::Int(w) => RcDoc::text(w.display_name()),
            Type::Bool => RcDoc::text("bool"),
            Type::Unit => RcDoc::text("()"),
            Type::Function(func) => {
                let FunctionType {
                    params: fn_params,
                    param_spans: _,
                    result,
                    kind,
                    name_span: _,
                    callee,
                } = &**func;
                let params_doc = if fn_params.is_empty() {
                    RcDoc::text("()")
                } else {
                    RcDoc::text("(")
                        .append(RcDoc::intersperse(
                            fn_params
                                .iter()
                                .map(|ty| ty.to_doc(TypeDocMode::Compact, params)),
                            ", ",
                        ))
                        .append(RcDoc::text(")"))
                };

                RcDoc::text(kind.declaration_keyword())
                    .append(params_doc)
                    .append(RcDoc::text(" -> "))
                    .append(result.to_doc(TypeDocMode::Compact, params))
                    .append(match callee {
                        Some(StaticFunction::Named(name)) => {
                            RcDoc::text(" [").append(name.to_owned()).append("]")
                        }
                        Some(StaticFunction::Constructor { variant }) => {
                            // TODO: use variant name here by inspecting `result`
                            RcDoc::text(" [").append(variant.to_string()).append("]")
                        }
                        None => RcDoc::nil(),
                    })
            }
            Type::Tuple(items) => RcDoc::text("(")
                .append(RcDoc::intersperse(
                    items
                        .iter()
                        .map(|ty| ty.to_doc(TypeDocMode::Compact, params)),
                    ", ",
                ))
                .append(")"),
            Type::Record(record) => match mode {
                TypeDocMode::Compact => RcDoc::as_string(&record.name),
                TypeDocMode::Expanded => record_doc(record, params),
            },
            Type::Enum(enum_type) => match mode {
                TypeDocMode::Compact => {
                    if enum_type.type_args.is_empty() {
                        RcDoc::as_string(&enum_type.name)
                    } else {
                        let args = enum_type
                            .type_args
                            .iter()
                            .map(|ty| ty.to_doc(TypeDocMode::Compact, params));
                        RcDoc::as_string(&enum_type.name)
                            .append("<")
                            .append(RcDoc::intersperse(args, ", "))
                            .append(">")
                    }
                }
                TypeDocMode::Expanded => enum_doc(enum_type, params),
            },
            Type::UtxoAny => RcDoc::text("Utxo"),
            Type::UtxoNamed(id) => RcDoc::text(id.to_owned()),
            Type::TokenAny => RcDoc::text("Token"),
            Type::TokenNamed(id) => RcDoc::text(id.to_owned()),
            Type::Abi(abi) => RcDoc::text(abi.name.to_string()),
        }
    }
}

/// Like [pretty::PrettyFmt], but owns the doc, so we can `-> impl Display`.
struct RenderDoc<'a>(RcDoc<'a, ()>);

impl<'a> Display for RenderDoc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.render_fmt(TYPE_FORMAT_WIDTH, f)
    }
}

#[derive(Clone, Copy)]
enum TypeDocMode {
    Expanded,
    Compact,
}

fn record_doc(record: &RecordType, params: &HashMap<TypeVarId, String>) -> RcDoc<'static, ()> {
    if record.fields.is_empty() {
        RcDoc::text("struct ").append(RcDoc::text(record.name.clone()))
    } else {
        let fields = RcDoc::intersperse(
            record.fields.iter().map(|field| {
                RcDoc::text(field.name.to_string())
                    .append(": ")
                    .append(field.ty.to_doc(TypeDocMode::Compact, params))
                    .append(",")
            }),
            RcDoc::hardline(),
        );

        RcDoc::text("struct ")
            .append(RcDoc::text(record.name.clone()))
            .append(RcDoc::space())
            .append("{")
            .append(RcDoc::hardline().append(fields).nest(4))
            .append(RcDoc::hardline())
            .append("}")
    }
}

fn enum_name_doc(enum_type: &EnumType, params: &HashMap<TypeVarId, String>) -> RcDoc<'static, ()> {
    let name = RcDoc::text(enum_type.name.clone());
    if enum_type.type_args.is_empty() {
        name
    } else {
        let args = enum_type
            .type_args
            .iter()
            .map(|ty| ty.to_doc(TypeDocMode::Compact, params));
        name.append("<")
            .append(RcDoc::intersperse(args, ", "))
            .append(">")
    }
}

fn enum_doc(enum_type: &EnumType, params: &HashMap<TypeVarId, String>) -> RcDoc<'static, ()> {
    if enum_type.variants.is_empty() {
        RcDoc::text("enum ")
            .append(enum_name_doc(enum_type, params))
            .append(RcDoc::space())
            .append("{}")
    } else {
        let variants = RcDoc::intersperse(
            enum_type
                .variants
                .iter()
                .map(|variant| match &variant.kind {
                    EnumVariantKind::Unit => RcDoc::text(variant.name.clone()).append(","),
                    EnumVariantKind::Tuple(payload) => RcDoc::text(variant.name.clone())
                        .append("(")
                        .append(RcDoc::intersperse(
                            payload
                                .iter()
                                .map(|ty| ty.to_doc(TypeDocMode::Compact, params)),
                            ", ",
                        ))
                        .append("),"),
                    EnumVariantKind::Struct(fields) => {
                        enum_variant_struct_doc(&variant.name, fields, params)
                    }
                }),
            RcDoc::hardline(),
        );

        RcDoc::text("enum ")
            .append(enum_name_doc(enum_type, params))
            .append(RcDoc::space())
            .append("{")
            .append(RcDoc::hardline().append(variants).nest(4))
            .append(RcDoc::hardline())
            .append("}")
    }
}

fn enum_variant_struct_doc(
    variant_name: &str,
    fields: &[RecordFieldType],
    params: &HashMap<TypeVarId, String>,
) -> RcDoc<'static, ()> {
    if fields.is_empty() {
        return RcDoc::text(format!("{variant_name} {{}},"));
    }

    if let Some(inline) = inline_struct_variant(variant_name, fields, params) {
        RcDoc::text(inline).append(RcDoc::text(","))
    } else {
        let body = RcDoc::intersperse(
            fields.iter().map(|field| {
                RcDoc::text(field.name.to_string())
                    .append(RcDoc::text(": "))
                    .append(field.ty.to_doc(TypeDocMode::Compact, params))
                    .append(RcDoc::text(","))
            }),
            RcDoc::hardline(),
        );

        RcDoc::text(variant_name.to_string())
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(body).nest(4))
            .append(RcDoc::hardline())
            .append(RcDoc::text("},"))
    }
}

fn inline_struct_variant(
    variant_name: &str,
    fields: &[RecordFieldType],
    params: &HashMap<TypeVarId, String>,
) -> Option<String> {
    if fields.len() >= 3 {
        return None;
    }

    let contents = fields
        .iter()
        .map(|field| {
            format!(
                "{}: {}",
                field.name,
                field.ty.compact_display_with_params(params)
            )
        })
        .collect::<Vec<_>>()
        .join(", ");

    let inline = format!("{variant_name} {{ {contents} }}");
    if inline.len() <= TYPE_FORMAT_WIDTH {
        Some(inline)
    } else {
        None
    }
}
