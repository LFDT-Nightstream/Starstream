//! Core type representations for the Starstream DSL.
//!
//! The current language only needs a handful of primitive types, but the
//! structures in this module are intentionally flexible so we can layer in
//! features like generics, traits, linear resources, and effect sets without
//! discarding the API surface introduced here.

use std::{
    fmt,
    hash::{Hash, Hasher},
};

/// Identifier for a type variable.
///
/// During inference we generate fresh type variables to represent unknown
/// types. They are later unified with concrete types or quantified into
/// [`Scheme`]s. Using a small newtype keeps the representation compact while
/// still allowing us to attach formatting logic.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVarId(pub u32);

impl TypeVarId {
    /// Render the identifier using the conventional `t0`, `t1`, … scheme.
    pub fn as_str(&self) -> String {
        format!("t{}", self.0)
    }
}

/// Starstream type.
///
/// This is deliberately conservative for now: the current grammar only
/// exposes integers and booleans, but the enum leaves room for function types
/// and other structured forms we plan to add shortly (structs, enums, linear
/// resources, etc.).
#[derive(Clone, Debug)]
pub enum Type {
    /// An unknown type represented by a type variable.
    Var(TypeVarId),
    /// 64-bit signed integer.
    Int,
    /// Boolean.
    Bool,
    /// Unit `()` value used for statement expressions and other places that
    /// conceptually return nothing.
    Unit,
    /// Function type `params -> result`.
    ///
    /// We don't expose user-defined functions yet but code generation already
    /// relies on the concept of callable blocks. Keeping this variant gives us
    /// a convenient hook for when functions land.
    Function(Vec<Type>, Box<Type>),
    /// Tuple type `(T0, T1, …)`.
    Tuple(Vec<Type>),
    /// Struct/record type with named fields.
    Record(RecordType),
    /// Enum/sum type with named variants.
    Enum(EnumType),
}

impl Type {
    pub fn unit() -> Self {
        Type::Unit
    }

    pub fn bool() -> Self {
        Type::Bool
    }

    pub fn int() -> Self {
        Type::Int
    }

    /// Canonical record type helper that sorts fields by name.
    pub fn record(name: impl Into<String>, mut fields: Vec<RecordFieldType>) -> Self {
        fields.sort_by(|a, b| a.name.cmp(&b.name));
        Type::Record(RecordType {
            name: name.into(),
            fields,
        })
    }

    /// Canonical enum type helper that sorts variants by name.
    pub fn enum_type(name: impl Into<String>, mut variants: Vec<EnumVariantType>) -> Self {
        variants.sort_by(|a, b| a.name.cmp(&b.name));
        Type::Enum(EnumType {
            name: name.into(),
            variants,
        })
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Var(a), Type::Var(b)) => a == b,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Function(a_params, a_result), Type::Function(b_params, b_result)) => {
                a_params == b_params && a_result == b_result
            }
            (Type::Tuple(a), Type::Tuple(b)) => a == b,
            (Type::Record(a), Type::Record(b)) => a.fields == b.fields,
            (Type::Enum(a), Type::Enum(b)) => a.variants == b.variants,
            _ => false,
        }
    }
}

impl Eq for Type {}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Var(id) => {
                0u8.hash(state);
                id.hash(state);
            }
            Type::Int => 1u8.hash(state),
            Type::Bool => 2u8.hash(state),
            Type::Unit => 3u8.hash(state),
            Type::Function(params, result) => {
                4u8.hash(state);
                params.hash(state);
                result.hash(state);
            }
            Type::Tuple(items) => {
                5u8.hash(state);
                items.hash(state);
            }
            Type::Record(record) => {
                6u8.hash(state);
                record.fields.hash(state);
            }
            Type::Enum(enum_type) => {
                7u8.hash(state);
                enum_type.variants.hash(state);
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Var(id) => write!(f, "{}", id.as_str()),
            Type::Int => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Function(params, result) => {
                if params.is_empty() {
                    write!(f, "fn() -> {result}")
                } else {
                    let params = params
                        .iter()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "fn({params}) -> {result}")
                }
            }
            Type::Tuple(types) => {
                let contents = types
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({contents})")
            }
            Type::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|field| format!("{}: {}", field.name, field.ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                if fields.is_empty() {
                    write!(f, "{}", record.name)
                } else {
                    write!(f, "{} {{ {fields} }}", record.name)
                }
            }
            Type::Enum(enum_type) => {
                if enum_type.variants.is_empty() {
                    write!(f, "enum {} {{}}", enum_type.name)
                } else {
                    let contents = enum_type
                        .variants
                        .iter()
                        .map(|variant| match &variant.kind {
                            EnumVariantKind::Unit => variant.name.clone(),
                            EnumVariantKind::Tuple(payload) => {
                                let payload = payload
                                    .iter()
                                    .map(|ty| ty.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                format!("{}({})", variant.name, payload)
                            }
                            EnumVariantKind::Struct(fields) => {
                                let contents = fields
                                    .iter()
                                    .map(|field| format!("{}: {}", field.name, field.ty))
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                format!("{} {{ {contents} }}", variant.name)
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{} {{ {contents} }}", enum_type.name)
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordFieldType {
    pub name: String,
    pub ty: Type,
}

impl RecordFieldType {
    pub fn new(name: impl Into<String>, ty: Type) -> Self {
        Self {
            name: name.into(),
            ty,
        }
    }
}

#[derive(Clone, Debug)]
pub struct EnumType {
    pub name: String,
    pub variants: Vec<EnumVariantType>,
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

    pub fn struct_variant(name: impl Into<String>, mut fields: Vec<RecordFieldType>) -> Self {
        fields.sort_by(|a, b| a.name.cmp(&b.name));
        Self {
            name: name.into(),
            kind: EnumVariantKind::Struct(fields),
        }
    }
}

/// A polymorphic type scheme `∀vars. ty`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub vars: Vec<TypeVarId>,
    pub ty: Type,
}

impl Scheme {
    pub fn monomorphic(ty: Type) -> Self {
        Scheme {
            vars: Vec::new(),
            ty,
        }
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            let vars = self
                .vars
                .iter()
                .map(TypeVarId::as_str)
                .collect::<Vec<_>>()
                .join(" ");
            write!(f, "forall {vars}. {}", self.ty)
        }
    }
}
#[derive(Clone, Debug)]
pub struct RecordType {
    pub name: String,
    pub fields: Vec<RecordFieldType>,
}
