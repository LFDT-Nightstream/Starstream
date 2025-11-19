//! Core type representations for the Starstream DSL.
//!
//! The current language only needs a handful of primitive types, but the
//! structures in this module are intentionally flexible so we can layer in
//! features like generics, traits, linear resources, and effect sets without
//! discarding the API surface introduced here.

use std::fmt;

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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    Record(Vec<RecordFieldType>),
    /// Enum/sum type with named variants.
    Enum(Vec<EnumVariantType>),
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
    pub fn record(mut fields: Vec<RecordFieldType>) -> Self {
        fields.sort_by(|a, b| a.name.cmp(&b.name));
        Type::Record(fields)
    }

    /// Canonical enum type helper that sorts variants by name.
    pub fn enum_type(mut variants: Vec<EnumVariantType>) -> Self {
        variants.sort_by(|a, b| a.name.cmp(&b.name));
        Type::Enum(variants)
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
            Type::Record(fields) => {
                if fields.is_empty() {
                    write!(f, "{{}}")
                } else {
                    let contents = fields
                        .iter()
                        .map(|field| format!("{}: {}", field.name, field.ty))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{{ {contents} }}")
                }
            }
            Type::Enum(variants) => {
                if variants.is_empty() {
                    write!(f, "enum {{}}")
                } else {
                    let contents = variants
                        .iter()
                        .map(|variant| {
                            if variant.payload.is_empty() {
                                variant.name.clone()
                            } else {
                                let payload = variant
                                    .payload
                                    .iter()
                                    .map(|ty| ty.to_string())
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                format!("{}({})", variant.name, payload)
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "enum {{ {contents} }}")
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumVariantType {
    pub name: String,
    pub payload: Vec<Type>,
}

impl EnumVariantType {
    pub fn new(name: impl Into<String>, payload: Vec<Type>) -> Self {
        Self {
            name: name.into(),
            payload,
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
