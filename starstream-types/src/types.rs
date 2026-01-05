//! Core type representations for the Starstream DSL.
//!
//! The current language only needs a handful of primitive types, but the
//! structures in this module are intentionally flexible so we can layer in
//! features like generics, traits, linear resources, and effect sets without
//! discarding the API surface introduced here.

use pretty::RcDoc;
use std::fmt;

const TYPE_FORMAT_WIDTH: usize = 80;

/// Effect kind for functions - tracks whether a function performs side effects.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub enum EffectKind {
    /// Pure function with no side effects.
    #[default]
    Pure,
    /// Effectful function that raises effects which can be caught, handled, and resumed (requires `raise`).
    Effectful,
    /// Runtime function that calls external runtime/host functions (requires `runtime`).
    Runtime,
}

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
    /// Function type `params -> result` with an optional effect.
    ///
    /// We don't expose user-defined functions yet but code generation already
    /// relies on the concept of callable blocks. Keeping this variant gives us
    /// a convenient hook for when functions land.
    Function {
        params: Vec<Type>,
        result: Box<Type>,
        effect: EffectKind,
    },
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

    /// Canonical record type helper.
    pub fn record(name: impl Into<String>, fields: Vec<RecordFieldType>) -> Self {
        Type::Record(RecordType {
            name: name.into(),
            fields,
        })
    }

    /// Canonical enum type helper.
    pub fn enum_type(name: impl Into<String>, variants: Vec<EnumVariantType>) -> Self {
        Type::Enum(EnumType {
            name: name.into(),
            variants,
        })
    }

    /// Pure function type helper.
    pub fn function(params: Vec<Type>, result: Type) -> Self {
        Type::Function {
            params,
            result: Box::new(result),
            effect: EffectKind::Pure,
        }
    }

    /// Effectful function type helper.
    pub fn effectful_function(params: Vec<Type>, result: Type) -> Self {
        Type::Function {
            params,
            result: Box::new(result),
            effect: EffectKind::Effectful,
        }
    }

    /// Runtime function type helper.
    pub fn runtime_function(params: Vec<Type>, result: Type) -> Self {
        Type::Function {
            params,
            result: Box::new(result),
            effect: EffectKind::Runtime,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_doc_mode(TypeDocMode::Expanded)
            .render_fmt(TYPE_FORMAT_WIDTH, f)
    }
}

impl Type {
    pub fn to_compact_string(&self) -> String {
        render_doc(self.to_doc_mode(TypeDocMode::Compact))
    }

    fn to_doc_mode(&self, mode: TypeDocMode) -> RcDoc<'static, ()> {
        match self {
            Type::Var(id) => RcDoc::text(id.as_str()),
            Type::Int => RcDoc::text("i64"),
            Type::Bool => RcDoc::text("bool"),
            Type::Unit => RcDoc::text("()"),
            Type::Function {
                params,
                result,
                effect,
            } => {
                let params_doc = if params.is_empty() {
                    RcDoc::text("()")
                } else {
                    RcDoc::text("(")
                        .append(comma_separated_docs(
                            params.iter().map(|ty| ty.to_doc_mode(TypeDocMode::Compact)),
                        ))
                        .append(RcDoc::text(")"))
                };

                let effect_prefix = match effect {
                    EffectKind::Pure => RcDoc::text("fn"),
                    EffectKind::Effectful => RcDoc::text("effectful fn"),
                    EffectKind::Runtime => RcDoc::text("runtime fn"),
                };

                effect_prefix
                    .append(params_doc)
                    .append(RcDoc::text(" -> "))
                    .append(result.to_doc_mode(TypeDocMode::Compact))
            }
            Type::Tuple(items) => RcDoc::text("(")
                .append(comma_separated_docs(
                    items.iter().map(|ty| ty.to_doc_mode(TypeDocMode::Compact)),
                ))
                .append(RcDoc::text(")")),
            Type::Record(record) => match mode {
                TypeDocMode::Compact => RcDoc::text(record.name.clone()),
                TypeDocMode::Expanded => record_doc(record),
            },
            Type::Enum(enum_type) => match mode {
                TypeDocMode::Compact => RcDoc::text(enum_type.name.clone()),
                TypeDocMode::Expanded => enum_doc(enum_type),
            },
        }
    }
}

fn render_doc(doc: RcDoc<'static, ()>) -> String {
    let mut out = String::new();
    doc.render_fmt(TYPE_FORMAT_WIDTH, &mut out)
        .expect("render type doc");
    out
}

#[derive(Clone, Copy)]
enum TypeDocMode {
    Expanded,
    Compact,
}

fn comma_separated_docs<I>(docs: I) -> RcDoc<'static, ()>
where
    I: IntoIterator<Item = RcDoc<'static, ()>>,
{
    RcDoc::intersperse(docs, RcDoc::text(", "))
}

fn record_doc(record: &RecordType) -> RcDoc<'static, ()> {
    if record.fields.is_empty() {
        RcDoc::text("struct ").append(RcDoc::text(record.name.clone()))
    } else {
        let fields = RcDoc::intersperse(
            record.fields.iter().map(|field| {
                RcDoc::text(field.name.clone())
                    .append(RcDoc::text(": "))
                    .append(field.ty.to_doc_mode(TypeDocMode::Compact))
                    .append(RcDoc::text(","))
            }),
            RcDoc::hardline(),
        );

        RcDoc::text("struct ")
            .append(RcDoc::text(record.name.clone()))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(fields).nest(4))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

fn enum_doc(enum_type: &EnumType) -> RcDoc<'static, ()> {
    if enum_type.variants.is_empty() {
        RcDoc::text("enum ")
            .append(RcDoc::text(enum_type.name.clone()))
            .append(RcDoc::space())
            .append(RcDoc::text("{}"))
    } else {
        let variants = RcDoc::intersperse(
            enum_type
                .variants
                .iter()
                .map(|variant| match &variant.kind {
                    EnumVariantKind::Unit => {
                        RcDoc::text(variant.name.clone()).append(RcDoc::text(","))
                    }
                    EnumVariantKind::Tuple(payload) => RcDoc::text(variant.name.clone())
                        .append(RcDoc::text("("))
                        .append(comma_separated_docs(
                            payload
                                .iter()
                                .map(|ty| ty.to_doc_mode(TypeDocMode::Compact)),
                        ))
                        .append(RcDoc::text("),")),
                    EnumVariantKind::Struct(fields) => {
                        enum_variant_struct_doc(&variant.name, fields)
                    }
                }),
            RcDoc::hardline(),
        );

        RcDoc::text("enum ")
            .append(RcDoc::text(enum_type.name.clone()))
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline().append(variants).nest(4))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

fn enum_variant_struct_doc(variant_name: &str, fields: &[RecordFieldType]) -> RcDoc<'static, ()> {
    if fields.is_empty() {
        return RcDoc::text(format!("{variant_name} {{}},"));
    }

    if let Some(inline) = inline_struct_variant(variant_name, fields) {
        RcDoc::text(inline).append(RcDoc::text(","))
    } else {
        let body = RcDoc::intersperse(
            fields.iter().map(|field| {
                RcDoc::text(field.name.clone())
                    .append(RcDoc::text(": "))
                    .append(field.ty.to_doc_mode(TypeDocMode::Compact))
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

fn inline_struct_variant(variant_name: &str, fields: &[RecordFieldType]) -> Option<String> {
    if fields.len() >= 3 {
        return None;
    }

    let contents = fields
        .iter()
        .map(|field| format!("{}: {}", field.name, field.ty.to_compact_string()))
        .collect::<Vec<_>>()
        .join(", ");

    let inline = format!("{variant_name} {{ {contents} }}");
    if inline.len() <= TYPE_FORMAT_WIDTH {
        Some(inline)
    } else {
        None
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RecordType {
    pub name: String,
    pub fields: Vec<RecordFieldType>,
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

    pub fn struct_variant(name: impl Into<String>, fields: Vec<RecordFieldType>) -> Self {
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
