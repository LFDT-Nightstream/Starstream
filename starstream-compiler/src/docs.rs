//! Documentation generation for Starstream programs.

use serde::Serialize;
use starstream_types::{
    CommentMap,
    ast::{Definition, EnumDef, Program, StructDef},
    typed_ast::{
        TypedDefinition, TypedEnumDef, TypedEnumVariantPayload, TypedFunctionDef, TypedProgram,
        TypedStructDef,
    },
    types::{EffectKind, Type},
};

/// Root documentation output structure.
#[derive(Debug, Clone, Serialize)]
pub struct DocsOutput {
    pub version: String,
    pub functions: Vec<FunctionDoc>,
    pub structs: Vec<StructDoc>,
    pub enums: Vec<EnumDoc>,
}

/// Type reference that preserves kind information for cross-referencing.
///
/// Primitives serialize as strings (e.g., `"i64"`), while user-defined types
/// serialize as objects with name and kind (e.g., `{"name": "Point", "kind": "struct"}`).
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum TypeRef {
    /// Primitive types: i64, bool, ()
    Primitive(String),
    /// Reference to a struct type
    Struct { name: String, kind: TypeKind },
    /// Reference to an enum type
    Enum { name: String, kind: TypeKind },
    /// Tuple type
    Tuple { tuple: Vec<TypeRef> },
    /// Function type
    Function {
        params: Vec<TypeRef>,
        #[serde(rename = "returnType")]
        return_type: Box<TypeRef>,
        effect: String,
    },
}

/// Type kind discriminator for serialization.
#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum TypeKind {
    Struct,
    Enum,
}

impl From<&Type> for TypeRef {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int => TypeRef::Primitive("i64".to_string()),
            Type::Bool => TypeRef::Primitive("bool".to_string()),
            Type::Unit => TypeRef::Primitive("()".to_string()),
            Type::Var(id) => TypeRef::Primitive(id.as_str()),
            Type::Record(r) => TypeRef::Struct {
                name: r.name.clone(),
                kind: TypeKind::Struct,
            },
            Type::Enum(e) => TypeRef::Enum {
                name: e.name.clone(),
                kind: TypeKind::Enum,
            },
            Type::Tuple(types) => TypeRef::Tuple {
                tuple: types.iter().map(TypeRef::from).collect(),
            },
            Type::Function {
                params,
                result,
                effect,
            } => TypeRef::Function {
                params: params.iter().map(TypeRef::from).collect(),
                return_type: Box::new(TypeRef::from(result.as_ref())),
                effect: match effect {
                    EffectKind::Pure => "pure".to_string(),
                    EffectKind::Effectful => "effectful".to_string(),
                    EffectKind::Runtime => "runtime".to_string(),
                },
            },
        }
    }
}

/// Documentation for a function.
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDoc {
    pub name: String,
    pub doc: Option<String>,
    pub params: Vec<ParamDoc>,
    #[serde(rename = "returnType")]
    pub return_type: TypeRef,
    pub effect: String,
    pub export: Option<String>,
}

/// Documentation for a function parameter.
#[derive(Debug, Clone, Serialize)]
pub struct ParamDoc {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: TypeRef,
}

/// Documentation for a struct.
#[derive(Debug, Clone, Serialize)]
pub struct StructDoc {
    pub name: String,
    pub doc: Option<String>,
    pub fields: Vec<FieldDoc>,
}

/// Documentation for a struct field.
#[derive(Debug, Clone, Serialize)]
pub struct FieldDoc {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: TypeRef,
    pub doc: Option<String>,
}

/// Documentation for an enum.
#[derive(Debug, Clone, Serialize)]
pub struct EnumDoc {
    pub name: String,
    pub doc: Option<String>,
    pub variants: Vec<VariantDoc>,
}

/// Documentation for an enum variant.
#[derive(Debug, Clone, Serialize)]
pub struct VariantDoc {
    pub name: String,
    pub doc: Option<String>,
    pub payload: VariantPayload,
}

/// Payload type for enum variants.
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum VariantPayload {
    Unit(String),
    Tuple {
        tuple: Vec<TypeRef>,
    },
    Struct {
        #[serde(rename = "struct")]
        fields: Vec<FieldDoc>,
    },
}

/// Generate documentation from a parsed and type-checked program.
pub fn generate_docs(
    program: &Program,
    typed: &TypedProgram,
    comments: &CommentMap,
    source: &str,
) -> DocsOutput {
    let mut functions = Vec::new();
    let mut structs = Vec::new();
    let mut enums = Vec::new();

    for (typed_def, untyped_def) in typed.definitions.iter().zip(program.definitions.iter()) {
        match typed_def {
            TypedDefinition::Function(f) => {
                let doc = comments.doc_comments(untyped_def.span, source);
                functions.push(function_doc(f, doc));
            }
            TypedDefinition::Struct(s) => {
                let doc = comments.doc_comments(untyped_def.span, source);
                let field_docs = extract_struct_field_docs(&untyped_def.node, comments, source);
                structs.push(struct_doc(s, doc, field_docs));
            }
            TypedDefinition::Enum(e) => {
                let doc = comments.doc_comments(untyped_def.span, source);
                let variant_docs = extract_enum_variant_docs(&untyped_def.node, comments, source);
                enums.push(enum_doc(e, doc, variant_docs));
            }
            _ => {}
        }
    }

    DocsOutput {
        version: "1.0.0".to_string(),
        functions,
        structs,
        enums,
    }
}

fn function_doc(f: &TypedFunctionDef, doc: Option<String>) -> FunctionDoc {
    FunctionDoc {
        name: f.name.name.clone(),
        doc,
        params: f
            .params
            .iter()
            .map(|p| ParamDoc {
                name: p.name.name.clone(),
                ty: TypeRef::from(&p.ty),
            })
            .collect(),
        return_type: TypeRef::from(&f.return_type),
        effect: match f.effect {
            EffectKind::Pure => "pure".to_string(),
            EffectKind::Effectful => "effectful".to_string(),
            EffectKind::Runtime => "runtime".to_string(),
        },
        export: f.export.as_ref().map(|e| format!("{:?}", e).to_lowercase()),
    }
}

fn struct_doc(
    s: &TypedStructDef,
    doc: Option<String>,
    field_docs: Vec<Option<String>>,
) -> StructDoc {
    StructDoc {
        name: s.name.name.clone(),
        doc,
        fields: s
            .fields
            .iter()
            .zip(field_docs.into_iter().chain(std::iter::repeat(None)))
            .map(|(f, doc)| FieldDoc {
                name: f.name.name.clone(),
                ty: TypeRef::from(&f.ty),
                doc,
            })
            .collect(),
    }
}

fn enum_doc(e: &TypedEnumDef, doc: Option<String>, variant_docs: Vec<Option<String>>) -> EnumDoc {
    EnumDoc {
        name: e.name.name.clone(),
        doc,
        variants: e
            .variants
            .iter()
            .zip(variant_docs.into_iter().chain(std::iter::repeat(None)))
            .map(|(v, doc)| VariantDoc {
                name: v.name.name.clone(),
                doc,
                payload: match &v.payload {
                    TypedEnumVariantPayload::Unit => VariantPayload::Unit("unit".to_string()),
                    TypedEnumVariantPayload::Tuple(types) => VariantPayload::Tuple {
                        tuple: types.iter().map(TypeRef::from).collect(),
                    },
                    TypedEnumVariantPayload::Struct(fields) => VariantPayload::Struct {
                        fields: fields
                            .iter()
                            .map(|f| FieldDoc {
                                name: f.name.name.clone(),
                                ty: TypeRef::from(&f.ty),
                                doc: None,
                            })
                            .collect(),
                    },
                },
            })
            .collect(),
    }
}

fn extract_struct_field_docs(
    def: &Definition,
    comments: &CommentMap,
    source: &str,
) -> Vec<Option<String>> {
    if let Definition::Struct(StructDef { fields, .. }) = def {
        fields
            .iter()
            .map(|f| comments.doc_comments(f.span, source))
            .collect()
    } else {
        Vec::new()
    }
}

fn extract_enum_variant_docs(
    def: &Definition,
    comments: &CommentMap,
    source: &str,
) -> Vec<Option<String>> {
    if let Definition::Enum(EnumDef { variants, .. }) = def {
        variants
            .iter()
            .map(|v| comments.doc_comments(v.span, source))
            .collect()
    } else {
        Vec::new()
    }
}
