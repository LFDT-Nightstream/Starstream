//! Pretty-printing and [Display] impls for [Type]s.

use pretty::RcDoc;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

use super::*;

const TYPE_FORMAT_WIDTH: usize = 80;

impl Display for TypeVarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "t{}", self.0)
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
                    callee: _,
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
            Type::Utxo(utxo) => RcDoc::text(utxo.name.to_owned()),
            Type::TokenAny => RcDoc::text("Token"),
            Type::Token(token) => RcDoc::text(token.name.to_owned()),
            Type::Abi(abi) => RcDoc::text(abi.name.to_string()),
        }
    }
}

/// Like [pretty::PrettyFmt], but owns the doc, so we can `-> impl Display`.
struct RenderDoc<'a>(RcDoc<'a, ()>);

impl<'a> Display for RenderDoc<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
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
        RcDoc::text("struct ").append(RcDoc::as_string(&record.name))
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
            .append(RcDoc::as_string(&record.name))
            .append(RcDoc::space())
            .append("{")
            .append(RcDoc::hardline().append(fields).nest(4))
            .append(RcDoc::hardline())
            .append("}")
    }
}

fn enum_name_doc(enum_type: &EnumType, params: &HashMap<TypeVarId, String>) -> RcDoc<'static, ()> {
    let name = RcDoc::as_string(&enum_type.name);
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
                    EnumVariantKind::Unit => RcDoc::as_string(&variant.name).append(","),
                    EnumVariantKind::Tuple(payload) => RcDoc::as_string(&variant.name)
                        .append("(")
                        .append(RcDoc::intersperse(
                            payload
                                .iter()
                                .map(|ty| ty.to_doc(TypeDocMode::Compact, params)),
                            ", ",
                        ))
                        .append("),"),
                    EnumVariantKind::Struct(fields) => {
                        enum_variant_struct_doc(variant.name.as_str(), fields, params)
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
