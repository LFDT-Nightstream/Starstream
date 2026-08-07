//! Built-in registry for starstream:std imports.
//!
//! This module provides type information for the standard library functions
//! that are resolved at compile time (not downloaded via wit-dep).

use std::sync::Arc;

use starstream_types::{
    AbiType, DUMMY_SPAN, EnumType, EnumVariantKind, EnumVariantType, FunctionKind, FunctionType,
    Identifier, IntWidth, NameId, StaticFunction, Type, TypeParam, TypeVarId, TypedAbiMethodDecl,
};

use crate::typecheck::{
    TypeError, TypeErrorKind,
    env::{ConstantInfo, Namespace, StructConstructor, TypeEntry},
};

/// Registry of built-in interfaces and their exports.
#[derive(Default)]
pub struct BuiltinRegistry {
    prelude: Namespace,

    /// Maps `namespace:package/interface` as `namespace::package::interface`.
    wit_tree: Namespace,
}

impl BuiltinRegistry {
    pub fn new() -> (Self, TypeVarId, NameId) {
        let mut registry = Self::default();
        let mut next_type_var = TypeVarId(0);
        let mut next_name_id = NameId(0);
        register_prelude(&mut registry.prelude, &mut next_type_var, &mut next_name_id);
        let starstream = registry.wit_tree.add_child("starstream".to_owned());
        let std = starstream.add_child("std".to_owned());
        register_std_cardano(std.add_child("cardano".to_owned()), &mut next_name_id);
        (registry, next_type_var, next_name_id)
    }

    /// Get the namespace representing the prelude imported into all source files.
    pub fn prelude(&self) -> &Namespace {
        &self.prelude
    }

    /// Get the namespace representing the WIT tree for a namespace:package, optionally /interface.
    pub fn get_as_namespace(
        &self,
        namespace: &Identifier,
        package: &Identifier,
        interface: Option<&Identifier>,
    ) -> Result<&Namespace, TypeError> {
        let ns = &self.wit_tree;
        let Some(ns) = ns.namespaces.get(namespace.as_str()) else {
            return Err(TypeError::new(
                TypeErrorKind::UnknownImportPackage {
                    namespace: namespace.to_string(),
                    package: package.to_string(),
                },
                namespace.span,
            ));
        };
        let Some(ns) = ns.namespaces.get(package.as_str()) else {
            return Err(TypeError::new(
                TypeErrorKind::UnknownImportPackage {
                    namespace: namespace.to_string(),
                    package: package.to_string(),
                },
                package.span,
            ));
        };
        if let Some(interface) = interface {
            let Some(ns) = ns.namespaces.get(interface.as_str()) else {
                return Err(TypeError::new(
                    TypeErrorKind::UnknownImportInterface {
                        namespace: namespace.to_string(),
                        package: package.to_string(),
                        interface: interface.to_string(),
                    },
                    interface.span,
                ));
            };
            Ok(ns)
        } else {
            Ok(ns)
        }
    }
}

// ----------------------------------------------------------------------------
// Prelude builtins

/// Register builtin prelude types, including primitives, Option, Result, Utxo, and Token.
fn register_prelude(
    prelude: &mut Namespace,
    next_type_var: &mut TypeVarId,
    next_name_id: &mut NameId,
) {
    // Primitives
    let mut primitive = |name: &str, ty| prelude.types.insert(name.to_owned(), TypeEntry::from(ty));
    primitive("()", Type::Unit);
    primitive("bool", Type::Bool);
    primitive("i8", Type::from(IntWidth::I8));
    primitive("i16", Type::from(IntWidth::I16));
    primitive("i32", Type::from(IntWidth::I32));
    primitive("i64", Type::from(IntWidth::I64));
    primitive("u8", Type::from(IntWidth::U8));
    primitive("u16", Type::from(IntWidth::U16));
    primitive("u32", Type::from(IntWidth::U32));
    primitive("u64", Type::from(IntWidth::U64));

    primitive("Utxo", Type::UtxoAny);
    primitive("Token", Type::TokenAny); // TODO: currently being overridden, decide what to do with this.

    // Option and Result helper

    // Option<T>
    let t = next_type_var.fresh();
    register_prelude_enum(
        prelude,
        "Option",
        &[
            ("None", EnumVariantKind::Unit),
            ("Some", EnumVariantKind::Tuple(vec![Type::Var(t)])),
        ],
        vec![TypeParam {
            id: t,
            name: "T".into(),
        }],
        "A value that may or may not be present.",
        &[("Some", "Contains a value."), ("None", "No value present.")],
    );

    // Result<T, E>
    let t2 = next_type_var.fresh();
    let e = next_type_var.fresh();
    register_prelude_enum(
        prelude,
        "Result",
        &[
            ("Ok", EnumVariantKind::Tuple(vec![Type::Var(t2)])),
            ("Err", EnumVariantKind::Tuple(vec![Type::Var(e)])),
        ],
        vec![
            TypeParam {
                id: t2,
                name: "T".into(),
            },
            TypeParam {
                id: e,
                name: "E".into(),
            },
        ],
        "A value representing either success or failure.",
        &[
            ("Ok", "Contains a success value."),
            ("Err", "Contains an error value."),
        ],
    );

    // Register the built-in `Token` ABI that every `token` definition's
    // `impl Token { ... }` block is checked against. It declares
    // `attach(Utxo) -> ()` and `detach(Utxo) -> ()`. User code may not
    // redeclare `abi Token` (guarded in `register_abi`).
    let mut method = |name: &str| TypedAbiMethodDecl {
        name: Identifier::new(name, DUMMY_SPAN),
        ty: Arc::new(FunctionType {
            kind: FunctionKind::Normal,
            name_span: DUMMY_SPAN,
            params: vec![Type::UtxoAny],
            param_spans: vec![DUMMY_SPAN],
            result: Type::Unit,
            callee: Some(StaticFunction::Named(next_name_id.fresh())),
        }),
        span: DUMMY_SPAN,
    };
    prelude.types.insert(
        "Token".to_owned(),
        TypeEntry::from(Type::from(AbiType {
            name: Identifier::new("Token", DUMMY_SPAN),
            methods: vec![method("attach"), method("detach")],
        })),
    );
}

/// Helper to register a prelude enum type, building both the `Type::Enum`
/// and the internal `EnumVariantInfo` from a single variant description.
fn register_prelude_enum(
    prelude: &mut Namespace,
    name: &str,
    variants: &[(&str, EnumVariantKind)],
    type_params: Vec<TypeParam>,
    doc: &str,
    variant_docs: &[(&str, &str)],
) {
    let type_variants: Vec<EnumVariantType> = variants
        .iter()
        .map(|(vname, kind)| EnumVariantType {
            name: vname.to_string(),
            kind: kind.clone(),
        })
        .collect();

    let ty = Type::from(EnumType {
        name: name.to_owned(),
        variants: type_variants,
        type_args: vec![],
    });
    prelude.types.insert(
        name.to_string(),
        TypeEntry {
            ty: ty.clone(),
            span: DUMMY_SPAN,
            type_params: type_params.clone(),
            doc: Some(doc.into()),
            variant_docs: variant_docs
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
        },
    );

    // TODO: we need to create fresh variables for type parameters on use
    let namespace = prelude.add_child(name.to_string());
    for (i, (name, kind)) in variants.iter().enumerate() {
        match kind {
            EnumVariantKind::Unit => {
                // Unit variants are constants
                namespace.constants.insert(
                    name.to_string(),
                    ConstantInfo {
                        span: DUMMY_SPAN,
                        ty: ty.clone(),
                        type_params: type_params.clone(),
                        variant: i,
                    },
                );
            }
            EnumVariantKind::Tuple(params) => {
                // Tuple variants are functions
                namespace.constants.insert(
                    name.to_string(),
                    ConstantInfo {
                        span: DUMMY_SPAN,
                        ty: Type::from(FunctionType {
                            kind: FunctionKind::Normal,
                            name_span: DUMMY_SPAN,
                            params: params.clone(),
                            param_spans: vec![],
                            result: ty.clone(),
                            callee: Some(StaticFunction::Constructor { variant: i }),
                        }),
                        type_params: type_params.clone(),
                        variant: 0,
                    },
                );
            }
            EnumVariantKind::Struct(_fields) => {
                // Struct variants are constructors
                namespace.struct_constructors.insert(
                    name.to_string(),
                    StructConstructor {
                        span: DUMMY_SPAN,
                        ty: ty.clone(),
                        type_params: type_params.clone(),
                        enum_variant: i,
                    },
                );
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Library builtins

fn register_std_cardano(cardano: &mut Namespace, next_name_id: &mut NameId) {
    // blockHeight() -> i64
    cardano.constants.insert(
        "blockHeight".to_owned(),
        ConstantInfo::new(
            DUMMY_SPAN,
            Type::from(FunctionType {
                kind: FunctionKind::Runtime,
                name_span: DUMMY_SPAN,
                params: vec![],
                param_spans: vec![],
                result: Type::int(),
                callee: Some(StaticFunction::Named(next_name_id.fresh())),
            }),
        ),
    );

    // currentSlot() -> i64
    cardano.constants.insert(
        "currentSlot".to_owned(),
        ConstantInfo::new(
            DUMMY_SPAN,
            Type::from(FunctionType {
                kind: FunctionKind::Runtime,
                name_span: DUMMY_SPAN,
                params: vec![],
                param_spans: vec![],
                result: Type::int(),
                callee: Some(StaticFunction::Named(next_name_id.fresh())),
            }),
        ),
    );
}

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_block_height() {
        let (registry, _, _) = BuiltinRegistry::new();
        let func = registry
            .get_as_namespace(
                &Identifier::anon("starstream"),
                &Identifier::anon("std"),
                Some(&Identifier::anon("cardano")),
            )
            .expect("starstream:std/cardano missing")
            .constants
            .get("blockHeight")
            .expect("blockHeight missing");
        let Type::Function(func) = &func.ty else {
            panic!();
        };
        assert_eq!(func.params, vec![]);
        assert_eq!(func.result, Type::int());
        assert_eq!(func.kind, FunctionKind::Runtime);
    }

    #[test]
    fn package_exists() {
        let (registry, _, _) = BuiltinRegistry::new();
        registry
            .get_as_namespace(
                &Identifier::anon("starstream"),
                &Identifier::anon("std"),
                None,
            )
            .expect("starstream:std missing");
        registry
            .get_as_namespace(
                &Identifier::anon("starstream"),
                &Identifier::anon("nonexistent"),
                None,
            )
            .expect_err("starstream:nonexistent present");
    }
}
