use crate::{
    ast::{TypeArg, TypeDefRhs, TypedBindings},
    symbols::{FuncInfo, SymbolId, SymbolInformation, Symbols, TypeInfo},
};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    F32,
    F64,
    U32,
    I32,
    U64,
    I64,
    Bool,
    StrRef,
}

/// A type that can be compared for syntactic equivalence.
///
/// Similar to the AST type, but with typedefs resolved to the actual structure.
/// Also identifiers are replaced with the SymbolId for nominal types (like
/// utxos).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ComparableType {
    // TODO: unify with codegen StaticType?
    Primitive(PrimitiveType),
    Intermediate,
    Product(Vec<(String, ComparableType)>),
    Sum(Vec<(String, ComparableType)>),
    FnType(Vec<ComparableType>, Box<ComparableType>),
    Utxo(SymbolId),
    Var(TypeVar),
    Ref(Box<ComparableType>),

    // Void as in the type with cardinality 0
    Void,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeVar(pub u32);

impl ComparableType {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub const fn u32() -> Self {
        Self::Primitive(PrimitiveType::U32)
    }

    pub const fn boolean() -> Self {
        Self::Primitive(PrimitiveType::Bool)
    }

    pub const fn unit() -> Self {
        Self::Primitive(PrimitiveType::Unit)
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            ComparableType::Primitive(PrimitiveType::U32)
                | ComparableType::Primitive(PrimitiveType::I32)
                | ComparableType::Primitive(PrimitiveType::U64)
                | ComparableType::Primitive(PrimitiveType::I64)
        )
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            ComparableType::Primitive(PrimitiveType::U32)
                | ComparableType::Primitive(PrimitiveType::I32)
                | ComparableType::Primitive(PrimitiveType::U64)
                | ComparableType::Primitive(PrimitiveType::I64)
                | ComparableType::Primitive(PrimitiveType::F32)
                | ComparableType::Primitive(PrimitiveType::F64)
        )
    }

    pub fn from_fn_info(f: &FuncInfo, symbols: &Symbols) -> Self {
        Self::FnType(
            f.inputs_ty
                .iter()
                .map(|ty| ty.canonical_form(symbols))
                .collect(),
            f.output_ty
                .as_ref()
                .map(|ty| ty.canonical_form(symbols))
                .unwrap_or(ComparableType::unit())
                .boxed(),
        )
    }

    pub fn occurs_check(&self, v: &TypeVar) {
        match self {
            ComparableType::Primitive(_) => (),
            ComparableType::Intermediate => (),
            ComparableType::Utxo(_) => (),
            ComparableType::Product(args) | ComparableType::Sum(args) => {
                for (_, arg) in args {
                    arg.occurs_check(v);
                }
            }
            ComparableType::FnType(inputs, output) => {
                for input in inputs {
                    input.occurs_check(v);
                }

                output.occurs_check(v);
            }
            ComparableType::Var(type_var) => {
                // TODO: error
                assert!(type_var != v, "recursive type");
            }
            ComparableType::Void => (),
            ComparableType::Ref(ty) => ty.occurs_check(v),
        }
    }

    pub const fn is_linear(&self) -> bool {
        matches!(self, ComparableType::Intermediate)
    }

    pub const fn is_affine(&self) -> bool {
        matches!(self, ComparableType::Utxo(_))
    }

    pub(crate) fn token_storage() -> ComparableType {
        ComparableType::Product(vec![
            ("id".to_string(), ComparableType::u32()),
            ("amount".to_string(), ComparableType::u32()),
        ])
    }

    pub fn deref_1(&self) -> ComparableType {
        match self {
            ComparableType::Ref(inner) => *inner.clone(),
            ty => ty.clone(),
        }
    }
}

impl TypeArg {
    pub fn canonical_form_tys(
        &self,
        symbols: &HashMap<SymbolId, SymbolInformation<TypeInfo>>,
    ) -> ComparableType {
        match self {
            TypeArg::Unit => ComparableType::Primitive(PrimitiveType::Unit),
            TypeArg::Bool => ComparableType::Primitive(PrimitiveType::Bool),
            TypeArg::String => ComparableType::Primitive(PrimitiveType::StrRef),
            TypeArg::U32 => ComparableType::Primitive(PrimitiveType::U32),
            TypeArg::I32 => ComparableType::Primitive(PrimitiveType::I32),
            TypeArg::U64 => ComparableType::Primitive(PrimitiveType::U32),
            TypeArg::I64 => ComparableType::Primitive(PrimitiveType::U64),
            TypeArg::F32 => ComparableType::Primitive(PrimitiveType::F32),
            TypeArg::F64 => ComparableType::Primitive(PrimitiveType::F64),
            TypeArg::Intermediate { abi: _, storage: _ } => ComparableType::Intermediate,
            TypeArg::TypeApplication(_, _) => {
                // TODO: proper types
                ComparableType::Void
            }
            TypeArg::TypeRef(type_ref) => {
                let symbol_id = type_ref.0.uid.unwrap();
                let symbol = symbols.get(&symbol_id).unwrap();

                if let Some(type_def) = &symbol.info.type_def {
                    match type_def {
                        TypeDefRhs::TypeArg(type_arg) => type_arg.canonical_form_tys(symbols),
                        TypeDefRhs::Object(typed_bindings) => {
                            typed_bindings_to_product(typed_bindings, symbols)
                        }
                        TypeDefRhs::Variant(variant) => ComparableType::Sum(
                            variant
                                .0
                                .iter()
                                .map(|(name, ty)| {
                                    (name.raw.clone(), typed_bindings_to_product(ty, symbols))
                                })
                                .collect(),
                        ),
                    }
                } else {
                    ComparableType::Utxo(symbol_id)
                }
            }
            TypeArg::FnType(fn_type) => ComparableType::FnType(
                fn_type
                    .inputs
                    .values
                    .iter()
                    .map(|(_, ty)| ty.canonical_form_tys(symbols))
                    .collect(),
                fn_type
                    .output
                    .as_ref()
                    .map(|ty| ty.canonical_form_tys(symbols))
                    .unwrap_or(ComparableType::unit())
                    .boxed(),
            ),
            TypeArg::Ref(type_arg) => {
                ComparableType::Ref(type_arg.canonical_form_tys(symbols).boxed())
            }
        }
    }

    pub fn canonical_form(&self, symbols: &Symbols) -> ComparableType {
        self.canonical_form_tys(&symbols.types)
    }
}

fn typed_bindings_to_product(
    typed_bindings: &TypedBindings,
    symbols: &HashMap<SymbolId, SymbolInformation<TypeInfo>>,
) -> ComparableType {
    ComparableType::Product(
        typed_bindings
            .values
            .iter()
            .map(|(name, t)| (name.raw.clone(), t.canonical_form_tys(symbols)))
            .collect::<Vec<_>>(),
    )
}
