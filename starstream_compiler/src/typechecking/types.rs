use crate::{
    ast::{Storage, TypeArg, TypeDefRhs, TypedBindings},
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
    Utxo(SymbolId, String),
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
            ComparableType::Utxo(_, _) => (),
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
        // matches!(self, ComparableType::Utxo(_, _))
        // disable this for now for simplicity
        // we need the syntax/types to properly differentiate the method type
        false
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

    pub fn from_storage(storage: &Storage, symbols: &Symbols) -> Self {
        typed_bindings_to_product(&storage.bindings, &symbols.types)
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
            TypeArg::U64 => ComparableType::Primitive(PrimitiveType::U64),
            TypeArg::I64 => ComparableType::Primitive(PrimitiveType::I64),
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
                    ComparableType::Utxo(symbol_id, type_ref.0.raw.clone())
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

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Unit => write!(f, "()"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::StrRef => write!(f, "str"),
        }
    }
}

impl std::fmt::Display for ComparableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparableType::Primitive(prim_type) => write!(f, "{}", prim_type),
            ComparableType::Intermediate => write!(f, "Intermediate"),
            ComparableType::Product(fields) => {
                write!(f, "{{")?;
                for (i, (name, field_type)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, field_type)?;
                }
                write!(f, "}}")
            }
            ComparableType::Sum(variants) => {
                if variants.is_empty() {
                    return write!(f, "void");
                }

                for (i, (name, variant_type)) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}({})", name, variant_type)?;
                }
                Ok(())
            }
            ComparableType::FnType(params, return_type) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }
            ComparableType::Utxo(_id, name) => {
                write!(f, "{}", name)
            }
            ComparableType::Var(type_var) => {
                write!(f, "unbound type variable: {}", type_var.0)
            }
            ComparableType::Ref(inner) => {
                write!(f, "&{}", inner)
            }
            ComparableType::Void => {
                write!(f, "void")
            }
        }
    }
}
