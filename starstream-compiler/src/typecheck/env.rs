use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

use starstream_types::{
    AbiType, DUMMY_SPAN, Identifier, RecordFieldType, Scheme, Span, SubstituteType, Type,
    TypeParam, TypeVarId, types::EnumVariantKind,
};

use crate::typecheck::{TypeError, TypeErrorKind};

/// The type environment in which a type annotation or expression can be analyzed.
#[derive(Debug)]
pub struct TypeEnv {
    /// The stack of local scopes.
    scopes: Vec<HashMap<String, Binding>>,
    /// The root namespace for global lookups.
    pub root: Namespace,
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub decl_span: Span,
    pub mutable: bool,
    pub scheme: Scheme,
    pub class: BindingClass,
    pub visibility: BindingVisibility,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindingClass {
    Local,
    Storage,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindingVisibility {
    Private,
    Public,
}

/// Namespace containing names divided into zones.
#[derive(Clone, Default, Debug)]
pub struct Namespace {
    /// Child namespaces.
    pub namespaces: HashMap<String, Namespace>,
    /// Constants in the value zone, namely functions and unit enum variants.
    pub constants: HashMap<String, ConstantInfo>,
    /// Struct constructor zone, including those for struct enum variants.
    pub struct_constructors: HashMap<String, StructConstructor>,
    /// Type zone.
    pub types: HashMap<String, TypeEntry>,
}

#[derive(Clone, Debug)]
pub struct ConstantInfo {
    pub span: Span,
    pub ty: Type,
    pub type_params: Vec<TypeParam>,
    /// If this is a constant of an enum type, which unit variant it is.
    pub variant: usize,
}

#[derive(Clone, Debug)]
pub struct StructConstructor {
    pub span: Span,
    pub ty: Type,
    pub type_params: Vec<TypeParam>,
    pub enum_variant: usize,
}

#[derive(Clone, Debug)]
pub struct TypeEntry {
    pub ty: Type,
    pub span: Span,
    pub type_params: Vec<TypeParam>,
    pub doc: Option<String>,
    pub variant_docs: HashMap<String, String>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            root: Namespace::default(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, binding: Binding) -> Option<Binding> {
        self.scopes
            .last_mut()
            .expect("type env scope missing")
            .insert(name, binding)
    }

    pub fn get(&self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some(binding);
            }
        }
        None
    }

    pub fn get_in_current_scope(&self, name: &str) -> Option<&Binding> {
        self.scopes.last().and_then(|scope| scope.get(name))
    }

    pub fn free_type_vars(&self) -> HashSet<TypeVarId> {
        let mut free = HashSet::new();
        for scope in &self.scopes {
            for binding in scope.values() {
                free.extend(free_type_vars_scheme(&binding.scheme));
            }
        }
        free
    }

    /// Collect a snapshot of the current bindings, with inner scopes shadowing
    /// outer ones, returned as a sorted map for deterministic formatting.
    pub fn snapshot(&self) -> BTreeMap<String, Scheme> {
        let mut map = BTreeMap::new();
        for scope in &self.scopes {
            for (name, binding) in scope {
                map.insert(name.clone(), binding.scheme.clone());
            }
        }
        map
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantInfo {
    pub fn new(span: Span, ty: Type) -> ConstantInfo {
        ConstantInfo {
            span,
            ty,
            type_params: vec![],
            variant: 0,
        }
    }
}

impl StructConstructor {
    pub fn fields(&self) -> &[RecordFieldType] {
        match &self.ty {
            Type::Record(r) => &r.fields,
            Type::Enum(enum_) => match &enum_.variants[self.enum_variant].kind {
                EnumVariantKind::Struct(r) => r,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

impl From<Type> for TypeEntry {
    fn from(value: Type) -> Self {
        TypeEntry {
            ty: value,
            span: DUMMY_SPAN,
            type_params: vec![],
            doc: None,
            variant_docs: Default::default(),
        }
    }
}

impl Namespace {
    pub fn add_child(&mut self, name: String) -> &mut Namespace {
        self.namespaces.entry(name).or_default()
    }

    pub fn insert_constant(&mut self, name: &Identifier, v: ConstantInfo) -> Result<(), TypeError> {
        if let Some(old) = self.constants.insert(name.to_string(), v) {
            return Err(collision2(name.as_str(), name.span, old.span));
        }
        Ok(())
    }

    pub fn insert_struct_constructor(
        &mut self,
        name: &Identifier,
        v: StructConstructor,
    ) -> Result<(), TypeError> {
        if let Some(old) = self.struct_constructors.insert(name.to_string(), v) {
            return Err(collision(name.as_str(), name.span, old.span));
        }
        Ok(())
    }

    pub fn insert_type(&mut self, name: &Identifier, v: TypeEntry) -> Result<(), TypeError> {
        if let Some(old) = self.types.insert(name.to_string(), v) {
            return Err(collision(name.as_str(), name.span, old.span));
        }
        Ok(())
    }

    /// Import like `import ns from ...`
    pub fn import_as_namespace(
        &mut self,
        name: &Identifier,
        other: &Namespace,
    ) -> Result<(), TypeError> {
        self.add_child(name.to_string()).import_all_from(other)
    }

    /// Import like `import { their_name as name } from ...`
    pub fn import_name_from(
        &mut self,
        name: &Identifier,
        other: &Namespace,
        their_name: &str,
        source: &str,
    ) -> Result<(), TypeError> {
        let mut ok = false;
        if let Some(child) = other.namespaces.get(their_name) {
            self.import_as_namespace(name, child)?;
            ok = true;
        }
        if let Some(v) = other.constants.get(their_name) {
            self.insert_constant(name, v.clone())?;
            ok = true;
        }
        if let Some(v) = other.struct_constructors.get(their_name) {
            self.insert_struct_constructor(name, v.clone())?;
            ok = true;
        }
        if let Some(v) = other.types.get(their_name) {
            self.insert_type(name, v.clone())?;
            ok = true;
        }
        if ok {
            Ok(())
        } else {
            Err(TypeError::new(
                TypeErrorKind::UnknownImportFunction {
                    path: source.to_string(),
                    name: their_name.to_string(),
                },
                name.span,
            ))
        }
    }

    /// Import like `import * from ...`
    pub fn import_all_from(&mut self, other: &Namespace) -> Result<(), TypeError> {
        for (k, v) in &other.namespaces {
            self.add_child(k.to_owned()).import_all_from(v)?;
        }
        for (k, v) in &other.constants {
            if let Some(old) = self.constants.insert(k.to_owned(), v.clone()) {
                return Err(collision2(k, v.span, old.span));
            }
        }
        for (k, v) in &other.struct_constructors {
            if let Some(old) = self.struct_constructors.insert(k.to_owned(), v.clone()) {
                return Err(collision(k, v.span, old.span));
            }
        }
        for (k, v) in &other.types {
            if let Some(old) = self.types.insert(k.to_owned(), v.clone()) {
                return Err(collision(k, v.span, old.span));
            }
        }
        Ok(())
    }

    pub fn get_child(&self, path: &[Identifier]) -> Result<&Namespace, TypeError> {
        let mut ns = self;
        for each in path {
            match ns.namespaces.get(each.as_str()) {
                Some(next) => ns = next,
                None => {
                    return Err(TypeError::new(
                        TypeErrorKind::UnknownNamespace {
                            name: each.to_string(),
                        },
                        each.span,
                    ));
                }
            }
        }
        Ok(ns)
    }

    pub fn get_abi(&self, name: &Identifier) -> Result<&Arc<AbiType>, TypeError> {
        if let Some(type_entry) = self.types.get(name.as_str())
            && let Type::Abi(abi) = &type_entry.ty
        {
            return Ok(abi);
        }
        Err(TypeError::new(
            TypeErrorKind::UnknownAbi {
                name: name.to_string(),
            },
            name.span(),
        ))
    }
}

fn collision(name: &str, span: Span, prev: Span) -> TypeError {
    TypeError::new(
        TypeErrorKind::TypeAlreadyDefined {
            name: name.to_string(),
        },
        span,
    )
    .with_secondary(prev, "previously defined here")
}

fn collision2(name: &str, span: Span, prev: Span) -> TypeError {
    TypeError::new(
        TypeErrorKind::FunctionAlreadyDefined {
            name: name.to_string(),
        },
        span,
    )
    .with_secondary(prev, "previously defined here")
}

fn free_type_vars_scheme(scheme: &Scheme) -> HashSet<TypeVarId> {
    let mut free = scheme.ty.free_type_vars();
    for bound in &scheme.vars {
        free.remove(bound);
    }
    free
}
