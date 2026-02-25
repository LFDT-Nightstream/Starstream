use std::collections::{BTreeMap, HashMap, HashSet};

use starstream_types::{Scheme, Span, Type, TypeVarId, types::EnumVariantKind};

#[derive(Clone, Debug)]
pub struct Binding {
    pub decl_span: Span,
    pub mutable: bool,
    pub scheme: Scheme,
}

#[derive(Debug)]
pub struct TypeEnv {
    scopes: Vec<HashMap<String, Binding>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
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

fn free_type_vars_type(ty: &Type, out: &mut HashSet<TypeVarId>) {
    match ty {
        Type::Var(id) => {
            out.insert(*id);
        }
        Type::Function { params, result, .. } => {
            for ty in params {
                free_type_vars_type(ty, out);
            }
            free_type_vars_type(result, out);
        }
        Type::Tuple(types) => {
            for ty in types {
                free_type_vars_type(ty, out);
            }
        }
        Type::Record(record) => {
            for field in &record.fields {
                free_type_vars_type(&field.ty, out);
            }
        }
        Type::Enum(enum_type) => {
            for variant in &enum_type.variants {
                match &variant.kind {
                    EnumVariantKind::Unit => {}
                    EnumVariantKind::Tuple(payload) => {
                        for ty in payload {
                            free_type_vars_type(ty, out);
                        }
                    }
                    EnumVariantKind::Struct(fields) => {
                        for field in fields {
                            free_type_vars_type(&field.ty, out);
                        }
                    }
                }
            }
        }
        Type::Int(_) | Type::Bool | Type::Unit => {}
    }
}

fn free_type_vars_scheme(scheme: &Scheme) -> HashSet<TypeVarId> {
    let mut free = HashSet::new();
    free_type_vars_type(&scheme.ty, &mut free);

    for bound in &scheme.vars {
        free.remove(bound);
    }

    free
}
