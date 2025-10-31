use std::collections::{BTreeMap, HashMap, HashSet};

use starstream_types::{Scheme, Type, TypeVarId};

#[derive(Debug)]
pub struct TypeEnv {
    scopes: Vec<HashMap<String, Scheme>>,
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

    pub fn insert(&mut self, name: String, scheme: Scheme) -> Option<Scheme> {
        self.scopes
            .last_mut()
            .expect("type env scope missing")
            .insert(name, scheme)
    }

    pub fn get(&self, name: &str) -> Option<&Scheme> {
        for scope in self.scopes.iter().rev() {
            if let Some(scheme) = scope.get(name) {
                return Some(scheme);
            }
        }
        None
    }

    pub fn contains_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .map(|scope| scope.contains_key(name))
            .unwrap_or(false)
    }

    pub fn free_type_vars(&self) -> HashSet<TypeVarId> {
        let mut free = HashSet::new();
        for scope in &self.scopes {
            for scheme in scope.values() {
                free.extend(free_type_vars_scheme(scheme));
            }
        }
        free
    }

    /// Collect a snapshot of the current bindings, with inner scopes shadowing
    /// outer ones, returned as a sorted map for deterministic formatting.
    pub fn snapshot(&self) -> BTreeMap<String, Scheme> {
        let mut map = BTreeMap::new();
        for scope in &self.scopes {
            for (name, scheme) in scope {
                map.insert(name.clone(), scheme.clone());
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
        Type::Function(params, result) => {
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
        Type::Int | Type::Bool | Type::Unit => {}
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
