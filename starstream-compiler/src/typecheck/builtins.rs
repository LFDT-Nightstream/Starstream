//! Built-in registry for starstream:std imports.
//!
//! This module provides type information for the standard library functions
//! that are resolved at compile time (not downloaded via wit-dep).

use std::collections::HashMap;

use starstream_types::{EffectKind, Type};

/// Information about a built-in function from the standard library.
#[derive(Clone, Debug)]
pub struct BuiltinFunction {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub effect: EffectKind,
}

impl BuiltinFunction {
    pub fn to_function_type(&self) -> Type {
        Type::Function {
            params: self.params.clone(),
            result: Box::new(self.return_type.clone()),
            effect: self.effect,
        }
    }
}

/// Registry of built-in interfaces and their exports.
#[derive(Default)]
pub struct BuiltinRegistry {
    /// Maps `namespace:package` -> `interface` -> `name` -> `BuiltinFunction`
    packages: HashMap<String, HashMap<String, HashMap<String, BuiltinFunction>>>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        let mut registry = Self::default();
        registry.register_std();
        registry
    }

    /// Look up a function in a specific interface.
    #[allow(dead_code)]
    pub fn get_interface_function(
        &self,
        namespace: &str,
        package: &str,
        interface: &str,
        name: &str,
    ) -> Option<&BuiltinFunction> {
        let pkg_path = format!("{namespace}:{package}");
        self.packages
            .get(&pkg_path)
            .and_then(|pkg| pkg.get(interface))
            .and_then(|iface| iface.get(name))
    }

    /// Look up an interface in a package (for namespace imports).
    /// Returns all functions in the interface.
    pub fn get_interface(
        &self,
        namespace: &str,
        package: &str,
        interface: &str,
    ) -> Option<&HashMap<String, BuiltinFunction>> {
        let pkg_path = format!("{namespace}:{package}");
        self.packages
            .get(&pkg_path)
            .and_then(|pkg| pkg.get(interface))
    }

    /// Check if a package exists.
    pub fn has_package(&self, namespace: &str, package: &str) -> bool {
        let path = format!("{namespace}:{package}");
        self.packages.contains_key(&path)
    }

    /// Get all interfaces in a package.
    #[allow(dead_code)]
    pub fn get_package_interfaces(
        &self,
        namespace: &str,
        package: &str,
    ) -> Option<&HashMap<String, HashMap<String, BuiltinFunction>>> {
        let path = format!("{namespace}:{package}");
        self.packages.get(&path)
    }

    fn register_std(&mut self) {
        // starstream:std/cardano - Cardano blockchain context functions
        let mut cardano = HashMap::new();

        // blockHeight() -> i64
        cardano.insert(
            "blockHeight".to_string(),
            BuiltinFunction {
                params: vec![],
                return_type: Type::Int,
                effect: EffectKind::Runtime,
            },
        );

        // currentSlot() -> i64
        cardano.insert(
            "currentSlot".to_string(),
            BuiltinFunction {
                params: vec![],
                return_type: Type::Int,
                effect: EffectKind::Runtime,
            },
        );

        let mut std_package = HashMap::new();
        std_package.insert("cardano".to_string(), cardano);
        self.packages
            .insert("starstream:std".to_string(), std_package);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_block_height() {
        let registry = BuiltinRegistry::new();
        let func = registry
            .get_interface_function("starstream", "std", "cardano", "blockHeight")
            .expect("blockHeight should exist");

        assert_eq!(func.params, vec![]);
        assert_eq!(func.return_type, Type::Int);
        assert_eq!(func.effect, EffectKind::Runtime);
    }

    #[test]
    fn package_exists() {
        let registry = BuiltinRegistry::new();
        assert!(registry.has_package("starstream", "std"));
        assert!(!registry.has_package("starstream", "nonexistent"));
    }
}
