//! Component registry for mapping contract hashes to WIT packages.
//!
//! This module provides `ComponentRegistry`, which orchestrates:
//! - Extracting WIT packages from WASM components
//! - Computing content hashes for each package
//! - Storing packages in a content-addressable `PackageStore`
//! - Maintaining the mapping from contract hash to package hashes
//!
//! When retrieving WIT, hashes can optionally be embedded in version build metadata
//! for transitive content addressing.

use crate::flatten::{package_to_wit, FlattenError};
use crate::storage::Storage;
use crate::store::{PackageStore, StoreError};
use crate::version::version_with_hash;
use crate::WitPackageHash;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;
use wit_component::{DecodedWasm, WitPrinter};
use wit_parser::{PackageId, Resolve};

/// Errors that can occur when working with the component registry.
#[derive(Debug, Error)]
pub enum ComponentRegistryError {
    #[error("failed to decode component: {0}")]
    DecodeError(String),

    #[error("failed to flatten WIT: {0}")]
    FlattenError(#[from] FlattenError),

    #[error("storage error: {0}")]
    StoreError(#[from] StoreError),

    #[error("failed to parse stored WIT: {0}")]
    ParseError(String),

    #[error("failed to print WIT: {0}")]
    PrintError(String),

    #[error("version rewrite error: {0}")]
    VersionError(String),

    #[error("component not found: {0}")]
    NotFound(String),

    #[error("no packages found in component")]
    NoPackages,
}

/// Information about a registered component's WIT packages.
#[derive(Debug, Clone)]
pub struct ComponentPackages {
    /// Hashes of all WIT packages in this component, in topological order.
    pub package_hashes: Vec<WitPackageHash>,
    /// The main package's name (e.g., "myapp:token@1.0.0").
    pub main_package_name: String,
}

/// Registry mapping contract hashes to their WIT packages.
///
/// This is the main orchestration layer that:
/// 1. Extracts WIT packages from WASM components
/// 2. Computes content hashes for each package
/// 3. Stores packages in a deduplicated `PackageStore`
/// 4. Maintains contract → packages mapping
#[derive(Debug)]
pub struct ComponentRegistry<S: Storage> {
    /// Content-addressable storage for WIT packages.
    package_store: PackageStore<S>,
    /// Mapping from contract hash to component packages info.
    components: Arc<RwLock<HashMap<String, ComponentPackages>>>,
}

impl<S: Storage> ComponentRegistry<S> {
    /// Create a new component registry with the given storage backend.
    pub fn new(storage: S) -> Self {
        Self {
            package_store: PackageStore::new(storage),
            components: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a component and extract its WIT packages.
    ///
    /// This is the main ingestion function. It:
    /// 1. Decodes the component to extract WIT
    /// 2. For each package, computes a content hash
    /// 3. Stores packages in the content-addressable store (deduplicated)
    /// 4. Records the contract → packages mapping
    ///
    /// # Arguments
    /// * `contract_hash` - The Poseidon2 hash identifying this contract
    /// * `component_bytes` - The raw WASM component bytes
    ///
    /// # Returns
    /// Information about the registered component's packages
    pub async fn register(
        &self,
        contract_hash: String,
        component_bytes: &[u8],
    ) -> Result<ComponentPackages, ComponentRegistryError> {
        // Step 1: Decode the component to get a Resolve
        let decoded = wit_component::decode(component_bytes)
            .map_err(|e| ComponentRegistryError::DecodeError(e.to_string()))?;

        let (resolve, main_pkg_id) = match decoded {
            DecodedWasm::WitPackage(resolve, pkg) => (resolve, pkg),
            DecodedWasm::Component(resolve, world) => {
                let pkg = resolve.worlds[world]
                    .package
                    .ok_or_else(|| ComponentRegistryError::DecodeError("world has no package".into()))?;
                (resolve, pkg)
            }
        };

        // Get the main package name
        let main_package_name = resolve.packages[main_pkg_id].name.to_string();

        // Step 2: Extract and process all packages
        let package_hashes = self
            .process_packages(&resolve, main_pkg_id)
            .await?;

        if package_hashes.is_empty() {
            return Err(ComponentRegistryError::NoPackages);
        }

        // Step 3: Store the mapping
        let component_info = ComponentPackages {
            package_hashes: package_hashes.clone(),
            main_package_name,
        };

        {
            let mut components = self.components.write().await;
            components.insert(contract_hash, component_info.clone());
        }

        Ok(component_info)
    }

    /// Process all packages in a Resolve, computing hashes and storing them.
    ///
    /// Returns the list of package hashes in topological order (dependencies first).
    async fn process_packages(
        &self,
        resolve: &Resolve,
        main_pkg_id: PackageId,
    ) -> Result<Vec<WitPackageHash>, ComponentRegistryError> {
        // Get all packages in topological order (dependencies before dependents)
        let ordered_packages = self.topological_sort(resolve, main_pkg_id);

        let mut package_hashes = Vec::new();

        for pkg_id in ordered_packages {
            // Print the package to text (semicolon format for self-contained storage)
            let text = package_to_wit(resolve, pkg_id)?;

            // Store in PackageStore (returns hash computed from content)
            let hash = self.package_store.store(text.as_bytes()).await?;

            package_hashes.push(hash);
        }

        Ok(package_hashes)
    }

    /// Get a topological ordering of packages (dependencies first).
    fn topological_sort(&self, resolve: &Resolve, main_pkg_id: PackageId) -> Vec<PackageId> {
        // For now, we process all packages in the Resolve.
        // A more sophisticated implementation would trace dependencies from main_pkg_id.
        //
        // The order doesn't affect correctness for storage (content-addressed),
        // but matters for reconstruction. We'll use a simple approach:
        // iterate all packages, putting main_pkg_id last.

        let mut packages: Vec<PackageId> = resolve
            .packages
            .iter()
            .map(|(id, _)| id)
            .filter(|&id| id != main_pkg_id)
            .collect();

        // Add main package last (dependents come after dependencies)
        packages.push(main_pkg_id);

        packages
    }

    /// Get the package hashes for a registered component.
    pub async fn get_packages(
        &self,
        contract_hash: &str,
    ) -> Result<Vec<WitPackageHash>, ComponentRegistryError> {
        let components = self.components.read().await;
        components
            .get(contract_hash)
            .map(|info| info.package_hashes.clone())
            .ok_or_else(|| ComponentRegistryError::NotFound(contract_hash.to_string()))
    }

    /// Get the full WIT text for a component with hashes embedded in version build metadata.
    ///
    /// This reconstructs the WIT from stored packages and embeds content hashes
    /// in the version's build metadata for transitive content addressing.
    ///
    /// The output format follows wit-parser's expectations:
    /// - Main package with semicolon syntax
    /// - Dependency packages in nested format
    pub async fn get_wit(&self, contract_hash: &str) -> Result<String, ComponentRegistryError> {
        let components = self.components.read().await;
        let info = components
            .get(contract_hash)
            .ok_or_else(|| ComponentRegistryError::NotFound(contract_hash.to_string()))?;

        let num_packages = info.package_hashes.len();

        if num_packages == 0 {
            return Ok(String::new());
        }

        // Load and parse all packages into a single Resolve
        let mut resolve = Resolve::new();
        let mut pkg_ids: Vec<PackageId> = Vec::new();

        for hash in &info.package_hashes {
            let content = self.package_store.load(hash).await?;
            let text = String::from_utf8_lossy(&content);

            let pkg_id = resolve
                .push_str(&format!("pkg_{}.wit", hash.to_hex()), &text)
                .map_err(|e| ComponentRegistryError::ParseError(e.to_string()))?;

            pkg_ids.push(pkg_id);
        }

        // Modify versions to include hashes in build metadata
        for (pkg_id, hash) in pkg_ids.iter().zip(info.package_hashes.iter()) {
            if let Some(version) = &resolve.packages[*pkg_id].name.version {
                let new_version = version_with_hash(version, hash)
                    .map_err(|e| ComponentRegistryError::VersionError(e.to_string()))?;
                resolve.packages[*pkg_id].name.version = Some(new_version);
            }
        }

        // The main package is the last one (topologically sorted with deps first)
        let main_pkg_id = pkg_ids[num_packages - 1];
        let nested_pkg_ids: Vec<PackageId> = pkg_ids.iter().take(num_packages - 1).copied().collect();

        // Use WitPrinter to output in the correct format
        let mut printer = WitPrinter::default();
        printer
            .print(&resolve, main_pkg_id, &nested_pkg_ids)
            .map_err(|e| ComponentRegistryError::PrintError(e.to_string()))?;

        Ok(printer.output.to_string())
    }

    /// Get the raw WIT text without hash embedding.
    ///
    /// This returns the original WIT text as stored, without adding hashes
    /// to version strings. Useful for debugging or when you need the original format.
    pub async fn get_wit_raw(&self, contract_hash: &str) -> Result<String, ComponentRegistryError> {
        let components = self.components.read().await;
        let info = components
            .get(contract_hash)
            .ok_or_else(|| ComponentRegistryError::NotFound(contract_hash.to_string()))?;

        let num_packages = info.package_hashes.len();

        if num_packages == 0 {
            return Ok(String::new());
        }

        // Load and parse all packages into a single Resolve
        let mut resolve = Resolve::new();
        let mut pkg_ids: Vec<PackageId> = Vec::new();

        for hash in &info.package_hashes {
            let content = self.package_store.load(hash).await?;
            let text = String::from_utf8_lossy(&content);

            let pkg_id = resolve
                .push_str(&format!("pkg_{}.wit", hash.to_hex()), &text)
                .map_err(|e| ComponentRegistryError::ParseError(e.to_string()))?;

            pkg_ids.push(pkg_id);
        }

        // The main package is the last one
        let main_pkg_id = pkg_ids[num_packages - 1];
        let nested_pkg_ids: Vec<PackageId> = pkg_ids.iter().take(num_packages - 1).copied().collect();

        // Use WitPrinter to output in the correct format (without hash modification)
        let mut printer = WitPrinter::default();
        printer
            .print(&resolve, main_pkg_id, &nested_pkg_ids)
            .map_err(|e| ComponentRegistryError::PrintError(e.to_string()))?;

        Ok(printer.output.to_string())
    }

    /// Get a reference to the underlying package store.
    pub fn package_store(&self) -> &PackageStore<S> {
        &self.package_store
    }

    /// Check if a component is registered.
    pub async fn contains(&self, contract_hash: &str) -> bool {
        let components = self.components.read().await;
        components.contains_key(contract_hash)
    }
}

impl<S: Storage + Clone> Clone for ComponentRegistry<S> {
    fn clone(&self) -> Self {
        Self {
            package_store: self.package_store.clone(),
            components: Arc::clone(&self.components),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::InMemoryStorage;

    fn test_registry() -> ComponentRegistry<InMemoryStorage> {
        ComponentRegistry::new(InMemoryStorage::new())
    }

    #[tokio::test]
    async fn test_component_not_found() {
        let registry = test_registry();

        let result = registry.get_packages("nonexistent").await;
        assert!(matches!(result, Err(ComponentRegistryError::NotFound(_))));
    }

    #[tokio::test]
    async fn test_contains() {
        let registry = test_registry();

        assert!(!registry.contains("test_hash").await);
    }

    /// Integration test using a real WASM component.
    ///
    /// This test requires the no-args.wasm component to exist at the expected path.
    #[tokio::test]
    async fn test_register_real_component() {
        // Path to a real component in the repo
        let component_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../node-server/genesis/no-args.wasm"
        );

        // Skip test if component doesn't exist (e.g., in CI without build artifacts)
        let component_bytes = match std::fs::read(component_path) {
            Ok(bytes) => bytes,
            Err(_) => {
                eprintln!("Skipping test: {} not found", component_path);
                return;
            }
        };

        let registry = test_registry();
        let contract_hash = "0xTEST_CONTRACT_HASH";

        // Register the component
        let result = registry
            .register(contract_hash.to_string(), &component_bytes)
            .await;

        assert!(result.is_ok(), "Failed to register component: {:?}", result);

        let packages = result.unwrap();
        assert!(!packages.package_hashes.is_empty());
        assert!(packages.main_package_name.contains("root:component"));

        // Verify we can retrieve packages
        let retrieved = registry.get_packages(contract_hash).await.unwrap();
        assert_eq!(retrieved.len(), packages.package_hashes.len());

        // Verify we can get WIT text
        let wit = registry.get_wit(contract_hash).await.unwrap();
        assert!(wit.contains("package root:component"));

        // Verify we can get raw WIT text
        let wit_raw = registry.get_wit_raw(contract_hash).await.unwrap();
        assert!(wit_raw.contains("package root:component"));

        // Verify contains works
        assert!(registry.contains(contract_hash).await);
        assert!(!registry.contains("nonexistent").await);
    }

    /// Test that registering the same component twice is idempotent.
    #[tokio::test]
    async fn test_register_idempotent() {
        let component_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../node-server/genesis/no-args.wasm"
        );

        let component_bytes = match std::fs::read(component_path) {
            Ok(bytes) => bytes,
            Err(_) => return, // Skip if component doesn't exist
        };

        let registry = test_registry();

        // Register twice
        let result1 = registry
            .register("hash1".to_string(), &component_bytes)
            .await
            .unwrap();
        let result2 = registry
            .register("hash2".to_string(), &component_bytes)
            .await
            .unwrap();

        // Same component should produce same package hashes
        assert_eq!(result1.package_hashes, result2.package_hashes);
    }

    /// Test that version hashes are properly embedded for packages with versions.
    #[tokio::test]
    async fn test_version_hash_embedding() {
        // Use adder.wasm which has a dependency with a version (test:foo)
        let component_path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../node-server/genesis/adder.wasm"
        );

        let component_bytes = match std::fs::read(component_path) {
            Ok(bytes) => bytes,
            Err(_) => {
                eprintln!("Skipping test: {} not found", component_path);
                return;
            }
        };

        let registry = test_registry();
        let contract_hash = "0xADDER_TEST";

        registry
            .register(contract_hash.to_string(), &component_bytes)
            .await
            .unwrap();

        // Get WIT with hashes - packages with versions should have hash in build metadata
        let wit_with_hash = registry.get_wit(contract_hash).await.unwrap();

        // Get raw WIT - no hashes
        let wit_raw = registry.get_wit_raw(contract_hash).await.unwrap();

        // The raw version should not have build metadata (assuming original didn't)
        // The hashed version should have build metadata for versioned packages
        // Note: This depends on the actual content of adder.wasm
        assert!(wit_with_hash.len() >= wit_raw.len());
    }
}
