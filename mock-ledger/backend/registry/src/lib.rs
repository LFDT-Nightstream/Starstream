//! Starstream Registry - WIT package storage and lookup.
//!
//! This crate provides:
//! - `WitPackageHash`: Content-addressable hash for WIT packages
//! - `Storage`: Trait for storage backends (in-memory, filesystem, K-V, etc.)
//! - `PackageStore`: Content-addressable storage built on any `Storage` backend
//! - `ComponentRegistry`: Content-addressable WIT package storage with deduplication
//! - `Registry`: High-level API for registering and looking up component WIT definitions
//!
//! # Platform Support
//!
//! The storage abstraction allows this crate to run in browsers (where filesystem
//! access isn't available) by using in-memory storage or browser-specific backends.
//!
//! # Example
//!
//! ```rust
//! use starstream_registry::{PackageStore, InMemoryStorage, WitPackageHash};
//!
//! # tokio_test::block_on(async {
//! // Create an in-memory store (works in browsers)
//! let storage = InMemoryStorage::new();
//! let store = PackageStore::new(storage);
//!
//! // Store a package
//! let content = b"package content";
//! let hash = store.store(content).await.unwrap();
//!
//! // Load it back
//! let loaded = store.load(&hash).await.unwrap();
//! assert_eq!(loaded, content);
//! # });
//! ```

mod component;
mod flatten;
mod hash;
mod storage;
mod store;
mod version;

pub use component::{ComponentPackages, ComponentRegistry, ComponentRegistryError};
pub use hash::WitPackageHash;
pub use storage::{InMemoryStorage, Storage, StorageError, StorageKey};
pub use store::{PackageStore, StoreError};
pub use version::{
    extract_hash_from_version, strip_hash_from_version, version_with_hash, VersionError,
};

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Component interface information returned by the registry.
///
/// This struct is used for the wRPC interface to return WIT definitions
/// along with the entrypoint information.
#[derive(Debug, Clone)]
pub struct ComponentInterface {
    /// The WIT definition text (with content hashes in version strings).
    pub wit: String,
    /// The default export/entrypoint name (e.g., "root").
    pub entrypoint: String,
}

/// High-level registry for looking up component WIT definitions.
///
/// This wraps `ComponentRegistry` with `InMemoryStorage` and provides
/// a simple API for registering components and retrieving their WIT.
///
/// The registry:
/// - Extracts WIT packages from WASM component bytes
/// - Stores packages in content-addressable storage (deduplicated)
/// - Returns WIT with content hashes in version strings for transitive addressing
#[derive(Clone)]
pub struct Registry {
    /// The underlying content-addressable component registry.
    inner: ComponentRegistry<InMemoryStorage>,
    /// Entrypoint names per contract (stored separately since they're not part of WIT).
    entrypoints: Arc<RwLock<HashMap<String, String>>>,
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}

impl Registry {
    /// Create a new registry with in-memory storage.
    pub fn new() -> Self {
        Self {
            inner: ComponentRegistry::new(InMemoryStorage::new()),
            entrypoints: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a component by its contract hash and raw WASM bytes.
    ///
    /// This extracts WIT packages from the component, stores them with
    /// content-addressable deduplication, and records the contract → packages mapping.
    ///
    /// # Arguments
    /// * `contract_hash` - The Poseidon2 hash identifying this contract
    /// * `component_bytes` - The raw WASM component bytes
    /// * `entrypoint` - The default export name (e.g., "root")
    ///
    /// # Returns
    /// Information about the registered component's packages
    pub async fn register(
        &self,
        contract_hash: String,
        component_bytes: &[u8],
        entrypoint: String,
    ) -> Result<ComponentPackages, ComponentRegistryError> {
        // Store the entrypoint
        {
            let mut entrypoints = self.entrypoints.write().await;
            entrypoints.insert(contract_hash.clone(), entrypoint);
        }

        // Register with the underlying ComponentRegistry
        self.inner.register(contract_hash, component_bytes).await
    }

    /// Get the WIT interface for a component by its contract hash.
    ///
    /// Returns the flattened WIT text with content hashes embedded in version
    /// strings for transitive content addressing.
    ///
    /// # Arguments
    /// * `contract_hash` - The contract hash to look up
    /// * `_resolve` - Reserved for future use (dependency resolution options)
    pub async fn get_wit(
        &self,
        contract_hash: String,
        _resolve: bool,
    ) -> anyhow::Result<ComponentInterface> {
        let wit = self
            .inner
            .get_wit(&contract_hash)
            .await
            .map_err(|e| anyhow::anyhow!("{}", e))?;

        let entrypoint = {
            let entrypoints = self.entrypoints.read().await;
            entrypoints
                .get(&contract_hash)
                .cloned()
                .unwrap_or_else(|| "root".to_string())
        };

        Ok(ComponentInterface { wit, entrypoint })
    }

    /// Get the raw WIT without hash rewriting.
    ///
    /// Useful for debugging or when you need the original format.
    pub async fn get_wit_raw(&self, contract_hash: &str) -> anyhow::Result<String> {
        self.inner
            .get_wit_raw(contract_hash)
            .await
            .map_err(|e| anyhow::anyhow!("{}", e))
    }

    /// Get the package hashes for a component.
    pub async fn get_packages(&self, contract_hash: &str) -> anyhow::Result<Vec<WitPackageHash>> {
        self.inner
            .get_packages(contract_hash)
            .await
            .map_err(|e| anyhow::anyhow!("{}", e))
    }

    /// Check if a contract is registered.
    pub async fn contains(&self, contract_hash: &str) -> bool {
        self.inner.contains(contract_hash).await
    }

    /// Get a reference to the underlying ComponentRegistry.
    pub fn component_registry(&self) -> &ComponentRegistry<InMemoryStorage> {
        &self.inner
    }
}
