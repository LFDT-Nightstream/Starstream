//! Storage backend abstraction for content-addressable storage.
//!
//! This module provides a `Storage` trait that abstracts over different storage
//! backends (in-memory, filesystem, K-V store, browser storage, etc.). This
//! allows the registry to run in browsers where filesystem access isn't available.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// A key for content-addressable storage.
///
/// Keys are arbitrary byte sequences, typically hex-encoded hashes.
pub type StorageKey = String;

/// Errors that can occur during storage operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StorageError {
    /// The requested key was not found.
    NotFound(StorageKey),
    /// An I/O or backend-specific error occurred.
    Backend(String),
}

impl std::fmt::Display for StorageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StorageError::NotFound(key) => write!(f, "key not found: {}", key),
            StorageError::Backend(msg) => write!(f, "storage error: {}", msg),
        }
    }
}

impl std::error::Error for StorageError {}

/// Trait for storage backends.
///
/// Implementations can store data in memory, on the filesystem, in IndexedDB
/// (for browsers), or any other storage mechanism.
///
/// All operations are async to support both sync (in-memory) and async (I/O)
/// backends uniformly.
///
/// Design notes:
/// - No `delete`: Blockchain data is immutable—once stored, it cannot be removed.
/// - No `list_keys`: Would be a DOS vector with millions of entries. Use explicit
///   tracking (e.g., `Vec<WitPackageHash>` per contract) instead of enumeration.
pub trait Storage: Send + Sync {
    /// Store a value at the given key.
    ///
    /// If the key already exists, this is a no-op (content-addressable semantics).
    fn put(
        &self,
        key: StorageKey,
        value: Vec<u8>,
    ) -> impl std::future::Future<Output = Result<(), StorageError>> + Send;

    /// Retrieve a value by key.
    ///
    /// Returns `Err(StorageError::NotFound)` if the key doesn't exist.
    fn get(
        &self,
        key: &str,
    ) -> impl std::future::Future<Output = Result<Vec<u8>, StorageError>> + Send;

    /// Check if a key exists.
    fn exists(&self, key: &str) -> impl std::future::Future<Output = bool> + Send;
}

/// In-memory storage backend.
///
/// This is the simplest backend, suitable for:
/// - Browser environments (no filesystem)
/// - Testing
/// - Ephemeral/development usage
///
/// Data is lost when the process exits.
#[derive(Debug, Clone, Default)]
pub struct InMemoryStorage {
    data: Arc<RwLock<HashMap<StorageKey, Vec<u8>>>>,
}

impl InMemoryStorage {
    /// Create a new empty in-memory storage.
    pub fn new() -> Self {
        Self {
            data: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

impl Storage for InMemoryStorage {
    async fn put(&self, key: StorageKey, value: Vec<u8>) -> Result<(), StorageError> {
        let mut data = self.data.write().await;
        // Content-addressable: only insert if not present
        data.entry(key).or_insert(value);
        Ok(())
    }

    async fn get(&self, key: &str) -> Result<Vec<u8>, StorageError> {
        let data = self.data.read().await;
        data.get(key)
            .cloned()
            .ok_or_else(|| StorageError::NotFound(key.to_string()))
    }

    async fn exists(&self, key: &str) -> bool {
        let data = self.data.read().await;
        data.contains_key(key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_in_memory_put_get() {
        let storage = InMemoryStorage::new();

        storage
            .put("key1".to_string(), b"value1".to_vec())
            .await
            .unwrap();
        let value = storage.get("key1").await.unwrap();

        assert_eq!(value, b"value1");
    }

    #[tokio::test]
    async fn test_in_memory_not_found() {
        let storage = InMemoryStorage::new();

        let result = storage.get("nonexistent").await;
        assert!(matches!(result, Err(StorageError::NotFound(_))));
    }

    #[tokio::test]
    async fn test_in_memory_exists() {
        let storage = InMemoryStorage::new();

        assert!(!storage.exists("key1").await);

        storage
            .put("key1".to_string(), b"value1".to_vec())
            .await
            .unwrap();

        assert!(storage.exists("key1").await);
    }

    #[tokio::test]
    async fn test_in_memory_content_addressable_semantics() {
        let storage = InMemoryStorage::new();

        // First put succeeds
        storage
            .put("key1".to_string(), b"original".to_vec())
            .await
            .unwrap();

        // Second put with same key is a no-op (doesn't overwrite)
        storage
            .put("key1".to_string(), b"new value".to_vec())
            .await
            .unwrap();

        // Original value is preserved
        let value = storage.get("key1").await.unwrap();
        assert_eq!(value, b"original");
    }
}
