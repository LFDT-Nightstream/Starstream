//! Content-addressable hash for WIT packages.
//!
//! This module provides a SHA-256 hash (`WitPackageHash`)
//! used to uniquely identify WIT packages by their content.
//! This enables deduplication across contracts that may share common dependencies.

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fmt;

/// A content-addressable hash for a WIT package (SHA-256).
///
/// This hash is computed from the deterministic binary encoding of a WIT package
/// using `wit-component::encode()`. The same package content always produces the
/// same hash, enabling:
/// - **Deduplication**: Store identical packages once, reference many times
/// - **Integrity**: Verify packages haven't been modified
/// - **Identity**: Uniquely identify packages without trusting names
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WitPackageHash(#[serde(with = "hex_serde")] [u8; 32]);

impl WitPackageHash {
    /// Compute a hash from raw bytes (typically the output of `wit-component::encode()`).
    pub fn from_bytes(bytes: &[u8]) -> Self {
        let hash = Sha256::digest(bytes);
        Self(hash.into())
    }

    /// Create a hash from a known 32-byte array.
    pub fn from_raw(raw: [u8; 32]) -> Self {
        Self(raw)
    }

    /// Get the raw 32-byte hash.
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }

    /// Convert to a hex string (64 characters).
    pub fn to_hex(&self) -> String {
        hex::encode(self.0)
    }

    /// Parse from a hex string.
    pub fn from_hex(s: &str) -> Result<Self, hex::FromHexError> {
        let bytes = hex::decode(s)?;
        let array: [u8; 32] = bytes
            .try_into()
            .map_err(|_| hex::FromHexError::InvalidStringLength)?;
        Ok(Self(array))
    }

    /// Convert to semver build metadata format.
    ///
    /// Returns the full 64-character hex string. The full hash is required for
    /// cryptographic security—truncation would enable birthday attacks.
    ///
    /// # Example
    /// ```
    /// use starstream_registry::WitPackageHash;
    ///
    /// let hash = WitPackageHash::from_bytes(b"example content");
    /// let metadata = hash.to_build_metadata();
    /// assert_eq!(metadata.len(), 64);
    /// ```
    pub fn to_build_metadata(&self) -> String {
        self.to_hex()
    }
}

impl fmt::Debug for WitPackageHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WitPackageHash({})", self.to_hex())
    }
}

impl fmt::Display for WitPackageHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

/// Custom serde implementation for hex encoding/decoding.
mod hex_serde {
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(bytes: &[u8; 32], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&hex::encode(bytes))
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<[u8; 32], D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let bytes = hex::decode(&s).map_err(serde::de::Error::custom)?;
        bytes
            .try_into()
            .map_err(|_| serde::de::Error::custom("expected 32 bytes"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_bytes_deterministic() {
        let content = b"test content for hashing";
        let hash1 = WitPackageHash::from_bytes(content);
        let hash2 = WitPackageHash::from_bytes(content);
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_different_content_different_hash() {
        let hash1 = WitPackageHash::from_bytes(b"content A");
        let hash2 = WitPackageHash::from_bytes(b"content B");
        assert_ne!(hash1, hash2);
    }

    #[test]
    fn test_hex_roundtrip() {
        let hash = WitPackageHash::from_bytes(b"test content");
        let hex_str = hash.to_hex();
        let recovered = WitPackageHash::from_hex(&hex_str).unwrap();
        assert_eq!(hash, recovered);
    }

    #[test]
    fn test_hex_length() {
        let hash = WitPackageHash::from_bytes(b"test");
        assert_eq!(hash.to_hex().len(), 64);
        assert_eq!(hash.to_build_metadata().len(), 64);
    }

    #[test]
    fn test_serde_json_roundtrip() {
        let hash = WitPackageHash::from_bytes(b"test content");
        let json = serde_json::to_string(&hash).unwrap();
        let recovered: WitPackageHash = serde_json::from_str(&json).unwrap();
        assert_eq!(hash, recovered);
    }

    #[test]
    fn test_invalid_hex() {
        assert!(WitPackageHash::from_hex("not valid hex").is_err());
        assert!(WitPackageHash::from_hex("abcd").is_err()); // Too short
    }
}
