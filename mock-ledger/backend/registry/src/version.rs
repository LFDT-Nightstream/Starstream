//! Version rewriting for transitive content addressing.
//!
//! This module provides utilities to embed content hashes into semver build metadata.
//! This enables **transitive content addressing**: when Package A depends on Package B,
//! the reference includes B's hash in the version string, making the dependency
//! unambiguous without breaking semver compatibility.
//!
//! # Format
//!
//! Hashes are appended to the build metadata portion of the version:
//! - `0.2.0` → `0.2.0+a1b2c3d4...` (64 hex chars)
//! - `0.2.0+nightly` → `0.2.0+nightly.a1b2c3d4...` (preserves existing metadata)
//!
//! The full 64-character SHA-256 hash is required for cryptographic security.
//! Truncation would enable birthday attacks (~2^32 operations for 64-bit truncation).

use crate::WitPackageHash;
use semver::{BuildMetadata, Version};

/// Errors that can occur during version rewriting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VersionError {
    /// The build metadata string is invalid (should not happen with our format).
    InvalidBuildMetadata(String),
}

impl std::fmt::Display for VersionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionError::InvalidBuildMetadata(msg) => {
                write!(f, "invalid build metadata: {}", msg)
            }
        }
    }
}

impl std::error::Error for VersionError {}

/// Append a content hash to a version's build metadata.
///
/// This is the core operation for transitive content addressing. The hash is
/// appended to any existing build metadata (separated by `.`), or becomes the
/// entire build metadata if none exists.
///
/// # Arguments
/// * `version` - The original version
/// * `hash` - The content hash to embed
///
/// # Returns
/// A new version with the hash in build metadata
///
/// # Examples
///
/// ```
/// use starstream_registry::{version_with_hash, WitPackageHash};
/// use semver::Version;
///
/// let v = Version::parse("0.2.0").unwrap();
/// let hash = WitPackageHash::from_bytes(b"example");
/// let new_v = version_with_hash(&v, &hash).unwrap();
///
/// // Hash is appended as build metadata
/// assert!(new_v.to_string().starts_with("0.2.0+"));
/// assert_eq!(new_v.build.len(), 64); // Full SHA-256 hex
/// ```
///
/// ```
/// use starstream_registry::{version_with_hash, WitPackageHash};
/// use semver::Version;
///
/// let v = Version::parse("1.0.0+nightly").unwrap();
/// let hash = WitPackageHash::from_bytes(b"example");
/// let new_v = version_with_hash(&v, &hash).unwrap();
///
/// // Existing metadata is preserved, hash appended with `.`
/// assert!(new_v.to_string().starts_with("1.0.0+nightly."));
/// ```
pub fn version_with_hash(version: &Version, hash: &WitPackageHash) -> Result<Version, VersionError> {
    let hash_str = hash.to_build_metadata();

    let new_build = if version.build.is_empty() {
        hash_str
    } else {
        format!("{}.{}", version.build, hash_str)
    };

    let build_metadata = BuildMetadata::new(&new_build)
        .map_err(|e| VersionError::InvalidBuildMetadata(e.to_string()))?;

    Ok(Version {
        major: version.major,
        minor: version.minor,
        patch: version.patch,
        pre: version.pre.clone(),
        build: build_metadata,
    })
}

/// Extract the content hash from a version's build metadata.
///
/// This is the inverse of `version_with_hash`. It extracts the 64-character
/// hex hash from the end of the build metadata.
///
/// # Arguments
/// * `version` - A version that may contain a hash in build metadata
///
/// # Returns
/// * `Some(WitPackageHash)` if a valid 64-char hex hash is found at the end
/// * `None` if no valid hash is present
///
/// # Examples
///
/// ```
/// use starstream_registry::{extract_hash_from_version, version_with_hash, WitPackageHash};
/// use semver::Version;
///
/// let hash = WitPackageHash::from_bytes(b"example");
/// let v = Version::parse("0.2.0").unwrap();
/// let v_with_hash = version_with_hash(&v, &hash).unwrap();
///
/// let extracted = extract_hash_from_version(&v_with_hash);
/// assert_eq!(extracted, Some(hash));
/// ```
pub fn extract_hash_from_version(version: &Version) -> Option<WitPackageHash> {
    let build = version.build.as_str();
    if build.is_empty() {
        return None;
    }

    // Hash is the last 64 characters (or the entire build if exactly 64 chars)
    let hash_str = if build.len() == 64 {
        build
    } else if build.len() > 65 && build.as_bytes()[build.len() - 65] == b'.' {
        // Format: "existing.HASH" - extract the hash after the last `.`
        &build[build.len() - 64..]
    } else {
        return None;
    };

    WitPackageHash::from_hex(hash_str).ok()
}

/// Strip the content hash from a version's build metadata.
///
/// Returns the version without the hash portion. This is useful for displaying
/// "clean" versions to users or for compatibility with tools that don't understand
/// our hash format.
///
/// # Arguments
/// * `version` - A version that may contain a hash in build metadata
///
/// # Returns
/// A version with the hash removed from build metadata
pub fn strip_hash_from_version(version: &Version) -> Version {
    let build = version.build.as_str();

    let new_build = if build.len() == 64 && WitPackageHash::from_hex(build).is_ok() {
        // Entire build is the hash
        String::new()
    } else if build.len() > 65 && build.as_bytes()[build.len() - 65] == b'.' {
        // Format: "existing.HASH"
        let potential_hash = &build[build.len() - 64..];
        if WitPackageHash::from_hex(potential_hash).is_ok() {
            build[..build.len() - 65].to_string()
        } else {
            build.to_string()
        }
    } else {
        build.to_string()
    };

    Version {
        major: version.major,
        minor: version.minor,
        patch: version.patch,
        pre: version.pre.clone(),
        build: if new_build.is_empty() {
            BuildMetadata::EMPTY
        } else {
            BuildMetadata::new(&new_build).unwrap_or(BuildMetadata::EMPTY)
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_hash() -> WitPackageHash {
        WitPackageHash::from_bytes(b"test content for hashing")
    }

    #[test]
    fn test_version_with_hash_no_existing_metadata() {
        let v = Version::parse("0.2.0").unwrap();
        let hash = test_hash();

        let result = version_with_hash(&v, &hash).unwrap();

        assert_eq!(result.major, 0);
        assert_eq!(result.minor, 2);
        assert_eq!(result.patch, 0);
        assert!(result.pre.is_empty());
        assert_eq!(result.build.len(), 64);
        assert_eq!(result.build.as_str(), hash.to_hex());
    }

    #[test]
    fn test_version_with_hash_existing_metadata() {
        let v = Version::parse("1.0.0+nightly").unwrap();
        let hash = test_hash();

        let result = version_with_hash(&v, &hash).unwrap();

        assert_eq!(result.major, 1);
        assert_eq!(result.minor, 0);
        assert_eq!(result.patch, 0);
        let expected_build = format!("nightly.{}", hash.to_hex());
        assert_eq!(result.build.as_str(), expected_build);
    }

    #[test]
    fn test_version_with_hash_preserves_prerelease() {
        let v = Version::parse("2.0.0-alpha.1").unwrap();
        let hash = test_hash();

        let result = version_with_hash(&v, &hash).unwrap();

        assert_eq!(result.pre.as_str(), "alpha.1");
        assert_eq!(result.build.as_str(), hash.to_hex());
    }

    #[test]
    fn test_extract_hash_no_metadata() {
        let v = Version::parse("0.2.0").unwrap();
        assert_eq!(extract_hash_from_version(&v), None);
    }

    #[test]
    fn test_extract_hash_only_hash() {
        let hash = test_hash();
        let v = Version::parse("0.2.0").unwrap();
        let v_with_hash = version_with_hash(&v, &hash).unwrap();

        let extracted = extract_hash_from_version(&v_with_hash);
        assert_eq!(extracted, Some(hash));
    }

    #[test]
    fn test_extract_hash_with_prefix() {
        let hash = test_hash();
        let v = Version::parse("1.0.0+nightly").unwrap();
        let v_with_hash = version_with_hash(&v, &hash).unwrap();

        let extracted = extract_hash_from_version(&v_with_hash);
        assert_eq!(extracted, Some(hash));
    }

    #[test]
    fn test_extract_hash_invalid_metadata() {
        // Build metadata that's not a valid hash
        let v = Version::parse("1.0.0+foobar").unwrap();
        assert_eq!(extract_hash_from_version(&v), None);
    }

    #[test]
    fn test_strip_hash_no_hash() {
        let v = Version::parse("0.2.0").unwrap();
        let stripped = strip_hash_from_version(&v);
        assert_eq!(stripped.to_string(), "0.2.0");
    }

    #[test]
    fn test_strip_hash_only_hash() {
        let hash = test_hash();
        let v = Version::parse("0.2.0").unwrap();
        let v_with_hash = version_with_hash(&v, &hash).unwrap();

        let stripped = strip_hash_from_version(&v_with_hash);
        assert_eq!(stripped.to_string(), "0.2.0");
    }

    #[test]
    fn test_strip_hash_with_prefix() {
        let hash = test_hash();
        let v = Version::parse("1.0.0+nightly").unwrap();
        let v_with_hash = version_with_hash(&v, &hash).unwrap();

        let stripped = strip_hash_from_version(&v_with_hash);
        assert_eq!(stripped.to_string(), "1.0.0+nightly");
    }

    #[test]
    fn test_roundtrip() {
        let hash = test_hash();
        let original = Version::parse("2.5.3+custom.build").unwrap();

        let with_hash = version_with_hash(&original, &hash).unwrap();
        let extracted = extract_hash_from_version(&with_hash).unwrap();
        let stripped = strip_hash_from_version(&with_hash);

        assert_eq!(extracted, hash);
        assert_eq!(stripped.to_string(), "2.5.3+custom.build");
    }
}
