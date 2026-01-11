//! WIT flattening utilities using nested package syntax.
//!
//! This module provides functions to convert WIT packages to the nested package
//! format (using curly braces), which is required when multiple packages are
//! combined in a single WIT string.
//!
//! # Background
//!
//! WIT has two syntax forms for package declarations:
//!
//! 1. **Semicolon syntax** (single-package files only):
//!    ```wit
//!    package foo:bar;
//!    interface baz { ... }
//!    ```
//!
//! 2. **Nested syntax** (works for any number of packages):
//!    ```wit
//!    package foo:bar {
//!      interface baz { ... }
//!    }
//!    ```
//!
//! When we store multiple packages and later concatenate them, we must use the
//! nested syntax. For consistency, we always store packages in nested format.

use thiserror::Error;
use wit_component::WitPrinter;
use wit_parser::{PackageId, Resolve};

/// Errors that can occur during WIT flattening.
#[derive(Debug, Error)]
pub enum FlattenError {
    #[error("package not found in resolve: {0:?}")]
    PackageNotFound(PackageId),

    #[error("failed to print WIT: {0}")]
    PrintError(String),
}

/// Convert a single package from a Resolve to WIT text format.
///
/// This extracts the specified package and converts it to the semicolon syntax
/// which is the standard self-contained format for individual packages.
///
/// Uses `WitPrinter::print_package` with `is_main = true` for semicolon format.
/// When multiple packages need to be combined for output, use `WitPrinter::print()`
/// which handles the main/nested formatting automatically.
///
/// # Arguments
/// * `resolve` - The Resolve containing the package
/// * `pkg_id` - The ID of the package to convert
///
/// # Returns
/// The package in WIT text format as a string
pub fn package_to_wit(resolve: &Resolve, pkg_id: PackageId) -> Result<String, FlattenError> {
    // Verify package exists
    if resolve.packages.get(pkg_id).is_none() {
        return Err(FlattenError::PackageNotFound(pkg_id));
    }

    // Use WitPrinter with is_main=true to get semicolon format (self-contained)
    let mut printer = WitPrinter::default();
    printer
        .print_package(resolve, pkg_id, true)
        .map_err(|e| FlattenError::PrintError(e.to_string()))?;

    Ok(printer.output.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_wit(wit: &str) -> Resolve {
        let mut resolve = Resolve::new();
        resolve.push_str("test.wit", wit).unwrap();
        resolve
    }

    #[test]
    fn test_package_to_wit_simple() {
        let wit = r#"
package foo:bar;

interface baz {
  test: func();
}
"#;
        let resolve = parse_wit(wit);
        let pkg_id = resolve.packages.iter().next().unwrap().0;

        let output = package_to_wit(&resolve, pkg_id).unwrap();

        // Semicolon format
        assert!(output.contains("package foo:bar;"), "output: {}", output);
        assert!(output.contains("interface baz {"));
    }

    #[test]
    fn test_package_to_wit_with_version() {
        let wit = r#"
package foo:bar@1.0.0;

world root {
  export run: func();
}
"#;
        let resolve = parse_wit(wit);
        let pkg_id = resolve.packages.iter().next().unwrap().0;

        let output = package_to_wit(&resolve, pkg_id).unwrap();

        // Semicolon format
        assert!(output.contains("package foo:bar@1.0.0;"), "output: {}", output);
        assert!(output.contains("world root {"));
    }

    #[test]
    fn test_package_to_wit_with_inline_types() {
        // Worlds with inline type definitions
        let wit = r#"
package root:component;

world root {
  record token {
    amount: s64,
    price: s64,
  }

  export total-value: func(token: token) -> s64;
}
"#;
        let resolve = parse_wit(wit);
        let pkg_id = resolve.packages.iter().next().unwrap().0;

        let output = package_to_wit(&resolve, pkg_id).unwrap();

        // Semicolon format
        assert!(output.contains("package root:component;"), "output: {}", output);
        assert!(output.contains("world root {"));
        assert!(output.contains("record token {"));
    }

    #[test]
    fn test_package_to_wit_roundtrip() {
        // Test that output can be parsed back
        let wit = r#"
package foo:bar@1.0.0;

interface baz {
  test: func() -> string;
}
"#;
        let resolve = parse_wit(wit);
        let pkg_id = resolve.packages.iter().next().unwrap().0;

        let output = package_to_wit(&resolve, pkg_id).unwrap();

        // Should be parseable
        let mut resolve2 = Resolve::new();
        let result = resolve2.push_str("roundtrip.wit", &output);
        assert!(result.is_ok(), "Failed to parse output: {:?}", result);
    }
}
