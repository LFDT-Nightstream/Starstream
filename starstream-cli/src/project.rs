//! Shared helpers for the scan-based subcommands (`check`, `docs`, `build`):
//! deciding which directory to scan from when the user didn't pass one.

use std::path::PathBuf;

/// Default scan dir when the user invokes `check`/`docs`/`build` with no
/// argument: just the current working directory.
pub fn default_scan_dir() -> std::io::Result<PathBuf> {
    std::env::current_dir()
}
