//! Shared helpers for the scan-based subcommands (`check`, `docs`, `build`):
//! deciding which directory to scan from.

use std::path::{Path, PathBuf};

/// Resolve where compiled artifacts should land, and where the no-arg
/// commands should scan from.
///
/// Walks up from `start` until it finds a `.git` directory; if found, uses
/// that directory. Otherwise falls back to `start` itself.
pub fn project_root(start: &Path) -> PathBuf {
    let mut current = start;
    loop {
        if current.join(".git").exists() {
            return current.to_path_buf();
        }
        match current.parent() {
            Some(parent) => current = parent,
            None => return start.to_path_buf(),
        }
    }
}

/// Default scan dir when the user invokes `check`/`docs`/`build` with no
/// argument: walk up from cwd looking for `.git`; if none is found, use cwd.
pub fn default_scan_dir() -> std::io::Result<PathBuf> {
    let cwd = std::env::current_dir()?;
    Ok(project_root(&cwd))
}
