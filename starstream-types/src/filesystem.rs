use std::collections::BTreeMap;
use std::fmt::Debug;
use std::io;
use std::ops::Bound;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

/// Kind of a directory entry returned by [`Vfs::read_dir`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EntryKind {
    File,
    Dir,
    Other,
}

/// One entry returned by [`Vfs::read_dir`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DirEntry {
    pub path: PathBuf,
    pub kind: EntryKind,
}

/// Backend for filesystem access.
///
/// Deliberately synchronous: in the browser the compiler runs as Wasm and
/// can't block on JS promises, so web hosts populate a [`MemoryFs`] up front.
pub trait Vfs: Debug + Send + Sync {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>>;
    fn write(&mut self, path: &Path, contents: &[u8]) -> io::Result<()>;
    /// Resolve `path` to the absolute form used as a module's identity.
    /// Fails if nothing exists at `path`.
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
    /// List the immediate children of `path`, in no particular order.
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>>;
}

/// The host's real filesystem via `std::fs`.
#[derive(Debug, Default, Clone, Copy)]
pub struct NativeFs;

impl Vfs for NativeFs {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        std::fs::read(path)
    }

    fn write(&mut self, path: &Path, contents: &[u8]) -> io::Result<()> {
        std::fs::write(path, contents)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let mut out = Vec::new();
        // Skip entries that can't be read rather than failing the listing.
        for entry in std::fs::read_dir(path)?.flatten() {
            let path = entry.path();
            // Follow symlinks, like `Path::is_dir`/`is_file`.
            let kind = if path.is_dir() {
                EntryKind::Dir
            } else if path.is_file() {
                EntryKind::File
            } else {
                EntryKind::Other
            };
            out.push(DirEntry { path, kind });
        }
        Ok(out)
    }
}

/// In-memory filesystem. Directories exist implicitly whenever a file lives
/// under them. Paths are normalized lexically (no symlinks), with relative
/// paths resolved against `/`.
#[derive(Debug, Default, Clone)]
pub struct MemoryFs {
    files: BTreeMap<PathBuf, Arc<[u8]>>,
}

impl MemoryFs {
    #[must_use]
    pub fn new() -> MemoryFs {
        MemoryFs::default()
    }

    pub fn insert(&mut self, path: impl AsRef<Path>, contents: impl Into<Arc<[u8]>>) {
        self.files
            .insert(normalize_path(path.as_ref()), contents.into());
    }

    pub fn remove(&mut self, path: impl AsRef<Path>) -> Option<Arc<[u8]>> {
        self.files.remove(&normalize_path(path.as_ref()))
    }

    pub fn get(&self, path: impl AsRef<Path>) -> Option<&Arc<[u8]>> {
        self.files.get(&normalize_path(path.as_ref()))
    }

    pub fn clear(&mut self) {
        self.files.clear();
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.files
            .range::<Path, _>((Bound::Excluded(path), Bound::Unbounded))
            .next()
            .is_some_and(|(k, _)| k.starts_with(path))
    }

    /// Insert `dir`'s immediate children into `out` (path -> is_dir),
    /// keeping any entry already present.
    fn children(&self, dir: &Path, out: &mut BTreeMap<PathBuf, bool>) {
        // Paths order component-wise, so everything under `dir` is contiguous.
        for key in self
            .files
            .range::<Path, _>((Bound::Included(dir), Bound::Unbounded))
            .map(|(k, _)| k)
        {
            let Ok(rest) = key.strip_prefix(dir) else {
                break;
            };
            let mut components = rest.components();
            let Some(first) = components.next() else {
                continue;
            };
            let is_dir = components.next().is_some();
            out.entry(dir.join(first)).or_insert(is_dir);
        }
    }
}

impl Vfs for MemoryFs {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.get(path)
            .map(|c| c.to_vec())
            .ok_or_else(|| not_found(path))
    }

    fn write(&mut self, path: &Path, contents: &[u8]) -> io::Result<()> {
        self.insert(path, contents);
        Ok(())
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        let path = normalize_path(path);
        if self.files.contains_key(&path) || self.is_dir(&path) {
            Ok(path)
        } else {
            Err(not_found(&path))
        }
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let path = normalize_path(path);
        if !self.is_dir(&path) {
            return Err(not_found(&path));
        }
        let mut children = BTreeMap::new();
        self.children(&path, &mut children);
        Ok(to_entries(children))
    }
}

/// A [`MemoryFs`] layered over another [`Vfs`]. Reads and listings prefer the
/// upper layer, so e.g. a language server can shadow on-disk files with the
/// editor's unsaved buffers. Writes go to the upper layer.
#[derive(Debug, Clone)]
pub struct OverlayFs {
    upper: MemoryFs,
    base: Arc<dyn Vfs>,
}

impl OverlayFs {
    #[must_use]
    pub fn new(upper: MemoryFs, base: Arc<dyn Vfs>) -> OverlayFs {
        OverlayFs { upper, base }
    }
}

impl Vfs for OverlayFs {
    fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        // The upper layer only fails with "not found", so fall through.
        match self.upper.read(path) {
            Ok(contents) => Ok(contents),
            Err(_) => self.base.read(path),
        }
    }

    fn write(&mut self, path: &Path, contents: &[u8]) -> io::Result<()> {
        self.upper.write(path, contents)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        // Prefer the base so symlinks resolve the same way they would
        // without the overlay; fall back to the upper layer for files that
        // only exist in memory.
        match self.base.canonicalize(path) {
            Ok(path) => Ok(path),
            Err(error) => self.upper.canonicalize(path).map_err(|_| error),
        }
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let base = self.base.read_dir(path);
        let normalized = normalize_path(path);
        // A directory that only exists in memory is fine (the base may not
        // even support listing, as in the browser); otherwise report the
        // base's error.
        if base.is_err() && !self.upper.is_dir(&normalized) {
            return base;
        }
        let mut children: BTreeMap<PathBuf, bool> = base
            .unwrap_or_default()
            .into_iter()
            .filter(|e| e.kind != EntryKind::Other)
            .map(|e| (e.path, e.kind == EntryKind::Dir))
            .collect();
        self.upper.children(&normalized, &mut children);
        Ok(to_entries(children))
    }
}

fn to_entries(children: BTreeMap<PathBuf, bool>) -> Vec<DirEntry> {
    children
        .into_iter()
        .map(|(path, is_dir)| DirEntry {
            path,
            kind: if is_dir {
                EntryKind::Dir
            } else {
                EntryKind::File
            },
        })
        .collect()
}

fn not_found(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::NotFound,
        format!("no such file or directory: {}", path.display()),
    )
}

/// Lexically normalize `path`: make it absolute (relative to `/`), drop `.`
/// and resolve `..`. Does not touch the real filesystem.
#[must_use]
pub fn normalize_path(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    let mut rooted = false;
    for component in path.components() {
        match component {
            Component::Prefix(p) => {
                out.push(p.as_os_str());
            }
            Component::RootDir => {
                out.push(component.as_os_str());
                rooted = true;
            }
            Component::CurDir => {}
            Component::ParentDir => {
                out.pop();
            }
            Component::Normal(name) => out.push(name),
        }
    }
    if !rooted {
        out = Path::new("/").join(out);
    }
    out
}

/// Filesystem accessor that tracks dependency information for build system
/// integration. Delegates the actual I/O to a [`Vfs`] backend.
#[derive(Debug)]
pub struct FileSystem {
    vfs: Box<dyn Vfs>,
    pub dependencies: Vec<PathBuf>,
    pub outputs: Vec<PathBuf>,
}

impl Default for FileSystem {
    fn default() -> Self {
        Self::new()
    }
}

impl FileSystem {
    /// A tracker over the host's real filesystem.
    #[must_use]
    pub fn new() -> FileSystem {
        FileSystem::with_vfs(NativeFs)
    }

    /// A tracker over an arbitrary backend.
    #[must_use]
    pub fn with_vfs(vfs: impl Vfs + 'static) -> FileSystem {
        FileSystem {
            vfs: Box::new(vfs),
            dependencies: Vec::new(),
            outputs: Vec::new(),
        }
    }

    pub fn read(&mut self, path: &Path) -> io::Result<Vec<u8>> {
        self.dependencies.push(path.to_owned());
        self.vfs.read(path)
    }

    pub fn read_to_string(&mut self, path: &Path) -> io::Result<String> {
        String::from_utf8(self.read(path)?)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
    }

    pub fn write(&mut self, path: &Path, contents: &[u8]) -> io::Result<()> {
        self.outputs.push(path.to_owned());
        self.vfs.write(path, contents)
    }

    /// Not recorded in [`FileSystem::dependencies`]: only file contents are.
    pub fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.vfs.canonicalize(path)
    }

    /// Not recorded in [`FileSystem::dependencies`]: only file contents are.
    pub fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.vfs.read_dir(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn memory(files: &[(&str, &str)]) -> MemoryFs {
        let mut fs = MemoryFs::new();
        for (path, contents) in files {
            fs.insert(path, contents.as_bytes());
        }
        fs
    }

    #[test]
    fn normalize() {
        assert_eq!(normalize_path(Path::new("/a/./b/../c")), Path::new("/a/c"));
        assert_eq!(normalize_path(Path::new("a/b")), Path::new("/a/b"));
        assert_eq!(normalize_path(Path::new("/../a")), Path::new("/a"));
    }

    #[test]
    fn native_fs_read_dir() {
        let dir = std::env::temp_dir().join("starstream-native-fs-read-dir");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("sub")).unwrap();
        std::fs::write(dir.join("a.star"), "").unwrap();

        let mut entries = NativeFs.read_dir(&dir).unwrap();
        entries.sort_by(|a, b| a.path.cmp(&b.path));
        assert_eq!(
            entries,
            vec![
                DirEntry {
                    path: dir.join("a.star"),
                    kind: EntryKind::File
                },
                DirEntry {
                    path: dir.join("sub"),
                    kind: EntryKind::Dir
                },
            ]
        );
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn memory_fs() {
        let fs = memory(&[("/w/a.star", "a"), ("/w/lib/b.star", "b"), ("/wx", "x")]);
        assert_eq!(fs.read(Path::new("/w/lib/../a.star")).unwrap(), b"a");
        assert_eq!(
            fs.canonicalize(Path::new("/w/lib/./b.star")).unwrap(),
            Path::new("/w/lib/b.star")
        );
        assert_eq!(fs.canonicalize(Path::new("/w")).unwrap(), Path::new("/w"));
        assert!(fs.canonicalize(Path::new("/w/nope.star")).is_err());
        assert_eq!(fs.read(Path::new("/../w/a.star")).unwrap(), b"a");
        assert!(fs.read_dir(Path::new("/nope")).is_err());
        assert_eq!(
            fs.read_dir(Path::new("/w")).unwrap(),
            vec![
                DirEntry {
                    path: "/w/a.star".into(),
                    kind: EntryKind::File
                },
                DirEntry {
                    path: "/w/lib".into(),
                    kind: EntryKind::Dir
                },
            ]
        );
    }

    #[test]
    fn overlay_fs() {
        let base = memory(&[("/w/a.star", "disk a"), ("/w/b.star", "disk b")]);
        let upper = memory(&[("/w/a.star", "buffer a"), ("/w/c/d.star", "d")]);
        let fs = OverlayFs::new(upper, Arc::new(base));
        assert_eq!(fs.read(Path::new("/w/a.star")).unwrap(), b"buffer a");
        assert_eq!(fs.read(Path::new("/w/b.star")).unwrap(), b"disk b");
        // Only in the upper layer: canonicalize falls back to it.
        assert_eq!(
            fs.canonicalize(Path::new("/w/c/./d.star")).unwrap(),
            Path::new("/w/c/d.star")
        );
        assert!(fs.canonicalize(Path::new("/w/nope.star")).is_err());
        let names: Vec<_> = fs
            .read_dir(Path::new("/w"))
            .unwrap()
            .into_iter()
            .map(|e| (e.path, e.kind))
            .collect();
        assert_eq!(
            names,
            vec![
                ("/w/a.star".into(), EntryKind::File),
                ("/w/b.star".into(), EntryKind::File),
                ("/w/c".into(), EntryKind::Dir),
            ]
        );
    }
}
