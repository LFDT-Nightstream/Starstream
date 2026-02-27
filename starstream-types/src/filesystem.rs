use std::path::{Path, PathBuf};

/// Filesystem accessor that tracks dependency information for build system integration.
#[derive(Debug)]
pub struct FileSystem {
    pub dependencies: Vec<PathBuf>,
    pub outputs: Vec<PathBuf>,
}

impl FileSystem {
    pub fn new() -> FileSystem {
        FileSystem {
            dependencies: Vec::new(),
            outputs: Vec::new(),
        }
    }

    pub fn read_to_string(&mut self, path: &Path) -> std::io::Result<String> {
        self.dependencies.push(path.to_owned());
        std::fs::read_to_string(path)
    }

    pub fn write(&mut self, path: &Path, contents: &[u8]) -> std::io::Result<()> {
        self.outputs.push(path.to_owned());
        std::fs::write(path, contents)
    }
}
