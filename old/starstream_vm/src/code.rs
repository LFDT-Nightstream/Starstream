//! Utilities for loading and caching contract code WASM files.

use std::{
    collections::HashMap,
    path::Path,
    sync::{Arc, RwLock},
};

use sha2::{Sha256, digest::DynDigest};
use wasmi::{Engine, Module};

use crate::util::DisplayHex;

/// A raw ID describing a contract in a content-addressible way.
#[derive(Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct CodeHash([u8; 32]);

impl CodeHash {
    fn from_content(code: &[u8]) -> CodeHash {
        // Currently this is just sha256 of the whole WASM file. There might
        // be stuff in the WASM file that we don't want to count or that isn't
        // reproducible and should exclude here, but that seems tricky.
        let mut hash = [0; 32];
        let mut hasher = Sha256::default();
        hasher.update(code);
        hasher.finalize_into(&mut hash[..]).unwrap();
        CodeHash(hash)
    }

    pub(crate) fn raw(&self) -> [u8; 32] {
        self.0
    }
}

impl std::fmt::Debug for CodeHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CodeHash({})", DisplayHex(&self.0[..]))
    }
}

/// A loaded but not instantiated Wasm blob.
pub struct ContractCode {
    wasm: Vec<u8>,
    hash: CodeHash,
}

impl ContractCode {
    fn load(wasm: Vec<u8>) -> ContractCode {
        ContractCode {
            hash: CodeHash::from_content(&wasm),
            wasm,
        }
    }

    pub(crate) fn module(&self, engine: &Engine) -> Module {
        Module::new(engine, &self.wasm[..]).unwrap()
    }

    pub fn hash(&self) -> CodeHash {
        self.hash
    }

    pub fn wasm(&self) -> &[u8] {
        &self.wasm
    }
}

impl std::fmt::Debug for ContractCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContractCode")
            .field("hash", &self.hash)
            .finish()
    }
}

/// A content-addressed cache of Wasm blobs.
#[derive(Default)]
pub struct CodeCache {
    by_hash: RwLock<HashMap<CodeHash, Arc<ContractCode>>>,
}

impl CodeCache {
    pub fn load(&self, wasm: Vec<u8>) -> Arc<ContractCode> {
        let result = Arc::new(ContractCode::load(wasm));
        self.by_hash
            .write()
            .unwrap()
            .insert(result.hash(), result.clone());
        result
    }

    pub fn load_file(&self, path: &Path) -> Arc<ContractCode> {
        self.load(std::fs::read(path).expect("CodeCache::load_file"))
    }

    /// Load code by crate name from the Rust `target/` directory.
    pub fn load_debug(&self, name: &str) -> Arc<ContractCode> {
        let mut current = std::env::current_dir().unwrap();
        while !current.join("target").exists() && current.pop() {}
        if let Some(wat_name) = name.strip_prefix("wat:") {
            let wat = std::fs::read_to_string(
                &current.join(Path::new(&format!("starstream_vm/tests/{wat_name}.wat"))),
            )
            .expect("no such WAT file");
            let wasm = wat::parse_str(&wat).expect("invalid WAT");
            self.load(wasm)
        } else {
            self.load_file(&current.join(Path::new(&format!(
                "target/wasm32-unknown-unknown/debug/{name}.wasm"
            ))))
        }
    }

    pub fn get(&self, hash: CodeHash) -> Arc<ContractCode> {
        self.by_hash
            .read()
            .unwrap()
            .get(&hash)
            .expect("todo: load code by hash")
            .clone()
    }
}
