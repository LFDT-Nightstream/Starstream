//! Utilities for loading and caching contract code WASM files.

use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use sha2::{Sha256, digest::DynDigest};
use wasmi::{Engine, Module};

use crate::util::DisplayHex;

/// A raw ID describing a contract in a content-addressible way.
#[derive(Clone, Copy, PartialEq, Eq)]
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

    pub fn raw(&self) -> [u8; 32] {
        self.0
    }
}

impl std::fmt::Debug for CodeHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CodeHash({})", DisplayHex(&self.0[..]))
    }
}

/// A loaded but not instantiated WASM blob.
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

    pub fn module(&self, engine: &Engine) -> Module {
        Module::new(engine, &self.wasm[..]).unwrap()
    }

    pub fn hash(&self) -> CodeHash {
        self.hash
    }
}

impl std::fmt::Debug for ContractCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContractCode")
            .field("hash", &self.hash)
            .finish()
    }
}

/// A cache of WASM blobs.
#[derive(Default)]
pub struct CodeCache {
    contract_code: RwLock<HashMap<String, Arc<ContractCode>>>,
}

impl CodeCache {
    /// Load a debug .
    pub fn load_debug(&self, name: &str) -> Arc<ContractCode> {
        if let Some(code) = self.contract_code.read().unwrap().get(name) {
            code.clone()
        } else {
            let path = format!("target/wasm32-unknown-unknown/debug/{name}.wasm");
            let result = Arc::new(ContractCode::load(std::fs::read(path).unwrap()));
            self.contract_code
                .write()
                .unwrap()
                .insert(name.to_owned(), result.clone());
            result
        }
    }
}
