//! Compiles the example Starstream contracts under `examples/` and writes each
//! produced core `.wasm` into `$OUT_DIR`, where `src/lib.rs` `include_bytes!`s
//! them.
//!
//! This mirrors the `starstream wasm` CLI command's compile path, but in-process
//! and stopping at the core module: load the source as a single-contract module
//! graph, typecheck the whole graph, then emit the contract reachable from the
//! entry. The result is a *core* Wasm module carrying a `component-type` custom
//! section — the host's `componentize` step wraps it into a component at run
//! time, so it can be fed straight to `Contract::new`.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use starstream_compiler::{TypecheckOptions, module_graph, typecheck_modules};
use starstream_to_wasm::compile_contract;
use starstream_types::FileSystem;

/// Compile `examples/<name>.star` to a core module and write it to
/// `$OUT_DIR/<name>.wasm`.
fn build_contract(examples: &Path, out_dir: &Path, name: &str) {
    let src = examples.join(format!("{name}.star"));

    let mut fs = FileSystem::new();
    let graph = module_graph::load_from_entry(&src, &mut fs)
        .unwrap_or_else(|err| panic!("failed to load `{name}.star`: {err:?}"));

    // `load_from_entry` always treats the entry file as a contract, regardless
    // of whether it declares `contract;`, and records it as the single entry.
    let entry = graph
        .contract_entries()
        .first()
        .copied()
        .expect("load_from_entry always sets a single contract entry");

    let typed = typecheck_modules(&graph, TypecheckOptions::default())
        .unwrap_or_else(|err| panic!("typechecking `{name}.star` failed: {err:?}"));

    let result = compile_contract(&typed, entry);
    assert!(
        result.errors.is_empty(),
        "compiling `{name}.star` failed: {:?}",
        result.errors
    );
    let wasm = result
        .wasm
        .unwrap_or_else(|| panic!("compiling `{name}.star` produced no Wasm"));

    fs::write(out_dir.join(format!("{name}.wasm")), &wasm)
        .unwrap_or_else(|err| panic!("failed to write `{name}.wasm`: {err}"));

    println!("cargo:rerun-if-changed={}", src.display());
}

fn main() {
    let manifest_dir =
        PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR"));
    let examples = manifest_dir
        .join("../examples")
        .canonicalize()
        .expect("examples directory exists");
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR set by cargo"));

    build_contract(&examples, &out_dir, "score");
}
