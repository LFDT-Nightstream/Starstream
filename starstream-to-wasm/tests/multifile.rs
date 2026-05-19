//! End-to-end multi-file compilation tests.
//!
//! Each scenario lives in its own subdirectory under `tests/multifile/`
//! as real `.star` files on disk, so they can be opened in an editor and
//! debugged the same way as a real project.

use std::path::{Path, PathBuf};

fn fixture(scenario: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("multifile")
        .join(scenario)
}

fn compile_contract(entry: &Path) -> Vec<u8> {
    let mut tracker = starstream_types::FileSystem::new();
    let graph =
        starstream_compiler::module_graph::load_from_entry(entry, &mut tracker).expect("graph");
    let entry_id = graph.contract_entries()[0];
    let typed =
        starstream_compiler::typecheck_modules(&graph, Default::default()).expect("typecheck");
    let result = starstream_to_wasm::compile_contract(&typed, entry_id);
    assert!(
        result.errors.is_empty(),
        "compile errors: {:?}",
        result.errors
    );
    result.wasm.expect("wasm produced")
}

fn print_wit(wasm: &[u8]) -> String {
    let component_bytes = wit_component::ComponentEncoder::default()
        .validate(true)
        .module(wasm)
        .expect("ComponentEncoder::module")
        .encode()
        .expect("ComponentEncoder::encode");
    let decoded = wit_component::decode(&component_bytes).expect("decode");
    let mut printer = wit_component::WitPrinter::default();
    let ids: Vec<_> = decoded
        .resolve()
        .packages
        .iter()
        .map(|(id, _)| id)
        .filter(|id| *id != decoded.package())
        .collect();
    printer
        .print(decoded.resolve(), decoded.package(), &ids)
        .unwrap();
    printer.output.to_string()
}

#[test]
fn entry_imports_helper_and_compiles() {
    let entry = fixture("entry_imports_helper").join("main.star");
    let wasm = compile_contract(&entry);
    let wit = print_wit(&wasm);
    assert!(wit.contains("run"), "expected `run` export, got:\n{wit}");
    assert!(
        !wit.contains("export add"),
        "did not expect helper `add` exported, got:\n{wit}"
    );
}

#[test]
fn helper_script_is_exported() {
    // If you write `script fn` in a helper, you meant to expose it.
    // Both the entry's `run` and the helper's `helper_script` should appear
    // in the contract's WIT.
    let entry = fixture("helper_script_exported").join("main.star");
    let wasm = compile_contract(&entry);
    let wit = print_wit(&wasm);
    assert!(wit.contains("run"));
    assert!(
        wit.contains("helper-script") || wit.contains("helper_script"),
        "helper's `script fn` should be exported, WIT:\n{wit}"
    );
}

#[test]
fn imports_are_importer_relative() {
    // Importer lives in `sub/`; its `import "../helpers/math.star"` resolves
    // relative to the importer's directory.
    let entry = fixture("importer_relative").join("sub").join("nested.star");
    let wasm = compile_contract(&entry);
    let wit = print_wit(&wasm);
    assert!(wit.contains("run"));
}

#[test]
fn cross_contract_import_errors() {
    // Helper file also declares `contract;` — must be rejected at graph build.
    let entry = fixture("cross_contract").join("main.star");
    let mut tracker = starstream_types::FileSystem::new();
    match starstream_compiler::module_graph::load_from_entry(&entry, &mut tracker) {
        Err(starstream_compiler::ModuleGraphError::CrossContractImport { .. }) => {}
        other => panic!("expected CrossContractImport, got {:?}", other.map(|_| ())),
    }
}
