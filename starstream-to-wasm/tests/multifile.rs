//! End-to-end multi-file compilation: scan a project, build each contract.

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

fn tmp_dir(name: &str) -> PathBuf {
    let mut path = std::env::temp_dir();
    path.push(format!(
        "starstream-multifile-test-{}-{}",
        name,
        std::process::id()
    ));
    let _ = fs::remove_dir_all(&path);
    fs::create_dir_all(&path).unwrap();
    path
}

fn write_file(dir: &Path, name: &str, contents: &str) -> PathBuf {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    let mut f = fs::File::create(&path).unwrap();
    f.write_all(contents.as_bytes()).unwrap();
    path
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
    let dir = tmp_dir("entry-helper");
    write_file(
        &dir,
        "helpers/math.star",
        "fn add(a: i64, b: i64) -> i64 { a + b }\n",
    );
    let entry = write_file(
        &dir,
        "main.star",
        "contract;\n\
         import { add } from \"./helpers/math.star\";\n\
         \n\
         script fn run() -> i64 { add(2, 40) }\n",
    );

    let wasm = compile_contract(&entry);
    let wit = print_wit(&wasm);
    assert!(wit.contains("run"), "expected `run` export, got:\n{wit}");
    assert!(
        !wit.contains("export add"),
        "did not expect helper `add` exported, got:\n{wit}"
    );
}

#[test]
fn helper_script_is_not_exported() {
    let dir = tmp_dir("helper-script");
    write_file(
        &dir,
        "helpers/extra.star",
        "script fn helper_script() -> i64 { 7 }\n",
    );
    let entry = write_file(
        &dir,
        "main.star",
        "contract;\n\
         import { helper_script } from \"./helpers/extra.star\";\n\
         \n\
         script fn run() -> i64 { helper_script() }\n",
    );

    let wasm = compile_contract(&entry);
    let wit = print_wit(&wasm);
    assert!(wit.contains("run"));
    assert!(
        !wit.contains("helper-script") && !wit.contains("helper_script"),
        "helper's script must not be exported, WIT:\n{wit}"
    );
}

#[test]
fn imports_are_importer_relative() {
    let dir = tmp_dir("relative");
    write_file(
        &dir,
        "helpers/math.star",
        "fn add(a: i64, b: i64) -> i64 { a + b }\n",
    );
    let entry = write_file(
        &dir,
        "sub/nested.star",
        "contract;\n\
         import { add } from \"../helpers/math.star\";\n\
         \n\
         script fn run() -> i64 { add(1, 2) }\n",
    );

    let wasm = compile_contract(&entry);
    let wit = print_wit(&wasm);
    assert!(wit.contains("run"));
}

#[test]
fn cross_contract_import_errors() {
    let dir = tmp_dir("cross-contract");
    // Helper file also declares `contract;` — must be rejected.
    write_file(&dir, "other.star", "contract;\nfn helper() -> i64 { 9 }\n");
    let entry = write_file(
        &dir,
        "main.star",
        "contract;\n\
         import { helper } from \"./other.star\";\n\
         script fn run() -> i64 { helper() }\n",
    );

    let mut tracker = starstream_types::FileSystem::new();
    match starstream_compiler::module_graph::load_from_entry(&entry, &mut tracker) {
        Err(starstream_compiler::ModuleGraphError::CrossContractImport { .. }) => {}
        other => panic!("expected CrossContractImport, got {:?}", other.map(|_| ())),
    }
}
