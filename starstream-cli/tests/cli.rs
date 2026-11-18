//! End-to-end tests that run the `starstream` binary.

use std::{path::Path, process::Command};

#[test]
fn wasm_build_writes_nothing_to_stderr() {
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join("wasm_build_writes_nothing_to_stderr");
    std::fs::create_dir_all(&dir).unwrap();
    let source = dir.join("main.star");
    std::fs::write(&source, "script fn main() -> i64 {\n    1\n}\n").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_starstream"))
        .arg("wasm")
        .arg("-c")
        .arg(&source)
        .arg("-o")
        .arg(dir.join("main.wasm"))
        .output()
        .unwrap();

    assert!(output.status.success(), "wasm failed: {output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
}
