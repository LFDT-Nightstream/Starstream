use starstream_vm::*;
use tempfile::TempDir;

#[test]
pub fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    let output_dir = TempDir::new().unwrap();

    let mut output_path = output_dir.path().to_path_buf();
    output_path.push("codegen.wasm");

    let output = std::process::Command::new("cargo")
        .arg("run")
        .arg("--bin")
        .arg("starstream")
        .arg("compile")
        .arg("-c")
        .arg("grammar/examples/tokens.star")
        .arg("-o")
        .arg(&output_path)
        .current_dir("../")
        .output()
        .unwrap();

    // std::process::Command::new("wasm2wat")
    //     .arg("--no-check")
    //     .arg(&output_path)
    //     .spawn()
    //     .unwrap()
    //     .wait()
    //     .unwrap();

    // std::process::Command::new("wasm2wat")
    //     .arg(&output_path)
    //     .spawn()
    //     .unwrap()
    //     .wait()
    //     .unwrap();

    assert!(output.status.success());

    let mut tx = Transaction::new();

    let contract = tx.code_cache().load_file(&output_path);

    tx.run_coordination_script(&contract, "main", vec![]);

    // tx.prove();
}
