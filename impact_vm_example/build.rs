use std::env;
use std::path::Path;

fn main() {
    let lib_name = "impact_vm";

    let dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    let lib_dir = Path::new(&dir).join("libs");

    println!("cargo:rustc-link-search=native={}", lib_dir.display());

    println!("cargo:rustc-link-lib=static={}", lib_name);

    println!(
        "cargo:rerun-if-changed={}/{}.a",
        lib_dir.display(),
        lib_name
    );
}
