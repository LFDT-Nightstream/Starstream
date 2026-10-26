#[cfg(feature = "server")]
fn main() -> anyhow::Result<()> {
    use std::env;
    use std::fs;
    use std::path::PathBuf;

    use anyhow::Context as _;

    println!("cargo::rerun-if-changed=wit/ledger.wit");
    let mut resolve = wit_parser::Resolve::default();
    let pkg = resolve.push_file("wit/ledger.wit")?;
    let wasm =
        wit_component::encode(&resolve, pkg, false).context("failed to encode ledger WIT")?;
    let out_dir = env::var_os("OUT_DIR").context("`OUT_DIR` not set")?;
    let path = PathBuf::from(out_dir).join("ledger.wasm");
    fs::write(&path, wasm).with_context(|| format!("failed to write `{}`", path.display()))
}

#[cfg(not(feature = "server"))]
fn main() {
    println!("cargo::rerun-if-changed=build.rs");
}
