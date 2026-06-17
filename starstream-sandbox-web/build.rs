fn main() {
    // Cranelift recursion (egraph, regalloc) overflows the default 1 MiB
    // shadow stack when compiling a component in `deploy`; give it headroom.
    if std::env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32") {
        println!("cargo::rustc-link-arg=-zstack-size=4194304");
    }
}
