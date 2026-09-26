fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    if std::env::var("CARGO_CFG_TARGET_FAMILY").as_deref() == Ok("wasm") {
        // Cranelift recursion (egraph, regalloc) overflows the default 1 MiB
        // shadow stack when compiling a component in `deploy`; give it headroom.
        println!("cargo::rustc-link-arg=-zstack-size=4194304");
        // The JSPI fiber glue swaps the shadow-stack pointer on every switch.
        println!("cargo::rustc-link-arg=--export=__stack_pointer");
    }
}
