use std::env;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    if env::var("CARGO_CFG_TARGET_FAMILY").as_deref() == Ok("wasm") {
        println!("cargo::rustc-link-arg=--import-undefined");
        println!("cargo::rustc-link-arg=-zstack-size=4194304");
        println!("cargo::rustc-link-arg=--export=__stack_pointer");
        println!("cargo::rustc-link-arg=--export=starstream_fiber_enter");
    }
}
