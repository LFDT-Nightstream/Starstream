# Contributing

## Build steps

These steps assume you already have [rustup](https://www.rust-lang.org/tools/install) installed

```bash
rustup target add wasm32-unknown-unknown
make
```

## Interacting with built code

* `./starstream` to run Starstream compiler CLI
* `./test` to run the VM in test/example configuration
* `starstream_vscode/install_dev` to install extension working directory into VSC
    * Reload VSC after running
    * Uninstall with `rm ~/.vscode/extensions/starstream-dev`

## Implementation structure

* `starstream_compiler`: Compiler from Starstream language to WASM
* `starstream_cli`: Command-line interface to the compiler
    * Binary name is `starstream`
* `starstream_sandbox`: Exports the compiler and VM for use in the browser sandbox
* `starstream_sys`: WASM-side Rust bindings for Starstream VM exports
* `starstream_vm`: Host and example/test code
    * Uses [wasmi](https://docs.rs/wasmi/0.31.2/wasmi/) as WASM interpreter
* `starstream_vscode`: Visual Studio Code extension for Starstream language support