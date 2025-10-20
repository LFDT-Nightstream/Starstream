# Starstream

![GitHub License](https://img.shields.io/github/license/LFDT-Nightstream/Starstream)
[![OpenSSF Best Practices](https://bestpractices.coreinfrastructure.org/projects/:project-id/badge)](https://bestpractices.coreinfrastructure.org/projects/:project-id)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/:user/:repo/badge)](https://scorecard.dev/viewer/?uri=github.com/:user/:repo)

Starstream is a VM concept that uses delimited continuations as its core primitive.
The end goal is a language and VM that can be used across any blockchain that chooses to include it.

Unique features of Starstream:

* Native folding scheme support for both on variable updates & function application (only VM that provides both)
* UTXO-based (only zkVM in development with this property)
* Delimited continuations as its core primitive (only blockchain VM that does this)

Basic overview: [video](https://www.youtube.com/watch?v=zzk-hVfNW1A) and [slides](https://docs.google.com/presentation/d/1_o9lHQJqFQtUOJovLLBF7E--C73ikaRDpPurZPt1-q8/edit)

Technical overview: [video](https://www.youtube.com/watch?v=qjoSF7EV0BQ) and [slides](https://docs.google.com/presentation/d/127mS6K3XBkWJOmctxfDi2HrSQl3Zbr3JBBwWay9xHGo/edit)

Starstream working group on Discord: https://discord.gg/9eZaheySZE

## Entry points

* `make` to build everything
    * `cargo build` to build Rust crates
* `./starstream` to run Starstream CLI
    * Hello world example: `./starstream compile -c grammar/examples/hello_world.star -o target/hello.wasm`
        * Run: `./starstream run -m target/hello.wasm`
    * Rust example: `cargo build -p example_contract`
        * Run Rust example: `./starstream run -m target/wasm32-unknown-unknown/debug/example_contract.wasm -e new_nft`
        * Other `-e` entry points: `produce`
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

## Examples

* `example_contract`
* `example_contract_permissioned`

Note that a single WASM file can contain any combination of coordination scripts, UTXOs, and tokens. This allows them to ship together as a unit. The limitations on what each type of contract can do are enforced dynamically.
