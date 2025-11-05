# Starstream

![GitHub License](https://img.shields.io/github/license/LFDT-Nightstream/Starstream)
[![OpenSSF Best Practices](https://bestpractices.coreinfrastructure.org/projects/:project-id/badge)](https://bestpractices.coreinfrastructure.org/projects/:project-id)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/:user/:repo/badge)](https://scorecard.dev/viewer/?uri=github.com/:user/:repo)

<!-- Keep this section in sync with `website/docs/index.md`. -->

Starstream is a VM concept that uses delimited continuations as its core primitive.
The end goal is a language and VM that can be used across any blockchain that chooses to include it.

Unique features of Starstream:

- Native folding scheme support for both on variable updates & function application (only VM that provides both)
- UTXO-based (only zkVM in development with this property)
- Delimited continuations as its core primitive (only blockchain VM that does this)

Basic overview: [video](https://www.youtube.com/watch?v=zzk-hVfNW1A) and [slides](https://docs.google.com/presentation/d/1_o9lHQJqFQtUOJovLLBF7E--C73ikaRDpPurZPt1-q8/edit).

Technical overview: [video](https://www.youtube.com/watch?v=qjoSF7EV0BQ) and [slides](https://docs.google.com/presentation/d/127mS6K3XBkWJOmctxfDi2HrSQl3Zbr3JBBwWay9xHGo/edit).

Starstream working group on Discord: https://discord.gg/9eZaheySZE.

## References

- [Language spec]
- [Design document](docs/design.md) - Background and motivation
- [Implementation plan](impl-plan.md) - Todo list and completed/finished feature info

## Getting started

<!-- TODO: Link to the web sandbox, CLI releases, published VSC and Zed extensions. -->

Read more about how to use Starstream on the documentation website: https://lfdt-nightstream.github.io/Starstream/#getting-started

To begin working on Starstream from this repository:

- Use `./starstream` to build and run the command-line interface and show its help message with more information.
- Use `cargo test` to run the [tests](#tests).
- See [`website/`] for how to build it and its organization.
- See [`vscode-starstream/`] for build instructions. Use VSCode's "Launch extension" mode to debug.
- Use Zed to build and install `zed-starstream/` as a development extension.

### Compile a file

```bash
./starstream wasm -c $your_source.star -o $your_module.wasm
# Can view disassembly using:
wasm-dis $your_module.wasm  # from binaryen/emscripten
wasm2wat $your_module.wasm  # from wabt
```

[language spec]: ./docs/language-spec.md

## Codebase structure

The Starstream DSL implementation and documentation website live in this repository.

Starstream is being built bit-by-bit, starting with full tooling for a simple
language and adding each feature across the whole stack.

Concerns are separated into several crates. The 'compiler' turns source code
into a validated AST, which is then interpreted directly or compiled further
to a target such as WebAssembly.

Compiler:

- `starstream-types/` - Common AST types.
  - Used as the interface between parsing and code generation.
- `starstream-compiler/` - Starstream language implementation.
  - `parser/` - Parser from Starstream source code to AST.
  - `formatter.rs` - Opinionated auto-formatter.
  - TODO: type checker.
- `starstream-interpreter/` - AST-walking reference interpreter.
  - Implements the [language spec] in an easy-to-audit way.
  - Not optimized, but used as a comparison point for other targets.
- `starstream-to-wasm/` - Compiler from Starstream source to WebAssembly modules.

Tooling:

- `tree-sitter-starstream/` - [Tree-sitter] definitions including [grammar] for syntax highlighting and analysis.
- `starstream-language-server/` - [LSP] server implementation.
- `starstream-language-server-web/` - Compiles the language server to WebAssembly ([Web Worker] only, uses [wasm-bindgen]).
- `starstream-sandbox-web/` - Compiles `starstream-to-wasm/` to Wasm for use in the web sandbox (freestanding).

[Web Worker]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API
[wasm-bindgen]: https://wasm-bindgen.github.io/wasm-bindgen/reference/deployment.html

Executor and VM:

- TODO: [`IVC/`](./IVC/README.md)
- TODO: [`MCC/`](./MCC/README.md)
- TODO: [`lookups/`](./lookups/README.md)
- TODO: [`mock-ledger/`](./mock-ledger/README.md)

Interfaces:

- [`website/`] - Documentation website and web sandbox.
- `starstream-cli/` - Unified Starstream compiler and tooling CLI.
  - Frontend to Wasm compiler, formatter, language server, and so on.
  - Run `./starstream --help` for usage instructions.
- [`vscode-starstream/`] - Extension for [Visual Studio Code].
  - TODO: Publish to marketplace & OpenVSIX.
- `zed-starstream/` - Extension for [Zed].
  - TODO: Publish.

[`website/`]: ./website/README.md
[`vscode-starstream/`]: ./vscode-starstream/README.md
[LSP]: https://microsoft.github.io/language-server-protocol/
[Tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[grammar]: ./tree-sitter-starstream/grammar.js
[Visual Studio Code]: https://code.visualstudio.com/
[Zed]: https://zed.dev/

## Tests

```bash
# Run all tests
cargo test
```

### Snapshot tests

We co-locate snapshot tests with the parsers they exercise. Each module (`expression`, `statement`, `program`, …) exposes a tiny helper macro that:

- takes an `indoc!` snippet for readability,
- parses it with the module’s own `parser()` function, and
- records the full `Debug` output of the AST using `insta::assert_debug_snapshot!`.

Snapshots live under `starstream-compiler/src/parser/**/snapshots/` right next to the code. The snapshot headers include the literal source (via the Insta `description` field), so reviews don’t need to cross-reference input files.

```bash
# run unit + snapshot tests
cargo test

# (optional) focused snapshot cycle
cargo insta test
cargo insta review
cargo insta accept

# clean up renamed/removed snapshots in one go
cargo insta test --unreferenced delete --accept
```

Because the helpers sit in the modules themselves, adding a new grammar rule is as simple as writing another `#[test]` in that module and feeding the helper macro a snippet.

## Formalities

- [Code of Conduct](./CODE_OF_CONDUCT.md)
- [Contributing guidelines](./CONTRIBUTING.md)
- [Security policy](./SECURITY.md)
- [Maintainers list](./MAINTAINERS.md)
- License: [Apache-2.0](./LICENSE) or [MIT](./LICENSE-MIT) at your option
