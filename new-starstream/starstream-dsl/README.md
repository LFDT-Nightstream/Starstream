# Starstream DSL

This folder contains the Starstream DSL project.

## 🚀 Quick Start

### Use CLI to compile a file

```bash
./starstream wasm -c $your_source.star -o $your_module.wasm

# Can view disassembly, for example by:
wasm-dis $your_module.wasm  # from binaryen/emscripten
wasm2wat $your_module.wasm  # from wabt
```

## 📚 References

- [Language Spec](docs/language-spec.md)
- [Design Document](docs/design.md) - Architecture and motivation
- [Implementation Plan](impl-plan.md) - Todo list and completed/finished feature info

[language spec]: ./docs/language-spec.md

## 🔧 Architecture and components

Concerns are separated into several crates. The 'compiler' turns source code
into a validated AST, which is then interpreted directly or compiled further
to a target such as WebAssembly.

Compiler:

* `starstream-types/` - Common AST types.
  * Used as the interface between parsing and code generation.
* `starstream-compiler/` - Starstream language implementation.
  * `parser/` - Parser from Starstream source code to AST.
  * `formatter.rs` - Opinionated auto-formatter.
  * TODO: type checker.
* `starstream-interpreter/` - AST-walking reference interpreter.
  * Implements the [language spec] in an easy-to-audit way.
  * Not optimized, but used as a comparison point for other targets.
* `starstream-to-wasm/` - Compiler from Starstream source to WebAssembly modules.

Tooling:

* `tree-sitter-starstream/` - [Tree-sitter] definitions including [grammar].
* TODO: `starstream-language-server/` - [LSP] server implementation.

Interfaces:

* `starstream-cli` - Unified Starstream compiler and tooling CLI.
  * Frontend to Wasm compiler, formatter, language server, and so on.
  * Run `./starstream --help` for usage instructions.
* TODO: `vscode-starstream/` - Extension for [Visual Studio Code].
  * Syntax highlighting and language server integration.
  * TODO: Link to published extension on marketplace & OpenVSIX.
* TODO: Extension for [Zed].
* TODO: Web sandbox.

[LSP]: https://microsoft.github.io/language-server-protocol/
[Tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[grammar]: ./tree-sitter-starstream/grammar.js
[Visual Studio Code]: https://code.visualstudio.com/
[Zed]: https://zed.dev/

## 🧪 Tests

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

### Workflow

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
