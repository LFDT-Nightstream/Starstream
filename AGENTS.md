# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Starstream is a UTXO-based smart contract language using coroutines as its core primitive, optimized to run in a zkVM (Nightstream). This repo contains the language implementation (parser, type checker, formatter, interpreter, Wasm compiler), editor tooling (LSP, VSCode/Zed extensions, tree-sitter grammar), the proof/runtime work under `interleaving/`, and the documentation website.

## Commands

**Avoid running the full test suite** (`cargo test` at the workspace root). A few tests related to the proof system (under `interleaving/`) take forever in debug mode. Instead, scope tests to the crate(s) you actually changed: if you only touched `starstream-compiler`, run `cargo test -p starstream-compiler` and nothing more. Only CI runs the proof tests, in release mode.

```bash
# Run the CLI (compiler frontend: check, docs, build, wasm, lsp)
./starstream --help
./starstream wasm -c file.star -o file.wasm

# Test only the crate you changed (see note above)
cargo test -p starstream-compiler
cargo test -p starstream-compiler <test_name>   # single test

# Snapshot tests (insta)
cargo insta test --accept                        # accept new snapshots
cargo insta test --unreferenced delete --accept  # clean up renamed/removed snapshots

# Lint / format (CI enforces both)
cargo clippy
cargo fmt --check

# Slow proof tests — don't run these locally unless you changed the proof
# system itself; CI runs them in release mode
cargo test -p starstream-interleaving-proof --release circuit_test
cargo test -p starstream-runtime --release
```

**Before committing:** run `cargo fmt` (CI enforces `cargo fmt --check`), and commit with `-s` (DCO sign-off required).

## Architecture

The pipeline: source → parser → AST → type checker → typed AST → interpreter or Wasm codegen.

- **`starstream-types/`** — Shared types used across all crates: `ast.rs` (AST), `typed_ast.rs`, `types.rs` (the `Type` enum). `Span` is chumsky's `SimpleSpan`; use the `dummy_span()` helper rather than `Span::new()` (the latter needs the `chumsky::span::Span` trait in scope).
- **`starstream-compiler/`** — `parser/` (chumsky-based, snapshot tests co-located under `parser/**/snapshots/`), `formatter.rs` (opinionated auto-formatter), `typecheck/` (Hindley-Milner style `Inferencer` with `TypeRegistry`, `FunctionRegistry`, `EventRegistry`), `module_graph.rs`, `docs.rs`.
- **`starstream-to-wasm/`** — Compiles typed AST to WebAssembly Component Model / WIT (`component_abi.rs`, `ir.rs`, `encoder.rs`, `stackifier.rs`). Detects `Option`/`Result` by structure and emits native WIT `option<T>`/`result<T,E>`.
- **`starstream-interpreter/`** — AST-walking reference interpreter; implements the language spec (`docs/language-spec.md`) as the comparison point for other targets.
- **`starstream-language-server/`** (+ `-web/`) — LSP server; hover uses type display info (`GenericTypeDef`) flowing from the type checker via `TypecheckSuccess.generic_types`.
- **`starstream-cli/`** — Unified CLI; the root `./starstream` script is a `cargo run` wrapper for it.
- **`interleaving/`** — zkVM executor and proof work: `starstream-interleaving-spec` (transaction/interleaving semantics), `starstream-interleaving-proof` (CCS circuit + Nightstream folding integration), `starstream-component-runtime` (component execution runtime), `starstream-runtime` (minimal test runtime, to be replaced). Depends on Nightstream via pinned git deps (`neo-*`).
- **`tree-sitter-starstream/`** — Grammar for syntax highlighting; generated `src/` output must be committed (CI checks `git diff --exit-code`).
- **`website/`** — Docusaurus docs site + web sandbox; built with `npm ci && npm run build` (needs binaryen and wasm-bindgen).

## Language / Type System Notes

- Generics are internal-only (`Option`/`Result` prelude types); there are no user-defined generics in the syntax. Prelude types cannot be shadowed.
- Structural typing is declaration-order-sensitive: never sort fields/variants during unification, and compare field/variant names by text, not `Identifier` equality.
- Match arms require block syntax: `Pattern => { expr }`, not `Pattern => expr`.
- `get_store_fns` for Variant/Option/Result return types is still `todo!()` in Wasm codegen — don't export enum/option/result return types from script functions.
- Wasm tests assert `source == formatted_source`, so test `.star` files must be pre-formatted (run them through the formatter first).

## Testing Conventions

- Snapshot tests are co-located with the parser modules they exercise; each module has a helper macro taking an `indoc!` snippet and recording `Debug` output via `insta::assert_debug_snapshot!`. Adding a grammar rule = adding a `#[test]` in that module.
- The type checker has an `assert_typecheck_snapshot!` macro.
