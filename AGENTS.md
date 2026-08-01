# AGENTS.md

## Where to Find Things

- **`README.md`** — Codebase structure and crate-by-crate overview; start here.
- **`docs/language-spec.md`** — The language spec; the interpreter implements it as the reference.
- **`docs/cli.md`** — Full CLI command reference (`check`, `docs`, `build`, `wasm`, `lsp`).
- **`docs/design.md`** — Background and motivation.
- **`impl-plan.md`** — Todo list and completed/finished feature info.
- **`website/README.md`** — Docs website and web sandbox build instructions.
- **`CONTRIBUTING.md`** — Contribution process.

## Commands

**Avoid running the full test suite** (`cargo test` at the workspace root). A few tests related to the proof system (under `interleaving/`) take forever in debug mode. Instead, scope tests to the crate(s) you actually changed: if you only touched `starstream-compiler`, run `cargo test -p starstream-compiler` and nothing more. Only CI runs the proof tests, in release mode.

```bash
# Run the CLI (compiler frontend: check, docs, build, wasm, lsp)
./starstream --help
./starstream wasm -c file.star -o file.wasm

# Test only the crate you changed (see note above)
cargo test -p starstream-compiler
cargo test -p starstream-compiler <test_name>   # single test

# Snapshot tests (insta) — conventions are described in README.md
cargo insta test --accept                        # accept new snapshots
cargo insta test --unreferenced delete --accept  # clean up renamed/removed snapshots

# Lint / format
cargo clippy
cargo fmt --check
```

**Before committing:** run `cargo fmt` and `cargo clippy` (CI enforces both `cargo fmt --check` and clippy), and commit with `-s` (DCO sign-off required).

## Non-Obvious Gotchas

- `get_store_fns` for Variant/Option/Result return types is still `todo!()` in Wasm codegen — don't export enum/option/result return types from script functions.
- Wasm tests assert `source == formatted_source`, so test `.star` files must be pre-formatted.
- `Span` is chumsky's `SimpleSpan`; use the `dummy_span()` helper rather than `Span::new()`.
- Generated `tree-sitter-starstream/src/` output must be committed (CI checks `git diff --exit-code`).
