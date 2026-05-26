---
sidebar_position: 4
---

# CLI

The `starstream` binary is the unified entry point to the toolchain. It
exposes one single-file command (`wasm`) and three scan-based commands
(`check`, `docs`, `build`) plus the language server (`lsp`).

Run `starstream --help` for the auto-generated overview, or
`starstream <command> --help` for per-command flags.

See the [language spec](./language-spec.md) for the meaning of `contract;`,
path imports, and the workspace module graph these commands operate over.

## Single-file: `starstream wasm`

```
starstream wasm -c <file> [--output-core PATH] [--output-component PATH]
                          [--output-wit PATH] [--output-binary-wit PATH]
                          [-M, --depfile PATH]
```

- Compiles **one** `.star` file.
- The file is treated as a contract regardless of whether it declares
  `contract;`. Path imports it pulls in still must NOT themselves declare
  `contract;` — cross-contract imports remain a hard error.
- Output flags follow the legacy single-file shape and write to whichever
  paths the caller specifies. Nothing is written automatically.
- Use this for one-off scripted compilation. For project-wide builds, see
  [`starstream build`](#scan-based-starstream-check-docs-build).

## Scan-based: `starstream check`, `docs`, `build`

```
starstream check [DIR] [-D | --deny-warnings]
starstream docs  [DIR] [--pretty]
starstream build [DIR]
```

All three share the same shared-workspace-graph model:

- **Directory selection.**
  - With no `DIR`: scan the current working directory.
  - With an explicit `DIR`: scan exactly that directory.
  - The CLI does not walk up the filesystem looking for a marker file —
    if you want a different scan root, `cd` there or pass it explicitly.

- **File discovery.** Within the scan root the compiler recursively walks
  for `.star` files. Hidden directories (`.git`, `.vscode`, …) and the
  conventional `target/` and `artifacts/` directories are skipped. Every
  `.star` file becomes a node in the workspace graph; path imports may
  pull in additional files outside the scan root, which join the graph
  too. Files containing `contract;` are flagged as codegen entries.

- **One graph, one typecheck.** All three commands build the workspace
  graph once via `load_workspace`, then run `typecheck_modules` over the
  whole graph in a single pass. Diagnostics are stamped with the module
  they originated in. Cross-contract import edges (any edge whose target
  declares `contract;`) are rejected at graph-build time.

- **Per-command work on top of the shared graph:**
  - **`check`** prints every diagnostic from the typecheck pass and exits
    non-zero if there are any errors (or any warnings under `-D`). The
    summary reports modules and contracts scanned.
  - **`docs`** generates JSON per contract entry, writing it to
    `<artifacts>/<filename-stem>/docs.json`.
  - **`build`** runs `compile_contract` once per `contract;` entry. Each
    invocation walks only the subgraph reachable from that contract via
    path-import edges and emits `core.wasm`, `component.wasm`,
    `contract.wit`, and a Make/Ninja-style `deps.d`.

- **Artifacts location** (`docs` and `build`).
  Per-contract outputs land at `<scan-dir>/artifacts/<filename-stem>/` —
  i.e. directly under whatever directory you scanned (or cwd, if you
  passed no `DIR`).

## Language server: `starstream lsp`

The language server uses the same shared-workspace-graph model:

1. **Pick a workspace root** from the `workspaceFolders` the editor
   announced during `initialize`. The first folder that contains the open
   file becomes the scan root (the deepest one wins when nested folders
   overlap). If the editor announced no folders, or none contain this
   file, fall back to the open file's parent directory.
2. **Build one workspace graph** via `load_workspace`. This is the same
   graph the CLI scan-based commands build.
3. **Locate the open file** in the graph by canonical absolute path.
   - If found, run `typecheck_modules` once over the whole graph and
     surface diagnostics for the open file's module id only. Other files'
     diagnostics get published when those files are themselves analysed.
   - If the file isn't in the workspace graph (for example, it lives
     outside the scanned tree), or the workspace scan failed (cross-
     contract edge somewhere we don't own), fall back to a single-file
     graph rooted at the open file. This still goes through the multi-
     file pipeline, so W0002 doesn't fire.

The LSP **only** falls back to single-file `typecheck_program` when the
document URI isn't a filesystem path — i.e. the browser playground, or
`untitled:` / `vscode-vfs:` schemes. Single-file mode is the one place
where the [W0002 path-import-ignored warning](./warnings/W0002.md) can
fire.

## Other commands

- **`starstream format` (alias `fmt`)** — auto-format `.star` files in
  place. Takes one or more file/directory arguments.
- **`starstream explain <code>`** — show the long-form explanation for a
  diagnostic code (e.g. `starstream explain E0050`).
