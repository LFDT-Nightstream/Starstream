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
  `contract;`.
- Outputs are only written to paths specified as `--output-X` arguments.
- Use this for one-off scripted compilation. For project-wide builds, see
  [`starstream build`](#scan-based-starstream-check-docs-build).

## Scan-based: `starstream check`, `docs`, `build`

```
starstream check [DIR] [-D | --deny-warnings]
starstream docs  [DIR] [--pretty]
starstream build [DIR]
```

All three share the same shared-workspace-graph model:

- Directory selection:
  - With no `DIR`: scan the current working directory and children.
  - With an explicit `DIR`: scan that directory and children.

- File discovery: Within the scan root the compiler recursively walks
  for `.star` files. Hidden directories (`.git`, `.vscode`, …) and the
  conventional `target/` and `artifacts/` directories are skipped. Every
  `.star` file becomes a node in the workspace graph; path imports may
  pull in additional files outside the scan root, which join the graph
  too. Files containing `contract;` are flagged as codegen entries.

- Typechecking: All discovered files are typechecked as a unit. Diagnostics
  indicate which file they originated in.

- Per-command work:
  - `check` prints every diagnostic from the typecheck pass and exits
    non-zero if there are any errors (or any warnings under `-D`). The
    summary reports modules and contracts scanned.
  - `build` compiles each `contract;` entry point. Each
    invocation walks only the subgraph reachable from that contract via
    path-import edges and emits `core.wasm`, `component.wasm`,
    `contract.wit`, and a Make/Ninja-style `deps.d`.
  - `docs` generates JSON per contract entry, writing it to
    `artifacts/<filename-stem>/docs.json`.

- Artifacts:
  Per-contract outputs are written to `<scan-dir>/artifacts/<filename-stem>/`.

## Language server: `starstream lsp`

The language server uses the same shared-workspace-graph model:

1. **Pick a workspace root** from the `workspaceFolders` the editor
    announced during `initialize`. The first folder that contains the open
    file becomes the scan root (the deepest one wins when nested folders
    overlap). If the editor announced no folders, or none contain this
    file, fall back to the open file's parent directory.
2. **Build one workspace graph** of every `.star` file in the workspace,
    just like the CLI scan-based commands.
1. **Locate the open file** in the graph by canonical absolute path.
    - If found, typecheck the whole graph and
      surface diagnostics for the open file's module id only. Other files'
      diagnostics get published when those files are themselves analysed.
    - If the file isn't in the workspace graph (for example, it lives
      outside the scanned tree), or the workspace scan failed (cross-
      contract edge somewhere we don't own), fall back to a single-file
      graph rooted at the open file.

The LSP **only** falls back to single-file mode (no imports allowed) when the
document URI isn't a filesystem path — i.e. the browser playground, or
`untitled:` / `vscode-vfs:` schemes. Single-file mode is the one place
where the [W0002 path-import-ignored warning](./warnings/W0002.md) can
fire.

## Other commands

- **`starstream format` (alias `fmt`)** — auto-format `.star` files in
  place. Takes one or more file/directory arguments.
- **`starstream explain <code>`** — show the long-form explanation for a
  diagnostic code (e.g. `starstream explain E0050`).
