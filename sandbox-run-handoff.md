# Handoff: `feat/sandbox-run` in the Starstream repo

Written 2026-09-22 for an agent picking up the branch cold.

## Where things stand

**The branch.** `feat/sandbox-run` adds a Run panel to the website sandbox
(`website/src/pages/sandbox.tsx`). It lets a user deploy the contract compiled
in the editor, create UTXOs, call their methods, and view storage and events,
all in-browser with wasmtime compiled to wasm32. The branch was rebased onto
main at 42ef4edd (PR #231, `test` blocks), and the rebase broke it because the
runtime became async-only. The last session repaired that and verified the
result in a real browser.

**Commits on top of main (oldest first), all local, none pushed:**

| Commit   | What                                                                                                   | Trailer state                                                    |
|----------|--------------------------------------------------------------------------------------------------------|------------------------------------------------------------------|
| cbf53905 | Original Run panel (Opus-assisted, human signed)                                                       | signed off                                                       |
| 393daffa | Take cranelift-codegen from the wasmtime git pin so it unifies with wasmtime's copy for wasm32         | signed off                                                       |
| 99dfa17a | Port the Run panel to the async runtime on JSPI fibers                                                 | `Assisted-by: anthropic:claude-fable-5-1` only, **no Signed-off-by yet** |
| dd691225 | Attribute run-worker responses by request id; report deploy/construct failures; restart after a trap   | same, **no Signed-off-by yet**                                   |

**Remote divergence.** `origin/feat/sandbox-run` points at a0af2a54, a
pre-rebase commit that does not exist in the local clone. Publishing the
rebased branch requires a force-push, which the human should do after adding
`Signed-off-by` to the two unsigned commits, e.g.
`git rebase -x 'git commit --amend -s --no-edit' 393daffa`.

**Working tree** is clean. Build outputs that exist locally but are gitignored:
`vscode-starstream/starstream.vsix`, `website/starstream_sandbox_web.wasm`
(10.9 MB, built from the current tip), `website/build/`.

## How the Run panel works

**Three layers.**

- `starstream-sandbox-web/src/lib.rs` (Rust, cdylib for
  wasm32-unknown-unknown, no wasm-bindgen). Raw C-ABI exports: `run` (compile,
  pre-existing), `deploy`, `construct`, `call`, `storage_get`,
  `implemented_methods`, `drop_resource`, `set_cardano`. Raw imports from
  module `env`: `read_input`, `sandbox_log`, the `set_*` result sinks
  (`set_describe`, `set_call_result`, `set_storage`, `set_implemented`,
  `set_events`, plus the compile ones), and the fiber hooks. State is a
  thread-local map of deployments keyed by SHA-256 digest of the component,
  each holding a compiled `Component` plus one wasmtime `Store` per live UTXO
  (handle = index). The Rust side implements runtime-next's current `Host`
  trait (utxo-context resources, per-store `ResourceTable`) and drives the
  runtime's `*_async` entry points through `fiber::block_on`.
- `starstream-sandbox-web/src/fiber.rs`. Guest half of wasmtime's
  `custom-fiber` C ABI: exports `wasmtime_fiber_enter(entry, arg0,
  top_of_stack)` as the first-activation trampoline, imports
  `host_park`/`host_unpark`, and provides `block_on` whose waker unparks. The
  crate enables the `wasmtime-custom-fiber` and
  `wasmtime-custom-virtual-memory` features of `starstream-runtime-next` only
  on wasm targets. `build.rs` adds `-zstack-size=4194304` (cranelift recursion
  overflows 1 MiB) and `--export=__stack_pointer`.
- `website/src/run.worker.ts`. Long-lived Web Worker. Implements
  `wasmtime_fiber_init` and `wasmtime_fiber_switch` with JSPI: `fiberSwitch`
  is wrapped in `WebAssembly.Suspending`, each fiber record keeps a parked
  resolver for its top-of-stack, a switch parks the caller, swaps the exported
  `__stack_pointer` global, and wakes the other side. New fibers start with
  shadow stack at `top - 16`. Every export that can run guest code
  (`construct`, `call`, `storage_get`, `drop_resource`, `wasmtime_fiber_enter`)
  is wrapped in `WebAssembly.promising`. Requests are serialized in `onmessage`
  because root activations share the main shadow stack and are only safe LIFO.
  If `WebAssembly.Suspending` is missing the worker throws "running contracts
  requires JSPI: Chromium 137 or newer".

**Message protocol** (worker <-> page). Requests: `deploy`, `construct`,
`call`, `storageGet`, `implementedMethods`, `drop`, `setCardano`, each carrying
`request_id`. Responses: `idle`, `log {level,target,body}`, `deployed`,
`deploy_failed`, `constructed`, `construct_failed`, `called`, `storage`,
`implemented`, `dropped`. The page keys pending deploys and calls by
`request_id` (dd691225); stale responses are logged as "discarding response
for old request". After a trap the worker discards its instance and
re-instantiates, so a poisoned `RefCell` does not wedge the panel.

**Page details.** Deploy is disabled when the current compiled digest is
already deployed. The log panel shows only Rust log levels <= 2 (Error, Warn);
every level is mirrored to `console.log`. Cardano context (block height,
current slot) is per deployment via `setCardano`. `website/src/jspi.d.ts`
declares the JSPI types for tsc. `website/src/sandbox.worker.ts` (compile
worker) got the same `sandbox_log` import stub so the shared wasm instantiates
there too.

**Reference implementations of the shim**, if the fiber glue needs revisiting:
https://github.com/rvolosatovs/browsertime (README explains the design;
`web/glue.js`, `app/src/fiber.rs`, `jspi-fiber-harness/`) and the `crates/web`
directory of the sibling `starstream-run` repo (wasm-bindgen variant). The
sandbox port follows browsertime's raw-import style.

## What was verified

- **Rust**: `cargo build -p starstream-sandbox-web --target
  wasm32-unknown-unknown --release`, `cargo clippy`, `cargo fmt` clean. Scoped
  tests of touched crates pass; the full workspace suite was never run
  (project rule).
- **Website**: `cd website && npm ci && npm run build` succeeds (vsix, sandbox
  wasm via wasm-opt -O3, docusaurus). `tsc` clean.
- **Node 24 smoke test** of the worker's fiber logic against the built wasm.
- **Real browser, headless Chromium 153 via Playwright** on `docusaurus serve`
  of the production build, default example contract (`Score`/`ScoreProgress`
  in `website/src/editor.tsx`):
  - deploy; create UTXO (storage chips 0, mult 0, four methods listed);
  - `plus_chips(42)`, `plus_mult(4)`, `mult_mult(200)` -> chips 42, mult 8;
  - `finish()` -> event `score.finish(336)`;
  - calling `plus_chips` after finish correctly errors with "method ... is not
    callable: this UTXO did not declare it via implements-method";
  - change block height, create a second UTXO, drop the first;
  - edit `/ 100` -> `/ 10` in Monaco, recompile, redeploy -> second digest
    listed alongside the first; new UTXO computes mult 12 and emits 60.
  - Zero console errors or warnings apart from the expected gate error. No
    "discarding response" or restart messages.

## Gotchas and open items

- **Trace log flood.** `lib.rs` sets `log::set_max_level(Trace)` (from the
  original cbf53905) and the page mirrors every level to the console, so one
  deploy prints ~8,000 wasmtime trace lines in devtools. Left as is; a
  one-line level change or a page-side filter would fix it if the team wants.
- **Browser support.** Only Chromium was tested. Firefox and Safari lack JSPI
  and get the explicit error above by design.
- **wasm-bindgen version.** The extension crate (`starstream-language-server-web`)
  pins wasm-bindgen 0.2.105. `npx vsce package` fails if the wasm-bindgen CLI
  on PATH is a different version (the VM had 0.2.121); the Nix dev shell in
  `flake.nix` provides the matching one. Otherwise
  `cargo install wasm-bindgen-cli --version 0.2.105 --root <dir>` and prepend
  its `bin` to PATH.
- **Dependency pins.** wasmtime is a git pin in the workspace;
  `cranelift-codegen` must come from the same pin (393daffa). `Cargo.lock` is
  up to date; CI runs `cargo check --locked`.
- **`.cargo/config.toml`** applies `--import-undefined`, `--export-table` and
  a 60 KiB stack to all wasm32 builds; the sandbox overrides the stack size in
  its own `build.rs`. Check there before "fixing" link errors.
- **Monaco in browser tests.** `window.monaco` is not exposed;
  `keyboard.insertText` does not change the model. Double-click a token and
  `keyboard.type` works.
- **Playwright MCP** writes into `<repo>/.playwright-mcp/`; delete it before
  committing.

## Project conventions to keep

- No explanatory comments in code edits unless the surrounding code already
  has them there.
- Simplest change that works; don't code for edge cases nobody asked for.
- Never `cargo test --workspace`; test only the crates touched
  (`starstream-sandbox-web` has `test = false`, so its check is the wasm32
  build plus clippy). Codegen and runtime-next are coupled; test both if
  either is touched.
- `cargo fmt` and `cargo clippy` before committing. Use `tracing`, not
  `println!`.
