# Local transaction demo

```sh
cargo run --locked --release -p starstream-proving-runtime \
  --features nebula-proofs --example demo
```

Open <http://127.0.0.1:4317>. Compilation, Wasmtime execution, and proving run
natively. Proving requires Apple Metal. Browser assets are bundled; no Node
build or external services are needed.

The default example creates a counter at `55` and increments it once. The
second example creates an oracle and a consumer UTXO: the oracle returns `3`,
which determines how many times Coord increments the consumer from
`10` to `13`.
The Starstream tabs are editable in place; the generated Wasm tabs show the
latest capture. The program panels place each trace, lock, circuit check,
root, and proof controls together. Lock a program trace before rerunning to
keep its rows, root, proof status, and compiled Wasm tables. Unlocked traces
switch to the new capture.
The source edits live only in the browser and are lost on refresh.
Per-trace circuit checks and the transaction circuit check are separate actions.
Each program shows its captured root; a proof authenticates that root. The
transaction commitment binds the selected statement to the program roots.
Interleaving is a separate action: it captures the selected programs' host
events. After a rerun, checking the old interleaving against new roots can
fail; rebuilding can instead expose mismatched caller and callee events. The
transaction statement stays fixed until you press "Use latest statement". The
normalized trace tab also lets you edit
a row's displayed witness values. A one-row change to a
gathered argument normally fails its Wasm check because the stack source and
following permutation rows still carry the captured value. Rerunning edited
source produces a coherent new trace and root.

Run the script, inspect the generated Wasm, captured instructions, normalized
rows, and host events, then prove traces individually or sequentially. Each card reports real
preprocessing, proving, and verification times. A browser refresh reconnects
to the running job; a new example execution discards the previous session.

Wasm proofs are verified immediately. Their authenticated roots are saved for
the transaction proof, while per-program proof contexts are dropped. The
interleaving proof remains available for a final verification of the complete
transaction commitment.

This uses the same insecure demo parameters as the integration tests. Wasm
includes Nebula memory; interleaving RAM/ROM consistency is still host-checked,
not proven. The server shares one session across browser tabs.

Quick checks (no expensive proofs):

```sh
cargo test --locked --release -p starstream-proving-runtime \
  --features nebula-proofs --example demo
```
