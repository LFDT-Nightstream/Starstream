# Starstream Demo

This crate is the home for the interactive interleaving demo.

Current scope:

- coordination script + two UTXOs
- stepping through cross-contract execution
- per-process trace generation
- interleaved trace view
- mocked-verifier semantics check
- zk proof generation
- ledger application / verification

The implementation plan lives in [IMPLEMENTATION_PLAN.md](./IMPLEMENTATION_PLAN.md).

## Requirements

To run the demo you need:

- Rust toolchain
- `cargo-component`
- the `wasm32-wasip1` target

Install the missing pieces with:

```sh
cargo install cargo-component
rustup target add wasm32-wasip1
```

## Running

Start the backend:

```sh
cargo run -p starstream-demo
```

Then open:

```text
http://127.0.0.1:4317
```

Current capabilities:

- edit the coord / UTXO A / UTXO B demo components
- compile the demo contracts from the browser
- inspect the generated WAT for each component
- generate per-process traces
- step traces into an interleaved trace
- fold the interleaving proof
- inspect the public statement and transaction structure
- apply the transaction to the ledger and inspect ledger before/after

## Notes

- The demo guests are built with `cargo component`, so the first compile on a
  fresh machine may take a little longer while the component toolchain artifacts
  are prepared.
- The guest crates currently keep generated `src/bindings.rs` files because
  `cargo component` expects and regenerates them as part of its normal build
  flow.
