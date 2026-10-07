# starstream-interleaving-spec

This package contains a Quint specification for the Starstream interleaving
proof circuit.

The corresponding WIP circuit is in `starstream-interleaving-prover`; the
previous implementation is in `starstream-interleaving-proof-legacy`.

Note that while the specification is designed as a reference for a zk circuit,
it is also in a way a specification of the runtime, since those are necessarily
coupled.

However, this specification is about the ABI and transaction semantics, and not
really about mechanisms. So this doesn't intend to model neither of:

- WASM execution or semantics
- WIT types (control flow irrelevant types are represented as the opaque
StarstreamValue type: a four-field opaque root, represented in Quint as four
centered signed field representatives to stay within its evaluator's integer range).
- Circuit encodings

The goal however is for every quint action to be mapped to a semantic "opcode"
in the circuit, roughly to a single step of execution.

## Layout

The core specification is in the `spec/starstream.qnt` file.

[`src/events.rs`](src/events.rs) defines the outer event encoding for program trace commitments.

Transaction replay uses `new_transaction` / `verify_transaction`: load storage and
ABIs, then execute (the first execution step closes loading). After the terminal
return, the empty call stack allows output processing; the phase stays `Running`
until `finish_transaction` sets `Finished`.
Finalization scans every UTXO in ID order (`get_storage` or `skip_consumed`). Each
`get_storage` is followed by `read_abi` for every final-generation registration,
in order, including duplicates, matching `OutputUtxo.methods`. These host-synthesized
reads are not program events. After the UTXO/ABI scan, `finalize_coordinator`
enumerates Coord(1)'s trace root exactly once, then `finish_transaction` completes
the transaction. Coord(0) is only a sentinel; multiple coordinators are not modeled yet.
Execution-only tests can still use `new_tx` / `verify`.

The shared `spec/sim_core.qnt` loads sampled inputs, explores execution, then
synthesizes matching outputs once, tracked by a simulator-only `outputs_installed`
flag without changing the core phase. Fixed-statement rejection
tests run against the core spec, not this output-synthesis wrapper.

Two modules supply its domain constants:

- `spec/sim.qnt`: simulation/REPL use up to two inputs, four UTXOs/handles,
  three methods/values, initial ABIs of length 1–2, and up to three registrations.
- `spec/verify.qnt`: CI verification keeps at most one input, two UTXOs/handles,
  two methods, one value, single-method initial ABIs, and up to two registrations.

`npm run check` typechecks both profiles and simulates the larger one.

## Running

Install the repository-pinned Quint CLI and run the specification checks:

```sh
npm ci
npm run check
```

For model exploration, print one reproducible nondeterministic trace

```sh
npm run simulate
```

or open a REPL with the simulation module preloaded:

```sh
npm run repl
```

Inside the REPL, call `init`, concrete semantic actions, or the nondeterministic
`step`, and evaluate `state` to inspect the current model state.

Run the Quint-backed Rust tests:

```sh
npm ci
npm test
```

This also runs the differential trace tests against
`starstream-interleaving-prover`. These tests are ignored by default by
`cargo test` because they require Quint.
