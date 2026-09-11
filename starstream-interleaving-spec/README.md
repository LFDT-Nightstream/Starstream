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

The `spec/sim.qnt` file wraps the specification with small domains for bounded
model checking, plus with nondeterministic pickers for the simulator.

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
