# `starstream-wasm-opt` proofs in Rocq

Proofs that the passes in `starstream-wasm-opt` preserve the meaning of a
program, against SpecTec's Wasm 2.0 semantics: the same `Wasm2.v` that
Nightstream's `formal/wasm-rocq` proves the Neo-Wasm circuit against.

The main result is `invoke_refines` in `Nested.v`. Take a store, and
rewrite its function bodies with local-cleanup rules 1, 2 and 5 from
`src/locals.rs`, anywhere in a body and at any depth of blocks, loops and
ifs. Then every run of a call into the rewritten store that ends in values or
a trap has a run of the same call into the original store that ends in the
same values or trap. The final states agree except for function bodies. So
the rewrite adds no behaviour, which is the direction that matters when the
prover proves the rewritten code.

- `LocalRewrites.v`: each rule on its own. From the same state, the original
  instructions and their rewrite run to the same configuration.
- `StepInversion.v`: which steps a configuration can take. Every step happens
  at its first non-value instruction, checked across all of SpecTec's
  rules, plus lemmas for `local.tee`, `local.set` and traps.
- `Nested.v`: the simulation. Before each step of the rewritten program, the
  original runs away any junk the rewrite deleted, then takes the
  corresponding step. That is the same step for most instructions; for
  blocks, loops, ifs and calls it enters its own code, and inside labels and
  frames it recurses. The two programs have different stores, so it also
  shows that every rule other than a call reads only what both stores agree
  on.

Not yet proved:
- the rules that depend on a local never being read (3 and 4);
- removing and renumbering locals;
- that the Rust code applies exactly these rules, and that instantiating the
  two modules gives stores related as the theorem assumes.

The only assumptions are the 55 numeric primitives `Wasm2.v` leaves
abstract, the same list as Nightstream's `scripts/audit.expected`.

## Build

Requires the versions Nightstream checks `Wasm2.v` with: OCaml 5.3.0, Rocq
9.0.1, MathComp ssreflect 2.4.0 and RecordUpdate 0.3.6.

```sh
opam switch create nightstream-rocq 5.3.0
opam repository add rocq-released https://rocq-prover.org/opam/released --switch=nightstream-rocq
opam install --switch=nightstream-rocq coq.9.0.1 coq-mathcomp-ssreflect.2.4.0 coq-record-update.0.3.6
opam exec --switch=nightstream-rocq -- make
```

## `Wasm2.v`

Copied verbatim from Nightstream's `formal/wasm-rocq/Wasm2.v`, which is
[`spectec/test-rocq/theories/wasm.v`](https://github.com/Wasm-DSL/spectec/blob/95c256c2c/spectec/test-rocq/theories/wasm.v)
at SpecTec commit `95c256c2c`, under
[Apache-2.0](https://github.com/Wasm-DSL/spectec/blob/95c256c2c/LICENSE).
Update both copies together so the two proofs keep sharing one semantics.
