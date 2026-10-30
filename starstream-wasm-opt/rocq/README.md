# `starstream-wasm-opt` proofs in Rocq

Proofs that the passes in `starstream-wasm-opt` preserve the meaning of a
program, against SpecTec's Wasm 2.0 semantics: the same `Wasm2.v` that
Nightstream's `formal/wasm-rocq` proves the Neo-Wasm circuit against.

The main result is `invoke_refines` in `Nested.v`. Take a store, and
rewrite its function bodies with the five local-cleanup rules from
`src/locals.rs`, anywhere in a body, in any mix, and at any depth of blocks,
loops and ifs. Rules 3 and 4 may delete writes only to locals the function
never reads. Then every run of a call into the rewritten store that ends in
values or a trap has either:
- a run of the same call into the original store that ends in the same values
  or trap, with final states that agree except for function bodies; or
- a run of the original that goes wrong: it gets stuck before the end.

So the rewrite adds no behaviour, which is the direction that matters when
the prover proves the rewritten code. The second case is there because of
rule 3. Deleting `local.tee` drops an instruction that needs a value under
it, and without Wasm's type system the proof cannot rule out an original
with nothing there. Validated modules don't get stuck: SpecTec proves type
soundness against this same file, in
[`type_preservation.v`](https://github.com/Wasm-DSL/spectec/blob/95c256c2c/spectec/test-rocq/theories/type_preservation.v)
and
[`type_progress.v`](https://github.com/Wasm-DSL/spectec/blob/95c256c2c/spectec/test-rocq/theories/type_progress.v),
with two SIMD cases of progress left open. We don't import those proofs:
they would add 23 axioms and an admitted lemma to the assumptions below, and
the optimizer only has to be right on code that validates.

`module_ok_rewrite` in `Validation.v` shows that the rewrite keeps code valid:
rewriting any function bodies of a valid module, in the same way, gives a
valid module. The pass needs this because it runs in rounds. Each round's
original is the previous round's output, so ruling out the second case for
that round needs that output to be valid.

- `LocalRewrites.v`: each rule on its own. From the same state, the original
  instructions and their rewrite run to the same configuration.
- `StepInversion.v`: which steps a configuration can take. Every step happens
  at its first non-value instruction, checked across all of SpecTec's
  rules. Plus lemmas for `local.tee`, `local.set`, `drop` and traps.
- `Nested.v`: the simulation. Before each step of the rewritten program, the
  original runs away any code the rewrite deleted, then takes the matching
  step. For most instructions that is the same step. For blocks, loops, ifs
  and calls the original enters its own code, and inside labels and frames
  the simulation recurses.

  The two programs have different stores, and their frames differ in locals
  the function never reads. So `Nested.v` also shows that every rule other
  than a call reads only what both stores agree on. It shows that every
  `local.get` they share reads a local whose values agree. And it shows that
  every step of an instruction with no code in it produces no code and no
  reads of locals.
- `Validation.v`: retyping the rewritten code. Any typing derivation for a
  sequence of instructions splits into derivations for its parts, with some
  stack between them, so each rule and each block can be retyped alone.

Not yet proved:
- removing and renumbering locals;
- that the Rust code applies exactly these rules, and that instantiating the
  two modules gives valid stores related as the theorem assumes. With those
  and SpecTec's soundness, the results above compose across the pass's rounds.

The only assumptions are the 55 things `Wasm2.v` leaves abstract, the same
list as Nightstream's `scripts/audit.expected`: 54 numeric primitives and
decidable equality on its list type, `res_list_eq_dec`. `Validation.v` needs
only that last one.

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
