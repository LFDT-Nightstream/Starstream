---
sidebar_position: 3
---

# Future specifications

This document uses a specification style to describe features planned or
proposed for inclusion in the [Starstream language specification][spec] once
they are finalized and implemented.

[spec]: ./language-spec.md

## Types

- Anonymous enums.
- Utxo types are linear/affine (use exactly once, with including a Utxo in the output either explicit or implicit).
- `abi MyAbi` declares a handle type similar to that of `utxo MyUtxo`.
- Expand Utxo types, which currently allow constraints on ABI and contract, to allow constraints on tokens as well. Syntax TBD.
- Effects and resumable errors are typed as part of the signature of a function.
  - Fatal errors (fail the transaction) are not typed.

## Type identities

Structure type definitions can be hashed for comparison. Names do not matter (structural typing).

- Algebraic Data Types (ADTs) are supported
  - Struct identities are based on their field types, in order
    - So `(i32, i32)` == `struct Foo { a: i32, b: i32 }` == `struct Bar { b: i32, c: i32 }`
    - No such thing as anonymous `{ a: i32, b: i32 }`.
  - Enum identities are based on their variant discriminators (ordinals), and the field types in order of each variant
    - So `i32 | (i32, i32)` == `enum Foo { A { b: i32, }, C { d: i32, e: i32 } }`

- Function identities are based on their name and their type.
  - Function types are based on their parameter types in order, return type, and possible effect set

## Pattern matching

- Expand `let` to permit patterns, not just identifiers, with checking that the pattern is irrefutable (usually structs).
- Add spread operator `..` to ignore remainder of fields.

## Semantic environment

The Env of the semantics is defined by the following contexts:

- The instruction context: the program and the instructions left to process
  - A suspended UTXO must serialize itself; the VM expects to be able to simply run it from its entry point with its existing memory
- The local memory context: any variables local to the function (ex: "the stack")
  - objects can be removed from this context either by going out of scope, or by being used (linear types)
- The persistent memory context: any shared variables that are globally referable (ex: static variables, "the heap")
  - Including some notion of when a piece of persistent memory is "freed" and can be safely zeroed (immediately and deterministically)
- The type context: which types exist and their definitions
  - There are no pointer types to either functions or resources
  - Types have identities (hashes) and are structural (names are omitted when computing the ID)
- The resource context: which references exist to externally-managed resources (tokens)
  - UTXO external resources and token intermediates are passed around explicitly, not part of the context

## Struct updates

`Foo { a: 1, ..old_foo }`

## Tests

In-script unit and property tests.

## Imports

More import sources:

- `namespace:package/interface` with optional `@1.1.1` version
  - WIT data found at standard(?) search path (ex. `wit/` folder)
  - WIT imports stay imports and must be fulfilled by linking or a runtime extension
- `"./path/to/other.star"`
  - Access utxo, token, and ABI types
  - Access top-level `library fn`s, which get embedded/inlined
  - Access utxo, token, and script fns, which get imported as cross-contract calls
- `"./path/to/component.wasm"`
  - Embed an arbitrary component as library code
- `"./path/to/core.wasm"`
  - "unsafe" style FFI imports?
  - Or combine with WIT file?

## Utxo methods and coroutine lifetimes

The basic flow for a coordination script interacting with a Utxo resembles:

1. Coordination script starts
2. It calls a Utxo's `main fn`, which spawns the Utxo and starts its execution
3. The `main fn` runs until it ends or hits a `yield`
4. The Utxo makes itself suspendable by storing locals and program counter to globals / linear memory ("stackless")
5. Control flow then returns back to the coordination script caller
6. The main-fn call expression evaluates to a handle to the new Utxo
7. The contents of the `yield` determine what methods/ABIs are available
    - Some methods are resume-ish: consume the Utxo and produce a new one (possibly the handle stays partially valid, depending on syntax sugar)
      - These methods can end in one of three ways:
        - `return;` to the caller normally while remaining at the same `yield` point
        - `consume;` the utxo
        - `resume <expr>;` to jump to after the `yield` expression, with its value being that of `<expr>`
      - Calls to these methods conceptually invalidate the Utxo handle and returns a new one, but Utxo flow typing takes this into account
      - In Rust terms, `fn(self) -> Self`, `-> Utxo`, `-> Option<Utxo>`, etc.
    - Some methods are "normal": they execute in the Utxo's context and can read it (maybe write too?), but not affect its lifetime or ABI set
      - Can only `return` normally
      - Calls do not invalidate the Utxo handle
      - In Rust terms, `fn(&self)` and `fn(&mut self)`
