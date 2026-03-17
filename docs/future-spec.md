---
sidebar_position: 3
---

# Future specifications

This document uses a specification style to describe features planned or
proposed for inclusion in the [Starstream language specification][spec] once
they are finalized and implemented.

[spec]: ./language-spec.md

## Types

- Structural.
- Product `struct` and sum `enum` types.
  - Tuples = anonymous structs.
  - Unions = anonymous enums.
- Has support for linear types.
- Pseudo-generics for built-in constructs like `Utxo<AnotherContractName>`.
  - Because we eventually need functions that can accept "any UTXO satisfying X condition".
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

- Expand `let` to permit patterns, not just identifiers, with checking that
  - When one pattern can cover the whole value space (namely structs).
- Add spread operator `..` to ignore remainder of fields.

## Semantics

### Environment

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
