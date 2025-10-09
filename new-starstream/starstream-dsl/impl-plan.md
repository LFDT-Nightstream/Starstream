Starstream consists of two components: a compiler and interpreter

Instead of writing Starstream (the full DSL), we will instead start with something simple and build towards it.

Notably, we don't want to think about the following yet:
- coroutines (yield/resume)
- MCCs (or how to handle memory)
- coordination scripts (just handle standalone programs)
- effect handlers / algebraic effects
- linear types
- anything to do with proving or ZK
- lookups

## Compiler

-Write a clear specification (including grammar BNF and any other document one would expect) for a language based on the "IMP" programming language

- write it in Rust, and design it to run in the browser (wasm)
- use the `chumsky` crate to create a parser
- compiler output: stack machine opcodes

## Interpreter

Implement a interpreter for the stack machine

Requirements:
    - has to be a streaming interpreter (takes in one opcode at a time)
    - has a plugin-like architecture where you combine the compiler with an opcode handler (that is called at each step of the stream)

## Snapshot testing

A way to run a suite of different example programs for my language, and ensure that
1. they all compile properly
2. the parser produce the right AST
3. the compiler produces the correct WASM code for browser builds
4. the interpreter can run them with the expected result
