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

- Write a clear specification (including grammar BNF and any other
  document one would expect) for a language based on the "IMP" programming language
- write it in Rust, and design it to run in the browser (WASM)
- use the `chumsky` crate to create a parser
- Instead of opcodes we will compile directly to WASM

## Interpreter

Implement a tree-walking interpreter for the ast nodes

## Snapshot testing

A way to run a suite of different example programs for the language, and ensure that

1. they all compile properly
2. the parser produces the right AST
3. the compiler produces the correct WASM code
4. the interpreter can run them and produce the correct result

## TODOs

- [x] define grammar
- [x] strip down parser and ast - @rvcas
- [x] revamp snapshot test organization - @rvcas
- [x] ditch op codes and make a tree-walking interpreter (ast nodes) - @SpaceManiac
- [x] compile stripped down lang to wasm - @SpaceManiac
- [x] single-binary CLI - @SpaceManiac
- [x] formatter, including snapshot tests - @rvcas
  - [x] add to CLI
- [x] tree sitter grammar - @SpaceManiac
  - for editor-agnostic syntax highlighting
- [ ] type system setup - @SpaceManiac
  - the basics: `i64` and `bool`
- [ ] LSP server - @rvcas
  - Consider tower-lsp-community/tower-lsp-server w/ runtime-agnostic feature
  - Want abstracted filesystem for WASM support
  - add to CLI
- [ ] vscode extension (incl. highlighting and LSP launcher) - @SpaceManiac dogfooding
  - no WASM lsp yet, just run from dev, but probably for publishing
- [ ] web sandbox
  - need WASM lsp
- [ ] holistics diagnostics and error handling (miette?)
- [ ] zed extension - @rvcas dogfooding
- [ ] repo structure documentation

Farther in the future:

- [ ] enhanced parser with better error recovery
- [ ] try out [Verus](https://github.com/verus-lang/verus) on the reference interpreter
- [ ] specify (roughly) the "abi" that the compiler must target (maybe use WIT?)
  - this is what [`lookups`](../lookups) takes as input.
  - WASM + the rough shape of external calls
    - how resource types (utxos, tokens) are named
    - how freestanding functions are named

Even farther in the future:

- Coroutine support (yield/resume)
- Algebraic effects system
- Linear type system
- Memory consistency checks
- Full Starstream language features
