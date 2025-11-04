# Implementation plan

- See [Starstream language specification](docs/language-spec.md) for details on the language currently implemented.
- See [Codebase structure](./README.md#codebase-structure) for details on the components in this repository.

## TODOs

Components:

- [x] define grammar
- [x] strip down parser and ast - @rvcas
- [x] revamp snapshot test organization - @rvcas
- [x] ditch op codes and make a tree-walking interpreter (ast nodes) - @SpaceManiac
- [x] compile stripped down lang to wasm - @SpaceManiac
- [x] single-binary CLI - @SpaceManiac
- [x] formatter, including snapshot tests, included in CLI - @rvcas
- [x] tree sitter grammar - @SpaceManiac
- [ ] type system setup
  - [x] basics specs - @SpaceManiac
  - [ ] return lsp diagnostics to be displayed in editor - @rvcas
  - [ ] [Algorithm W](https://sdiehl.github.io/typechecker-zoo/algorithm-w/lambda-calculus.html) and typed AST - @rvcas
- [x] LSP server, included in CLI - @rvcas
- [x] vscode extension (incl. highlighting and LSP launcher) - @SpaceManiac dogfooding
  - [x] run LSP with `cargo run`
  - [x] compile & use wasm LSP
- [ ] web sandbox - @SpaceManiac
  - [x] basic editor
  - [x] highlighting and LSP via VSC web ext
  - [ ] compile to Wasm and show disassembly
  - [ ] when language has some form of print(), run reference interpreter
  - [ ] when Wasm ABI is known enough, run Wasm module
- [x] zed extension - @rvcas dogfooding

Language features:

- [x] `else if`
- [ ] functions
- [ ] type annotations on `let` bindings
- [ ] make `let` const and add `let mut`
- [ ] coordination script entry points
- [ ] string type & string literals
- [ ] integer primitive types
  - `Int<N>`, `UInt<N>` primitives
    - for now, Wasm backend errors if a type is too big for Wasm
    - eventually want to surface target-specific errors on check, not just build
  - literals have some pseudo-type that's convertible to primitives
  - `bool`, (i|u)(8|16|32|64) are aliases
  - `char` is an alias for `UInt<21>`?
  - no floats because of determinism/proving trouble
- [ ] struct types
  - remember structural typing (see spec)
- [ ] typedefs
- [ ] builtin container `List<T>` ?
- [ ] UTXO and token stuff
- [ ] linear/affine types
- [ ] `abi` interface exports for utxo
  - errors, effects, methods, tokens
- [ ] coroutine support (yield/resume)
- [ ] algebraic effects and effect handlers
- [ ] enum (tagged union) types
  - builtin `Option<T>`, `Result<T, E>`
- [ ] patterns and pattern matching
  - exhaustive patterns in arguments?
  - patterns in `let` LHS?
- [ ] fields and foreign field arithmetic (important for interop)
  - `Field<X>`
  - may require targetting Nightstream directly w/o Wasm intermediate

Research:

- try out [Verus](https://github.com/verus-lang/verus) as a way to verify the reference interpreter
- specify (roughly) the Wasm ABI that the compiler must target (maybe use WIT?)
  - this is what [`lookups`](../lookups) takes as input.
  - Wasm + the rough shape of external calls
    - how resource types (utxos, tokens) are named
    - how freestanding functions are named
- library/module/interop stuff
  - import `./another_file.star`
  - import `./external_module.wasm` (wasm target only?)
  - JavaScript bindings (WIT?) so dApps can call into Starstream contracts compiled to Wasm
- debugger
- proving and ZK
  - memory consistency checks (or how to handle memory)
  - lookups
