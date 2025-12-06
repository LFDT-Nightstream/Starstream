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
- [x] type system setup
  - [x] basics specs - @SpaceManiac
  - [x] return lsp diagnostics to be displayed in editor - @rvcas
  - [x] [Algorithm W](https://sdiehl.github.io/typechecker-zoo/algorithm-w/lambda-calculus.html) and typed AST - @rvcas
- [x] LSP server, included in CLI - @rvcas
- [x] vscode extension (incl. highlighting and LSP launcher) - @SpaceManiac dogfooding
  - [x] run LSP with `cargo run`
  - [x] compile & use wasm LSP
- [x] web sandbox - @SpaceManiac
  - [x] basic editor
  - [x] highlighting and LSP via VSC web ext
  - [x] compile to Wasm and show disassembly
  - [ ] when language has some form of print(), run reference interpreter
  - [ ] when Wasm ABI is known enough, run Wasm module
- [x] zed extension - @rvcas dogfooding

Language features:

- [x] `else if`
- [x] functions
- [x] function call expressions
- [x] if expressions
- [x] block expressions
- [x] make `let` const and add `let mut`
- [x] type annotations on `let` bindings
- [x] coordination script exports
- [x] `storage` blocks to declare UTXO state (Wasm globals)
  - [ ] scope to only the `utxo` block rather than whole program
- [ ] mock ledger
  - [ ] sandbox shows current "input" ledger state (starts empty)
  - [ ] sandbox allows calling coordination scripts with arguments, can pass existing UTXOs as input (JS console?)
  - [ ] sandbox shows "output" ledger state after running, with "save" button that copies it to the "input"
- [ ] "event" support
  - WASI? or custom component imports?
  - [ ] visible on sandbox a tab
- [ ] Base-level ABI stuff
  - [ ] underlying callable ABI
- [ ] `abi` interface exports for utxo
  - errors, effects, methods, tokens
- [ ] variable privacy
  - [ ] `disclose` builtin
  - [ ] check that `if` conditions are disclosed
  - [ ] check that variables across yield points are disclosed
- [ ] integer primitive types
  - `Int<N>`, `UInt<N>` primitives
    - for now, Wasm backend errors if a type is too big for Wasm
    - eventually want to surface target-specific errors on check, not just build
  - literals have some pseudo-type that's convertible to primitives
  - `bool`, (i|u)(8|16|32|64) are aliases
  - `char` is an alias for `UInt<21>`?
  - no floats because of determinism/proving trouble
- [x] struct types
  - remember structural typing (see spec)
  - [ ] codegen/interpreter support for struct values
- [ ] typedefs
- [ ] UTXO and token stuff
- [ ] linear/affine types
- [ ] builtin container `List<T>` ?
- [ ] coroutine support (yield/resume)
- [ ] algebraic effects and effect handlers
- [x] enum (tagged union) types
  - [ ] builtin `Option<T>`, `Result<T, E>`
  - [ ] support in interpreter/wasm codegen
- [ ] string type & string literals
- [x] patterns and pattern matching
  - exhaustive patterns in arguments?
  - [x] support literals in pattern
  - [ ] patterns in `let` LHS (structs make sense, enums not so much)
  - [x] exhaustiveness checking for `match`
  - [ ] improve diagnostics for pattern/type mismatches
  - [ ] flag inconsistent `match` arm return values (fallthrough semantics)
- [ ] fields and foreign field arithmetic (important for interop)
  - `Field<X>`
  - may require targetting Nightstream directly w/o Wasm intermediate
- [x] hover/go-to-definition for struct fields, enum variants, pattern bindings

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
