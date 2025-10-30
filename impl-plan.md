# Implementation plan

- See [Starstream language specification](docs/language-spec.md) for details on the language currently implemented.
- See [Hacking on Starstream](./HACKING.md) for details on the components in this repository.

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
  - [ ] add `HACKING.md` specifically for the extension to explain how to properly compile it
- [ ] web sandbox - @SpaceManiac
  - [x] basic editor
  - [x] highlighting and LSP via VSC web ext
  - [ ] compile to wasm
  - [ ] run with interpreter
- [ ] holistics diagnostics and error handling (miette?)
- [x] zed extension - @rvcas dogfooding

Language:

- [ ] functions
- [ ] `else if`

Farther in the future:

- [ ] UTXO and token stuff
- [ ] patterns and pattern matching
- [ ] enhanced parser with better error recovery
- [ ] try out [Verus](https://github.com/verus-lang/verus) on the reference interpreter
- [ ] specify (roughly) the "abi" that the compiler must target (maybe use WIT?)
  - this is what [`lookups`](../lookups) takes as input.
  - WASM + the rough shape of external calls
    - how resource types (utxos, tokens) are named
    - how freestanding functions are named

Even farther in the future:

- Coroutine support (yield/resume)
- Algebraic effects and effect handlers
- Linear types
- Memory consistency checks
- MCCs (or how to handle memory)
- coordination scripts (just handle standalone programs)
- anything to do with proving or ZK
- lookups
- JavaScript bindings (WIT?) so dApps can call into compiled Starstream contracts
