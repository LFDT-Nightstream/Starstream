# Implementation plan

- See [Starstream language specification](docs/language-spec.md) for details on the language currently implemented.
- See [Codebase structure](./README.md#codebase-structure) for details on the components in this repository.

## TODOs

Ledger/sandbox/CLI features:

- mock ledger
  - [ ] sandbox shows current "input" ledger state (starts empty)
  - [ ] sandbox allows calling coordination scripts with arguments, can pass existing UTXOs as input (JS console?)
  - [ ] sandbox shows "output" ledger state after running, with "save" button that copies it to the "input"
  - [ ] split and recombine code/state for compression of Utxo storage
  - [ ] deduplication
    - Optimized storage for duplicated nested components
    - Compiler keeps structure to make this possible
- web sandbox
  - [ ] "run" and/or "test" support
- CLI
  - [ ] State directory or DB file for mock ledger state
- [ ] JS frontend

Language features:

- [ ] `abi` elements
  - [ ] errors
    - [ ] throw(?) expr
    - [ ] fail the transaction
- [ ] effect handling
  - [ ] WIT World spec describing ABI
  - [ ] implementing ABI in compiler and runtime
- [ ] UTXO elements
  - [x] `storage` blocks to declare UTXO state (Wasm globals)
  - [x] basic `main fn`s
  - [x] private method `fn`s
  - [ ] public method `pub fn`s ?
  - [ ] abi impl blocks
  - [ ] coroutine support (yield/resume)
- [ ] tokens
  - Linearity enforcement in typechecker
  - Encode whether fn is `burn` or `mint` for runtime
  - How do you call detach?
  - Way for a Utxo to control attach/detach of tokens (currently only Token controls this)
- [ ] variable privacy
  - [x] `disclose` builtin
  - [x] check that `if` conditions are disclosed
  - [ ] check that variables across yield points are disclosed
- [ ] linear/affine typing
- [x] patterns and pattern matching
  - [ ] improve diagnostics for pattern/type mismatches
  - [ ] flag inconsistent `match` arm return values (fallthrough semantics)
- [ ] extra WIT functionality
  - [ ] owned and borrowed resource handles
  - [ ] flags types
  - [ ] declare intent to export a particular WIT world, compiler errors if it's not satisfied
- [ ] prelude
  - [ ] `Address` type
  - [ ] `PublicKey` type
  - [ ] `assert_transaction_signed_by` function
  - [ ] `TokenReleased` effect
    - [ ] definition
    - [ ] runtime emits it on Utxo drop (req. effect handling)
- [ ] imports
  - import from `starstream:std`
    - [ ] builtin cryptographic functions
  - [x] import parts from non-contract `.star` files to form a single contract
  - [ ] import abis, utxos, tokens, and script fns from other Starstream contracts
  - [ ] embed `library fn`s from other `.star` files
  - [ ] embed library code from component `.wasm`
    - [ ] witness mode
    - [ ] proven mode
  - [ ] import externals from arbitrary `.wit` files
    - use case: embedding in a ledger/chain/etc. that isn't hardcoded in the compiler
    - [ ] by path
    - [ ] by WIT package expression, follows standard(?) search path
    - [ ] runtime offers extension hooks to fulfill such imports
- [ ] fields and foreign field arithmetic (important for interop)
  - maybe `Field<N>` or `Int<N>`, `UInt<N>`
  - how to implement?

Low priority / waiting:

- mock ledger/CLI conveniences
  - [ ] automatically import compiled contracts from `artifacts/` directory
- [ ] typedefs
  - [ ] basic `type A = B;`
  - [ ] export `pub type A = B;` to WIT
- [ ] `char` type (is it u32 or `Field<21>`?)
- [ ] heap types
  - Waiting on Lazy Lowering and/or component GC support
  - [ ] `string` and string literals (WIT support)
  - [ ] builtin container `List<T>` (WIT support)
- pattern matching miscellany
  - [ ] maybe allow exhaustive patterns as LHS of function parameters?
  - [ ] positional field access (`t.0`)

Research:

- try out [Verus](https://github.com/verus-lang/verus) as a way to verify the reference interpreter
- library/module/interop stuff
  - import `./another_file.star`
  - import `./external_module.wasm` (wasm target only?)
- debugger
