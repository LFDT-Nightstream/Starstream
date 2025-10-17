# Starstream DSL - Consolidated Project

This folder contains the consolidated Starstream DSL project with all components merged from the split architecture.

## 🚀 Quick Start

### Run Tests

```bash
# Run all tests
cargo test
```

### Use CLI to compile a file

```bash
./starstream wasm -c $your_source.star -o $your_module.wasm

# Can view disassembly, for example by:
wasm-dis $your_module.wasm  # from binaryen/emscripten
wasm2wat $your_module.wasm  # from wabt
```

### Build for WASM

```bash
cargo build --target wasm32-unknown-unknown --release
```

## 🧪 Snapshot Testing

We co-locate snapshot tests with the parsers they exercise. Each module (`expression`, `statement`, `program`, …) exposes a tiny helper macro that:

- takes an `indoc!` snippet for readability,
- parses it with the module’s own `parser()` function, and
- records the full `Debug` output of the AST using `insta::assert_debug_snapshot!`.

Snapshots live under `starstream-compiler/src/parser/**/snapshots/` right next to the code. The snapshot headers include the literal source (via the Insta `description` field), so reviews don’t need to cross-reference input files.

### Workflow

```bash
# run unit + snapshot tests
cargo test

# (optional) focused snapshot cycle
cargo insta test
cargo insta review
cargo insta accept

# clean up renamed/removed snapshots in one go
cargo insta test --unreferenced delete --accept
```

Because the helpers sit in the modules themselves, adding a new grammar rule is as simple as writing another `#[test]` in that module and feeding the helper macro a snippet.

## 📦 Components

### Core Types (`starstream-types/`)

- **AST**: Abstract Syntax Tree for IMP-like language
- **Errors**: Comprehensive error handling

### Compiler (`starstream-compiler/`)

- **Parser**: Simple but functional parser for basic expressions
  - Handles variable declarations, assignments, arithmetic
  - Parses binary expressions with proper precedence
- **Compiler**: Compiles AST to WASM
  - Handles variable declarations and assignments
  - Symbol table management

### Interpreter (`starstream-interpreter/`)

- Tree-walking interpreter for IMP-like language
- Execution context with stack and variables
- Step-by-step execution capability

## 🔧 Architecture

The project follows a clean separation of concerns:

1. **Parser**: Source code → AST
2. **Compiler**: AST → WASM
3. **Interpreter**: AST → Execution

## 📋 Development Status

### ✅ Completed

- Basic parser for expressions and variable declarations
- Complete example demonstrating the pipeline
- Test suite with integration tests
- WASM support for browser deployment

### 📋 Future Features

- Coroutine support (yield/resume)
- Algebraic effects system
- Linear type system
- Memory consistency checks
- Full Starstream language features

## 📚 References

- [Design Document](docs/design.md) - Architecture and motivation
- [Language Spec](docs/language-spec.md) - Language Spec
- [Implementation Plan](impl-plan.md) - Development roadmap
