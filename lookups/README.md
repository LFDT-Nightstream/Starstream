This is a stub for a crate that supports lookup arguments on opcodes of various instruction sets

Notably, it represents each opcode as a folding-friendly lookup compatible with the CCS constraint system

Notably, we aim to support:
- wasm
- risc-v
- simple stack machine for IMP

This goal is to be extensible, such that developers can easily write lookups for their own custom instruction sets (i.e. you can a-la carte pick which opcode you want your system to support, with some common sets provided like WASM)

## Required WASM features

There are two independent sets of WASM featured for Starstream:
1. Features required for Starstream execution
2. Features required by the proof system (which may depend on more features than Starstream requires)

Beyond standard WASM, Starstream requires the following WASM proposals to be implemented for its compiler to work effectively:
- Garbage Collection: required for UTXO's linear memory to not grow unboundedly large (both to minimize ledger size, but also to avoid MCC performance degradation over time)

- Multiple Memories: ???
- Stack Switching: ???
- Component Model
- reference types: needed for GC proposal
- typed function references: needed for GC proposal

Additionally, for the proof system, the following are required:
- Memory64: required to handle large proof generation (careful: WASM Components [do not yet support](https://github.com/WebAssembly/component-model/issues/22) the memory64 proposal, but this shouldn't cause any issues)
- 