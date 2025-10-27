This is a stub for a crate that supports lookup arguments on opcodes of various instruction sets

Notably, it represents each opcode as a folding-friendly lookup compatible with the CCS constraint system

Notably, we aim to support:
- wasm
- risc-v
- simple stack machine for IMP

This goal is to be extensible, such that developers can easily write lookups for their own custom instruction sets (i.e. you can a-la carte pick which opcode you want your system to support, with some common sets provided like WASM)