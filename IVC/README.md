This is a stub for a crate that supports IVC & PCD folding using CCS based on ideas behind Hypernova

Notably: this is currently powered by Nightstream (https://github.com/nicarq/halo3)

This interface needs to be powerful enough to run IVC/PCD on multiple types:
- folding Memory Consistency Checks ([MCCs](../MCC/README.md))
- folding opcodes ([lookups](../lookups/))
- folding UTXOs (coroutines) from [starstream](../)
- folding transactions (utxo set updates) in the [ledger](../mock-ledger/)
