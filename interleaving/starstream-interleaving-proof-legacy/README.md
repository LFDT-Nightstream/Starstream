# starstream-interleaving-proof

This crate contains the Starstream-specific proof circuit (currently Arkworks
based) for transaction traces. It takes the public instance and witness types
from `starstream-interleaving-spec`, converts witness effects (opcodes) into an
interleaved trace, and proves that trace with Nightstream's folding stack.

The underlying folding/IVC, CCS, memory, and trace machinery comes from the
Nightstream `neo-*` crates. This crate wires those components to Starstream's
transaction semantics.

## Contents

- `src/lib.rs` - Public `prove` entry point, trace interleaving, Nightstream
  folding session setup, and exported proof helpers.
- `src/circuit.rs` - Main step circuit builder and inter-round state wiring.
- `src/neo.rs` - Adapter between the Starstream step circuit and Nightstream's
  VM/folding-session APIs.
- `src/memory/` - Memory abstraction used by the circuit, including Twist
  and Shout (Nightstream native) and Nebula (currently unused, but pure R1CS)
  integrations.
- `src/circuit_test.rs` - Circuit-level tests for the interleaving proof.
