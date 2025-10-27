This project is an implementation of various Memory Consistency Checks (multisets, grand-product sums, permutation checks) with a unified API.

The goal is to be able to connect these MCCs to various components to compare their performance in the real world, and potentially pick different MCCs from this crate based off the most efficient one for different scenarios.

Notably, we need all these MCC techniques to be folding-friendly (support IVC)

Some scenarios we care about:
- Representing read-write memory for registers (such as what is used in the Nebula paper)
- Represent the creation & deletion of UTXOs (coroutines) to manage an incremental commit (IVC) of the current UTXO set
- garbage collection (by adding and removing references and clearing anything that provably has 0 references)
