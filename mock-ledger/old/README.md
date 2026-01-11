This is a mock utxo-based blockchain used to test the implementation of [Starstream](../)

The blockchain only supports UTXO and tokens following Starstream's semantics. A running instance of this mock ledger keeps track of:
- The history of transactions (which are IVC'd into a single commitment)
- The current UTXO (coroutine) set for the latest commitment. A UTXO has:
    - Memory, code hash, and a program counter to resume with
    - Incremental commitment so far, for use in future folding
