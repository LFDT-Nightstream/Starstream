MVP Starstream:

We have a test ledger with UTXOs.
Each UTXO has a WASM program associated with it, by hash.
The UTXOs don't have any "values" or "amounts" in this model.
Each UTXO has a name which is the hash of the transaction that created
it along with the index of the output in that transaction,
as it is on Bitcoin and Cardano.

There is a "first" UTXO to let you create transactions,
since a transaction must have an input.
We call this the primal UTXO.

The ledger can accept transactions,
which consist of a list of inputs (UTXO hashes),
and a list of outputs (WASM module hash along with memory and stack hash).
If a transaction has no inputs, it is invalid.

A transaction begins by invoking a "coordination script".
The coordination script runs until it "yields", with one of the following options:
- Call another coordination script, sharing the stack
- Create a fresh UTXO with the specified parameters
- Call into an existing UTXO (in essence consuming and recreating it), sharing the stack
- Return to calling coordination script, or exit, if none
When it yields, the corresponding script is executed, until that script ends,
and control is returned to the caller script.
UTXOs can't do anything other than return in this MVP.

The above is implemented as single VM, the Starstream Transaction VM.
It is implemented as a WASM-like stack machine, the IR of which we shall just call IR,
such that the execution of WASM can be proven with it.
There are several simplifications compared to WASM, and the overall design is as such:
- There is a single global stack
- All control flow is done using jumps
- Calls don't exist as such but are emulated by pushing the
  program counter to stack then jumping followed by jumping to the pushed pc.
- There are obviously no types.
- Only i32 is supported.

The ledger does not take the R1CS (or CCS) instances directly,
but instead consumes a "compressed" Spartan (or other SNARK) proof.

The public input of the proof is a "summary" of the transaction,
notably, all input UTXOs (specified via IR, pc, memory, stack),
along with all output UTXOs (containing IR, pc, memory, stack).

Let us recall the first (prime) goal of Starstream:
We wish to enable smart contract developers to write smart contracts
targetting a UTXO ledger in an ergonomic manner, while enabling
_private computation_, and it is firstly this that folding enables us to do.

This guides us in how much we prove and how much we don't.
The program/function that takes a ledger and a transaction and produces a new ledger
needn't the translation of WASM to IR be proven, since there is nothing
private about the translation, nor is it (concretely and asymptotically) expensive.

If we _didn't_ know the concrete WASM module, then we would need it proven,
but that is not a requirement for _this_ MVP.
In the model this MVP presents, nodes would need to know the concrete
code for a script to consider a transaction using it valid.

Private scripts are certainly useful but not a part of this MVP.

We represent the on-going state of the transaction as some memories,
along with two memories per script, representing the content (or rather,
the _diff_ if possible) of its WASM linear memory and its stack,
along with a list of pcs for each UTXO.

For a UTXO to be consumed by a transaction, there must be a corresponding UTXO
in the instance such that the state specified matches the state
in the UTXO.

TODO: fill out how stack sharing is done
TOOD: fill out how coordination scripts are handled
TODO: fill out the rest
