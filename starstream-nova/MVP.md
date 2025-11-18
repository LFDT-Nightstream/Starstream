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
- Call another coordination script, sharing the whole stack
- Create a fresh UTXO with the specified parameters
- Call into an existing UTXO (in essence consuming and recreating it), sharing n elements of the stack
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

When calling into a UTXO, n stack values are passed in, where n is a constant
chosen by the program (after translation from WASM, so chosen by translator).
When yielding, the reverse happens and n stack values can be passed back up
for any constant n.

Coordination scripts are handled "uniformly", in the sense that they share
a single VM, and calls between the coordination scripts are handled similarly
to normal function calls, i.e. you jump to a pc, then jump back.

The number of coordination scripts in play is predetermined,
and every "instance" has its own memory, but they all share
the same stack.

Passing pointers to functions from other WASM modules naively
does not work, since the pointer in WASM land is just an i32,
and that in the callee would reference their own memory after translation.

There is instead functions to create "shared" strings,
such that you call `create_string : ptr -> len -> string`,
`len_string : string -> len` and `read_string : string -> ptr -> len -> ()`.

You can pass these across module boundaries but they require you to
serialize your data with e.g. serde, which isn't great.

TODO: support structured data directly,
      perhaps direct ADT support in a modified version of WASM
      using Zorp's Dyck word trick? Albeit this involves committing
      to random field elements and the security depends on field size.

TODO: fill out the rest

Non-extensive list of limitations:
- You need to choose the coordination scripts and UTXOs you interact with beforehand (big limitation!)
- You can't cooperate in as decentralized a manner as you would be able to with the other design
- No tokens (probably easy to add though)

## Possible proper solution

The proper solution is to split the VM from the Starstream specifics,
probably using something like "multi-level folding".

The core idea is that we can do arbitrary efficient PCD using folding
by just keeping running instances for each R1CS/CCS structure we fold with.
Of course this doesn't allow PCD with unknown circuits (i.e. a "dynamic" "vk")
without compressing the instance, or encoding the instance in a CCS structure
that emulates CCS within itself (inefficiently).

There are multiple possible designs, but one possible solution
is taking the above, removing all instructions, and then instead building
up a multiset-like thing that summarizes how the VM should have executed,
which you can compare with what you have in the public input of the instances
for the VM folding proofs.

This does still mean the VM must be adapted to work with Starstream to some extent.
