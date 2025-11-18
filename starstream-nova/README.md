We're trying to prove execution of the "Starstream VM" using (some form of) Nova.
We shall consider a simplified version of Starstream for now,
which consists of simple UTXOs and coordination scripts.

Each UTXO has an associated program, which controls the state and life cycle of the UTXO.
A transaction consists of a call to a coordination script.
Coordination scripts have principally three possible actions in our simplified model:
- Create a new UTXO with the given program.
- Execute an existing UTXO (with some input), getting some output from the program of the UTXO.
  The UTXO may die after this. Note that although we say "UTXO", from the perspective of the
  coordination script the UTXO is mutable state. From the perspective of the ledger,
  when you execute a UTXO and its state changes, the old one is consumed and a new one is made.
- Execute another coordination script, also with an input, and also with an output.
You can naturally extend this with more features as necessary.

Consider now the public input the circuit must expose.
The coordination script does not matter, in the end all we have is a list of
input UTXOs and output UTXOs.
We represent this using the same memory trick that Nebula uses.
A dedicated memory represents the states of the UTXOs.

Each UTXO is given a natural number as ID,
and that number is used as address in the memory.
The value at that address in the memory is a commitment
to the state of the program at that time.

The programs don't change and are thus supplied in a lookup table,
also indexed by the numerical ID of the UTXO.

The ledger will then consume X UTXOs and produce Y UTXOs
if there are X UTXOs in the IS where the commitments to the states
match and produce Y UTXOs for the remaining UTXOs in FS.

A UTXO that doesn't exist is represented by a special value
instead of a commitment, which when in IS means the UTXO doesn't
exist yet and will be created,
and when in FS means the UTXO no longer exists and has been destroyed.

This implies you can have ephemeral UTXOs that are created and destroyed in the same transaction.

Each program when proved must expose a commitment to its interactions with the host as a public input.

The VM in each step builds up a parallel list of interactions that must be checked together with
the proof for the program executions at the very end.

Each step of the VM executes a particular interaction,
say "call UTXO n with input x and output y", which means that
an interaction is recorded with UTXO n with such input and output.

Roughly:
- VM maintains a list of interactions with each UTXO and coordination script
- ~Every step adds interactions
- At the end, the built-up table must match what has been proven by the VM for the programs
- The proofs for the executions of the programs (for the UTXO and coordination scripts)
  are folded using a helper "VM" that folds itself along with the programs,
  accumuluating the interactions as seen by the programs.

Thus, we define two VMs we fold for with Nova.

They don't ever interact directly; the final verifier must pass the (partially) same public input to both.
