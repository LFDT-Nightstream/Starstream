TODO: math-ify the below
TODO: How would this interact with CycleFold?
TODO: Add separate part on handling and persisting (commitments to) memory

The code here attempts to prove Starstream.

TL;DR: put "augmentation" in separate circuit

We want everything to be folded up,
such that at the very "top", you end up with a
ledger, and a folded proof of its correctness.
(Aside: You should at any point be able to substitute the compressed (Spartan) proof for the folded instance,
but that doesn't affect any of the below since that can be implemented trivially.)

What does this specifically entail?

Assume we're using basic Nova.

Nova roughly lets us, given property P and instances z0 and z1 where P(z0) and P(z1)
both hold, calculate a new z where P(z) holds, where P is the R1CS structure and z the witness (consisting
of the public part, the private part, and the constant 1).

Nova also presents a technique for transforming the above into IVC,
by embedding the (Fiat-Shamir-ed) verifier into the circuit/structure,
such that you in a "recursive" manner, prove that the folding itself was also done correctly.
This is possible because the folding doesn't depend on the structure, thus the instance
can fold other instances of itself.
Then you keep a running instance, and an instance that has verified the running instance
while also performing the next step. To extend with another step, you fold the two instances,
and compute a new instance that verifies this folding (while acting as the prover counterpart
for the embedded verified in the circuit).

But we can't quite implement Starstream as IVC. Users are spread across the network,
and collaboration necessitates distributed proof generation, where multiple proofs
(i.e. instance pairs as described above) can be folded together.

You can naturally extend this to PCD by folding two pairs of instances
in the embedded verifier in the circuit, which gives you a tree
structure, but this isn't quite what we're looking for either.

We want something like "multi-level folding", where at each level, we can fold
in proofs from the level below.

For the sake of simplicity, take the levels to be very roughly in our simplified model:
- Ledger
- Transaction
- Script (execution)

Let us start from the Script level.
Assume we have an IVC scheme implemented using Nova for Brainfuck,
where memory is represented as a Merkle tree (very inefficient, but as
an example).

Our transaction circuit is then also IVC-like, except at every step,
in our simplified model,
where it "executes" a UTXO (as you do in Starstream), we also want
to verify the execution of the associated script.

Let us consider an alternative to Nova's augmentation.
Instead of augmenting the circuit itself to verify its predecessor's folding,
we create an entirely new auxiliary circuit.

Our proof is now two running instances, one for our helper circuit, one for the "foldee" circuit.
We also keep an instance for the latest "step" as before.

The helper circuit runs the verifier twice, once for its own running instance,
folding the previous "step" instance with the previous running instance,
and once for the "foldee", folding the previous running instance for
the foldee with the new foldee instance.

The circuit can implement custom logic for determining the public input
of the instance.

The "Transaction" level circuit then uses this to
verify script execution. Of course the script proof isn't a single instance,
but instead one running and one "step" instance, which we must first fold
before folding into our own running instance.

You can trivially extend this to the "Ledger" circuit, which then
uses the same trick to include the instances of the Transaction level,
which of course also include the instances of the Script level.

At the end you end up with 1 + n running instances, where n = O(levels) roughly.
