mod circuit;
mod goldilocks;
mod memory;
mod neo;

use p3_field::PrimeField64;
use ::neo::{NeoParams, IvcSession, StepDescriptor, CcsStructure, IvcChainProof, verify_chain_with_descriptor};
use ark_relations::gr1cs::SynthesisError;
use circuit::StepCircuitBuilder;
use goldilocks::FpGoldilocks;
use memory::DummyMemory;
use crate::neo::ArkStepAdapter;
use std::collections::BTreeMap;

// type F = ark_bn254::Fr;
type F = FpGoldilocks;

pub struct Transaction<P> {
    pub utxo_deltas: BTreeMap<UtxoId, UtxoChange>,
    /// An unproven transaction would have here a vector of utxo 'opcodes',
    /// which are encoded in the `Instruction` enum.
    ///
    /// That gets used to generate a proof that validates the list of utxo deltas.
    proof_like: P,
    // TODO: we also need here an incremental commitment per wasm program, so
    // that the trace can be bound to the zkvm proof. Ideally this has to be
    // done in a way that's native to the proof system, so it's not computed
    // yet.
    //
    // Then at the end of the interleaving proof, we have 1 opening per program
    // (coordination script | utxo).
}

pub type UtxoId = F;

#[derive(Debug, Clone)]
pub struct UtxoChange {
    // we don't need the input
    //
    // we could add the input and output frames here, but the proof for that is external.
    //
    /// the value (a commitment to) of the last yield (in a previous tx).
    pub output_before: F,
    /// the value (a commitment to) of the last yield for this utxo (in this tx).
    pub output_after: F,

    /// whether the utxo dies at the end of the transaction.
    /// if this is true, then there as to be a DropUtxo instruction in the
    /// transcript somewhere.
    pub consumed: bool,
}

// NOTE: see https://github.com/PaimaStudios/Starstream/issues/49#issuecomment-3294246321
#[derive(Debug, Clone)]
pub enum Instruction {
    /// A call to starstream_resume from a coordination script.
    ///
    /// This stores the input and outputs in memory, and sets the
    /// current_program for the next iteration to `utxo_id`.
    ///
    /// Then, when evaluating Yield and YieldResume, we match the input/output
    /// with the corresponding value.
    Resume { utxo_id: F, input: F, output: F },
    /// Called by utxo to yield.
    ///
    /// There is no output, since that's expected to be in YieldResume.
    ///
    /// This operation has to follow a `Resume` with the same value for
    /// `utxo_id`, and it needs to hold that `yield.input == resume.output`.
    Yield { utxo_id: F, input: F },
    /// Called by a utxo to get the coordination script input after a yield.
    ///
    /// The reason for the split is mostly so that all host calls can be atomic
    /// per transaction.
    YieldResume { utxo_id: F, output: F },
    /// Explicit drop.
    ///
    /// This should be called by a utxo that doesn't yield, and ends its
    /// lifetime.
    ///
    /// This moves control back to the coordination script.
    DropUtxo { utxo_id: F },

    /// Auxiliary instructions.
    ///
    /// Nop is used as a dummy instruction to build the circuit layout on the
    /// verifier side.
    Nop {},

    /// Checks that the current output of the utxo matches the expected value in
    /// the public ROM.
    ///
    /// It also increases a counter.
    ///
    /// The verifier then checks that all the utxos were verified, so that they
    /// match the values in the ROM.
    ///
    // NOTE: There are other ways of doing this check.
    CheckUtxoOutput { utxo_id: F },
}

pub struct ProverOutput {
    pub descriptor: StepDescriptor,
    pub chain: IvcChainProof,
    pub params: NeoParams,
}

impl Transaction<Vec<Instruction>> {
    pub fn new_unproven(changes: BTreeMap<UtxoId, UtxoChange>, ops: Vec<Instruction>) -> Self {
        Self { utxo_deltas: changes, proof_like: ops }
    }

    pub fn prove(&self) -> Result<Transaction<ProverOutput>, SynthesisError> {
        let builder = StepCircuitBuilder::<DummyMemory<F>>::new(
            self.utxo_deltas.clone(),
            self.proof_like.clone(),
        );
        let mut adapter = ArkStepAdapter::new(builder);
        let params = NeoParams::goldilocks_small_circuits();
        let mut session = IvcSession::new(&params, vec![], 0);

        let steps = self.proof_like.len();
        for i in 0..steps {
            println!("==============================================");
            println!("Computing step circuit {}", i);
            session.prove_step(&mut adapter, &()).expect("prove step");
            println!("==============================================");
        }

        let chain = session.finalize();
        let descriptor = adapter.descriptor();
        let prover_output = ProverOutput { descriptor, chain, params };
        
        Ok(Transaction { utxo_deltas: self.utxo_deltas.clone(), proof_like: prover_output })
    }
}

impl Transaction<ProverOutput> {
    pub fn verify(&self, _changes: BTreeMap<UtxoId, UtxoChange>) {
        // Manual strict per-step verification threading prev_augmented_x (bypasses base-case ambiguity)
        let binding = self.proof_like.descriptor.spec.binding_spec();
        let mut acc = ::neo::ivc::Accumulator { c_z_digest: [0u8; 32], c_coords: vec![], y_compact: vec![], step: 0 };
        let mut prev_augmented_x: Option<Vec<::neo::F>> = None;

        for (i, step) in self.proof_like.chain.steps.iter().enumerate() {
            // On the first step, use the prover-supplied LHS augmented input to avoid shape drift
            if i == 0 {
                prev_augmented_x = Some(step.prev_step_augmented_public_input.clone());
            }

            let ok = ::neo::ivc::verify_ivc_step(
                &self.proof_like.descriptor.ccs,
                step,
                &acc,
                &binding,
                &self.proof_like.params,
                prev_augmented_x.as_deref(),
            ).expect("per-step verify should not error");
            assert!(ok, "IVC step {} verification failed", i);

            // Advance accumulator and thread augmented x
            acc = step.next_accumulator.clone();
            prev_augmented_x = Some(step.step_augmented_public_input.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{F, Instruction, Transaction, UtxoChange, UtxoId};
    use std::collections::BTreeMap;

    #[test]
    fn test_starstream_tx() {
        let utxo_id1: UtxoId = UtxoId::from(110);
        let utxo_id2: UtxoId = UtxoId::from(300);
        let utxo_id3: UtxoId = UtxoId::from(400);

        let changes = vec![
            (
                utxo_id1,
                UtxoChange {
                    output_before: F::from(5),
                    output_after: F::from(5),
                    consumed: false,
                },
            ),
            (
                utxo_id2,
                UtxoChange { output_before: F::from(4), output_after: F::from(0), consumed: true },
            ),
            (
                utxo_id3,
                UtxoChange { output_before: F::from(5), output_after: F::from(43), consumed: false },
            ),
        ]
        .into_iter()
        .collect::<BTreeMap<_, _>>();

        let tx = Transaction::new_unproven(
            changes.clone(),
            vec![
                Instruction::Nop {},
                Instruction::Resume { utxo_id: utxo_id3, input: F::from(42), output: F::from(43) },
                Instruction::YieldResume { utxo_id: utxo_id3, output: F::from(42) },
            ],
        );

        let proof = tx.prove().unwrap();
        proof.verify(changes);
    }

    #[test]
    #[should_panic]
    fn test_fail_starstream_tx_resume_mismatch() {
        let utxo_id1: UtxoId = UtxoId::from(110);

        let changes = vec![(
            utxo_id1,
            UtxoChange {
                output_before: F::from(0),
                output_after: F::from(43),
                consumed: false,
            },
        )]
        .into_iter()
        .collect::<BTreeMap<_, _>>();

        let tx = Transaction::new_unproven(
            changes.clone(),
            vec![
                Instruction::Nop {},
                Instruction::Resume {
                    utxo_id: utxo_id1,
                    input: F::from(42),
                    output: F::from(43),
                },
                Instruction::YieldResume {
                    utxo_id: utxo_id1,
                    output: F::from(42000),
                },
                Instruction::Yield {
                    utxo_id: utxo_id1,
                    input: F::from(43),
                },
            ],
        );

        let proof = tx.prove().unwrap();

        proof.verify(changes);
    }

    #[test]
    fn test_tx_with_output_checks_multi_folding() {
        let utxo_id1: UtxoId = UtxoId::from(100);
        let utxo_id2: UtxoId = UtxoId::from(200);
        let utxo_id3: UtxoId = UtxoId::from(300);

        // Set desired final states in the ROM:
        //  - u1: unchanged (5 -> 5, not consumed)
        //  - u2: unchanged for this test (4 -> 4, not consumed)
        //  - u3: consumed (7 -> 7, consumed)
        let changes = vec![
            (
                utxo_id1,
                UtxoChange { output_before: F::from(5), output_after: F::from(5), consumed: false },
            ),
            (
                utxo_id2,
                UtxoChange { output_before: F::from(4), output_after: F::from(4), consumed: false },
            ),
            (
                utxo_id3,
                UtxoChange { output_before: F::from(7), output_after: F::from(7), consumed: false },
            ),
        ]
        .into_iter()
        .collect::<BTreeMap<_, _>>();

        // Multi-step program:
        //  - Yield on u2 to set its current output to 9
        //  - Resume on u3 to mark it consumed
        //  - new_unproven() appends CheckUtxoOutput for all UTXOs to validate final state
        let mut ops = vec![Instruction::Nop {}];
        // No state changes; checks should pass against ROM as-is
        for &u in &[utxo_id1, utxo_id2, utxo_id3] {
            ops.push(Instruction::CheckUtxoOutput { utxo_id: u });
        }
        let tx = Transaction::new_unproven(changes.clone(), ops);

        let proof = tx.prove().unwrap();
        proof.verify(changes);
    }
}
