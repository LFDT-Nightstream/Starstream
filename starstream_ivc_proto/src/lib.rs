mod circuit;
mod goldilocks;
mod memory;
mod neo;

use crate::neo::arkworks_to_neo;
use ::neo::{Accumulator, NeoParams, prove_ivc_step_with_extractor};
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use circuit::{InterRoundWires, StepCircuitBuilder};
use goldilocks::FpGoldilocks;
use memory::{DummyMemory, IVCMemory};
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

pub struct ProverOutput {}

impl Transaction<Vec<Instruction>> {
    pub fn new_unproven(changes: BTreeMap<UtxoId, UtxoChange>, ops: Vec<Instruction>) -> Self {
        // TODO: uncomment later when folding works
        // for utxo_id in changes.keys() {
        //     ops.push(Instruction::CheckUtxoOutput { utxo_id: *utxo_id });
        // }

        Self {
            utxo_deltas: changes,
            proof_like: ops,
        }
    }

    pub fn prove(&self) -> Result<Transaction<ProverOutput>, SynthesisError> {
        let mut tx = StepCircuitBuilder::<DummyMemory<F>>::new(
            self.utxo_deltas.clone(),
            self.proof_like.clone(),
        );

        let mb = tx.trace_memory_ops(());

        let mut mem_setup = mb.constraints();

        let num_iters = tx.ops.len();

        let mut cs = ConstraintSystem::<F>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);

        let irw = InterRoundWires::new(tx.rom_offset());

        println!("==============================================");
        println!("Computing step circuit {}", 0);

        let mut irw = tx.make_step_circuit(0, &mut mem_setup, cs.clone(), irw)?;

        println!("==============================================");

        let mut step = arkworks_to_neo(cs.clone());
        let ccs = step.ccs.clone();

        let mut acc = Accumulator {
            c_z_digest: [0u8; 32],
            c_coords: vec![],
            y_compact: step.input.clone(),
            step: 0,
        };

        let params = NeoParams::goldilocks_small_circuits();

        let mut ivc_proofs = vec![];

        let mut prev_augmented_x = None;

        for i in 0..num_iters {
            neo_ccs::relations::check_ccs_rowwise_zero(&step.ccs, &[], &step.witness).unwrap();

            let is_sat = cs.is_satisfied().unwrap();

            if !is_sat {
                let trace = cs.which_is_unsatisfied().unwrap().unwrap();
                panic!(
                    "The constraint system was not satisfied; here is a trace indicating which constraint was unsatisfied: \n{trace}",
                )
            }

            let step_result = prove_ivc_step_with_extractor(
                &params,
                &ccs,
                &step.witness,
                &acc,
                i as u64,
                None,
                &step.output_extractor,
                &step.step_binding_step,
            )
            .unwrap();

            let verify_step_ok = ::neo::verify_ivc_step(
                &ccs,
                &step_result.proof,
                &acc,
                &step.step_binding_step,
                &params,
                prev_augmented_x.as_deref(),
            )
            .expect("verify_ivc_step should not error");

            // FIXME: this doesn't work
            assert!(verify_step_ok, "step verification failed");

            acc = step_result.proof.next_accumulator.clone();
            prev_augmented_x = Some(step_result.proof.step_augmented_public_input.clone());

            ivc_proofs.push(step_result.proof);

            if i < num_iters - 1 {
                cs = ConstraintSystem::<F>::new_ref();
                cs.set_optimization_goal(OptimizationGoal::Constraints);

                println!("==============================================");

                println!("Computing step circuit {}", i + 1);

                let next_irw = tx.make_step_circuit(i + 1, &mut mem_setup, cs.clone(), irw)?;
                irw = next_irw;

                step = arkworks_to_neo(cs.clone());

                println!("==============================================");
            }
        }

        let prover_output = ProverOutput {};
        Ok(Transaction {
            utxo_deltas: self.utxo_deltas.clone(),
            proof_like: prover_output,
        })
    }
}

impl Transaction<ProverOutput> {
    pub fn verify(&self, _changes: BTreeMap<UtxoId, UtxoChange>) {
        // TODO: fill
        //
    }
}

#[cfg(test)]
mod tests {
    use crate::{F, Instruction, Transaction, UtxoChange, UtxoId};
    use std::collections::BTreeMap;

    use tracing_subscriber::{EnvFilter, fmt};

    fn init_test_logging() {
        static INIT: std::sync::Once = std::sync::Once::new();

        INIT.call_once(|| {
            fmt()
                .with_env_filter(
                    EnvFilter::from_default_env().add_directive(tracing::Level::DEBUG.into()),
                )
                .with_test_writer()
                .init();
        });
    }

    #[test]
    fn test_starstream_tx() {
        init_test_logging();

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
                UtxoChange {
                    output_before: F::from(4),
                    output_after: F::from(0),
                    consumed: true,
                },
            ),
            (
                utxo_id3,
                UtxoChange {
                    output_before: F::from(5),
                    output_after: F::from(43),
                    consumed: false,
                },
            ),
        ]
        .into_iter()
        .collect::<BTreeMap<_, _>>();

        let tx = Transaction::new_unproven(
            changes.clone(),
            vec![
                Instruction::Nop {},
                // Instruction::Nop {},
                // Instruction::Resume {
                //     utxo_id: utxo_id2,
                //     input: F::from(0),
                //     output: F::from(0),
                // },
                // Instruction::DropUtxo { utxo_id: utxo_id2 },
                Instruction::Resume {
                    utxo_id: utxo_id3,
                    input: F::from(42),
                    output: F::from(43),
                },
                Instruction::YieldResume {
                    utxo_id: utxo_id3,
                    output: F::from(42),
                },
                // Instruction::Yield {
                //     utxo_id: utxo_id3,
                //     input: F::from(43),
                // },
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
}
