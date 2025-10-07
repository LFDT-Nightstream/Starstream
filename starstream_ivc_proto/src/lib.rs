mod circuit;
mod goldilocks;
mod memory;
mod neo;

use crate::neo::arkworks_to_neo;
use ::neo::{
    F as NeoF,
    NeoParams,
    verify as neo_verify,
    session::{
        IvcSession, NeoStep as NeoStepTrait, StepArtifacts, StepSpec,
        StepDescriptor, IvcFinalizeOptions, finalize_ivc_chain_with_options
    }
};
use ark_relations::gr1cs::{ConstraintSystem, OptimizationGoal, SynthesisError};
use circuit::{InterRoundWires, StepCircuitBuilder};
use goldilocks::FpGoldilocks;
use memory::{DummyMemory, IVCMemory};
use p3_field::PrimeCharacteristicRing;
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

/// What we return from the prover: enough to verify.
pub struct ProverOutput {
    /// Final succinct proof (Stage 5)
    pub final_proof: ::neo::Proof,
    /// Augmented CCS used by the final SNARK
    pub final_ccs: neo_ccs::CcsStructure<NeoF>,
    /// Final public input for the SNARK
    pub final_public_input: Vec<NeoF>,
}

/// Adapter to bridge our Arkworks-based circuit to the neo session API
struct TransactionStepper {
    tx: StepCircuitBuilder<DummyMemory<F>>,
    mem_setup: Option<<DummyMemory<F> as IVCMemory<F>>::Allocator>,
    irw: Option<InterRoundWires>,
    /// Keep the (single) step shape so we can finalize/verify later
    step_ccs: Option<neo_ccs::CcsStructure<NeoF>>,
}

impl TransactionStepper {
    fn new(utxo_deltas: BTreeMap<UtxoId, UtxoChange>, ops: Vec<Instruction>) -> Self {
        let mut tx = StepCircuitBuilder::<DummyMemory<F>>::new(utxo_deltas, ops);
        let mb = tx.trace_memory_ops(());
        let mem_setup = mb.constraints();
        let irw = InterRoundWires::new(tx.rom_offset());
        
        Self {
            tx,
            mem_setup: Some(mem_setup),
            irw: Some(irw),
            step_ccs: None,
        }
    }
}

impl NeoStepTrait for TransactionStepper {
    type ExternalInputs = ();
    
    fn state_len(&self) -> usize {
        3 // current_program, utxos_len, n_finalized
    }
    
    fn step_spec(&self) -> StepSpec {
        StepSpec {
            y_len: 3,
            const1_index: 0,
            y_step_indices: vec![2, 4, 6], // output indices for IVC state
            y_prev_indices: Some(vec![1, 3, 5]), // input indices for IVC state
            app_input_indices: None,
        }
    }
    
    fn synthesize_step(
        &mut self,
        step_idx: usize,
        _z_prev: &[::neo::F],
        _inputs: &Self::ExternalInputs,
    ) -> StepArtifacts {
        let cs = ConstraintSystem::<F>::new_ref();
        cs.set_optimization_goal(OptimizationGoal::Constraints);
        
        println!("==============================================");
        println!("Computing step circuit {}", step_idx);
        
        let mut mem_setup = self.mem_setup.take().unwrap();
        let irw = self.irw.take().unwrap();
        
        let next_irw = self.tx.make_step_circuit(step_idx, &mut mem_setup, cs.clone(), irw)
            .expect("Failed to synthesize step circuit");
        
        println!("==============================================");
        
        let neo_step = arkworks_to_neo(cs.clone());
        // Cache the step CCS "shape" (single shape for all steps)
        if self.step_ccs.is_none() {
            self.step_ccs = Some(neo_step.ccs.clone());
        }
        
        // Verify constraint satisfaction
        let is_sat = cs.is_satisfied().unwrap();
        if !is_sat {
            let trace = cs.which_is_unsatisfied().unwrap().unwrap();
            panic!(
                "The constraint system was not satisfied; here is a trace: \n{trace}",
            );
        }
        
        // Store for next iteration
        self.mem_setup = Some(mem_setup);
        self.irw = Some(next_irw);
        
        StepArtifacts {
            ccs: neo_step.ccs,
            witness: neo_step.witness,
            public_app_inputs: vec![],
            spec: self.step_spec(),
        }
    }
}

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
        // Use "small circuits" preset from halo3 (simple & robust to start)
        let params = NeoParams::goldilocks_small_circuits();

        // Create our stepper adapter *first* so we can derive y0
        let mut stepper = TransactionStepper::new(
            self.utxo_deltas.clone(),
            self.proof_like.clone(),
        );

        // IMPORTANT: initialize IVC with the same state your circuit exposes
        // as y_prev in step 0 (matches `ivcify_wires` input slots).
        let y0 = vec![
            NeoF::from_u64(1),                              // current_program_in
            NeoF::from_u64(stepper.tx.utxos.len() as u64),  // utxos_len_in
            NeoF::from_u64(0),                              // n_finalized_in
        ];

        let mut session = IvcSession::new(&params, Some(y0.clone()), 0);
        
        let num_iters = stepper.tx.ops.len();
        
        // Prove each step using the session
        for _i in 0..num_iters {
            session.prove_step(&mut stepper, &()).map_err(|_e| {
                SynthesisError::Unsatisfiable
            })?;
        }

        // Finalize the IVC chain into a succinct SNARK (Stage 5)
        let descriptor = StepDescriptor {
            ccs: stepper.step_ccs.clone().expect("missing step CCS"),
            spec: stepper.step_spec(),
        };
        let chain = session.finalize();
        let (final_proof, final_ccs, final_public_input) =
            finalize_ivc_chain_with_options(
                &descriptor, &params, chain,
                IvcFinalizeOptions { embed_ivc_ev: false }
            )
            .map_err(|_| SynthesisError::Unsatisfiable)?
            .ok_or(SynthesisError::Unsatisfiable)?;

        // Optional: self-check the final SNARK before returning
        let ok = neo_verify(&final_ccs, &final_public_input, &final_proof)
            .map_err(|_| SynthesisError::Unsatisfiable)?;
        if !ok {
            return Err(SynthesisError::Unsatisfiable);
        }

        Ok(Transaction {
            utxo_deltas: self.utxo_deltas.clone(),
            proof_like: ProverOutput {
                final_proof,
                final_ccs,
                final_public_input,
            },
        })
    }
}

impl Transaction<ProverOutput> {
    pub fn verify(&self, _changes: BTreeMap<UtxoId, UtxoChange>) {
        // Verify the succinct outer proof
        let ok = neo_verify(&self.proof_like.final_ccs, &self.proof_like.final_public_input, &self.proof_like.final_proof)
            .expect("SNARK verification errored");
        assert!(ok, "Final SNARK verification failed");
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
