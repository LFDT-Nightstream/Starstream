mod circuit;
#[cfg(test)]
mod e2e;
mod goldilocks;
mod memory;
mod neo;
mod poseidon2;
#[cfg(test)]
mod test_utils;

pub use memory::nebula;
use neo_ajtai::AjtaiSModule;
use neo_ccs::CcsStructure;
use neo_fold::pi_ccs::FoldingMode;
use neo_fold::session::FoldingSession;
use neo_params::NeoParams;

use crate::circuit::InterRoundWires;
use crate::memory::IVCMemory;
use crate::neo::arkworks_to_neo_ccs;
use crate::{memory::DummyMemory, neo::StepCircuitNeo};
use ark_relations::gr1cs::{ConstraintSystem, SynthesisError};
use circuit::StepCircuitBuilder;
use goldilocks::FpGoldilocks;
use std::collections::BTreeMap;

type F = FpGoldilocks;

#[derive(Debug)]
pub struct Transaction<P> {
    pub utxo_deltas: BTreeMap<ProgramId, UtxoChange>,
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

pub type ProgramId = F;

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
pub enum LedgerOperation<F> {
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
    // pub proof: Proof,
    pub proof: (),
}

impl Transaction<Vec<LedgerOperation<F>>> {
    pub fn new_unproven(
        changes: BTreeMap<ProgramId, UtxoChange>,
        mut ops: Vec<LedgerOperation<F>>,
    ) -> Self {
        for utxo_id in changes.keys() {
            ops.push(LedgerOperation::CheckUtxoOutput { utxo_id: *utxo_id });
        }

        Self {
            utxo_deltas: changes,
            proof_like: ops,
        }
    }

    pub fn prove(&self) -> Result<Transaction<ProverOutput>, SynthesisError> {
        let shape_ccs = ccs_step_shape()?;

        let tx = StepCircuitBuilder::<DummyMemory<F>>::new(
            self.utxo_deltas.clone(),
            self.proof_like.clone(),
        );

        let num_iters = tx.ops.len();

        let n = shape_ccs.n.max(shape_ccs.m);

        let mut f_circuit = StepCircuitNeo::new(tx, shape_ccs.clone());

        // since we are using square matrices, n = m
        neo::setup_ajtai_for_dims(n);

        let l = AjtaiSModule::from_global_for_dims(neo_math::D, n).expect("AjtaiSModule init");

        let params = NeoParams::goldilocks_auto_r1cs_ccs(n)
            .expect("goldilocks_auto_r1cs_ccs should find valid params");

        let mut session = FoldingSession::new(FoldingMode::PaperExact, params, l.clone());

        for _i in 0..num_iters {
            session.prove_step(&mut f_circuit, &()).unwrap();
        }

        let run = session.finalize(&shape_ccs).unwrap();

        let mcss_public = session.mcss_public();
        let ok = session
            .verify(&shape_ccs, &mcss_public, &run)
            .expect("verify should run");
        assert!(ok, "optimized verification should pass");

        Ok(Transaction {
            utxo_deltas: self.utxo_deltas.clone(),
            proof_like: ProverOutput { proof: () },
        })
    }
}

fn ccs_step_shape() -> Result<CcsStructure<neo_math::F>, SynthesisError> {
    let _span = tracing::debug_span!("dummy circuit").entered();

    tracing::debug!("constructing nop circuit to get initial (stable) ccs shape");

    let cs = ConstraintSystem::new_ref();
    cs.set_optimization_goal(ark_relations::gr1cs::OptimizationGoal::Constraints);

    let mut dummy_tx = StepCircuitBuilder::<DummyMemory<F>>::new(
        Default::default(),
        vec![LedgerOperation::Nop {}],
    );

    let mb = dummy_tx.trace_memory_ops(());
    let irw = InterRoundWires::new(dummy_tx.rom_offset());
    dummy_tx.make_step_circuit(0, &mut mb.constraints(), cs.clone(), irw)?;

    cs.finalize();

    Ok(arkworks_to_neo_ccs(&cs))
}

impl Transaction<ProverOutput> {
    pub fn verify(&self, _changes: BTreeMap<ProgramId, UtxoChange>) {
        // TODO: fill
        //
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        F, LedgerOperation, ProgramId, Transaction, UtxoChange, test_utils::init_test_logging,
    };
    use std::collections::BTreeMap;

    #[test]
    fn test_nop() {
        init_test_logging();

        let changes = vec![].into_iter().collect::<BTreeMap<_, _>>();
        let tx = Transaction::new_unproven(changes.clone(), vec![LedgerOperation::Nop {}]);
        let proof = tx.prove().unwrap();

        proof.verify(changes);
    }

    #[test]
    fn test_starstream_tx_success() {
        init_test_logging();

        let utxo_id1: ProgramId = ProgramId::from(110);
        let utxo_id2: ProgramId = ProgramId::from(300);
        let utxo_id3: ProgramId = ProgramId::from(400);

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
                LedgerOperation::Nop {},
                LedgerOperation::Resume {
                    utxo_id: utxo_id2,
                    input: F::from(0),
                    output: F::from(0),
                },
                LedgerOperation::DropUtxo { utxo_id: utxo_id2 },
                LedgerOperation::Resume {
                    utxo_id: utxo_id3,
                    input: F::from(42),
                    output: F::from(43),
                },
                LedgerOperation::YieldResume {
                    utxo_id: utxo_id3,
                    output: F::from(42),
                },
                LedgerOperation::Yield {
                    utxo_id: utxo_id3,
                    input: F::from(43),
                },
            ],
        );

        let proof = tx.prove().unwrap();

        proof.verify(changes);
    }

    #[test]
    #[should_panic]
    fn test_fail_starstream_tx_resume_mismatch() {
        let utxo_id1: ProgramId = ProgramId::from(110);

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
                LedgerOperation::Nop {},
                LedgerOperation::Resume {
                    utxo_id: utxo_id1,
                    input: F::from(42),
                    output: F::from(43),
                },
                LedgerOperation::YieldResume {
                    utxo_id: utxo_id1,
                    output: F::from(42000),
                },
                LedgerOperation::Yield {
                    utxo_id: utxo_id1,
                    input: F::from(43),
                },
            ],
        );

        let proof = tx.prove().unwrap();

        proof.verify(changes);
    }

    #[test]
    #[should_panic]
    fn test_starstream_tx_invalid_witness() {
        init_test_logging();

        let utxo_id1: ProgramId = ProgramId::from(110);
        let utxo_id2: ProgramId = ProgramId::from(300);
        let utxo_id3: ProgramId = ProgramId::from(400);

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
                LedgerOperation::Nop {},
                LedgerOperation::Resume {
                    utxo_id: utxo_id2,
                    input: F::from(0),
                    output: F::from(0),
                },
                LedgerOperation::DropUtxo { utxo_id: utxo_id2 },
                LedgerOperation::Resume {
                    utxo_id: utxo_id3,
                    input: F::from(42),
                    // Invalid: output should be F::from(43) to match output_after,
                    // but we're providing a mismatched value
                    output: F::from(999),
                },
                LedgerOperation::YieldResume {
                    utxo_id: utxo_id3,
                    output: F::from(42),
                },
                LedgerOperation::Yield {
                    utxo_id: utxo_id3,
                    // Invalid: input should match Resume output but doesn't
                    input: F::from(999),
                },
            ],
        );

        // This should fail during proving because the witness is invalid
        tx.prove().unwrap();
    }
}
