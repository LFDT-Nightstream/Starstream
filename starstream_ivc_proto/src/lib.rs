mod circuit;
#[cfg(test)]
mod circuit_test;
// #[cfg(test)]
// mod e2e;
mod goldilocks;
mod memory;
mod neo;
mod poseidon2;
#[cfg(test)]
mod test_utils;

use std::collections::HashMap;
use std::time::Instant;

use crate::circuit::InterRoundWires;
use crate::memory::IVCMemory;
use crate::nebula::tracer::{NebulaMemory, NebulaMemoryParams};
use crate::neo::StepCircuitNeo;
use crate::neo::arkworks_to_neo_ccs;
use ark_relations::gr1cs::{ConstraintSystem, SynthesisError};
use circuit::StepCircuitBuilder;
use goldilocks::FpGoldilocks;
pub use memory::nebula;
use neo_ajtai::AjtaiSModule;
use neo_ccs::CcsStructure;
use neo_fold::pi_ccs::FoldingMode;
use neo_fold::session::FoldingSession;
use neo_params::NeoParams;
use starstream_mock_ledger::InterleavingInstance;

type F = FpGoldilocks;

pub type ProgramId = F;

#[derive(Debug, Clone)]
pub enum LedgerOperation<F> {
    /// A call to starstream_resume.
    ///
    /// This stores the input and outputs in memory, and sets the
    /// current_program for the next iteration to `utxo_id`.
    ///
    /// Then, when evaluating Yield and YieldResume, we match the input/output
    /// with the corresponding value.
    Resume {
        target: F,
        val: F,
        ret: F,
        id_prev: Option<F>,
    },
    /// Called by utxo to yield.
    ///
    Yield {
        val: F,
        ret: Option<F>,
        id_prev: Option<F>,
    },
    ProgramHash {
        target: F,
        program_hash: F,
    },
    NewUtxo {
        program_hash: F,
        val: F,
        target: F,
    },
    NewCoord {
        program_hash: F,
        val: F,
        target: F,
    },
    Burn {
        ret: F,
    },
    Activation {
        val: F,
        caller: F,
    },
    Init {
        val: F,
        caller: F,
    },
    Bind {
        owner_id: F,
    },
    Unbind {
        token_id: F,
    },

    NewRef {
        val: F,
        ret: F,
    },
    Get {
        reff: F,
        ret: F,
    },
    InstallHandler {
        interface_id: F,
    },
    UninstallHandler {
        interface_id: F,
    },
    GetHandlerFor {
        interface_id: F,
        handler_id: F,
    },
    /// Auxiliary instructions.
    ///
    /// Nop is used as a dummy instruction to build the circuit layout on the
    /// verifier side.
    Nop {},
}

pub struct ProverOutput {
    // pub proof: Proof,
    pub proof: (),
}

const SCAN_BATCH_SIZE: usize = 10;
// const SCAN_BATCH_SIZE: usize = 200;

pub fn prove(inst: InterleavingInstance) -> Result<ProverOutput, SynthesisError> {
    let shape_ccs = ccs_step_shape(inst.clone())?;

    // map all the disjoints vectors of traces (one per process) into a single
    // list, which is simpler to think about for ivc.
    let ops = make_interleaved_trace(&inst);

    println!("making proof, steps {}", ops.len());

    let circuit_builder = StepCircuitBuilder::<NebulaMemory<SCAN_BATCH_SIZE>>::new(inst, ops);

    let n = shape_ccs.n.max(shape_ccs.m);

    let (mut f_circuit, num_iters) = StepCircuitNeo::new(
        circuit_builder,
        shape_ccs.clone(),
        NebulaMemoryParams {
            // the proof system is still too slow to run the poseidon commitments, especially when iterating.
            unsound_disable_poseidon_commitment: true,
        },
    );

    // since we are using square matrices, n = m
    neo::setup_ajtai_for_dims(n);

    let l = AjtaiSModule::from_global_for_dims(neo_math::D, n).expect("AjtaiSModule init");

    let mut params = NeoParams::goldilocks_auto_r1cs_ccs(n)
        .expect("goldilocks_auto_r1cs_ccs should find valid params");

    params.b = 3;

    let mut session = FoldingSession::new(FoldingMode::Optimized, params, l.clone());
    session.unsafe_allow_unlinked_steps();

    for _i in 0..num_iters {
        let now = Instant::now();
        session.add_step(&mut f_circuit, &()).unwrap();
        tracing::info!("step added in {} ms", now.elapsed().as_millis());
    }

    let now = Instant::now();
    let run = session.fold_and_prove(&shape_ccs).unwrap();
    tracing::info!("proof generated in {} ms", now.elapsed().as_millis());

    let mcss_public = session.mcss_public();
    let ok = session
        .verify(&shape_ccs, &mcss_public, &run)
        .expect("verify should run");
    assert!(ok, "optimized verification should pass");

    // TODO: extract the actual proof
    Ok(ProverOutput { proof: () })
}

fn make_interleaved_trace(inst: &InterleavingInstance) -> Vec<LedgerOperation<crate::F>> {
    let mut ops = vec![];
    let mut id_curr = inst.entrypoint.0;
    let mut id_prev: Option<usize> = None;
    let mut counters: HashMap<usize, usize> = HashMap::new();

    loop {
        let c = counters.entry(id_curr).or_insert(0);

        let Some(trace) = inst.host_calls_roots.get(id_curr) else {
            // No trace for this process, this indicates the end of the transaction
            // as the entrypoint script has finished and not jumped to another process.
            break;
        };

        if *c >= trace.trace.len() {
            // We've reached the end of the current trace. This is the end.
            break;
        }

        let instr = trace.trace[*c].clone();
        *c += 1;

        let op = match instr {
            starstream_mock_ledger::WitLedgerEffect::Resume {
                target,
                val,
                ret,
                id_prev: op_id_prev,
            } => {
                id_prev = Some(id_curr);
                id_curr = target.0;

                LedgerOperation::Resume {
                    target: (target.0 as u64).into(),
                    // TODO: figure out how to manage these
                    // maybe for now just assume that these are short/fixed size
                    val: F::from(val.0),
                    ret: F::from(ret.0),
                    id_prev: op_id_prev.map(|p| (p.0 as u64).into()),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::Yield {
                val,
                ret,
                id_prev: op_id_prev,
            } => {
                let parent = id_prev.expect("Yield called without a parent process");
                let old_id_curr = id_curr;
                id_curr = parent;
                id_prev = Some(old_id_curr);

                LedgerOperation::Yield {
                    val: F::from(val.0),
                    ret: ret.map(|ret| F::from(ret.0)),
                    id_prev: op_id_prev.map(|p| (p.0 as u64).into()),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::Burn { ret } => {
                let parent = id_prev.expect("Burn called without a parent process");
                let old_id_curr = id_curr;
                id_curr = parent;
                id_prev = Some(old_id_curr);

                LedgerOperation::Burn {
                    ret: F::from(ret.0),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::NewUtxo {
                program_hash,
                val,
                id,
            } => LedgerOperation::NewUtxo {
                program_hash: F::from(program_hash.0[0] as u64),
                val: F::from(val.0),
                target: (id.0 as u64).into(),
            },
            starstream_mock_ledger::WitLedgerEffect::NewCoord {
                program_hash,
                val,
                id,
            } => LedgerOperation::NewCoord {
                program_hash: F::from(program_hash.0[0] as u64),
                val: F::from(val.0),
                target: (id.0 as u64).into(),
            },
            starstream_mock_ledger::WitLedgerEffect::Activation { val, caller } => {
                LedgerOperation::Activation {
                    val: F::from(val.0),
                    caller: (caller.0 as u64).into(),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::Init { val, caller } => {
                LedgerOperation::Init {
                    val: F::from(val.0),
                    caller: (caller.0 as u64).into(),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::Bind { owner_id } => LedgerOperation::Bind {
                owner_id: (owner_id.0 as u64).into(),
            },
            starstream_mock_ledger::WitLedgerEffect::Unbind { token_id } => {
                LedgerOperation::Unbind {
                    token_id: (token_id.0 as u64).into(),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::NewRef { val, ret } => {
                LedgerOperation::NewRef {
                    val: value_to_field(val),
                    ret: F::from(ret.0),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::Get { reff, ret } => LedgerOperation::Get {
                reff: F::from(reff.0),
                ret: value_to_field(ret),
            },
            starstream_mock_ledger::WitLedgerEffect::ProgramHash {
                target,
                program_hash,
            } => LedgerOperation::ProgramHash {
                target: (target.0 as u64).into(),
                program_hash: F::from(program_hash.0[0]),
            },
            starstream_mock_ledger::WitLedgerEffect::InstallHandler { interface_id } => {
                LedgerOperation::InstallHandler {
                    interface_id: F::from(interface_id.0[0]),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::UninstallHandler { interface_id } => {
                LedgerOperation::UninstallHandler {
                    interface_id: F::from(interface_id.0[0]),
                }
            }
            starstream_mock_ledger::WitLedgerEffect::GetHandlerFor {
                interface_id,
                handler_id,
            } => LedgerOperation::GetHandlerFor {
                interface_id: F::from(interface_id.0[0]),
                handler_id: (handler_id.0 as u64).into(),
            },
        };

        ops.push(op);
    }

    ops
}

fn value_to_field(val: starstream_mock_ledger::Value) -> F {
    F::from(val.0[0])
}

fn ccs_step_shape(inst: InterleavingInstance) -> Result<CcsStructure<neo_math::F>, SynthesisError> {
    let _span = tracing::debug_span!("dummy circuit").entered();

    tracing::debug!("constructing nop circuit to get initial (stable) ccs shape");

    let cs = ConstraintSystem::new_ref();
    cs.set_optimization_goal(ark_relations::gr1cs::OptimizationGoal::Constraints);

    // let mut dummy_tx = StepCircuitBuilder::<DummyMemory<F>>::new(
    //     Default::default(),
    //     vec![LedgerOperation::Nop {}],
    // );
    //
    let mut dummy_tx = StepCircuitBuilder::<NebulaMemory<SCAN_BATCH_SIZE>>::new(
        inst,
        vec![LedgerOperation::Nop {}],
    );

    // let mb = dummy_tx.trace_memory_ops(());
    let mb = dummy_tx.trace_memory_ops(NebulaMemoryParams {
        unsound_disable_poseidon_commitment: true,
    });

    let irw = InterRoundWires::new(
        F::from(dummy_tx.p_len() as u64),
        dummy_tx.instance.entrypoint.0 as u64,
    );
    dummy_tx.make_step_circuit(0, &mut mb.constraints(), cs.clone(), irw)?;

    cs.finalize();

    Ok(arkworks_to_neo_ccs(&cs))
}

#[cfg(test)]
mod tests {
    // use crate::{F, LedgerOperation, ProgramId, test_utils::init_test_logging};
    // use std::collections::BTreeMap;

    // #[test]
    // fn test_starstream_tx_success() {
    //     init_test_logging();

    //     let utxo_id1: ProgramId = ProgramId::from(110);
    //     let utxo_id2: ProgramId = ProgramId::from(300);
    //     let utxo_id3: ProgramId = ProgramId::from(400);

    //     let changes = vec![
    //         (
    //             utxo_id1,
    //             UtxoChange {
    //                 output_before: F::from(5),
    //                 output_after: F::from(5),
    //                 consumed: false,
    //             },
    //         ),
    //         (
    //             utxo_id2,
    //             UtxoChange {
    //                 output_before: F::from(4),
    //                 output_after: F::from(0),
    //                 consumed: true,
    //             },
    //         ),
    //         (
    //             utxo_id3,
    //             UtxoChange {
    //                 output_before: F::from(5),
    //                 output_after: F::from(43),
    //                 consumed: false,
    //             },
    //         ),
    //     ]
    //     .into_iter()
    //     .collect::<BTreeMap<_, _>>();

    //     let tx = Transaction::new_unproven(
    //         changes.clone(),
    //         vec![
    //             LedgerOperation::Nop {},
    //             LedgerOperation::Resume {
    //                 target: utxo_id2,
    //                 val: F::from(0),
    //                 ret: F::from(0),
    //             },
    //             LedgerOperation::DropUtxo { utxo_id: utxo_id2 },
    //             LedgerOperation::Resume {
    //                 target: utxo_id3,
    //                 val: F::from(42),
    //                 ret: F::from(43),
    //             },
    //             LedgerOperation::YieldResume {
    //                 utxo_id: utxo_id3,
    //                 output: F::from(42),
    //             },
    //             LedgerOperation::Yield {
    //                 utxo_id: utxo_id3,
    //                 input: F::from(43),
    //             },
    //         ],
    //     );

    //     let proof = tx.prove().unwrap();

    //     proof.verify(changes);
    // }

    // #[test]
    // #[should_panic]
    // fn test_fail_starstream_tx_resume_mismatch() {
    //     let utxo_id1: ProgramId = ProgramId::from(110);

    //     let changes = vec![(
    //         utxo_id1,
    //         UtxoChange {
    //             output_before: F::from(0),
    //             output_after: F::from(43),
    //             consumed: false,
    //         },
    //     )]
    //     .into_iter()
    //     .collect::<BTreeMap<_, _>>();

    //     let tx = Transaction::new_unproven(
    //         changes.clone(),
    //         vec![
    //             LedgerOperation::Nop {},
    //             LedgerOperation::Resume {
    //                 target: utxo_id1,
    //                 val: F::from(42),
    //                 ret: F::from(43),
    //             },
    //             LedgerOperation::YieldResume {
    //                 utxo_id: utxo_id1,
    //                 output: F::from(42000),
    //             },
    //             LedgerOperation::Yield {
    //                 utxo_id: utxo_id1,
    //                 input: F::from(43),
    //             },
    //         ],
    //     );

    //     let proof = tx.prove().unwrap();

    //     proof.verify(changes);
    // }

    // #[test]
    // #[should_panic]
    // fn test_starstream_tx_invalid_witness() {
    //     init_test_logging();

    //     let utxo_id1: ProgramId = ProgramId::from(110);
    //     let utxo_id2: ProgramId = ProgramId::from(300);
    //     let utxo_id3: ProgramId = ProgramId::from(400);

    //     let changes = vec![
    //         (
    //             utxo_id1,
    //             UtxoChange {
    //                 output_before: F::from(5),
    //                 output_after: F::from(5),
    //                 consumed: false,
    //             },
    //         ),
    //         (
    //             utxo_id2,
    //             UtxoChange {
    //                 output_before: F::from(4),
    //                 output_after: F::from(0),
    //                 consumed: true,
    //             },
    //         ),
    //         (
    //             utxo_id3,
    //             UtxoChange {
    //                 output_before: F::from(5),
    //                 output_after: F::from(43),
    //                 consumed: false,
    //             },
    //         ),
    //     ]
    //     .into_iter()
    //     .collect::<BTreeMap<_, _>>();

    //     let tx = Transaction::new_unproven(
    //         changes.clone(),
    //         vec![
    //             LedgerOperation::Nop {},
    //             LedgerOperation::Resume {
    //                 target: utxo_id2,
    //                 val: F::from(0),
    //                 ret: F::from(0),
    //             },
    //             LedgerOperation::DropUtxo { utxo_id: utxo_id2 },
    //             LedgerOperation::Resume {
    //                 target: utxo_id3,
    //                 val: F::from(42),
    //                 // Invalid: output should be F::from(43) to match output_after,
    //                 // but we're providing a mismatched value
    //                 ret: F::from(999),
    //             },
    //             LedgerOperation::YieldResume {
    //                 utxo_id: utxo_id3,
    //                 output: F::from(42),
    //             },
    //             LedgerOperation::Yield {
    //                 utxo_id: utxo_id3,
    //                 // Invalid: input should match Resume output but doesn't
    //                 input: F::from(999),
    //             },
    //         ],
    //     );

    //     // This should fail during proving because the witness is invalid
    //     tx.prove().unwrap();
    // }
}
