mod circuit;
#[cfg(test)]
mod circuit_test;
// #[cfg(test)]
// mod e2e;
mod abi;
mod logging;
mod memory;
mod neo;

use crate::circuit::InterRoundWires;
pub use crate::circuit::OptionalF;
use crate::memory::IVCMemory;
use crate::memory::twist_and_shout::{TSMemLayouts, TSMemory};
use crate::neo::{StarstreamVm, StepCircuitNeo};
use abi::ledger_operation_from_wit;
use ark_relations::gr1cs::{ConstraintSystem, ConstraintSystemRef, SynthesisError};
use circuit::StepCircuitBuilder;
pub use memory::nebula;
use neo_ajtai::AjtaiSModule;
use neo_fold::pi_ccs::FoldingMode;
use neo_fold::session::{FoldingSession, preprocess_shared_bus_r1cs};
use neo_fold::shard::StepLinkingConfig;
use neo_params::NeoParams;
use rand::SeedableRng as _;
use starstream_mock_ledger::{
    InterleavingInstance, InterleavingWitness, ProcessId, ZkTransactionProof,
};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

pub type F = ark_goldilocks::FpGoldilocks;

pub type ProgramId = F;

pub use abi::commit;

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
        id_prev: OptionalF,
    },
    /// Called by utxo to yield.
    ///
    Yield {
        val: F,
        ret: Option<F>,
        id_prev: OptionalF,
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
        size: F,
        ret: F,
    },
    RefPush {
        val: F,
    },
    Get {
        reff: F,
        offset: F,
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

pub const OPCODE_ARG_COUNT: usize = 4;

#[derive(Copy, Clone, Debug)]
pub enum ArgName {
    Target,
    Val,
    Ret,
    IdPrev,
    Offset,
    Size,
    ProgramHash,
    Caller,
    OwnerId,
    TokenId,
    InterfaceId,
}

impl ArgName {
    // maps argument names to positional indices
    //
    // these need to match the order in the ABI used by the wasm/program vm.
    pub const fn idx(self) -> usize {
        match self {
            ArgName::Target | ArgName::OwnerId | ArgName::TokenId => 0,
            ArgName::Val | ArgName::InterfaceId => 1,
            ArgName::Ret => 2,
            ArgName::IdPrev
            | ArgName::Offset
            | ArgName::Size
            | ArgName::ProgramHash
            | ArgName::Caller => 3,
        }
    }
}

pub fn prove(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
) -> Result<ZkTransactionProof, SynthesisError> {
    logging::setup_logger();

    let output_binding_config = inst.output_binding_config();

    // map all the disjoints vectors of traces (one per process) into a single
    // list, which is simpler to think about for ivc.
    let ops = make_interleaved_trace(&inst, &wit);
    let max_steps = ops.len();

    tracing::info!("making proof, steps {}", ops.len());

    let mut circuit_builder = StepCircuitBuilder::<TSMemory<F>>::new(inst, ops);

    let mb = circuit_builder.trace_memory_ops(());

    let circuit = Arc::new(StepCircuitNeo::new(mb.init_tables()));
    let pre = preprocess_shared_bus_r1cs(Arc::clone(&circuit)).expect("preprocess_shared_bus_r1cs");
    let m = pre.m();

    // params copy-pasted from nightstream tests, this needs review
    let base_params = NeoParams::goldilocks_auto_r1cs_ccs(m).expect("params");
    let params = NeoParams::new(
        base_params.q,
        base_params.eta,
        base_params.d,
        base_params.kappa,
        base_params.m,
        4,  // b
        16, // k_rho
        base_params.T,
        base_params.s,
        base_params.lambda,
    )
    .expect("params");

    let committer = setup_ajtai_committer(m, params.kappa as usize);
    let prover = pre
        .into_prover(params, committer.clone())
        .expect("into_prover (R1csCpu shared-bus config)");

    let mut session = FoldingSession::new(FoldingMode::Optimized, params, committer);

    // TODO: not sound, but not important right now
    session.set_step_linking(StepLinkingConfig::new(vec![(0, 0)]));

    let (constraints, shout, twist) = mb.split();

    prover
        .execute_into_session(
            &mut session,
            StarstreamVm::new(circuit_builder, constraints),
            twist,
            shout,
            max_steps,
        )
        .expect("execute_into_session should succeed");

    let t_prove = Instant::now();
    let run = session
        .fold_and_prove_with_output_binding_auto_simple(prover.ccs(), &output_binding_config)
        .unwrap();
    tracing::info!("proof generated in {} ms", t_prove.elapsed().as_millis());

    let status = session
        .verify_with_output_binding_collected_simple(prover.ccs(), &run, &output_binding_config)
        .unwrap();

    assert!(status, "optimized verification should pass");

    let mcss_public = session.mcss_public();
    let steps_public = session.steps_public();

    let prover_output = ZkTransactionProof::NeoProof {
        proof: run,
        session,
        ccs: prover.ccs().clone(),
        mcss_public,
        steps_public,
    };

    Ok(prover_output)
}

fn setup_ajtai_committer(m: usize, kappa: usize) -> AjtaiSModule {
    let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(42);
    let pp = neo_ajtai::setup(&mut rng, neo_math::D, kappa, m).expect("Ajtai setup");
    AjtaiSModule::new(Arc::new(pp))
}

fn make_interleaved_trace(
    inst: &InterleavingInstance,
    wit: &InterleavingWitness,
) -> Vec<LedgerOperation<crate::F>> {
    let mut ops = vec![];
    let mut id_curr = inst.entrypoint.0;
    let mut id_prev: Option<usize> = None;
    let mut counters: HashMap<usize, usize> = HashMap::new();

    loop {
        let c = counters.entry(id_curr).or_insert(0);

        let Some(trace) = wit.traces.get(id_curr) else {
            // No trace for this process, this indicates the end of the transaction
            // as the entrypoint script has finished and not jumped to another process.
            break;
        };

        if *c >= trace.len() {
            // We've reached the end of the current trace. This is the end.
            break;
        }

        let instr = trace[*c].clone();
        *c += 1;

        match instr {
            starstream_mock_ledger::WitLedgerEffect::Resume { target, .. } => {
                id_prev = Some(id_curr);
                id_curr = target.0;
            }
            starstream_mock_ledger::WitLedgerEffect::Yield { .. } => {
                let parent = id_prev.expect("Yield called without a parent process");
                let old_id_curr = id_curr;
                id_curr = parent;
                id_prev = Some(old_id_curr);
            }
            starstream_mock_ledger::WitLedgerEffect::Burn { .. } => {
                let parent = id_prev.expect("Burn called without a parent process");
                let old_id_curr = id_curr;
                id_curr = parent;
                id_prev = Some(old_id_curr);
            }
            _ => {}
        }

        let op = ledger_operation_from_wit(&instr);

        ops.push(op);
    }

    ops
}

fn ccs_step_shape() -> Result<(ConstraintSystemRef<F>, TSMemLayouts), SynthesisError> {
    let _span = tracing::debug_span!("dummy circuit").entered();

    tracing::debug!("constructing nop circuit to get initial (stable) ccs shape");

    let cs = ConstraintSystem::new_ref();
    cs.set_optimization_goal(ark_relations::gr1cs::OptimizationGoal::Constraints);

    let hash = starstream_mock_ledger::Hash([0u8; 32], std::marker::PhantomData);

    let inst = InterleavingInstance {
        host_calls_roots: vec![],
        host_calls_lens: vec![],
        process_table: vec![hash],
        is_utxo: vec![false],
        must_burn: vec![false],
        n_inputs: 0,
        n_new: 0,
        n_coords: 1,
        ownership_in: vec![None],
        ownership_out: vec![None],
        entrypoint: ProcessId(0),
        input_states: vec![],
    };

    let mut dummy_tx = StepCircuitBuilder::<TSMemory<F>>::new(inst, vec![LedgerOperation::Nop {}]);

    let mb = dummy_tx.trace_memory_ops(());

    let irw = InterRoundWires::new(
        F::from(dummy_tx.p_len() as u64),
        dummy_tx.instance.entrypoint.0 as u64,
    );

    let mut running_mem = mb.constraints();

    dummy_tx.make_step_circuit(0, &mut running_mem, cs.clone(), irw)?;

    cs.finalize();

    let mem_spec = running_mem.ts_mem_layouts();

    Ok((cs, mem_spec))
}
