mod abi;
mod circuit;
#[cfg(test)]
mod circuit_test;
mod coroutine_args_gadget;
mod execution_switches;
mod handler_stack_gadget;
mod ledger_operation;
mod logging;
mod memory;
mod memory_tags;
mod neo;
mod opcode_dsl;
mod optional;
mod program_hash_gadget;
mod program_state;
mod ref_arena_gadget;
mod switchboard;

use crate::circuit::{InterRoundWires, IvcWireLayout};
use crate::memory::IVCMemory;
use crate::memory::twist_and_shout::{TSMemLayouts, TSMemory};
use crate::neo::{CHUNK_SIZE, StarstreamVm, StepCircuitNeo};
use abi::ledger_operation_from_wit;
use ark_relations::gr1cs::{ConstraintSystem, ConstraintSystemRef, SynthesisError};
use circuit::StepCircuitBuilder;
pub use memory::nebula;
use neo_ajtai::AjtaiSModule;
use neo_fold::pi_ccs::FoldingMode;
use neo_fold::session::{FoldingSession, preprocess_shared_bus_r1cs};
use neo_fold::shard::StepLinkingConfig;
use neo_params::NeoParams;
pub use optional::{OptionalF, OptionalFpVar};
use rand::SeedableRng as _;
use starstream_interleaving_spec::{
    InterleavingInstance, InterleavingWitness, ProcessId, ZkTransactionProof,
};
use std::sync::Arc;
use std::time::Instant;

pub type F = ark_goldilocks::FpGoldilocks;

pub type ProgramId = F;

pub use abi::commit;
pub use ledger_operation::LedgerOperation;

pub fn prove(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
) -> Result<ZkTransactionProof, SynthesisError> {
    logging::setup_logger();

    let output_binding_config = inst.output_binding_config();

    // map all the disjoints vectors of traces (one per process) into a single
    // list, which is simpler to think about for ivc.
    let mut ops = make_interleaved_trace(&inst, &wit);

    ops.resize(
        ops.len().next_multiple_of(CHUNK_SIZE),
        crate::LedgerOperation::Nop {},
    );

    let max_steps = ops.len();

    tracing::info!("making proof, steps {}", ops.len());

    let mut circuit_builder = StepCircuitBuilder::<TSMemory<F>>::new(inst, ops);

    let mb = circuit_builder.trace_memory_ops(());

    let circuit = Arc::new(StepCircuitNeo::new(mb.init_tables()));

    let pre = {
        let now = std::time::Instant::now();
        let pre =
            preprocess_shared_bus_r1cs(Arc::clone(&circuit)).expect("preprocess_shared_bus_r1cs");
        tracing::info!(
            "preprocess_shared_bus_r1cs took {}ms",
            now.elapsed().as_millis()
        );

        pre
    };

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

    session.set_step_linking(StepLinkingConfig::new(neo::ivc_step_linking_pairs()));

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
    let mut next_op_idx = vec![0usize; inst.process_table.len()];
    let mut on_yield = vec![true; inst.process_table.len()];
    let mut yield_to: Vec<Option<usize>> = vec![None; inst.process_table.len()];

    let expected_len: usize = wit.traces.iter().map(|t| t.len()).sum();

    loop {
        let c = next_op_idx[id_curr];

        let Some(trace) = wit.traces.get(id_curr) else {
            // No trace for this process, this indicates the end of the transaction
            // as the entrypoint script has finished and not jumped to another process.
            break;
        };

        if c >= trace.len() {
            // We've reached the end of the current trace. This is the end.
            break;
        }

        let instr = trace[c].clone();
        next_op_idx[id_curr] += 1;

        match instr {
            starstream_interleaving_spec::WitLedgerEffect::Resume { target, .. } => {
                if on_yield[target.0] && !inst.is_utxo[id_curr] {
                    yield_to[target.0] = Some(id_curr);
                    on_yield[target.0] = false;
                }
                id_prev = Some(id_curr);
                id_curr = target.0;
            }
            starstream_interleaving_spec::WitLedgerEffect::Yield { .. } => {
                on_yield[id_curr] = true;
                let Some(parent) = yield_to[id_curr] else {
                    break;
                };
                let old_id_curr = id_curr;
                id_curr = parent;
                id_prev = Some(old_id_curr);
            }
            starstream_interleaving_spec::WitLedgerEffect::Return {} => {
                if let Some(parent) = yield_to[id_curr] {
                    let old_id_curr = id_curr;
                    id_curr = parent;
                    id_prev = Some(old_id_curr);
                } else if id_curr != inst.entrypoint.0 {
                    break;
                }
            }
            starstream_interleaving_spec::WitLedgerEffect::Burn { .. } => {
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

    assert_eq!(
        ops.len(),
        expected_len,
        "interleaved trace doesn't match original length"
    );

    ops
}

fn ccs_step_shape() -> Result<(ConstraintSystemRef<F>, TSMemLayouts, IvcWireLayout), SynthesisError>
{
    let _span = tracing::info_span!("dummy circuit").entered();

    tracing::debug!("constructing nop circuit to get initial (stable) ccs shape");

    let cs = ConstraintSystem::new_ref();
    cs.set_optimization_goal(ark_relations::gr1cs::OptimizationGoal::Constraints);

    let hash = starstream_interleaving_spec::Hash([0u8; 32], std::marker::PhantomData);

    let inst = InterleavingInstance {
        host_calls_roots: vec![],
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

    let irw = InterRoundWires::new(dummy_tx.instance.entrypoint.0 as u64);

    let mut running_mem = mb.constraints();

    let (_irw, _mem, captured_layout) =
        dummy_tx.make_step_circuit(0, &mut running_mem, cs.clone(), irw, true)?;
    let ivc_layout = captured_layout.expect("ivc layout requested");

    cs.finalize();

    let mem_spec = running_mem.ts_mem_layouts();

    Ok((cs, mem_spec, ivc_layout))
}
