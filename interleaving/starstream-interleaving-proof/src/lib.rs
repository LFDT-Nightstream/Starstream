pub mod abi;
mod circuit;
#[cfg(test)]
mod circuit_test;
mod coroutine_args_gadget;
mod execution_switches;
mod folding;
mod handler_stack_gadget;
mod ledger_operation;
mod logging;
mod memory;
mod must_enter_gadget;
mod opcode_dsl;
mod optional;
mod program_hash_gadget;
mod program_state;
mod ref_arena_gadget;
mod switchboard;

pub use abi::commit;
use abi::ledger_operation_from_wit;
use ark_relations::gr1cs::SynthesisError;
pub use ledger_operation::LedgerOperation;
pub use memory::nebula;
pub use optional::{OptionalF, OptionalFpVar};
use starstream_interleaving_spec::{InterleavingInstance, InterleavingWitness, ZkTransactionProof};
use std::{collections::BTreeMap, num::NonZeroUsize};

pub type F = ark_goldilocks::FpGoldilocks;
pub type ProgramId = F;

/// Default number of interleaving sub-steps folded as one R1CS instance. Each
/// fold step pays the (largely fixed) recursion/NIFS overhead once, so batching
/// B sub-steps amortizes it across B. The per-batch program-state continuity is
/// enforced in-circuit (`IvcWiresVars::enforce_equal`); only the batch boundary
/// is cross-linked by the fold.
///
/// This is only the default for [`crate::prove`]; the batch size is a per-call
/// argument ([`crate::prove_with_batch_size`]) since it's a pure performance
/// knob — the verifier re-derives preprocessing from the carried R1CS shape, so
/// each proof may use a different B. Bump until the marginal gain flattens.
pub const DEFAULT_BATCH_SIZE: usize = 4;

/// Build the interleaving circuit, extract per-step R1CS assignments, and fold
/// them into a verifiable transaction proof, using [`circuit::DEFAULT_BATCH_SIZE`].
pub fn prove(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
) -> Result<ZkTransactionProof, SynthesisError> {
    prove_with_batch_size(inst, wit, DEFAULT_BATCH_SIZE.try_into().unwrap())
}

pub fn prove_with_batch_size(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
    batch_size: NonZeroUsize,
) -> Result<ZkTransactionProof, SynthesisError> {
    logging::setup_logger();

    let anchor = starstream_interleaving_spec::expected_initial_semantic_state_anchor(&inst);

    let t_extract = std::time::Instant::now();
    let (shape, assignments) = folding::extract_r1cs_and_assignments(inst, wit, batch_size)?;
    let extract_ms = t_extract.elapsed().as_millis();

    let batches = assignments.len();
    tracing::info!(
        batches,
        batch_size,
        n = shape.r1cs.n,
        m = shape.r1cs.m,
        extract_ms,
        "folding interleaving trace with neo-fold-clean (r1cs_f_prime)"
    );

    let t_fold = std::time::Instant::now();
    let (r1cs, plan, audit) = folding::fold(&shape, &assignments, anchor, batch_size.get());
    let fold_ms = t_fold.elapsed().as_millis();
    tracing::info!(
        batches,
        batch_size,
        extract_ms,
        fold_ms,
        per_batch_ms = fold_ms / (batches.max(1) as u128),
        "interleaving proof produced"
    );

    Ok(ZkTransactionProof::Neo {
        r1cs: Box::new(r1cs),
        plan: Box::new(plan),
        audit: Box::new(audit),
    })
}

pub(crate) fn make_interleaved_trace(
    inst: &InterleavingInstance,
    wit: &InterleavingWitness,
) -> Vec<LedgerOperation<crate::F>> {
    let mut ops = vec![];
    let mut id_curr = inst.entrypoint.0;
    let mut next_op_idx = vec![0usize; inst.process_table.len()];
    let mut on_yield = vec![true; inst.process_table.len()];
    let mut yield_to: Vec<Option<usize>> = vec![None; inst.process_table.len()];
    let mut handler_stack: BTreeMap<[u64; 4], Vec<usize>> = BTreeMap::new();

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
                id_curr = target.0;
            }
            starstream_interleaving_spec::WitLedgerEffect::InstallHandler { interface_id } => {
                handler_stack
                    .entry(interface_id.0)
                    .or_default()
                    .push(id_curr);
            }
            starstream_interleaving_spec::WitLedgerEffect::UninstallHandler { interface_id } => {
                let stack = handler_stack.entry(interface_id.0).or_default();

                stack
                    .pop()
                    .expect("UninstallHandler with empty stack in interleaving trace");
            }
            starstream_interleaving_spec::WitLedgerEffect::CallEffectHandler {
                interface_id,
                ..
            } => {
                let target = *handler_stack
                    .get(&interface_id.0)
                    .and_then(|stack| stack.last())
                    .expect("CallEffectHandler with empty stack in interleaving trace");
                id_curr = target;
            }
            starstream_interleaving_spec::WitLedgerEffect::Yield { .. } => {
                on_yield[id_curr] = true;
                let Some(parent) = yield_to[id_curr] else {
                    break;
                };
                id_curr = parent;
            }
            starstream_interleaving_spec::WitLedgerEffect::Return {} => {
                if let Some(parent) = yield_to[id_curr] {
                    id_curr = parent;
                } else if id_curr != inst.entrypoint.0 {
                    break;
                }
            }
            starstream_interleaving_spec::WitLedgerEffect::Burn {} => {}
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
