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

use abi::ledger_operation_from_wit;
use ark_relations::gr1cs::SynthesisError;
pub use memory::nebula;
pub use optional::{OptionalF, OptionalFpVar};
use starstream_interleaving_spec::{InterleavingInstance, InterleavingWitness, ZkTransactionProof};
use std::collections::BTreeMap;

pub type F = ark_goldilocks::FpGoldilocks;

pub type ProgramId = F;

pub use abi::commit;
pub use ledger_operation::LedgerOperation;

/// Build and check the interleaving circuit for the whole transaction using the
/// Nebula memory argument.
///
/// This drives the step circuit one step at a time and asserts each step's
/// constraint system is satisfied, with Nebula as the memory backend; the final
/// step runs Nebula's IS/FS and RS/WS commitment reconciliation.
///
/// NOTE: the verifiable, folded proof artifact was previously produced by the
/// Twist/Shout neo-fold session, which has been removed. Until the Nightstream
/// dependency is bumped onto the pure-R1CS folding backend, this only checks the
/// circuit is satisfiable and returns a placeholder [`ZkTransactionProof::Dummy`].
pub fn prove(
    inst: InterleavingInstance,
    wit: InterleavingWitness,
) -> Result<ZkTransactionProof, SynthesisError> {
    logging::setup_logger();

    // Build the per-step R1CS (Nebula memory) + one assignment per step; each
    // step's constraint system is checked satisfiable inside.
    let (shape, assignments) = folding::extract_r1cs_and_assignments(inst, wit)?;

    tracing::info!(
        steps = assignments.len(),
        n = shape.r1cs.n,
        m = shape.r1cs.m,
        "folding interleaving trace with neo-fold-clean (r1cs_f_prime)"
    );

    // Fold all steps and verify the IVC proof.
    //
    // NOTE: this first proof is STATELESS (no per-step cross-linking yet) and
    // runs with Nebula's Poseidon commitment disabled (already unsound while the
    // Fiat-Shamir challenges are stubbed) to keep the circuit foldable. The
    // verifiable artifact is checked here but not yet threaded back into
    // ZkTransactionProof (verify() still uses the placeholder) — both are
    // follow-ups. See folding.rs.
    folding::fold_and_verify(&shape, &assignments);

    Ok(ZkTransactionProof::Dummy)
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
