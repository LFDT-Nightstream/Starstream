//! Transaction-only chain: inputs/preloads, UTXO outputs/ABIs, coordinator, finish.
//! Records have distinct tags and fixed lengths, padded to eight-field blocks.
//! Roots are transaction data, not persistent ledger storage. ABI duplicates stay.
//! TODO(proof): Bind the initial zero and expected final digest via the proof API;
//! the satisfiability API checks endpoints on the host and RAM is not yet proved.
use neo_math::F;
use p3_field::{PrimeCharacteristicRing, PrimeField64};
use starstream_interleaving_spec::TransactionStatement;

use crate::{
    Error, TraceCommitments, Unsatisfied, ccs::layout::*, ivc_state::CoroutineId, opcode::Opcode,
};

// 32-bit tag: ASCII "ST" in the high two bytes, record kind in the low two bytes.
const RECORD_PREFIX: u64 = ((b'S' as u64) << 24) | ((b'T' as u64) << 16);
const INPUT: u64 = RECORD_PREFIX | 1;
const INPUT_METHOD: u64 = RECORD_PREFIX | 2;
const OUTPUT: u64 = RECORD_PREFIX | 3;
const OUTPUT_METHOD: u64 = RECORD_PREFIX | 4;
const CONSUMED: u64 = RECORD_PREFIX | 5;
const COORDINATOR: u64 = RECORD_PREFIX | 6;
const FINISH: u64 = RECORD_PREFIX | 7;

pub(crate) fn schema(op: Opcode) -> Vec<(usize, F)> {
    let (tag, columns): (_, Vec<usize>) = match op {
        Opcode::SetStorage => (
            INPUT,
            std::iter::once(COL_BOUNDARY_UTXO)
                .chain(COL_ARGUMENT_ROOT)
                .collect(),
        ),
        Opcode::PreloadMethod => (INPUT_METHOD, COL_METHOD_HASH_VALUE.to_vec()),
        Opcode::GetStorage => (
            OUTPUT,
            std::iter::once(COL_BOUNDARY_UTXO)
                .chain(COL_RESULT_ROOT)
                .chain(COL_OUT)
                .collect(),
        ),
        Opcode::ReadAbi => (OUTPUT_METHOD, COL_METHOD_HASH_VALUE.to_vec()),
        Opcode::SkipConsumed => (
            CONSUMED,
            std::iter::once(COL_BOUNDARY_UTXO).chain(COL_IN).collect(),
        ),
        Opcode::FinalizeCoordinator => (
            COORDINATOR,
            std::iter::once(COL_EVENT_OWNER).chain(COL_IN).collect(),
        ),
        Opcode::FinishTransaction => (FINISH, vec![]),
        _ => return vec![],
    };
    std::iter::once((COL_ONE, F::new(tag)))
        .chain(columns.into_iter().map(|c| (c, F::ONE)))
        .collect()
}

fn absorb(mut chain: [F; 4], words: &[F]) -> [F; 4] {
    for chunk in words.chunks(8) {
        let mut block = [F::ZERO; 8];
        block[..chunk.len()].copy_from_slice(chunk);
        chain = neo_application::event_commitment::commit_block(chain, block);
    }
    chain
}

/// Compute the verifier's transaction digest from ledger inputs and proposed
/// outputs/instance roots. All allocated UTXOs (including consumed ones) and
/// Coord(1) must be represented. Coord(0) is only a sentinel.
/// TODO: Generalize coordinator enumeration when coordinator calls are modeled.
pub fn transaction_commitment(
    statement: &TransactionStatement,
    roots: &TraceCommitments,
) -> Result<[u64; 4], Error> {
    let reject = || Error::from(Unsatisfied::TransactionCommitment);
    let canonical = |values: &[u64]| -> Result<Vec<F>, Error> {
        values
            .iter()
            .map(|&x| {
                if x < F::ORDER_U64 {
                    Ok(F::new(x))
                } else {
                    Err(reject())
                }
            })
            .collect()
    };
    let coordinators =
        roots
            .iter()
            .filter_map(|(&packed, root)| match CoroutineId::from_packed(packed) {
                id @ CoroutineId::Coord(_) => Some((id, root)),
                CoroutineId::Utxo(_) => None,
            });
    // TODO: When the circuit enumerates multiple coordinators, require contiguous
    // IDs starting at 1 (excluding the Coord(0) sentinel), as with the UTXO scan.
    if coordinators
        .clone()
        .map(|(id, _)| id)
        .ne([CoroutineId::Coord(1)])
    {
        return Err(reject());
    }
    let mut chain = [F::ZERO; 4];
    let mut record = |tag: u64, values: Vec<F>| {
        chain = absorb(
            chain,
            &std::iter::once(F::new(tag))
                .chain(values)
                .collect::<Vec<_>>(),
        );
    };
    for (id, input) in statement.inputs.iter().enumerate() {
        let id = u32::try_from(id).map_err(|_| reject())?;
        let packed = CoroutineId::Utxo(id).checked_encoded().ok_or_else(reject)?;
        record(
            INPUT,
            std::iter::once(F::new(u64::from(packed)))
                .chain(canonical(&input.storage.0)?)
                .collect(),
        );
        for method in &input.methods {
            record(
                INPUT_METHOD,
                method.0.map(|x| F::new(u64::from(x))).to_vec(),
            );
        }
    }
    let mut outputs = statement.outputs.iter().peekable();
    let mut next_utxo = 0u64;
    let utxos = roots
        .iter()
        .filter_map(|(&packed, root)| match CoroutineId::from_packed(packed) {
            CoroutineId::Utxo(id) => Some((id, root)),
            CoroutineId::Coord(_) => None,
        });
    for (id, root) in utxos {
        if u64::from(id) != next_utxo {
            return Err(reject());
        }
        if outputs
            .peek()
            .is_some_and(|o| u64::from(o.utxo) == next_utxo)
        {
            let out = outputs.next().unwrap();
            record(
                OUTPUT,
                std::iter::once(CoroutineId::Utxo(id).field())
                    .chain(canonical(&out.storage.0)?)
                    .chain(canonical(root)?)
                    .collect(),
            );
            for method in &out.methods {
                record(
                    OUTPUT_METHOD,
                    method.0.map(|x| F::new(u64::from(x))).to_vec(),
                );
            }
        } else {
            record(
                CONSUMED,
                std::iter::once(CoroutineId::Utxo(id).field())
                    .chain(canonical(root)?)
                    .collect(),
            );
        }
        next_utxo += 1;
    }
    if outputs.next().is_some() || statement.inputs.len() as u64 > next_utxo {
        return Err(reject());
    }
    for (id, root) in coordinators {
        record(
            COORDINATOR,
            std::iter::once(id.field())
                .chain(canonical(root)?)
                .collect(),
        );
    }
    record(FINISH, vec![]);
    Ok(chain.map(|x| x.as_canonical_u64()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use starstream_interleaving_spec::{InputUtxo, MethodHash, OutputUtxo, StarstreamValue};

    #[test]
    fn empty_transaction_frames_coordinator_and_finish() {
        let root = [11, 12, 13, 14];
        let roots = TraceCommitments::from([(CoroutineId::Coord(1).encoded(), root)]);
        let chain = neo_application::event_commitment::commit_block(
            [F::ZERO; 4],
            [COORDINATOR, 2, 11, 12, 13, 14, 0, 0].map(F::new),
        );
        let expected = neo_application::event_commitment::commit_block(
            chain,
            [FINISH, 0, 0, 0, 0, 0, 0, 0].map(F::new),
        );
        assert_eq!(
            transaction_commitment(&TransactionStatement::default(), &roots).unwrap(),
            expected.map(|x| x.as_canonical_u64())
        );
    }

    #[test]
    fn digest_binds_roots_storage_order_duplicates_and_input_framing() {
        let a = MethodHash([1; 8]);
        let b = MethodHash([2; 8]);
        let input = |methods| InputUtxo {
            storage: StarstreamValue::UNIT_VALUE,
            methods,
        };
        let statement = TransactionStatement {
            inputs: vec![input(vec![a, b]), input(vec![a])],
            outputs: vec![OutputUtxo {
                utxo: 0,
                storage: StarstreamValue([5; 4]),
                methods: vec![a, b, a],
            }],
        };
        // UTXO 1 is consumed but its root must still contribute.
        let roots = TraceCommitments::from([(1, [1; 4]), (2, [2; 4]), (3, [3; 4])]);
        let digest = transaction_commitment(&statement, &roots).unwrap();
        for id in [1, 2, 3] {
            let mut changed = roots.clone();
            changed.get_mut(&id).unwrap()[0] += 1;
            assert_ne!(
                digest,
                transaction_commitment(&statement, &changed).unwrap()
            );
        }
        let mut variants = vec![];
        let mut changed = statement.clone();
        changed.inputs[0].methods.pop();
        changed.inputs[1].methods.insert(0, b); // same flat sequence, different framing
        variants.push(changed);
        let mut changed = statement.clone();
        changed.outputs[0].methods.swap(0, 1);
        variants.push(changed);
        let mut changed = statement.clone();
        changed.outputs[0].methods.pop();
        variants.push(changed);
        let mut changed = statement.clone();
        changed.outputs[0].storage.0[0] += 1;
        variants.push(changed);
        let mut changed = statement.clone();
        changed.inputs[0].storage.0[0] += 1;
        variants.push(changed);
        for changed in variants {
            assert_ne!(digest, transaction_commitment(&changed, &roots).unwrap());
        }
        for id in [CoroutineId::Coord(0), CoroutineId::Coord(2)] {
            let mut changed = roots.clone();
            changed.insert(id.encoded(), [0; 4]);
            // Extra coordinators are not supported yet.
            assert!(transaction_commitment(&statement, &changed).is_err());
            // A single coordinator must still be Coord(1), not the sentinel
            // or an unsupported replacement coordinator.
            changed.remove(&CoroutineId::Coord(1).encoded());
            assert!(transaction_commitment(&statement, &changed).is_err());
        }
        let mut missing = roots.clone();
        missing.remove(&CoroutineId::Coord(1).encoded());
        assert!(transaction_commitment(&statement, &missing).is_err());
        let mut noncanonical = roots.clone();
        noncanonical
            .get_mut(&CoroutineId::Coord(1).encoded())
            .unwrap()[0] = F::ORDER_U64;
        assert!(transaction_commitment(&statement, &noncanonical).is_err());
    }
}
