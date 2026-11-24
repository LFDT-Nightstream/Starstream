//! Transaction-only chain: inputs/preloads, entry call, UTXO outputs/ABIs, coordinators, finish.
//! Records have distinct tags and fixed lengths, padded to eight-field blocks.
//! Roots are transaction data, not persistent ledger storage. ABI duplicates stay.
//! The proving API binds the expected final digest.
//! Nebula binds memory consistency and the verifier-owned initial memory image.
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
const ENTRY: u64 = RECORD_PREFIX | 8;

pub(crate) fn schema(op: Opcode) -> Vec<(usize, F)> {
    let (tag, columns): (_, Vec<usize>) = match op {
        Opcode::SetStorage => (
            INPUT,
            std::iter::once(COL_BOUNDARY_UTXO)
                .chain(COL_ARGUMENT_ROOT)
                .collect(),
        ),
        Opcode::PreloadMethod => (INPUT_METHOD, COL_METHOD_HASH_VALUE.to_vec()),
        Opcode::BeginExecution => (
            ENTRY,
            COL_METHOD_HASH_VALUE
                .into_iter()
                .chain(COL_ARGUMENT_ROOT)
                .collect(),
        ),
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

/// Compute the verifier's transaction digest from ledger inputs, the entry
/// script call, and proposed outputs/instance roots. All allocated UTXOs (including consumed ones) and
/// coordinators must be represented, each as a contiguous range: UTXOs from 0, coordinators from
/// Coord(1), the entry script. Coord(0) is the transaction context and has no trace.
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
    // Only coordinator and UTXO traces have finalization rows today.
    for &packed in roots.keys() {
        if !matches!(
            CoroutineId::from_packed(packed),
            Some(CoroutineId::Coord(_) | CoroutineId::Utxo(_))
        ) {
            return Err(reject());
        }
    }
    let coordinators =
        roots
            .iter()
            .filter_map(|(&packed, root)| match CoroutineId::from_packed(packed) {
                Some(id @ CoroutineId::Coord(_)) => Some((id, root)),
                _ => None,
            });
    // The map iterates in packed-id order, which is allocation order.
    let mut ids = coordinators.clone().map(|(id, _)| id).peekable();
    if ids.peek().is_none()
        || ids
            .zip(1..)
            .any(|(id, expected)| id != CoroutineId::Coord(expected))
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
    record(
        ENTRY,
        statement
            .entry
            .script
            .0
            .map(|x| F::new(u64::from(x)))
            .into_iter()
            .chain(canonical(&statement.entry.arguments.0)?)
            .collect(),
    );
    let mut outputs = statement.outputs.iter().peekable();
    let mut next_utxo = 0u64;
    let utxos = roots
        .iter()
        .filter_map(|(&packed, root)| match CoroutineId::from_packed(packed) {
            Some(CoroutineId::Utxo(id)) => Some((id, root)),
            _ => None,
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
        // The entry record spans two blocks: tag and seven script limbs, then
        // the last limb and the argument root.
        let statement = TransactionStatement {
            input_tokens: Vec::new(),
            output_tokens: Vec::new(),
            signers: Default::default(),
            entry: starstream_interleaving_spec::EntryCall {
                script: MethodHash([1, 2, 3, 4, 5, 6, 7, 8]),
                arguments: StarstreamValue([21, 22, 23, 24]),
            },
            inputs: vec![],
            outputs: vec![],
        };
        let chain = neo_application::event_commitment::commit_block(
            [F::ZERO; 4],
            [ENTRY, 1, 2, 3, 4, 5, 6, 7].map(F::new),
        );
        let chain = neo_application::event_commitment::commit_block(
            chain,
            [8, 21, 22, 23, 24, 0, 0, 0].map(F::new),
        );
        let chain = neo_application::event_commitment::commit_block(
            chain,
            [COORDINATOR, 4, 11, 12, 13, 14, 0, 0].map(F::new),
        );
        let expected = neo_application::event_commitment::commit_block(
            chain,
            [FINISH, 0, 0, 0, 0, 0, 0, 0].map(F::new),
        );
        assert_eq!(
            transaction_commitment(&statement, &roots).unwrap(),
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
            input_tokens: Vec::new(),
            output_tokens: Vec::new(),
            signers: Default::default(),
            entry: starstream_interleaving_spec::EntryCall {
                script: a,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            inputs: vec![input(vec![a, b]), input(vec![a])],
            outputs: vec![OutputUtxo {
                utxo: 0,
                storage: StarstreamValue([5; 4]),
                methods: vec![a, b, a],
            }],
        };
        // UTXO 1 is consumed but its root must still contribute.
        let roots = TraceCommitments::from([
            (CoroutineId::Utxo(0).encoded(), [1; 4]),
            (CoroutineId::Coord(1).encoded(), [2; 4]),
            (CoroutineId::Utxo(1).encoded(), [3; 4]),
        ]);
        let digest = transaction_commitment(&statement, &roots).unwrap();
        for id in roots.keys() {
            let mut changed = roots.clone();
            changed.get_mut(id).unwrap()[0] += 1;
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
        let mut changed = statement.clone();
        changed.entry.script = b;
        variants.push(changed);
        let mut changed = statement.clone();
        changed.entry.arguments.0[3] += 1;
        variants.push(changed);
        for changed in variants {
            assert_ne!(digest, transaction_commitment(&changed, &roots).unwrap());
        }
        // Called scripts follow the entry script, and each root is bound.
        let mut called = roots.clone();
        called.insert(CoroutineId::Coord(2).encoded(), [0; 4]);
        let with_callee = transaction_commitment(&statement, &called).unwrap();
        assert_ne!(digest, with_callee);
        called.get_mut(&CoroutineId::Coord(2).encoded()).unwrap()[0] += 1;
        assert_ne!(
            with_callee,
            transaction_commitment(&statement, &called).unwrap()
        );
        // Coordinators are a contiguous range from Coord(1): the transaction
        // context has no trace, and an allocated instance cannot be left out.
        for id in [CoroutineId::Coord(0), CoroutineId::Coord(3)] {
            let mut changed = roots.clone();
            changed.insert(id.encoded(), [0; 4]);
            assert!(transaction_commitment(&statement, &changed).is_err());
        }
        for packed in [CoroutineId::Token(0).encoded(), 3] {
            let mut changed = roots.clone();
            changed.insert(packed, [0; 4]);
            assert!(transaction_commitment(&statement, &changed).is_err());
        }
        called.remove(&CoroutineId::Coord(1).encoded());
        assert!(transaction_commitment(&statement, &called).is_err());
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
