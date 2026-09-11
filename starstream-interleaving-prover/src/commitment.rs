//! Per-coroutine outer event chains. RAM remains host-checked until Nebula is
//! wired; this gadget alone is not a proof of the global per-program statement.
use neo_application::{EVENT_COMMITMENT_AUX_COLUMNS, EventCommitment, TaggedR1csBuilder};
use neo_math::F;
use p3_field::{Field, PrimeCharacteristicRing, PrimeField64};
use starstream_interleaving_spec::events::{EventKind, Word};

use crate::{
    ccs::{
        layout::*,
        tags::{ConstraintScope, always},
    },
    opcode::Opcode,
    step::Wit,
};

fn kind(op: Opcode) -> Option<EventKind> {
    Some(match op {
        Opcode::NewUtxo => EventKind::NewUtxo,
        Opcode::EnterConstructor => EventKind::EnterConstructor,
        Opcode::YieldBegin => EventKind::YieldBegin,
        Opcode::RegisterMethod => EventKind::RegisterMethod,
        Opcode::Return => EventKind::Return,
        Opcode::CallMethod => EventKind::CallMethod,
        Opcode::EnterMethod => EventKind::EnterMethod,
        Opcode::Padding => return None,
    })
}

fn gadget(block: usize) -> EventCommitment {
    EventCommitment {
        previous: if block == 0 {
            COL_IN
        } else {
            std::array::from_fn(|i| COL_EVENT_HASHES[(block - 1) * 4 + i])
        },
        block: std::array::from_fn(|i| COL_EVENT_BLOCKS[block * 8 + i]),
        output: std::array::from_fn(|i| COL_EVENT_HASHES[block * 4 + i]),
        auxiliary_start: COL_EVENT_AUX[0] + block * EVENT_COMMITMENT_AUX_COLUMNS,
    }
}

fn output_columns(block_count: usize) -> [usize; 4] {
    if block_count == 0 {
        COL_IN
    } else {
        gadget(block_count - 1).output
    }
}

fn source(word: Word) -> (usize, F) {
    match word {
        Word::Constant(x) => (COL_ONE, F::new(u64::from(x))),
        Word::Resource => (COL_RESOURCE_RESOLVER_ADDR_HANDLE, F::ONE),
        Word::Method(i) => (COL_METHOD_HASH_VALUE[i], F::ONE),
        Word::Argument(i) => (COL_ARGUMENT_ROOT[i], F::ONE),
        Word::Result(i) => (COL_RESULT_ROOT[i], F::ONE),
    }
}

fn roots() -> [([usize; 4], [usize; 8]); 4] {
    [
        (COL_IN, COL_IN_WORDS),
        (COL_OUT, COL_OUT_WORDS),
        (COL_ARGUMENT_ROOT, COL_CALL_STACK_EXPECTED_ARG_VALUE),
        (COL_RESULT_ROOT, COL_CALL_STACK_EXPECTED_RESULT_VALUE),
    ]
}

pub(crate) fn constraints(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    b.with_tag(always("canonical event roots"), |b| {
        for (group, (fields, words)) in roots().into_iter().enumerate() {
            for i in 0..4 {
                let lo = words[2 * i];
                let hi = words[2 * i + 1];
                let flag = COL_CANONICAL_HIGH_MAX[group * 4 + i];
                let inv = COL_CANONICAL_HIGH_INV[group * 4 + i];
                let delta = [(hi, F::ONE), (COL_ONE, -F::new(u64::from(u32::MAX)))];
                b.push_linear_zero([(fields[i], F::ONE), (lo, -F::ONE), (hi, -F::new(1 << 32))]);
                b.push_row(delta, [(inv, F::ONE)], [(COL_ONE, F::ONE), (flag, -F::ONE)]);
                b.push_row(delta, [(flag, F::ONE)], []);
                // p = 0xffffffff00000001: if hi is maximal, lo must be zero.
                // The family's range decomposition supplies both u32 bounds.
                b.push_row([(lo, F::ONE)], [(flag, F::ONE)], []);
            }
        }
    });
    b.with_tag(always("event block encoding"), |b| {
        for opcode in Opcode::all() {
            let blocks = kind(opcode).map(EventKind::blocks).unwrap_or_default();
            assert!(
                blocks.len() <= 3,
                "event schema exceeds circuit block capacity"
            );
            for block in 0..3 {
                for lane in 0..8 {
                    let word = blocks.get(block).map_or(Word::Constant(0), |b| b[lane]);
                    let (column, coefficient) = source(word);
                    b.push_gated_linear_zero(
                        opcode.selector(),
                        [
                            (COL_EVENT_BLOCKS[block * 8 + lane], F::ONE),
                            (column, -coefficient),
                        ],
                    );
                }
            }
        }
    });
    b.with_tag(always("event commitment"), |b| {
        for block in 0..3 {
            gadget(block).push_constraints(b);
        }
        // Active blocks are always a prefix. Compute the entire chain, then
        // select its last active output; padding selects the original input.
        // Hashes of the unused zero-block suffix never enter the transcript.
        for count in 0..=3 {
            let output = output_columns(count);
            for lane in 0..4 {
                b.push_row(
                    Opcode::all()
                        .into_iter()
                        .filter(|op| kind(*op).map_or(0, |k| k.blocks().len()) == count)
                        .map(|op| (op.selector(), F::ONE)),
                    [(COL_OUT[lane], F::ONE), (output[lane], -F::ONE)],
                    [],
                );
            }
        }
    });
}

/// Recompute compression advice from the semantic bus. Tests may call this
/// after tampering with bus values, so rejection must not rely on stale hashes.
pub(crate) fn assign_from_bus(row: &mut [F], opcode: Opcode) {
    for (fields, words) in roots() {
        if fields == COL_OUT {
            continue;
        }
        if fields == COL_IN {
            for i in 0..4 {
                let x = row[fields[i]].as_canonical_u64();
                row[words[2 * i]] = F::new(x & u64::from(u32::MAX));
                row[words[2 * i + 1]] = F::new(x >> 32);
            }
        } else {
            for i in 0..4 {
                row[fields[i]] = row[words[2 * i]] + F::new(1 << 32) * row[words[2 * i + 1]];
            }
        }
    }
    let blocks = kind(opcode).map(EventKind::blocks).unwrap_or_default();
    for block in 0..3 {
        let g = gadget(block);
        for lane in 0..8 {
            let (column, coefficient) =
                source(blocks.get(block).map_or(Word::Constant(0), |b| b[lane]));
            row[g.block[lane]] = row[column] * coefficient;
        }
        let hash = neo_application::event_commitment::commit_block(
            g.previous.map(|c| row[c]),
            g.block.map(|c| row[c]),
        );
        for lane in 0..4 {
            row[g.output[lane]] = hash[lane];
        }
        g.assign_auxiliaries(row);
    }
    let output = output_columns(blocks.len());
    for i in 0..4 {
        row[COL_OUT[i]] = row[output[i]];
        let x = row[COL_OUT[i]].as_canonical_u64();
        row[COL_OUT_WORDS[2 * i]] = F::new(x & u64::from(u32::MAX));
        row[COL_OUT_WORDS[2 * i + 1]] = F::new(x >> 32);
    }
    for (group, (_, words)) in roots().into_iter().enumerate() {
        for i in 0..4 {
            let delta = row[words[2 * i + 1]] - F::new(u64::from(u32::MAX));
            row[COL_CANONICAL_HIGH_MAX[group * 4 + i]] =
                if delta == F::ZERO { F::ONE } else { F::ZERO };
            row[COL_CANONICAL_HIGH_INV[group * 4 + i]] = delta.try_inverse().unwrap_or(F::ZERO);
        }
    }
}

pub(crate) fn assign(row: &mut [F], input: &Wit) {
    for (column, value) in COL_IN.into_iter().zip(input.commitment_before) {
        row[column] = value;
    }
    assign_from_bus(row, input.opcode);
}

#[cfg(test)]
mod tests;
