//! Shared compression slots for per-coroutine events and transaction records.
//! RAM remains host-checked; hashing alone does not prove the memory argument.
use std::collections::BTreeMap;

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
        Opcode::SetStorage => EventKind::SetStorage,
        Opcode::GetStorage => EventKind::GetStorage,
        Opcode::Padding
        | Opcode::ReadAbi
        | Opcode::PreloadMethod
        | Opcode::SkipConsumed
        | Opcode::FinalizeCoordinator
        | Opcode::FinishTransaction => return None,
    })
}

fn gadget(block: usize) -> EventCommitment {
    EventCommitment {
        previous: std::array::from_fn(|i| COL_POSEIDON_INPUT_ROOT[block * 4 + i]),
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

type Source = (usize, F);
const ZERO: Source = (COL_ONE, F::ZERO);
const SLOTS: usize = 3;

struct Slot {
    previous: [Source; 4],
    words: [Source; 8],
}

struct Schedule {
    slots: [Slot; SLOTS],
    program_blocks: usize,
    io_blocks: usize,
}

fn schedule(op: Opcode) -> Schedule {
    let program = kind(op).map(EventKind::blocks).unwrap_or_default();
    let io = crate::transaction_commitment::schema(op);
    let program_blocks = program.len();
    let io_blocks = io.len().div_ceil(8);
    assert!(
        program_blocks + io_blocks <= SLOTS,
        "commitment schedule exceeds shared capacity: {op:?}"
    );
    let slots = std::array::from_fn(|slot| {
        let (previous, words) = if slot < program_blocks {
            let previous = if slot == 0 {
                COL_IN
            } else {
                gadget(slot - 1).output
            };
            (previous.map(|c| (c, F::ONE)), program[slot].map(source))
        } else if slot < program_blocks + io_blocks {
            let block = slot - program_blocks;
            let previous = if block == 0 {
                COL_IO_BEFORE
            } else {
                gadget(slot - 1).output
            };
            (
                previous.map(|c| (c, F::ONE)),
                std::array::from_fn(|i| io.get(block * 8 + i).copied().unwrap_or(ZERO)),
            )
        } else {
            ([ZERO; 4], [ZERO; 8])
        };
        Slot { previous, words }
    });
    Schedule {
        slots,
        program_blocks,
        io_blocks,
    }
}

impl Schedule {
    fn program_output(&self) -> [usize; 4] {
        output_columns(self.program_blocks)
    }

    fn io_output(&self) -> [usize; 4] {
        if self.io_blocks == 0 {
            COL_IO_BEFORE
        } else {
            gadget(self.program_blocks + self.io_blocks - 1).output
        }
    }
}

// Identical assignments share one selector-sum guard, including zero lanes.
// One group covering every opcode can be enforced unconditionally.
fn constrain_grouped_equalities(
    b: &mut TaggedR1csBuilder<'_, ConstraintScope>,
    target: usize,
    assignments: impl IntoIterator<Item = (Opcode, Source)>,
) {
    let mut groups = BTreeMap::<(usize, u64), Vec<(usize, F)>>::new();
    let mut count = 0;
    for (op, (column, coefficient)) in assignments {
        groups
            .entry((column, coefficient.as_canonical_u64()))
            .or_default()
            .push((op.selector(), F::ONE));
        count += 1;
    }
    let unconditional = groups.len() == 1 && count == Opcode::all().len();
    for ((source, coefficient), selectors) in groups {
        let equality = [(target, F::ONE), (source, -F::new(coefficient))];
        if unconditional {
            b.push_linear_zero(equality);
        } else {
            b.push_row(selectors, equality, []);
        }
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
    let schedules: Vec<_> = Opcode::all()
        .into_iter()
        .map(|op| (op, schedule(op)))
        .collect();
    b.with_tag(always("commitment block encoding"), |b| {
        for slot in 0..SLOTS {
            let g = gadget(slot);
            for lane in 0..8 {
                constrain_grouped_equalities(
                    b,
                    g.block[lane],
                    schedules
                        .iter()
                        .map(|(op, s)| (*op, s.slots[slot].words[lane])),
                );
            }
            for lane in 0..4 {
                constrain_grouped_equalities(
                    b,
                    g.previous[lane],
                    schedules
                        .iter()
                        .map(|(op, s)| (*op, s.slots[slot].previous[lane])),
                );
            }
        }
    });
    b.with_tag(always("shared commitment compression"), |b| {
        for slot in 0..SLOTS {
            gadget(slot).push_constraints(b);
        }
    });
    b.with_tag(always("event commitment"), |b| {
        for (lane, target) in COL_OUT.into_iter().enumerate() {
            constrain_grouped_equalities(
                b,
                target,
                schedules
                    .iter()
                    .map(|(op, s)| (*op, (s.program_output()[lane], F::ONE))),
            );
        }
    });
    b.with_tag(always("transaction commitment"), |b| {
        for (lane, target) in COL_IO_AFTER.into_iter().enumerate() {
            // Padding already preserves the transaction chain in ccs.rs.
            constrain_grouped_equalities(
                b,
                target,
                schedules
                    .iter()
                    .filter(|(op, _)| *op != Opcode::Padding)
                    .map(|(op, s)| (*op, (s.io_output()[lane], F::ONE))),
            );
        }
    });
}

/// Recompute compression advice from the semantic bus. Tests may call this
/// after tampering with bus values, so rejection must not rely on stale hashes.
pub(crate) fn assign_from_bus(row: &mut [F], opcode: Opcode) {
    assign_native_from_bus(row, opcode);
    for slot in 0..SLOTS {
        gadget(slot).assign_auxiliaries(row);
    }
}

/// Native chains for normalization, without Poseidon auxiliaries or range bits.
pub(crate) fn assign_native_from_bus(row: &mut [F], opcode: Opcode) {
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
    let plan = schedule(opcode);
    for lane in 0..4 {
        row[COL_OUT[lane]] = row[COL_IN[lane]];
    }
    for (slot, sources) in plan.slots.iter().enumerate() {
        let g = gadget(slot);
        for (column, &(source, coefficient)) in g.previous.iter().zip(&sources.previous) {
            row[*column] = row[source] * coefficient;
        }
        for (column, &(source, coefficient)) in g.block.iter().zip(&sources.words) {
            row[*column] = row[source] * coefficient;
        }
        // Unused slots still need a satisfying assignment in the full witness.
        let hash = neo_application::event_commitment::commit_block(
            g.previous.map(|c| row[c]),
            g.block.map(|c| row[c]),
        );
        for (column, value) in g.output.into_iter().zip(hash) {
            row[column] = value;
        }
        if slot + 1 == plan.program_blocks {
            // GetStorage's IO record consumes this freshly computed program root.
            for lane in 0..4 {
                row[COL_OUT[lane]] = row[g.output[lane]];
            }
        }
    }
    for lane in 0..4 {
        row[COL_IO_AFTER[lane]] = row[plan.io_output()[lane]];
        let x = row[COL_OUT[lane]].as_canonical_u64();
        row[COL_OUT_WORDS[2 * lane]] = F::new(x & u64::from(u32::MAX));
        row[COL_OUT_WORDS[2 * lane + 1]] = F::new(x >> 32);
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

#[cfg(test)]
mod tests;
