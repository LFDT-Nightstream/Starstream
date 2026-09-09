use neo_application::{
    ApplicationRelation, ColumnRegistry, ContinuityCatalog, MemoryCatalog, MemoryPortActivation,
    MemoryPortKind, MemoryPreload, R1csBuilder, check_continuity_rows, check_memory_rows,
};
use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::Trace;

use crate::{
    Error, Unsatisfied,
    ccs::{
        build_relation,
        layout::*,
        tags::{ConstraintScope, always},
    },
    ivc_state::build_ivc_state_continuity_links,
    memory::{MemoryId, build_memory_layout, sanity_checking_policy},
    step::{Wit, normalize},
    witness::build_witness_vector,
};

struct PackedWitness {
    rows: Vec<Vec<F>>,
    // Trusted provenance from packing, separate from mutable witness columns.
    // None denotes padding; Some is an index in the original execution trace.
    origins: Vec<Option<usize>>,
}

// The public prefix `0..PUBLIC_INPUTS` of a batched assignment consists of
// `COL_ONE`'s slot cells, so only a single public constant is supported.
const _: () = assert!(
    PUBLIC_INPUTS == 1,
    "batched column layout assumes the constant is the only public input"
);

struct Batch {
    size: usize,
    single_width: usize,
    single_constraints: usize,
    /// Continuity links per slot boundary, both within and across batches.
    link_count: usize,
    relation: ApplicationRelation<ConstraintScope>,
    memory: MemoryCatalog<MemoryId>,
    continuity: ContinuityCatalog,
}

impl Batch {
    fn new(batch_size: usize) -> Result<Self, Error> {
        if batch_size == 0 {
            return Err(Error::InvalidBatchSize);
        }
        let single = build_relation()?;
        let single_width = single.columns().column_count();
        // Guards only the `usize` arithmetic below; realistic oversized batches
        // fail later by exhausting memory.
        let width = single_width
            .checked_mul(batch_size)
            .ok_or(Error::InvalidBatchSize)?;
        let single_constraints = single.r1cs().catalog().len();
        let continuity_groups = build_ivc_state_continuity_links();
        let link_count: usize = continuity_groups
            .iter()
            .map(|group| group.links.len())
            .sum();
        single_constraints
            .checked_mul(batch_size)
            .and_then(|rows| {
                link_count
                    .checked_mul(batch_size - 1)
                    .and_then(|links| rows.checked_add(links))
            })
            .ok_or(Error::InvalidBatchSize)?;
        let columns = ColumnRegistry::new(single.columns().families().iter().map(|family| {
            let mut family = *family;
            family.start *= batch_size;
            family.len *= batch_size;
            family
        }))?;
        let column = |c: usize, step: usize| c * batch_size + step;
        // Only the first constant is public. All copied equations reference it.
        let map = |c: usize, slot: usize| {
            if c == COL_ONE {
                COL_ONE
            } else {
                column(c, slot)
            }
        };
        let mut builder = R1csBuilder::new(width, PUBLIC_INPUTS, COL_ONE)?;
        for step in 0..batch_size {
            for tagged in single.r1cs().catalog().rows() {
                let row = tagged.row();
                builder.tagged(tagged.tag().clone()).push_row(
                    row.a_terms().iter().map(|&(c, v)| (map(c, step), v)),
                    row.b_terms().iter().map(|&(c, v)| (map(c, step), v)),
                    row.c_terms().iter().map(|&(c, v)| (map(c, step), v)),
                );
            }
        }
        for step in 0..batch_size - 1 {
            for group in &continuity_groups {
                for link in &group.links {
                    builder
                        .tagged(always("within-batch continuity"))
                        .push_linear_zero([
                            (column(link.previous_step_column, step), F::ONE),
                            (column(link.next_step_column, step + 1), -F::ONE),
                        ]);
                }
            }
        }
        let relation = ApplicationRelation::new(builder.build()?, columns)?;
        // Only the first input and last output remain as cross-batch links.
        let continuity = ContinuityCatalog::new(
            continuity_groups
                .into_iter()
                .map(|mut group| {
                    for link in &mut group.links {
                        link.previous_step_column =
                            column(link.previous_step_column, batch_size - 1);
                        link.next_step_column = column(link.next_step_column, 0);
                    }
                    group
                })
                .collect::<Vec<_>>(),
            relation.columns(),
        )?;

        let memory = MemoryCatalog::new(
            build_memory_layout().entries().iter().map(|memory| {
                let mut batched = memory.clone();
                // port declaration order is important, so we iterate by
                // step/slot number
                batched.ports = (0..batch_size)
                    .flat_map(|slot| {
                        memory.ports.iter().map(move |port| {
                            let mut port = port.clone();
                            port.address_columns =
                                port.address_columns.iter().map(|&c| map(c, slot)).collect();
                            port.value_column = map(port.value_column, slot);
                            port.kind = match port.kind {
                                MemoryPortKind::Read => MemoryPortKind::Read,
                                MemoryPortKind::Write {
                                    value_before_column,
                                } => MemoryPortKind::Write {
                                    value_before_column: value_before_column.map(|c| map(c, slot)),
                                },
                            };
                            port.activation = match port.activation {
                                MemoryPortActivation::Always => MemoryPortActivation::Always,
                                MemoryPortActivation::When(c) => {
                                    MemoryPortActivation::When(map(c, slot))
                                }
                                MemoryPortActivation::Unless(c) => {
                                    MemoryPortActivation::Unless(map(c, slot))
                                }
                            };
                            port
                        })
                    })
                    .collect();
                batched
            }),
            relation.columns(),
        )?;

        Ok(Self {
            size: batch_size,
            single_width,
            single_constraints,
            link_count,
            relation,
            memory,
            continuity,
        })
    }

    fn pack(&self, steps: &[Wit]) -> PackedWitness {
        let mut origins = Vec::new();
        let mut trace_step = 0;
        let rows = steps
            .chunks(self.size)
            .map(|chunk| {
                // Only a short final chunk needs state-preserving padding.
                let padding = (chunk.len() < self.size)
                    .then(|| chunk.last().expect("nonempty chunk").padding_after());
                // Assign repeated trailing padding only once, through the
                // same assigner as all other opcodes.
                let padding_row = padding.as_ref().map(build_witness_vector);
                let mut batch = vec![F::ZERO; self.single_width * self.size];
                for slot in 0..self.size {
                    let assigned;
                    let row = if let Some(step) = chunk.get(slot) {
                        if step.opcode.is_execution() {
                            origins.push(Some(trace_step));
                            trace_step += 1;
                        } else {
                            origins.push(None);
                        }
                        assigned = build_witness_vector(step);
                        &assigned
                    } else {
                        origins.push(None);
                        padding_row.as_ref().expect("padding covers missing slots")
                    };
                    for (column, value) in row.iter().enumerate() {
                        // Every copied equation references the public constant
                        // in slot 0, so the other slots' `COL_ONE` cells are
                        // dead. Leaving them zero keeps the commitment sparse.
                        if column == COL_ONE && slot != 0 {
                            continue;
                        }
                        batch[column * self.size + slot] = *value;
                    }
                }
                batch
            })
            .collect();

        PackedWitness { rows, origins }
    }

    fn check(
        &self,
        packed: &PackedWitness,
        preload: &MemoryPreload<MemoryId>,
    ) -> Result<(), Error> {
        let rows = &packed.rows;
        let step_coordinates = self.size == 1 && packed.origins.iter().all(Option::is_some);
        for (batch, assignment) in rows.iter().enumerate() {
            match neo_ccs::check_ccs_rowwise_zero(
                self.relation.r1cs().structure(),
                &assignment[..PUBLIC_INPUTS],
                &assignment[PUBLIC_INPUTS..],
            ) {
                Ok(()) => {}
                Err(neo_ccs::CcsError::RowFail { row }) => {
                    let slot = if row < self.size * self.single_constraints {
                        row / self.single_constraints
                    } else {
                        // Link rows are emitted boundary-major. A failed link
                        // is attributed to the later slot, whose `before`
                        // state disagrees with its predecessor's `after`.
                        (row - self.size * self.single_constraints) / self.link_count + 1
                    };
                    let constraint = self.relation.r1cs().catalog().rows()[row].tag().label();
                    return Err(match packed.origins[batch * self.size + slot] {
                        Some(step) => Unsatisfied::Constraint {
                            step,
                            row,
                            constraint,
                        },
                        None => Unsatisfied::PaddingConstraint {
                            batch,
                            slot,
                            batch_size: self.size,
                            row,
                            constraint,
                        },
                    }
                    .into());
                }
                Err(source) => {
                    return Err(if self.size == 1 && packed.origins[batch].is_some() {
                        Error::CcsCheck {
                            step: packed.origins[batch].unwrap(),
                            source,
                        }
                    } else {
                        Error::BatchedCcsCheck {
                            batch,
                            batch_size: self.size,
                            source,
                        }
                    });
                }
            }
        }

        check_memory_rows(
            &self.memory,
            self.relation.columns(),
            rows,
            preload,
            &sanity_checking_policy(&self.memory),
        )
        .map_err(|source| {
            if step_coordinates {
                Unsatisfied::Memory(source)
            } else {
                Unsatisfied::BatchedMemory {
                    batch_size: self.size,
                    source,
                }
            }
        })?;

        check_continuity_rows(&self.continuity, rows).map_err(|source| match source {
            neo_application::ContinuityCheckError::Mismatch { .. } => {
                Error::Unsatisfied(if step_coordinates {
                    Unsatisfied::Continuity(source)
                } else {
                    Unsatisfied::BatchedContinuity {
                        batch_size: self.size,
                        source,
                    }
                })
            }
            _ if step_coordinates => Error::ContinuityCheckError(source),
            _ => Error::BatchedContinuityCheck {
                batch_size: self.size,
                source,
            },
        })?;

        Ok(())
    }
}

/// Check fixed-size batches, padding the final batch with state-preserving
/// circuit slots. Constraint errors identify an original trace step, or a
/// separate padding batch/slot. With batches larger than one, memory and
/// continuity failures use explicitly batched error variants; their nested
/// row/boundary and column indices refer to the batched witness.
/// This does not construct a proof.
pub fn verify_sat_batched(trace: &Trace, batch_size: usize) -> Result<(), Error> {
    let batch = Batch::new(batch_size)?;
    let normalized = normalize(trace);
    let preload = crate::memory::preload_tables(&normalized.method_table);
    let packed = batch.pack(&normalized.steps);
    batch.check(&packed, &preload)?;
    // Check the actual final slot, including padding.
    let terminal = Vec::from_iter(packed.rows.last().map(|row| {
        (0..batch.single_width)
            .map(|c| row[c * batch.size + batch.size - 1])
            .collect()
    }));
    crate::verify_execution_statement(&terminal)
}

#[cfg(test)]
pub(super) fn check_single_rows(
    rows: &[Vec<F>],
    preload: &MemoryPreload<MemoryId>,
) -> Result<(), Error> {
    let batch = Batch::new(1)?;
    batch.check(
        &PackedWitness {
            rows: rows.to_vec(),
            origins: (0..rows.len()).map(Some).collect(),
        },
        preload,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::{constructor_trace, method_call_trace};

    fn replace_slot(packed: &mut PackedWitness, size: usize, index: usize, row: &[F]) {
        for (column, &value) in row.iter().enumerate() {
            if column != COL_ONE || index.is_multiple_of(size) {
                packed.rows[index / size][column * size + index % size] = value;
            }
        }
    }

    #[test]
    fn rejects_invalid_batch_sizes_and_empty_execution() {
        let trace = constructor_trace([1, 2, 3, 4]);
        for size in [0, usize::MAX] {
            assert!(matches!(
                verify_sat_batched(&trace, size),
                Err(Error::InvalidBatchSize)
            ));
        }
        // Constraint-count overflow is reachable even when column-count
        // multiplication fits; reject it before trying to allocate the batch.
        let relation = build_relation().unwrap();
        let size = usize::MAX / relation.r1cs().catalog().len() + 1;
        assert!(
            relation
                .columns()
                .column_count()
                .checked_mul(size)
                .is_some()
        );
        assert!(matches!(Batch::new(size), Err(Error::InvalidBatchSize)));
        assert!(matches!(
            verify_sat_batched(&Trace::new([]), 3),
            Err(Error::Unsatisfied(
                Unsatisfied::TerminalCallStackNotEmpty { .. }
            ))
        ));
    }

    #[test]
    fn real_step_indices_survive_interspersed_padding() {
        for size in [1, 3, 8] {
            let normalized = normalize(&method_call_trace(false));
            let preload = crate::memory::preload_tables(&normalized.method_table);
            let mut steps = Vec::new();
            for step in normalized.steps {
                let padding = step.padding_after();
                steps.extend([step, padding]);
            }
            let batch = Batch::new(size).unwrap();
            assert!(matches!(
                batch.check(&batch.pack(&steps), &preload),
                Err(Error::Unsatisfied(Unsatisfied::Constraint {
                    step: 5,
                    constraint: "return constraints",
                    ..
                }))
            ));
        }
    }

    #[test]
    fn padding_can_stutter_during_and_after_execution() {
        let normalized = normalize(&method_call_trace(true));
        let preload = crate::memory::preload_tables(&normalized.method_table);
        let mut padded = Vec::new();
        for step in normalized.steps {
            let padding = step.padding_after();
            let second_padding = padding.padding_after();
            padded.extend([step, padding, second_padding]);
        }
        for size in [1, 3, 8] {
            let batch = Batch::new(size).unwrap();
            batch.check(&batch.pack(&padded), &preload).unwrap();
        }
    }

    #[test]
    fn rejects_tampered_padding_state() {
        let normalized = normalize(&constructor_trace([1, 2, 3, 4]));
        let preload = crate::memory::preload_tables(&normalized.method_table);
        let batch = Batch::new(8).unwrap();
        let mut packed = batch.pack(&normalized.steps);
        // Last slot has no outgoing link: preservation must be constrained
        // inside the padding row itself.
        let mut pad = build_witness_vector(&normalized.steps.last().unwrap().padding_after());
        pad[COL_PENDING_CTOR_PRESENT_AFTER] = F::ONE;
        range_check_layout().assign_bits(&mut pad).unwrap();
        replace_slot(&mut packed, batch.size, batch.size - 1, &pad);
        assert!(matches!(
            batch.check(&packed, &preload),
            Err(Error::Unsatisfied(Unsatisfied::PaddingConstraint {
                batch: 0,
                slot: 7,
                batch_size: 8,
                constraint: "padding preserves state",
                ..
            }))
        ));
    }

    #[test]
    fn rejects_broken_internal_and_external_continuity() {
        let (mut rows, preload) = crate::build_witness_rows(&constructor_trace([1, 2, 3, 4]));
        let normalized = normalize(&constructor_trace([1, 2, 3, 4]));
        // Preserve every row's local allocator transition, but break boundary 1.
        for row in &mut rows[2..] {
            row[COL_NEXT_UTXO_ID_BEFORE] += F::ONE;
            row[COL_NEXT_UTXO_ID_AFTER] += F::ONE;
            range_check_layout().assign_bits(row).unwrap();
        }
        let internal = Batch::new(3).unwrap();
        let mut packed = internal.pack(&normalized.steps);
        for (index, row) in rows.iter().enumerate() {
            replace_slot(&mut packed, internal.size, index, row);
        }
        assert!(matches!(
            internal.check(&packed, &preload),
            Err(Error::Unsatisfied(Unsatisfied::Constraint {
                constraint: "within-batch continuity",
                step: 2,
                ..
            }))
        ));
        let external = Batch::new(2).unwrap();
        let mut packed = external.pack(&normalized.steps);
        for (index, row) in rows.iter().enumerate() {
            replace_slot(&mut packed, external.size, index, row);
        }
        // Keep trailing padding consistent with the altered final state.
        let mut pad = build_witness_vector(&normalized.steps.last().unwrap().padding_after());
        pad[COL_NEXT_UTXO_ID_BEFORE] += F::ONE;
        pad[COL_NEXT_UTXO_ID_AFTER] += F::ONE;
        range_check_layout().assign_bits(&mut pad).unwrap();
        replace_slot(&mut packed, external.size, 5, &pad);
        assert!(matches!(
            external.check(&packed, &preload),
            Err(Error::Unsatisfied(Unsatisfied::BatchedContinuity {
                batch_size: 2,
                source: neo_application::ContinuityCheckError::Mismatch { boundary: 0, .. },
            }))
        ));
    }

    #[test]
    fn rejects_tampered_memory_across_slots_and_batches() {
        let (mut rows, preload) = crate::build_witness_rows(&method_call_trace(true));
        let normalized = normalize(&method_call_trace(true));
        rows[5][COL_METHOD_HASH_VALUE[0]] += F::ONE;
        range_check_layout().assign_bits(&mut rows[5]).unwrap();
        // Call is slot 4, entry is slot 5: different batches at size 5,
        // the same batch at size 8.
        for size in [1, 5, 8] {
            let batch = Batch::new(size).unwrap();
            let mut packed = batch.pack(&normalized.steps);
            replace_slot(&mut packed, size, 5, &rows[5]);
            let error = batch.check(&packed, &preload).unwrap_err();
            match error {
                Error::Unsatisfied(Unsatisfied::Memory(
                    neo_application::MemoryCheckError::ReadMismatch { row: 5, .. },
                )) => assert_eq!(size, 1),
                Error::Unsatisfied(Unsatisfied::BatchedMemory {
                    batch_size,
                    source: neo_application::MemoryCheckError::ReadMismatch { row, .. },
                }) => {
                    assert_eq!(batch_size, size);
                    assert_eq!(row, 5 / size);
                    assert_ne!(size, 1);
                }
                other => panic!("unexpected memory diagnostic: {other:?}"),
            }
        }
    }
}
