use neo_application::{
    ApplicationRelation, ContinuityCatalog, MemoryCatalog, MemoryPreload, check_continuity_rows,
    check_memory_rows,
};

use neo_math::F;
use p3_field::PrimeCharacteristicRing;
use starstream_interleaving_spec::Trace;

use crate::{
    Error, Unsatisfied,
    ccs::{build_relation, layout::*, tags::ConstraintScope},
    ivc_state::build_ivc_state_continuity_links,
    memory::{MemoryId, build_memory_layout, sanity_checking_policy},
    step::Wit,
    witness::build_witness_vector,
};

#[cfg(test)]
use crate::step::normalize;
use std::collections::BTreeSet;

pub mod proving;

/// One witness row per circuit slot, in execution order.
struct PackedWitness {
    rows: Vec<Vec<F>>,
    // Trusted provenance from packing, separate from mutable witness columns.
    // None denotes padding; Some is an index in the original execution trace.
    origins: Vec<Option<usize>>,
}

/// The one-row relation and how many rows one proof step holds.
///
/// The proof backend builds the step from the one-row relation: a copy of
/// its rows per slot, an equality per carried column between neighbor slots,
/// and the memory ports of every slot. Rows in order are memory consistent
/// exactly when their steps are, so the host checks here work row by row.
struct Batch {
    size: usize,
    relation: ApplicationRelation<ConstraintScope>,
    memory: MemoryCatalog<MemoryId>,
    continuity: ContinuityCatalog,
}

impl Batch {
    fn new(batch_size: usize) -> Result<Self, Error> {
        if batch_size == 0 {
            return Err(Error::InvalidBatchSize);
        }
        let relation = build_relation()?;
        let groups = build_ivc_state_continuity_links();
        let links: usize = groups.iter().map(|group| group.links.len()).sum();
        // Guards only the dimensions of a step; realistic oversized batches
        // fail later by exhausting memory.
        relation
            .columns()
            .column_count()
            .checked_mul(batch_size)
            .ok_or(Error::InvalidBatchSize)?;
        relation
            .r1cs()
            .catalog()
            .len()
            .checked_mul(batch_size)
            .and_then(|rows| {
                links
                    .checked_mul(batch_size - 1)
                    .and_then(|links| rows.checked_add(links))
            })
            .ok_or(Error::InvalidBatchSize)?;
        let continuity = ContinuityCatalog::new(groups, relation.columns())?;
        Ok(Self {
            size: batch_size,
            relation,
            memory: build_memory_layout(),
            continuity,
        })
    }

    /// One row per step, then state-preserving padding up to a whole batch.
    fn pack(&self, steps: &[Wit]) -> PackedWitness {
        let mut trace_step = 0;
        let mut origins = Vec::new();
        let mut rows: Vec<Vec<F>> = steps
            .iter()
            .map(|step| {
                if step.opcode == crate::opcode::Opcode::Padding {
                    origins.push(None);
                } else {
                    origins.push(Some(trace_step));
                    trace_step += 1;
                }
                build_witness_vector(step)
            })
            .collect();
        if let Some(last) = steps.last()
            && !rows.len().is_multiple_of(self.size)
        {
            // Assign repeated trailing padding only once, through the same
            // assigner as all other opcodes.
            let padding = build_witness_vector(&last.padding_after());
            rows.resize(rows.len().next_multiple_of(self.size), padding);
            origins.resize(rows.len(), None);
        }
        PackedWitness { rows, origins }
    }

    fn check(
        &self,
        packed: &PackedWitness,
        preload: &MemoryPreload<MemoryId>,
    ) -> Result<(), Error> {
        let rows = &packed.rows;
        for (slot, assignment) in rows.iter().enumerate() {
            match neo_ccs::check_ccs_rowwise_zero(
                self.relation.r1cs().structure(),
                &assignment[..PUBLIC_INPUTS],
                &assignment[PUBLIC_INPUTS..],
            ) {
                Ok(()) => {}
                Err(neo_ccs::CcsError::RowFail { row }) => {
                    let constraint = self.relation.r1cs().catalog().rows()[row]
                        .tag()
                        .label()
                        .to_owned();
                    return Err(match packed.origins[slot] {
                        Some(step) => Unsatisfied::Constraint {
                            step,
                            row,
                            constraint,
                        },
                        None => Unsatisfied::PaddingConstraint {
                            batch: slot / self.size,
                            slot: slot % self.size,
                            batch_size: self.size,
                            row,
                            constraint,
                        },
                    }
                    .into());
                }
                Err(source) => {
                    return Err(Error::CcsCheck { slot, source });
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
        .map_err(Unsatisfied::Memory)?;

        check_continuity_rows(&self.continuity, rows).map_err(|source| match source {
            neo_application::ContinuityCheckError::Mismatch { .. } => {
                Error::Unsatisfied(Unsatisfied::Continuity(source))
            }
            _ => Error::ContinuityCheckError(source),
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
pub fn verify_sat_batched(
    trace: &Trace,
    batch_size: usize,
    signers: &BTreeSet<u64>,
) -> Result<(), Error> {
    verify_sat_inner(trace, batch_size, None, signers)
}

/// Check the relation and the exact final per-instance trace commitment map.
/// The expected map is supplied by the caller, not derived from the witness.
/// Transaction proofs bind this map through the aggregate transaction digest.
/// This is a host diagnostic; the proving API authenticates memory accesses with Nebula.
pub fn verify_sat_with_commitments(
    trace: &Trace,
    batch_size: usize,
    expected: &crate::TraceCommitments,
    signers: &BTreeSet<u64>,
) -> Result<(), Error> {
    verify_sat_inner(trace, batch_size, Some(expected), signers)
}

fn verify_sat_inner(
    trace: &Trace,
    batch_size: usize,
    expected: Option<&crate::TraceCommitments>,
    signers: &BTreeSet<u64>,
) -> Result<(), Error> {
    let normalized = crate::step::normalize_with_inputs(trace, signers);
    verify_normalized(normalized, batch_size, expected, None, signers)
}

pub(crate) fn verify_transaction(
    trace: &Trace,
    batch_size: usize,
    statement: &starstream_interleaving_spec::TransactionStatement,
    commitments: &crate::TraceCommitments,
) -> Result<(), Error> {
    let normalized = crate::step::normalize_with_inputs(trace, &statement.signers);
    verify_normalized(
        normalized,
        batch_size,
        Some(commitments),
        Some(statement),
        &statement.signers,
    )
}

fn verify_normalized(
    normalized: crate::step::NormalizedTrace,
    batch_size: usize,
    expected: Option<&crate::TraceCommitments>,
    statement: Option<&starstream_interleaving_spec::TransactionStatement>,
    signers: &BTreeSet<u64>,
) -> Result<(), Error> {
    let batch = Batch::new(batch_size)?;
    check_normalized(
        &batch,
        normalized,
        Roots::Expected(expected),
        statement,
        signers,
    )?;
    Ok(())
}

enum Roots<'a> {
    Expected(Option<&'a crate::TraceCommitments>),
    FromFinalization(&'a mut crate::TraceCommitments),
}

fn check_normalized(
    batch: &Batch,
    normalized: crate::step::NormalizedTrace,
    roots: Roots<'_>,
    statement: Option<&starstream_interleaving_spec::TransactionStatement>,
    signers: &BTreeSet<u64>,
) -> Result<PackedWitness, Error> {
    let mut preload = crate::memory::preload_tables(&normalized.method_table);
    crate::memory::preload_signers(&mut preload, signers);
    let packed = batch.pack(&normalized.steps);
    batch.check(&packed, &preload)?;
    check_packed_statement(&packed, roots, statement)?;
    Ok(packed)
}

fn check_packed_statement(
    packed: &PackedWitness,
    mut roots: Roots<'_>,
    statement: Option<&starstream_interleaving_spec::TransactionStatement>,
) -> Result<(), Error> {
    let mut terminal_claim = crate::terminal::TerminalClaim::Execution;
    if let Some(statement) = statement {
        let mut rows = packed
            .rows
            .iter()
            .zip(&packed.origins)
            .filter(|(_, origin)| origin.is_some())
            .map(|(row, _)| row.as_slice());
        let destination = match &mut roots {
            Roots::Expected(_) => None,
            Roots::FromFinalization(observed) => Some(&mut **observed),
        };
        crate::transaction::check_statement(rows.clone(), statement, destination)?;
        let roots = match roots {
            Roots::Expected(expected) => expected.ok_or(Unsatisfied::TransactionStatement)?,
            Roots::FromFinalization(observed) => observed,
        };
        let digest = crate::transaction_commitment(statement, roots)?;
        if rows
            .next()
            .is_none_or(|row| COL_IO_BEFORE.map(|c| row[c]) != [F::ZERO; 4])
        {
            return Err(Unsatisfied::TransactionStatement.into());
        }
        terminal_claim = crate::terminal::TerminalClaim::Transaction {
            commitment: digest.map(F::new),
        };
    } else if let Roots::Expected(Some(expected)) = roots {
        // Execution-only diagnostics compare roots directly; transactions
        // authenticate them through the aggregate digest above.
        check_commitment_statement(packed, expected)?;
    }
    // Check the actual final slot, including padding.
    let terminal = packed
        .rows
        .last()
        .ok_or(Unsatisfied::TerminalCallStackNotEmpty { actual: F::ONE })?;
    terminal_claim.check(|c| terminal[c])?;
    Ok(())
}

fn check_commitment_statement(
    packed: &PackedWitness,
    expected: &crate::TraceCommitments,
) -> Result<(), Error> {
    use p3_field::PrimeField64;
    let mut actual = crate::TraceCommitments::new();
    for (row, origin) in packed.rows.iter().zip(&packed.origins) {
        if origin.is_none() {
            continue;
        }
        let at = |column: usize| row[column].as_canonical_u64();
        if at(crate::ccs::layout::COL_EVENT_ACTIVE) == 0 {
            continue;
        }
        actual.insert(
            u32::try_from(at(crate::ccs::layout::COL_EVENT_OWNER))
                .expect("range-checked coroutine id"),
            COL_OUT.map(at),
        );
    }
    if &actual != expected {
        return Err(Unsatisfied::TraceCommitments.into());
    }
    Ok(())
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

    #[test]
    fn transaction_without_proposed_roots_returns_an_error() {
        use starstream_interleaving_spec::{StarstreamValue, Step};
        let trace = crate::tests::entered([
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::FinalizeCoordinator,
            Step::FinishTransaction,
        ]);
        let normalized = crate::step::normalize(&trace);
        assert!(matches!(
            verify_normalized(
                normalized,
                1,
                None,
                Some(&crate::tests::empty_statement()),
                &Default::default()
            ),
            Err(Error::Unsatisfied(Unsatisfied::TransactionStatement))
        ));
    }
    use crate::tests::{constructor_trace, method_call_trace};

    #[test]
    fn rejects_invalid_batch_sizes_and_empty_execution() {
        let trace = constructor_trace([1, 2, 3, 4]);
        for size in [0, usize::MAX] {
            assert!(matches!(
                verify_sat_batched(&trace, size, &Default::default()),
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
            verify_sat_batched(&Trace::new([]), 3, &Default::default()),
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
                    step: 8,
                    constraint,
                    ..
                })) if constraint == "return constraints"));
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
        // One slot more than the trace, so the last slot is padding.
        let batch = Batch::new(normalized.steps.len() + 1).unwrap();
        let mut packed = batch.pack(&normalized.steps);
        // Last slot has no outgoing link: preservation must be constrained
        // inside the padding row itself.
        let mut pad = build_witness_vector(&normalized.steps.last().unwrap().padding_after());
        pad[COL_CURR_PHASE_AFTER] = F::ONE;
        range_check_layout().assign_bits(&mut pad).unwrap();
        *packed.rows.last_mut().unwrap() = pad;
        assert!(matches!(
            batch.check(&packed, &preload),
            Err(Error::Unsatisfied(Unsatisfied::PaddingConstraint {
                batch: 0,
                slot: 8,
                batch_size: 9,
                constraint,
                ..
            })) if constraint == "padding preserves state"));
    }

    #[test]
    fn rejects_broken_continuity_inside_and_between_batches() {
        let (mut rows, preload) = crate::build_witness_rows(&constructor_trace([1, 2, 3, 4]));
        let normalized = normalize(&constructor_trace([1, 2, 3, 4]));
        // Preserve every row's local allocator transition, but break boundary 3.
        for row in &mut rows[4..] {
            row[COL_NEXT_UTXO_ID_BEFORE] += F::ONE;
            row[COL_NEXT_UTXO_ID_AFTER] += F::ONE;
            range_check_layout().assign_bits(row).unwrap();
        }
        // Rows 3 and 4 share a batch at size 3 and are split at size 2.
        for size in [3, 2] {
            let batch = Batch::new(size).unwrap();
            let mut packed = batch.pack(&normalized.steps);
            packed.rows[..rows.len()].clone_from_slice(&rows);
            assert!(matches!(
                batch.check(&packed, &preload),
                Err(Error::Unsatisfied(Unsatisfied::Continuity(
                    neo_application::ContinuityCheckError::Mismatch { boundary: 3, .. },
                )))
            ));
        }
    }

    #[test]
    fn rejects_tampered_memory_inside_and_between_batches() {
        let (mut rows, preload) = crate::build_witness_rows(&method_call_trace(true));
        let normalized = normalize(&method_call_trace(true));
        // The entry extends the callee's trace root, stored by the callee's
        // last event in an earlier slot: another batch at size 8, the same
        // batch at size 9.
        rows[8][crate::ccs::layout::COL_IN[0]] += F::ONE;
        crate::commitment::assign_from_bus(&mut rows[8], crate::opcode::Opcode::EnterCall);
        range_check_layout().assign_bits(&mut rows[8]).unwrap();
        for size in [1, 8, 9] {
            let batch = Batch::new(size).unwrap();
            let mut packed = batch.pack(&normalized.steps);
            packed.rows[8] = rows[8].clone();
            assert!(matches!(
                batch.check(&packed, &preload),
                Err(Error::Unsatisfied(Unsatisfied::Memory(
                    neo_application::MemoryCheckError::ReadModifyWriteMismatch { row: 8, .. },
                )))
            ));
        }
    }
}
