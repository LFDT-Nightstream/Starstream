//! Transaction boundaries; execution-only diagnostics remain available separately.
use neo_application::TaggedR1csBuilder;
use neo_math::F;
use p3_field::{PrimeCharacteristicRing, PrimeField64};
use starstream_interleaving_spec::{Trace, TransactionStatement};

use crate::{
    Error, Unsatisfied,
    ccs::{
        layout::*,
        tags::{ConstraintScope, always},
    },
    ivc_state::TxPhase,
    opcode::Opcode,
};

#[cfg(test)]
mod tests;

/// Check loading, execution, finalization, transaction IO and event commitments.
/// RAM and statement binding are host checks; this does not construct a proof.
pub fn verify_transaction_sat(
    trace: &Trace,
    batch_size: usize,
    statement: &TransactionStatement,
    commitments: &crate::TraceCommitments,
) -> Result<(), Error> {
    crate::batch::verify_transaction(trace, batch_size, statement, commitments)
}

pub(crate) fn constraints(b: &mut TaggedR1csBuilder<'_, ConstraintScope>) {
    b.with_tag(always("transaction boundaries"), |b| {
        let sum = |predicate: fn(&Opcode) -> bool| {
            Opcode::all()
                .into_iter()
                .filter(predicate)
                .map(|op| (op.selector(), F::ONE))
                .collect::<Vec<_>>()
        };
        for (flag, terms) in [
            (COL_METHOD_APPEND, sum(Opcode::appends_method)),
            (COL_EVENT_ACTIVE, sum(Opcode::has_event)),
            (COL_ABI_METHOD_COUNT_READ, sum(Opcode::scans_output)),
            (
                COL_ABI_METHOD_COUNT_WRITE,
                sum(|op| op.appends_method() || *op == Opcode::YieldBegin),
            ),
        ] {
            b.push_linear_zero(terms.into_iter().chain([(flag, -F::ONE)]));
        }
        b.push_linear_zero([
            (COL_OUTPUT_CURSOR_AFTER, F::ONE),
            (COL_OUTPUT_CURSOR_BEFORE, -F::ONE),
            (COL_SEL_GET_STORAGE, -F::ONE),
            (COL_SEL_SKIP_CONSUMED, -F::ONE),
        ]);
        // Loading=00 and Running=01 are the only admissible execution phases.
        let high_bit = range_check_layout()
            .bit_columns_for(COL_TX_PHASE_BEFORE)
            .expect("transaction phase has two bits")
            .start
            + 1;
        let execution = sum(Opcode::is_execution);
        for (column, value) in [
            (high_bit, 0),
            (COL_TX_PHASE_AFTER, TxPhase::Running as u64),
            (COL_LAST_INPUT_HAS_ABI_BEFORE, 1),
        ] {
            b.push_row(
                execution.iter().copied(),
                [(column, F::ONE), (COL_ONE, -F::new(value))],
                [],
            );
        }
        let loading = [
            (COL_SEL_SET_STORAGE, F::ONE),
            (COL_SEL_PRELOAD_METHOD, F::ONE),
        ];
        b.push_row(
            loading,
            [
                (COL_TX_PHASE_BEFORE, F::ONE),
                (COL_ONE, -F::new(TxPhase::Loading as u64)),
            ],
            [],
        );
        b.push_gated_linear_zero(
            COL_SEL_READ_ABI,
            [
                (COL_TX_PHASE_BEFORE, F::ONE),
                (COL_ONE, -F::new(TxPhase::Running as u64)),
            ],
        );
        // Loading and ABI enumeration preserve the transaction phase.
        b.push_row(
            loading.into_iter().chain([(COL_SEL_READ_ABI, F::ONE)]),
            [(COL_TX_PHASE_AFTER, F::ONE), (COL_TX_PHASE_BEFORE, -F::ONE)],
            [],
        );
        // Only loading changes this flag; padding preserves it separately.
        b.push_row(
            sum(|op| {
                !matches!(
                    op,
                    Opcode::SetStorage | Opcode::PreloadMethod | Opcode::Padding
                )
            }),
            [
                (COL_LAST_INPUT_HAS_ABI_AFTER, F::ONE),
                (COL_LAST_INPUT_HAS_ABI_BEFORE, -F::ONE),
            ],
            [],
        );
        let finalization = sum(|op| op.scans_output() || *op == Opcode::FinishTransaction);
        // The empty stack separates output processing from execution.
        for (column, value) in [
            (COL_TX_PHASE_BEFORE, TxPhase::Running as u64),
            (COL_CALL_SP_BEFORE, 0),
        ] {
            b.push_row(
                finalization.iter().copied(),
                [(column, F::ONE), (COL_ONE, -F::new(value))],
                [],
            );
        }
        b.push_row(
            finalization,
            [
                (COL_TX_PHASE_AFTER, F::ONE),
                (COL_ONE, -F::new(TxPhase::Running as u64)),
            ],
            [(
                COL_SEL_FINISH_TRANSACTION,
                F::new(TxPhase::Finished as u64) - F::new(TxPhase::Running as u64),
            )],
        );
        b.push_row(
            sum(|op| !matches!(op, Opcode::ReadAbi | Opcode::Padding)),
            [(COL_ABI_READ_REMAINING_BEFORE, F::ONE)],
            [],
        );
        let preserve_read =
            sum(|op| !matches!(op, Opcode::GetStorage | Opcode::ReadAbi | Opcode::Padding));
        for (after, before) in [
            (COL_ABI_READ_REMAINING_AFTER, COL_ABI_READ_REMAINING_BEFORE),
            (COL_ABI_READ_ORDINAL_AFTER, COL_ABI_READ_ORDINAL_BEFORE),
        ] {
            b.push_row(
                preserve_read.iter().copied(),
                [(after, F::ONE), (before, -F::ONE)],
                [],
            );
        }
        b.push_row(
            sum(|op| matches!(op, Opcode::SetStorage | Opcode::GetStorage)),
            [(COL_EVENT_OWNER, F::ONE), (COL_BOUNDARY_UTXO, -F::ONE)],
            [],
        );
        b.push_row(
            sum(|op| op.has_event() && !matches!(op, Opcode::SetStorage | Opcode::GetStorage)),
            [(COL_EVENT_OWNER, F::ONE), (COL_CURR_BEFORE, -F::ONE)],
            [],
        );
        for op in Opcode::all() {
            // Padding's carried-state equalities are emitted by ccs.rs.
            if op == Opcode::Padding {
                continue;
            }
            let gate = op.selector();
            let constant = |b: &mut TaggedR1csBuilder<'_, ConstraintScope>, col, value: u64| {
                b.push_gated_linear_zero(gate, [(col, F::ONE), (COL_ONE, -F::new(value))]);
            };
            let equal = |b: &mut TaggedR1csBuilder<'_, ConstraintScope>, a, c| {
                b.push_gated_linear_zero(gate, [(a, F::ONE), (c, -F::ONE)]);
            };
            match op {
                Opcode::GetStorage => {
                    equal(b, COL_ABI_READ_REMAINING_AFTER, COL_ABI_METHOD_COUNT_BEFORE);
                    constant(b, COL_ABI_READ_ORDINAL_AFTER, 0);
                }
                Opcode::ReadAbi => {
                    // Both counters are 8-bit: decrementing zero cannot wrap.
                    b.push_gated_linear_zero(
                        gate,
                        [
                            (COL_ABI_READ_REMAINING_AFTER, F::ONE),
                            (COL_ABI_READ_REMAINING_BEFORE, -F::ONE),
                            (COL_ONE, F::ONE),
                        ],
                    );
                    b.push_gated_linear_zero(
                        gate,
                        [
                            (COL_ABI_READ_ORDINAL_AFTER, F::ONE),
                            (COL_ABI_READ_ORDINAL_BEFORE, -F::ONE),
                            (COL_ONE, -F::ONE),
                        ],
                    );
                    b.push_gated_linear_zero(
                        gate,
                        [
                            (COL_BOUNDARY_UTXO, F::ONE),
                            (COL_OUTPUT_CURSOR_BEFORE, -F::new(2)),
                            (COL_ONE, F::ONE),
                        ],
                    );
                    equal(b, COL_ABI_GENERATION_ADDR, COL_BOUNDARY_UTXO);
                    equal(b, COL_ENABLED_METHOD_LOG_UTXO, COL_BOUNDARY_UTXO);
                    equal(
                        b,
                        COL_ENABLED_METHOD_LOG_GENERATION,
                        COL_ABI_GENERATION_BEFORE,
                    );
                    equal(
                        b,
                        COL_ENABLED_METHOD_LOG_ORDINAL,
                        COL_ABI_READ_ORDINAL_BEFORE,
                    );
                }
                _ => {}
            }
            match op {
                Opcode::SetStorage => {
                    constant(b, COL_LAST_INPUT_HAS_ABI_BEFORE, 1);
                    constant(b, COL_LAST_INPUT_HAS_ABI_AFTER, 0);
                }
                Opcode::PreloadMethod => constant(b, COL_LAST_INPUT_HAS_ABI_AFTER, 1),
                _ => {}
            }
            if op == Opcode::FinishTransaction {
                equal(b, COL_OUTPUT_CURSOR_BEFORE, COL_NEXT_UTXO_ID_BEFORE);
            }
            if op.scans_output() {
                b.push_gated_linear_zero(
                    gate,
                    [
                        (COL_BOUNDARY_UTXO, F::ONE),
                        (COL_OUTPUT_CURSOR_BEFORE, -F::new(2)),
                        (COL_ONE, -F::ONE),
                    ],
                );
                b.push_gated_linear_zero(
                    gate,
                    [
                        (COL_OUTPUT_REMAINING, F::ONE),
                        (COL_NEXT_UTXO_ID_BEFORE, -F::ONE),
                        (COL_OUTPUT_CURSOR_BEFORE, F::ONE),
                        (COL_ONE, F::ONE),
                    ],
                );
            }
            if matches!(op, Opcode::SetStorage | Opcode::PreloadMethod) {
                b.push_gated_linear_zero(
                    gate,
                    [
                        (COL_BOUNDARY_UTXO, F::ONE),
                        (COL_NEXT_UTXO_ID_BEFORE, -F::new(2)),
                        (
                            COL_ONE,
                            if op == Opcode::SetStorage {
                                -F::ONE
                            } else {
                                F::ONE
                            },
                        ),
                    ],
                );
            }
            if op == Opcode::SetStorage {
                equal(b, COL_RESOURCE_RESOLVER_ADDR_CID, COL_CURR_BEFORE);
                equal(b, COL_RESOURCE_RESOLVER_VALUE, COL_BOUNDARY_UTXO);
            }
            if op == Opcode::PreloadMethod {
                equal(b, COL_ABI_GENERATION_ADDR, COL_BOUNDARY_UTXO);
                equal(b, COL_ENABLED_METHOD_LOG_UTXO, COL_BOUNDARY_UTXO);
                equal(
                    b,
                    COL_ENABLED_METHOD_LOG_GENERATION,
                    COL_ABI_GENERATION_BEFORE,
                );
                equal(
                    b,
                    COL_ENABLED_METHOD_LOG_ADDR,
                    COL_ENABLED_METHOD_LOG_LEN_BEFORE,
                );
            }
            if op.appends_method() || op == Opcode::YieldBegin {
                equal(b, COL_ABI_METHOD_COUNT_ADDR, COL_ABI_GENERATION_ADDR);
                if op.appends_method() {
                    b.push_gated_linear_zero(
                        op.selector(),
                        [
                            (COL_ABI_METHOD_COUNT_AFTER, F::ONE),
                            (COL_ABI_METHOD_COUNT_BEFORE, -F::ONE),
                            (COL_ONE, -F::ONE),
                        ],
                    );
                    equal(
                        b,
                        COL_ENABLED_METHOD_LOG_ORDINAL,
                        COL_ABI_METHOD_COUNT_BEFORE,
                    );
                } else {
                    constant(b, COL_ABI_METHOD_COUNT_AFTER, 0);
                }
            } else if op.scans_output() {
                equal(b, COL_ABI_METHOD_COUNT_ADDR, COL_BOUNDARY_UTXO);
                if op == Opcode::SkipConsumed {
                    constant(b, COL_ABI_METHOD_COUNT_BEFORE, 0);
                }
            }
        }
        b.push_row(
            [(COL_ABI_METHOD_COUNT_BEFORE, F::ONE)],
            [(COL_ABI_METHOD_COUNT_INVERSE, F::ONE)],
            [(COL_SEL_GET_STORAGE, F::ONE)],
        );
    });
}

/// Bind checked row buses, not the source trace or normalization metadata.
/// TODO(proof): Authenticate these boundary values and RAM endpoints in the
/// eventual proof statement. This API is a satisfiability diagnostic only.
pub(crate) fn check_statement(
    rows: &[Vec<F>],
    statement: &TransactionStatement,
) -> Result<(), Error> {
    let reject = || Error::from(Unsatisfied::TransactionStatement);
    let first = rows.first().ok_or_else(reject)?;
    for (column, value) in [
        (COL_TX_PHASE_BEFORE, 0),
        (COL_LAST_INPUT_HAS_ABI_BEFORE, 1),
        (COL_OUTPUT_CURSOR_BEFORE, 0),
        (COL_ABI_READ_REMAINING_BEFORE, 0),
        (COL_ABI_READ_ORDINAL_BEFORE, 0),
        (COL_CURR_BEFORE, 2),
        (COL_CURR_PHASE_BEFORE, 0),
        (COL_CALL_SP_BEFORE, 1),
        (COL_NEXT_UTXO_ID_BEFORE, 0),
        (COL_ENABLED_METHOD_LOG_LEN_BEFORE, 0),
        (COL_PENDING_CTOR_PRESENT_BEFORE, 0),
        (COL_PENDING_CTOR_HOLDER_BEFORE, 0),
        (COL_PENDING_CTOR_HANDLE_BEFORE, 0),
    ] {
        if first[column] != F::new(value) {
            return Err(reject());
        }
    }
    if rows.last().ok_or_else(reject)?[COL_TX_PHASE_AFTER] != F::new(TxPhase::Finished as u64) {
        return Err(reject());
    }
    let mut inputs = 0;
    let mut outputs = 0;
    let mut methods = Vec::<Vec<[u32; 8]>>::new();
    let mut output_methods = Vec::<Vec<[u32; 8]>>::new();
    for row in rows {
        let at = |c: usize| row[c].as_canonical_u64();
        if row[COL_SEL_SET_STORAGE] == F::ONE {
            let input = statement.inputs.get(inputs).ok_or_else(reject)?;
            if COL_ARGUMENT_ROOT.map(at) != input.storage.0 {
                return Err(reject());
            }
            inputs += 1;
            methods.push(Default::default());
        }
        if row[COL_SEL_PRELOAD_METHOD] == F::ONE {
            methods.last_mut().ok_or_else(reject)?.push(
                COL_METHOD_HASH_VALUE
                    .map(|c| u32::try_from(at(c)).expect("range checked method limb")),
            );
        }
        if row[COL_SEL_GET_STORAGE] == F::ONE {
            let output = statement.outputs.get(outputs).ok_or_else(reject)?;
            if at(COL_OUTPUT_CURSOR_BEFORE) != u64::from(output.utxo)
                || COL_RESULT_ROOT.map(at) != output.storage.0
            {
                return Err(reject());
            }
            outputs += 1;
            output_methods.push(Vec::new());
        }
        if row[COL_SEL_READ_ABI] == F::ONE {
            output_methods.last_mut().ok_or_else(reject)?.push(
                COL_METHOD_HASH_VALUE
                    .map(|c| u32::try_from(at(c)).expect("range checked method limb")),
            );
        }
    }
    if inputs != statement.inputs.len()
        || outputs != statement.outputs.len()
        || output_methods
            .iter()
            .zip(&statement.outputs)
            .any(|(actual, output)| {
                actual
                    .iter()
                    .copied()
                    .ne(output.methods.iter().map(|m| m.0))
            })
        || methods
            .iter()
            .zip(&statement.inputs)
            .any(|(actual, input)| actual.iter().copied().ne(input.methods.iter().map(|m| m.0)))
    {
        return Err(reject());
    }
    Ok(())
}
