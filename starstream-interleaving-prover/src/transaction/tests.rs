use super::*;
use crate::{step::normalize, witness::build_witness_vector};
use starstream_interleaving_spec::{
    InputUtxo, MethodHash, OutputUtxo, ResourceHandle, StarstreamValue, Step,
};

pub(crate) fn fixture() -> (Trace, TransactionStatement) {
    let method = MethodHash([1; 8]);
    let storage = StarstreamValue([11, 12, 13, 14]);
    (
        Trace::new([
            Step::SetStorage {
                storage: storage.clone(),
                resource: ResourceHandle(0).into(),
            },
            Step::PreloadMethod { method },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::GetStorage {
                storage: storage.clone().into(),
            },
            Step::ReadAbi { method },
            Step::FinalizeCoordinator,
            Step::FinishTransaction,
        ]),
        TransactionStatement {
            inputs: vec![InputUtxo {
                storage: storage.clone(),
                methods: vec![method],
            }],
            outputs: vec![OutputUtxo {
                utxo: 0,
                storage,
                methods: vec![method],
            }],
        },
    )
}

#[test]
fn final_instance_roots_are_authenticated_by_memory() {
    let unit = StarstreamValue::UNIT_VALUE;
    let consumed = Trace::new([
        Step::NewUtxo {
            arguments: unit.clone(),
            resource: ResourceHandle(0).into(),
        },
        Step::EnterConstructor {
            arguments: unit.clone(),
        },
        Step::Return {
            result: unit.clone().into(),
        },
        Step::Return {
            result: unit.into(),
        },
        Step::SkipConsumed,
        Step::FinalizeCoordinator,
        Step::FinishTransaction,
    ]);
    let normalized = normalize(&consumed);
    let rows: Vec<_> = normalized.steps.iter().map(build_witness_vector).collect();
    let preload = crate::memory::preload_tables(&normalized.method_table);
    crate::verify_witness_rows(&rows, &preload).unwrap();
    for opcode in [Opcode::SkipConsumed, Opcode::FinalizeCoordinator] {
        let index = normalized
            .steps
            .iter()
            .position(|s| s.opcode == opcode)
            .unwrap();
        let mut tampered = rows.clone();
        tampered[index][COL_IN[0]] += F::ONE;
        crate::commitment::assign_from_bus(&mut tampered[index], opcode);
        range_check_layout()
            .assign_bits(&mut tampered[index])
            .unwrap();
        // Refresh the entire aggregate suffix: rejection must come from RAM,
        // not stale Poseidon advice or a broken digest continuity link.
        let mut chain = [F::ZERO; 4];
        for (row, step) in tampered.iter_mut().zip(&normalized.steps) {
            for (c, value) in COL_IO_BEFORE.into_iter().zip(chain) {
                row[c] = value;
            }
            crate::commitment::assign_from_bus(row, step.opcode);
            chain = COL_IO_AFTER.map(|c| row[c]);
        }
        assert!(matches!(
            crate::verify_witness_rows(&tampered, &preload),
            Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
        ));
    }
}

#[test]
fn count_ram_resets_and_outputs_accept_multiple_registrations() {
    let method = MethodHash([1; 8]);
    let (mut trace, _) = fixture();
    trace.0.insert(trace.0.len() - 2, Step::ReadAbi { method });
    trace.0.splice(
        2..2,
        [
            Step::PreloadMethod { method },
            Step::PreloadMethod { method },
            Step::CallMethod {
                resource: ResourceHandle(0),
                method,
                arguments: StarstreamValue::UNIT_VALUE,
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::EnterMethod {
                method,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::YieldBegin,
            Step::RegisterMethod { method },
            Step::RegisterMethod { method },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
        ],
    );
    let normalized = normalize(&trace);
    let rows: Vec<_> = normalized.steps.iter().map(build_witness_vector).collect();
    // Calls choose the latest duplicate; enumeration chooses each ordinal in
    // the current generation, not the matching methods from before the reset.
    let lookup_addresses = normalized
        .steps
        .iter()
        .filter(|step| matches!(step.opcode, Opcode::CallMethod | Opcode::ReadAbi))
        .map(|step| step.enabled_method_log_address)
        .collect::<Vec<_>>();
    assert_eq!(lookup_addresses, [2, 3, 4]);
    let preload = crate::memory::preload_tables(&normalized.method_table);
    crate::verify_witness_rows(&rows, &preload).unwrap();
    let index = normalized
        .steps
        .iter()
        .position(|s| s.opcode == Opcode::YieldBegin)
        .unwrap();
    assert_eq!(rows[index][COL_ABI_METHOD_COUNT_BEFORE], F::new(3));
    assert_eq!(rows[index][COL_ABI_METHOD_COUNT_AFTER], F::ZERO);
    assert_eq!(rows[index + 1][COL_ENABLED_METHOD_LOG_ORDINAL], F::ZERO);
    let output = normalized
        .steps
        .iter()
        .position(|s| s.opcode == Opcode::GetStorage)
        .unwrap();
    assert_eq!(rows[output][COL_ABI_METHOD_COUNT_BEFORE], F::new(2));

    let read = output + 1;
    let mut old_generation = rows.clone();
    // Same method, UTXO and ordinal exist in generation zero. All local
    // lookup equalities still hold; the current-generation RAM must reject it.
    old_generation[read][COL_ENABLED_METHOD_LOG_ADDR] = F::ZERO;
    old_generation[read][COL_ENABLED_METHOD_LOG_GENERATION] = F::ZERO;
    old_generation[read][COL_ABI_GENERATION_BEFORE] = F::ZERO;
    range_check_layout()
        .assign_bits(&mut old_generation[read])
        .unwrap();
    assert!(matches!(
        crate::verify_witness_rows(&old_generation, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
    ));

    // Duplicate methods still need distinct ordinals; rereading the first
    // registration cannot stand in for the second one.
    let mut duplicate_read = rows.clone();
    duplicate_read[read + 1][COL_ENABLED_METHOD_LOG_ADDR] = rows[read][COL_ENABLED_METHOD_LOG_ADDR];
    range_check_layout()
        .assign_bits(&mut duplicate_read[read + 1])
        .unwrap();
    assert!(matches!(
        crate::verify_witness_rows(&duplicate_read, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
    ));

    // Locally consistent increments must still read the actual RAM count.
    let mut changed = rows;
    changed[index + 1][COL_ABI_METHOD_COUNT_BEFORE] = F::new(3);
    changed[index + 1][COL_ABI_METHOD_COUNT_AFTER] = F::new(4);
    changed[index + 1][COL_ENABLED_METHOD_LOG_ORDINAL] = F::new(3);
    range_check_layout()
        .assign_bits(&mut changed[index + 1])
        .unwrap();
    assert!(matches!(
        crate::verify_witness_rows(&changed, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory { .. }))
    ));
}

#[test]
fn registration_counts_are_bounded_and_reset_per_generation() {
    let method = MethodHash([1; 8]);
    let (base, _) = fixture();
    for opcode in [Opcode::PreloadMethod, Opcode::RegisterMethod] {
        let mut trace = Trace::new([base.0[0].clone()]);
        if opcode == Opcode::RegisterMethod {
            trace.0.extend([
                Step::PreloadMethod { method },
                Step::CallMethod {
                    resource: ResourceHandle(0),
                    method,
                    arguments: StarstreamValue::UNIT_VALUE,
                    result: StarstreamValue::UNIT_VALUE.into(),
                },
                Step::EnterMethod {
                    method,
                    arguments: StarstreamValue::UNIT_VALUE,
                },
                Step::YieldBegin,
            ]);
        }
        let append = if opcode == Opcode::PreloadMethod {
            Step::PreloadMethod { method }
        } else {
            Step::RegisterMethod { method }
        };
        trace.0.extend(std::iter::repeat_n(append, 256));
        let normalized = normalize(&trace);
        let relation = crate::ccs::build_relation().unwrap();
        // Exercise both sides of the bound without hashing/checking 256 full rows.
        for (from_end, accepted) in [(2, true), (1, false)] {
            let row = build_witness_vector(&normalized.steps[normalized.steps.len() - from_end]);
            assert_eq!(
                row[COL_ABI_METHOD_COUNT_BEFORE],
                F::new(256 - from_end as u64)
            );
            assert_eq!(
                row[COL_ENABLED_METHOD_LOG_ORDINAL],
                row[COL_ABI_METHOD_COUNT_BEFORE]
            );
            let result = neo_ccs::check_ccs_rowwise_zero(
                relation.r1cs().structure(),
                &row[..PUBLIC_INPUTS],
                &row[PUBLIC_INPUTS..],
            );
            if accepted {
                result.unwrap();
            } else {
                assert!(matches!(result, Err(neo_ccs::CcsError::RowFail { .. })));
            }
        }
    }
}

#[test]
fn finalization_requires_completed_execution_and_cannot_reopen_it() {
    let (trace, _) = fixture();
    let normalized = normalize(&trace);
    let relation = crate::ccs::build_relation().unwrap();
    for opcode in [
        Opcode::GetStorage,
        Opcode::FinalizeCoordinator,
        Opcode::FinishTransaction,
    ] {
        let step = normalized
            .steps
            .iter()
            .find(|s| s.opcode == opcode)
            .unwrap();
        for phase in [TxPhase::Loading, TxPhase::Running, TxPhase::Finished] {
            for stack_depth in [0, 1] {
                let mut row = build_witness_vector(step);
                row[COL_TX_PHASE_BEFORE] = F::new(phase as u64);
                row[COL_CALL_SP_BEFORE] = F::new(stack_depth);
                row[COL_CALL_SP_AFTER] = F::new(stack_depth);
                crate::witness::assign_stride_columns(&mut row);
                range_check_layout().assign_bits(&mut row).unwrap();
                let result = neo_ccs::check_ccs_rowwise_zero(
                    relation.r1cs().structure(),
                    &row[..PUBLIC_INPUTS],
                    &row[PUBLIC_INPUTS..],
                );
                if phase == TxPhase::Running && stack_depth == 0 {
                    result.unwrap();
                } else {
                    let Err(neo_ccs::CcsError::RowFail { row }) = result else {
                        panic!("{opcode:?}, {phase:?}, {stack_depth}: {result:?}");
                    };
                    assert_eq!(
                        relation.r1cs().catalog().rows()[row].tag().label(),
                        "transaction boundaries"
                    );
                }
            }
        }
    }
}

#[test]
fn coordinator_and_finish_require_the_completed_output_cursor() {
    let (trace, _) = fixture();
    let normalized = normalize(&trace);
    let relation = crate::ccs::build_relation().unwrap();
    for opcode in [Opcode::FinalizeCoordinator, Opcode::FinishTransaction] {
        let step = normalized
            .steps
            .iter()
            .find(|s| s.opcode == opcode)
            .unwrap();
        for cursor in [0, 1, 2] {
            let mut row = build_witness_vector(step);
            // We just care about the value in BEFORE for this technically (it
            // should fail if it doesn't match the number of utxos, which would
            // mean finalization is not complete)
            //
            // We make them both the same though so that the checks fail because
            // of that, and not because of the preservation constraint
            row[COL_OUTPUT_CURSOR_BEFORE] = F::new(cursor);
            row[COL_OUTPUT_CURSOR_AFTER] = F::new(cursor);
            range_check_layout().assign_bits(&mut row).unwrap();
            let result = neo_ccs::check_ccs_rowwise_zero(
                relation.r1cs().structure(),
                &row[..PUBLIC_INPUTS],
                &row[PUBLIC_INPUTS..],
            );
            // the constraints should only be satisfiable with 1, since there is
            // only one utxo allocated
            if cursor == 1 {
                result.unwrap();
            } else {
                let Err(neo_ccs::CcsError::RowFail { row }) = result else {
                    panic!("{opcode:?}, {cursor}: {result:?}");
                };
                assert_eq!(
                    relation.r1cs().catalog().rows()[row].tag().label(),
                    "transaction boundaries"
                );
            }
        }
    }
}

#[test]
fn execution_phase_transition_is_one_way() {
    let (trace, _) = fixture();
    let normalized = normalize(&trace);
    let step = normalized
        .steps
        .iter()
        .find(|step| step.opcode.is_execution())
        .unwrap();
    let relation = crate::ccs::build_relation().unwrap();
    for before in [TxPhase::Loading, TxPhase::Running, TxPhase::Finished] {
        let mut row = build_witness_vector(step);
        row[COL_TX_PHASE_BEFORE] = F::new(before as u64);
        range_check_layout().assign_bits(&mut row).unwrap();
        let result = neo_ccs::check_ccs_rowwise_zero(
            relation.r1cs().structure(),
            &row[..PUBLIC_INPUTS],
            &row[PUBLIC_INPUTS..],
        );
        // Check the row in isolation so continuity cannot mask a missing phase guard.
        if matches!(before, TxPhase::Loading | TxPhase::Running) {
            result.unwrap();
            assert_eq!(row[COL_TX_PHASE_AFTER], F::new(TxPhase::Running as u64));
        } else {
            let Err(neo_ccs::CcsError::RowFail { row }) = result else {
                panic!("unexpected phase check: {result:?}");
            };
            assert_eq!(
                relation.r1cs().catalog().rows()[row].tag().label(),
                "transaction boundaries"
            );
        }
    }
}

#[test]
fn transaction_witness_tampering_is_rejected() {
    let (trace, statement) = fixture();
    let normalized = normalize(&trace);
    let rows = normalized
        .steps
        .iter()
        .map(build_witness_vector)
        .collect::<Vec<_>>();
    let preload = crate::memory::preload_tables(&normalized.method_table);
    crate::verify_witness_rows(&rows, &preload).unwrap();
    check_statement(&rows, &statement).unwrap();

    for (opcode, column, value) in [
        (Opcode::SetStorage, COL_TX_PHASE_BEFORE, F::ONE),
        (Opcode::Return, COL_TX_PHASE_AFTER, F::ZERO),
        (Opcode::Return, COL_TX_PHASE_BEFORE, F::new(2)),
        (Opcode::PreloadMethod, COL_METHOD_APPEND, F::ZERO),
        (Opcode::PreloadMethod, COL_ABI_METHOD_COUNT_AFTER, F::ZERO),
        (
            Opcode::PreloadMethod,
            COL_ENABLED_METHOD_LOG_ORDINAL,
            F::ONE,
        ),
        (Opcode::GetStorage, COL_EVENT_OWNER, F::new(2)),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_ORDINAL, F::ONE),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_ADDR, F::ONE),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_GENERATION, F::ONE),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_UTXO, F::new(3)),
        (Opcode::ReadAbi, COL_ABI_READ_REMAINING_AFTER, F::ONE),
        (Opcode::GetStorage, COL_OUTPUT_REMAINING, F::ONE),
        (Opcode::FinishTransaction, COL_OUTPUT_CURSOR_BEFORE, F::ZERO),
        (Opcode::FinalizeCoordinator, COL_TX_PHASE_BEFORE, F::ZERO),
        (Opcode::FinalizeCoordinator, COL_CALL_SP_BEFORE, F::ONE),
        (
            Opcode::FinalizeCoordinator,
            COL_OUTPUT_CURSOR_BEFORE,
            F::ZERO,
        ),
        (
            Opcode::FinalizeCoordinator,
            COL_COORD_FINALIZED_BEFORE,
            F::ONE,
        ),
        (Opcode::FinalizeCoordinator, COL_TRACE_ROOT_READ, F::ZERO),
    ] {
        let mut changed = rows.clone();
        let index = normalized
            .steps
            .iter()
            .position(|step| step.opcode == opcode)
            .unwrap();
        changed[index][column] = value;
        crate::witness::assign_stride_columns(&mut changed[index]);
        range_check_layout()
            .assign_bits(&mut changed[index])
            .unwrap();
        assert!(
            matches!(
                crate::verify_witness_rows(&changed, &preload),
                Err(Error::Unsatisfied(_))
            ),
            "{opcode:?}, column {column}"
        );
    }
}

#[test]
fn output_binding_reads_checked_witness_not_source_trace() {
    let (trace, statement) = fixture();
    let normalized = normalize(&trace);
    let mut rows = normalized
        .steps
        .iter()
        .map(build_witness_vector)
        .collect::<Vec<_>>();
    let preload = crate::memory::preload_tables(&normalized.method_table);
    let index = normalized
        .steps
        .iter()
        .position(|step| step.opcode == Opcode::GetStorage)
        .unwrap();
    rows[index][COL_CALL_STACK_EXPECTED_RESULT_VALUE[0]] += F::ONE;
    crate::commitment::assign_from_bus(&mut rows[index], Opcode::GetStorage);
    range_check_layout().assign_bits(&mut rows[index]).unwrap();
    let mut chain = [F::ZERO; 4];
    for (row, step) in rows.iter_mut().zip(&normalized.steps) {
        for (c, value) in COL_IO_BEFORE.into_iter().zip(chain) {
            row[c] = value;
        }
        crate::commitment::assign_from_bus(row, step.opcode);
        chain = COL_IO_AFTER.map(|c| row[c]);
    }
    // A different opaque result is locally valid; the old transaction output isn't.
    crate::verify_witness_rows(&rows, &preload).unwrap();
    assert!(matches!(
        check_statement(&rows, &statement),
        Err(Error::Unsatisfied(Unsatisfied::TransactionStatement))
    ));
}
