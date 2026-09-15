use super::*;
use crate::{step::normalize_with_phase, witness::build_witness_vector};
use starstream_interleaving_spec::{
    InputUtxo, MethodHash, OutputUtxo, ResourceHandle, StarstreamValue, Step,
};

fn fixture() -> (Trace, TransactionStatement) {
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
fn count_ram_resets_and_outputs_accept_multiple_registrations() {
    let method = MethodHash([1; 8]);
    let (mut trace, _) = fixture();
    trace.0.insert(trace.0.len() - 1, Step::ReadAbi { method });
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
    let normalized = normalize_with_phase(&trace, TxPhase::Loading);
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
        let normalized = normalize_with_phase(&trace, TxPhase::Loading);
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
    let normalized = normalize_with_phase(&trace, TxPhase::Loading);
    let relation = crate::ccs::build_relation().unwrap();
    for opcode in [Opcode::GetStorage, Opcode::FinishTransaction] {
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
                range_check_layout().assign_bits(&mut row).unwrap();
                let result = neo_ccs::check_ccs_rowwise_zero(
                    relation.r1cs().structure(),
                    &row[..PUBLIC_INPUTS],
                    &row[PUBLIC_INPUTS..],
                );
                if phase == TxPhase::Running && stack_depth == 0 {
                    result.unwrap();
                } else {
                    assert!(
                        matches!(result, Err(neo_ccs::CcsError::RowFail { .. })),
                        "{opcode:?}, {phase:?}, {stack_depth}: {result:?}"
                    );
                }
            }
        }
    }
}

#[test]
fn execution_phase_transition_is_one_way() {
    let (trace, _) = fixture();
    let normalized = normalize_with_phase(&trace, TxPhase::Loading);
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
    let normalized = normalize_with_phase(&trace, TxPhase::Loading);
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
    let normalized = normalize_with_phase(&trace, TxPhase::Loading);
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
    // A different opaque result is locally valid; the old transaction output isn't.
    crate::verify_witness_rows(&rows, &preload).unwrap();
    assert!(matches!(
        check_statement(&rows, &statement),
        Err(Error::Unsatisfied(Unsatisfied::TransactionStatement))
    ));
}
