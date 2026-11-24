use super::*;
use crate::{step::normalize, witness::build_witness_vector};
use starstream_interleaving_spec::{
    InputUtxo, MethodHash, OutputUtxo, ResourceHandle, StarstreamValue, Step,
};

const CONSTRUCTOR: MethodHash = MethodHash([0xc; 8]);

pub(crate) fn fixture() -> (Trace, TransactionStatement) {
    let method = MethodHash([1; 8]);
    let storage = StarstreamValue([11, 12, 13, 14]);
    (
        crate::tests::loaded(
            [
                Step::SetStorage {
                    storage: storage.clone(),
                    entry_utxo_handle: ResourceHandle(0),
                },
                Step::PreloadMethod { method },
            ],
            [
                Step::Return {
                    result: StarstreamValue::UNIT_VALUE.into(),
                },
                Step::GetStorage {
                    storage: storage.clone().into(),
                },
                Step::ReadAbi { method },
                Step::FinalizeCoordinator,
                Step::FinishTransaction,
            ],
        ),
        TransactionStatement {
            input_tokens: Vec::new(),
            output_tokens: Vec::new(),
            signers: Default::default(),
            entry: crate::tests::ENTRY,
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
    let consumed = crate::tests::entered([
        Step::NewUtxo {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        Step::EnterConstructor {
            constructor: CONSTRUCTOR,
            arguments: unit.clone(),
        },
        Step::Return {
            result: unit.clone().into(),
        },
        Step::ReceiveNewUtxoHandle {
            resource: ResourceHandle(0).into(),
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
    // Two more preloads while loading, then a call in the entry body.
    trace.0.splice(
        2..2,
        [
            Step::PreloadMethod { method },
            Step::PreloadMethod { method },
        ],
    );
    let body = 4 + crate::tests::ENTRY_STEPS;
    trace.0.splice(
        body..body,
        [
            Step::CallMethod {
                resource: ResourceHandle(0),
                method,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::EnterCall {
                method,
                arguments: StarstreamValue::UNIT_VALUE,
            },
            Step::YieldBegin,
            Step::RegisterMethod { method },
            Step::RegisterMethod { method },
            Step::Return {
                result: StarstreamValue::UNIT_VALUE.into(),
            },
            Step::ReceiveCallResult {
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
            trace.0.push(Step::PreloadMethod { method });
            trace.0.extend(crate::tests::entered([]).0);
            trace.0.extend([
                Step::CallMethod {
                    resource: ResourceHandle(0),
                    method,
                    arguments: StarstreamValue::UNIT_VALUE,
                },
                Step::EnterCall {
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
    let entry = normalized
        .steps
        .iter()
        .find(|step| step.opcode == Opcode::BeginExecution)
        .unwrap();
    let relation = crate::ccs::build_relation().unwrap();
    // Only the entry call leaves Loading; execution rows stay in Running.
    for (step, accepted) in [(entry, TxPhase::Loading), (step, TxPhase::Running)] {
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
            if before == accepted {
                result.unwrap();
                assert_eq!(row[COL_TX_PHASE_AFTER], F::new(TxPhase::Running as u64));
            } else {
                let Err(neo_ccs::CcsError::RowFail { row }) = result else {
                    panic!(
                        "unexpected phase check: {:?}, {before:?}: {result:?}",
                        step.opcode
                    );
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
fn entry_call_is_bound_to_the_statement_and_the_entry_event() {
    let (trace, statement) = fixture();
    let normalized = normalize(&trace);
    let rows = normalized
        .steps
        .iter()
        .map(build_witness_vector)
        .collect::<Vec<_>>();
    let preload = crate::memory::preload_tables(&normalized.method_table);
    check_statement(&rows, &statement, None).unwrap();
    // A different script or argument root is a different statement.
    let mut script = statement.clone();
    script.entry.script.0[0] ^= 1;
    let mut arguments = statement.clone();
    arguments.entry.arguments.0[0] += 1;
    for changed in [script, arguments] {
        assert!(matches!(
            check_statement(&rows, &changed, None),
            Err(Error::Unsatisfied(Unsatisfied::TransactionStatement))
        ));
    }
    // The entry coordinator's own event must name the called script and
    // arguments: the entry call hands both to the entry step.
    let begin = normalized
        .steps
        .iter()
        .position(|step| step.opcode == Opcode::BeginExecution)
        .unwrap();
    for column in [
        COL_METHOD_HASH_VALUE[0],
        COL_CALL_STACK_EXPECTED_ARG_VALUE[0],
    ] {
        let mut changed = rows.clone();
        changed[begin][column] += F::ONE;
        crate::commitment::assign_from_bus(&mut changed[begin], Opcode::BeginExecution);
        range_check_layout()
            .assign_bits(&mut changed[begin])
            .unwrap();
        // Refresh the transaction chain so only the hand-off can reject.
        let mut chain = [F::ZERO; 4];
        for (row, step) in changed.iter_mut().zip(&normalized.steps) {
            for (c, value) in COL_IO_BEFORE.into_iter().zip(chain) {
                row[c] = value;
            }
            crate::commitment::assign_from_bus(row, step.opcode);
            chain = COL_IO_AFTER.map(|c| row[c]);
        }
        assert!(
            matches!(
                crate::verify_witness_rows(&changed, &preload),
                Err(Error::Unsatisfied(Unsatisfied::Constraint { step, constraint, .. }))
                    if step == begin && constraint == "call hand-off registers"
            ),
            "column {column}"
        );
    }
    // Execution cannot start without the entry call, nor the call repeat.
    let mut skipped = trace.clone();
    skipped.0.remove(begin);
    assert!(crate::verify_sat(&skipped, &Default::default()).is_err());
    let mut repeated = trace;
    repeated.0.insert(begin, repeated.0[begin].clone());
    assert!(crate::verify_sat(&repeated, &Default::default()).is_err());
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
    check_statement(&rows, &statement, None).unwrap();

    use crate::CoroutineId::{Coord, Token, Utxo};
    for (opcode, column, value) in [
        (Opcode::SetStorage, COL_TX_PHASE_BEFORE, F::ONE),
        (Opcode::BeginExecution, COL_TX_PHASE_BEFORE, F::ONE),
        (Opcode::BeginExecution, COL_TX_PHASE_AFTER, F::ZERO),
        (Opcode::BeginExecution, COL_CALL_TARGET, Coord(2).field()),
        (Opcode::BeginExecution, COL_CALL_SP_AFTER, F::ZERO),
        (Opcode::EnterCall, COL_TX_PHASE_BEFORE, F::ZERO),
        (Opcode::Return, COL_TX_PHASE_AFTER, F::ZERO),
        (Opcode::Return, COL_TX_PHASE_BEFORE, F::new(2)),
        (Opcode::PreloadMethod, COL_METHOD_APPEND, F::ZERO),
        (Opcode::PreloadMethod, COL_ABI_METHOD_COUNT_AFTER, F::ZERO),
        (
            Opcode::PreloadMethod,
            COL_ENABLED_METHOD_LOG_ORDINAL,
            F::ONE,
        ),
        (Opcode::GetStorage, COL_EVENT_OWNER, Coord(1).field()),
        (Opcode::GetStorage, COL_EVENT_OWNER, Token(0).field()),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_ORDINAL, F::ONE),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_ADDR, F::ONE),
        (Opcode::ReadAbi, COL_ENABLED_METHOD_LOG_GENERATION, F::ONE),
        (
            Opcode::ReadAbi,
            COL_ENABLED_METHOD_LOG_UTXO,
            Utxo(1).field(),
        ),
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
            COL_COORD_CURSOR_BEFORE,
            F::new(2),
        ),
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
            "{opcode:?}, column {column}, tampered value {value:?}"
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
    rows[index][COL_RESULT_ROOT[0]] += F::ONE;
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
        check_statement(&rows, &statement, None),
        Err(Error::Unsatisfied(Unsatisfied::TransactionStatement))
    ));
}

#[test]
fn signer_membership_checks_both_limbs_and_slot_presence() {
    let key = 0x1234_5678_9abc_def0;
    let trace = crate::tests::entered([
        Step::AssertSignedBy { key },
        Step::AssertSignedBy { key: 0 },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    let signers = [0, key, u64::MAX].into();
    crate::verify_sat(&trace, &signers).unwrap();
    for wrong in [key ^ 1, key ^ (1 << 32)] {
        assert!(crate::verify_sat(&trace, &[0, wrong].into()).is_err());
    }
    let normalized = crate::step::normalize_with_inputs(&trace, &signers);
    let mut rows: Vec<_> = normalized.steps.iter().map(build_witness_vector).collect();
    let mut preload = crate::memory::preload_tables(&normalized.method_table);
    crate::memory::preload_signers(&mut preload, &signers);
    let zero_step = crate::tests::ENTRY_STEPS + 1;
    // Both limbs of signer 0 contain zero. Reading the low limb twice would
    // pass the ROM value checks, but violates the high limb's address constraint.
    rows[zero_step][COL_SIGNER_KEY_ADDR[1]] = F::ZERO;
    assert!(matches!(
        crate::verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Constraint { constraint, .. }))
            if constraint == "signer key lookup addresses"
    ));

    // Give the zero key a consistent but unoccupied slot.
    rows[zero_step][COL_SIGNER_INDEX] = F::from_u32(3);
    for (offset, address) in COL_SIGNER_KEY_ADDR.into_iter().enumerate() {
        rows[zero_step][address] = F::new((3 * SIGNER_KEY_LIMBS + offset) as u64);
    }
    range_check_layout()
        .assign_bits(&mut rows[zero_step])
        .unwrap();
    assert!(matches!(
        crate::verify_witness_rows(&rows, &preload),
        Err(Error::Unsatisfied(Unsatisfied::Memory(_)))
    ));
    let zero = crate::tests::entered([
        Step::AssertSignedBy { key: 0 },
        Step::Return {
            result: StarstreamValue::UNIT_VALUE.into(),
        },
    ]);
    assert!(crate::verify_sat(&zero, &Default::default()).is_err());
}

#[test]
fn token_records_check_carry_forward() {
    use crate::ivc_state::CoroutineId::{Coord, Utxo};
    use starstream_interleaving_spec::{InputToken, OutputToken, StarstreamValue};
    let (trace, mut statement) = fixture();
    let roots = crate::commitment::tests::expected(
        &Trace::new(trace.0[..6].to_vec()),
        &[Utxo(0), Coord(1), Coord(1), Utxo(0)],
    );
    let empty = crate::transaction_commitment(&statement, &roots).unwrap();
    let storage = StarstreamValue([1, 2, 3, 4]);
    statement.input_tokens.push(InputToken {
        owner: 0,
        storage: storage.clone(),
    });
    statement.output_tokens.push(OutputToken {
        token: 0,
        owner: 0,
        storage,
    });
    verify_transaction_sat(&trace, 2, &statement, &roots).unwrap();
    let original = crate::transaction_commitment(&statement, &roots).unwrap();
    assert_eq!(original, empty);
    statement.output_tokens[0].token = 1;
    assert!(verify_transaction_sat(&trace, 2, &statement, &roots).is_err());
    statement.output_tokens[0].token = 0;
    statement.output_tokens[0].owner = 1;
    assert!(check_token_carry_forward(&statement).is_err());
    statement.output_tokens[0].owner = 0;
    statement.output_tokens[0].storage.0[0] += 1;
    assert!(check_token_carry_forward(&statement).is_err());
    statement.input_tokens[0].storage = statement.output_tokens[0].storage.clone();
    let updated = crate::transaction_commitment(&statement, &roots).unwrap();
    assert_eq!(updated, original);
    let token = statement.output_tokens.pop().unwrap();
    assert!(check_token_carry_forward(&statement).is_err());
    statement.output_tokens.push(token);

    // Two distinct tokens can share both an owner and a storage value.
    statement
        .input_tokens
        .push(statement.input_tokens[0].clone());
    statement
        .output_tokens
        .push(statement.output_tokens[0].clone());
    assert!(check_token_carry_forward(&statement).is_err());
    statement.output_tokens[1].token = 1;
    verify_transaction_sat(&trace, 2, &statement, &roots).unwrap();

    statement.outputs.clear();
    assert!(check_token_carry_forward(&statement).is_err());
}
