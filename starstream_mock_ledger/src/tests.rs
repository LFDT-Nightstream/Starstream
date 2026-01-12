use super::*;
use crate::builder::{RefGenerator, TransactionBuilder, h, v};
use crate::{mocked_verifier::InterleavingError, transaction_effects::witness::WitLedgerEffect};

fn mock_genesis() -> (Ledger, UtxoId, UtxoId, CoroutineId, CoroutineId) {
    let input_hash_1 = h(10);
    let input_hash_2 = h(11);

    // Create input UTXO IDs
    let input_utxo_1 = UtxoId {
        contract_hash: input_hash_1.clone(),
        nonce: 0,
    };
    let input_utxo_2 = UtxoId {
        contract_hash: input_hash_2.clone(),
        nonce: 0,
    };

    let mut ledger = Ledger {
        utxos: HashMap::new(),
        contract_counters: HashMap::new(),
        utxo_to_coroutine: HashMap::new(),
        ownership_registry: HashMap::new(),
    };

    let input_1_coroutine = CoroutineId {
        creation_tx_hash: Hash([1u8; 32], PhantomData),
        creation_output_index: 0,
    };

    let input_2_coroutine = CoroutineId {
        creation_tx_hash: Hash([1u8; 32], PhantomData),
        creation_output_index: 1,
    };

    ledger.utxos.insert(
        input_utxo_1.clone(),
        UtxoEntry {
            state: CoroutineState {
                pc: 0,
                last_yield: v(b"yield_1"),
            },
            contract_hash: input_hash_1.clone(),
        },
    );
    ledger.utxos.insert(
        input_utxo_2.clone(),
        UtxoEntry {
            state: CoroutineState {
                pc: 0,
                last_yield: v(b"yield_2"),
            },
            contract_hash: input_hash_2.clone(),
        },
    );

    ledger
        .utxo_to_coroutine
        .insert(input_utxo_1.clone(), input_1_coroutine.clone());
    ledger
        .utxo_to_coroutine
        .insert(input_utxo_2.clone(), input_2_coroutine.clone());

    // Set up contract counters
    ledger.contract_counters.insert(input_hash_1.clone(), 1);
    ledger.contract_counters.insert(input_hash_2.clone(), 1);
    ledger.contract_counters.insert(h(1), 0); // coord_hash
    ledger.contract_counters.insert(h(2), 0); // utxo_hash_a
    ledger.contract_counters.insert(h(3), 0); // utxo_hash_b

    (
        ledger,
        input_utxo_1,
        input_utxo_2,
        input_1_coroutine,
        input_2_coroutine,
    )
}

fn mock_genesis_and_apply_tx(proven_tx: ProvenTransaction) -> Result<Ledger, VerificationError> {
    let (ledger, _, _, _, _) = mock_genesis();
    ledger.apply_transaction(&proven_tx)
}

#[test]
fn test_transaction_with_coord_and_utxos() {
    // This test simulates a "complex" transaction involving 2 input UTXOs, 2 new UTXOs,
    // and 1 coordination script that orchestrates the control flow.
    // The diagram below shows the lifecycle of each process: `(input)` marks a UTXO
    // that is consumed by the transaction, and `(output)` marks one that is
    // created by the transaction. P1 is burned, so it is an input but not an output.
    //
    //  P4 (Coord)           P0 (Input 1)         P1 (Input 2)         P2 (New UTXO A)      P3 (New UTXO B)
    //      |                     |                     |                     |                     |
    //  NewRef("init_a")          |                     |                     |                     |
    //  NewUtxo(A) ------------> Init                   |                     |                     |
    //  NewRef("init_b")          |                     |                     |                     |
    //  NewUtxo(B) ----------------------------------> Init                   |                     |
    //  NewRef("spend_input_1")   |                     |                     |                     |
    //  NewRef("continued_1")     |                     |                     |                     |
    //  Resume ----------------> Activation             |                     |                     |
    //      |<--- -------------- Yield                  |                     |                     |
    //  NewRef("spend_input_2")   |                     |                     |                     |
    //  NewRef("burned_2")        |                     |                     |                     |
    //  Resume ------------------------------------> Activation               |                     |
    //      |<--------------------------------------- Burn                    |                     |
    //  NewRef("done_a")          |                     |                     |                     |
    //  Resume -----------------------------------------------------------> Activation              |
    //      |<------------------------------------------------------------- Bind                    |
    //      |<------------------------------------------------------------- Yield                   |
    //  NewRef("done_b")          |                     |                     |                     |
    //  Resume ---------------------------------------------------------------------------------> Activation
    //      |<----------------------------------------------------------------------------------- Yield
    //      |                     |                     |                     |                     |
    //    (end)              (continued)            (burned)             (new_output)          (new_output)

    let (ledger, input_utxo_1, input_utxo_2, _, _) = mock_genesis();

    let coord_hash = h(1);
    let utxo_hash_a = h(2);
    let utxo_hash_b = h(3);

    let mut refs = RefGenerator::new();

    // Pre-allocate all refs in the order they appear in coord_trace to ensure consistent numbering
    let init_a_ref = refs.get("init_a");
    let init_b_ref = refs.get("init_b");
    let spend_input_1_ref = refs.get("spend_input_1");
    let continued_1_ref = refs.get("continued_1");
    let spend_input_2_ref = refs.get("spend_input_2");
    let burned_2_ref = refs.get("burned_2");
    let done_a_ref = refs.get("done_a");
    let done_b_ref = refs.get("done_b");

    // Host refs.get("init_b")r each process in canonical order: inputs ++ new_outputs ++ coord_scripts
    // Process 0: Input 1, Process 1: Input 2, Process 2: UTXO A (spawn), Process 3: UTXO B (spawn), Process 4: Coordination script
    let input_1_trace = vec![
        WitLedgerEffect::Activation {
            val: spend_input_1_ref,
            caller: ProcessId(4),
        },
        WitLedgerEffect::Yield {
            val: continued_1_ref,
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let input_2_trace = vec![
        WitLedgerEffect::Activation {
            val: spend_input_2_ref,
            caller: ProcessId(4),
        },
        WitLedgerEffect::Burn { ret: burned_2_ref },
    ];

    let utxo_a_trace = vec![
        WitLedgerEffect::Init {
            val: init_a_ref,
            caller: ProcessId(4),
        },
        WitLedgerEffect::Activation {
            val: init_a_ref,
            caller: ProcessId(4),
        },
        WitLedgerEffect::Bind {
            owner_id: ProcessId(3),
        },
        WitLedgerEffect::Yield {
            val: done_a_ref,
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let utxo_b_trace = vec![
        WitLedgerEffect::Init {
            val: init_b_ref,
            caller: ProcessId(4),
        },
        WitLedgerEffect::Activation {
            val: init_b_ref,
            caller: ProcessId(4),
        },
        WitLedgerEffect::Yield {
            val: done_b_ref,
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let coord_trace = vec![
        WitLedgerEffect::NewRef {
            size: 1,
            ret: init_a_ref,
        },
        WitLedgerEffect::RefPush { val: v(b"init_a") },
        WitLedgerEffect::NewUtxo {
            program_hash: utxo_hash_a.clone(),
            val: init_a_ref,
            id: ProcessId(2),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: init_b_ref,
        },
        WitLedgerEffect::RefPush { val: v(b"init_b") },
        WitLedgerEffect::NewUtxo {
            program_hash: utxo_hash_b.clone(),
            val: init_b_ref,
            id: ProcessId(3),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: spend_input_1_ref,
        },
        WitLedgerEffect::RefPush {
            val: v(b"spend_input_1"),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: continued_1_ref,
        },
        WitLedgerEffect::RefPush {
            val: v(b"continued_1"),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: spend_input_1_ref,
            ret: continued_1_ref,
            id_prev: None,
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: spend_input_2_ref,
        },
        WitLedgerEffect::RefPush {
            val: v(b"spend_input_2"),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: burned_2_ref,
        },
        WitLedgerEffect::RefPush {
            val: v(b"burned_2"),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(1),
            val: spend_input_2_ref,
            ret: burned_2_ref,
            id_prev: Some(ProcessId(0)),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: done_a_ref,
        },
        WitLedgerEffect::RefPush { val: v(b"done_a") },
        WitLedgerEffect::Resume {
            target: ProcessId(2),
            val: init_a_ref,
            ret: done_a_ref,
            id_prev: Some(ProcessId(1)),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: done_b_ref,
        },
        WitLedgerEffect::RefPush { val: v(b"done_b") },
        WitLedgerEffect::Resume {
            target: ProcessId(3),
            val: init_b_ref,
            ret: done_b_ref,
            id_prev: Some(ProcessId(2)),
        },
    ];

    let proven_tx = TransactionBuilder::new()
        .with_input(
            input_utxo_1,
            Some(CoroutineState {
                pc: 1,
                last_yield: v(b"continued_1"),
            }),
            input_1_trace,
        )
        .with_input(input_utxo_2, None, input_2_trace)
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: v(b"done_a"),
                },
                contract_hash: utxo_hash_a.clone(),
            },
            utxo_a_trace,
        )
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: v(b"done_b"),
                },
                contract_hash: utxo_hash_b.clone(),
            },
            utxo_b_trace,
        )
        .with_coord_script(coord_hash, coord_trace)
        .with_ownership(OutputRef(2), OutputRef(3))
        .with_entrypoint(4)
        .build(ZkTransactionProof::Dummy);

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    assert_eq!(ledger.utxos.len(), 3);
    assert_eq!(ledger.ownership_registry.len(), 1);
}

#[test]
fn test_effect_handlers() {
    let coord_hash = h(1);
    let utxo_hash = h(2);
    let interface_id = h(42);

    let mut ref_gen = RefGenerator::new();

    let utxo_trace = vec![
        WitLedgerEffect::Activation {
            val: ref_gen.get("init_utxo"),
            caller: ProcessId(1),
        },
        WitLedgerEffect::ProgramHash {
            target: ProcessId(1),
            program_hash: coord_hash.clone(),
        },
        WitLedgerEffect::GetHandlerFor {
            interface_id: interface_id.clone(),
            handler_id: ProcessId(1),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_gen.get("effect_request"),
        },
        WitLedgerEffect::RefPush {
            val: v(b"Interface::Effect(42)"),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(1),
            val: ref_gen.get("effect_request"),
            ret: ref_gen.get("effect_request_response"),
            id_prev: Some(ProcessId(1)),
        },
        WitLedgerEffect::Yield {
            val: ref_gen.get("utxo_final"),
            ret: None,
            id_prev: Some(ProcessId(1)),
        },
    ];

    let coord_trace = vec![
        WitLedgerEffect::InstallHandler {
            interface_id: interface_id.clone(),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_gen.get("init_utxo"),
        },
        WitLedgerEffect::RefPush {
            val: v(b"init_utxo"),
        },
        WitLedgerEffect::NewUtxo {
            program_hash: h(2),
            val: ref_gen.get("init_utxo"),
            id: ProcessId(0),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: ref_gen.get("init_utxo"),
            ret: ref_gen.get("effect_request"),
            id_prev: None,
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_gen.get("effect_request_response"),
        },
        WitLedgerEffect::RefPush {
            val: v(b"Interface::EffectResponse(43)"),
        },
        WitLedgerEffect::NewRef {
            size: 1,
            ret: ref_gen.get("utxo_final"),
        },
        WitLedgerEffect::RefPush {
            val: v(b"utxo_final"),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: ref_gen.get("effect_request_response"),
            ret: ref_gen.get("utxo_final"),
            id_prev: Some(ProcessId(0)),
        },
        WitLedgerEffect::UninstallHandler { interface_id },
    ];

    let proven_tx = TransactionBuilder::new()
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: v(b"utxo_final"),
                },
                contract_hash: utxo_hash.clone(),
            },
            utxo_trace,
        )
        .with_coord_script(coord_hash, coord_trace)
        .with_entrypoint(1)
        .build(ZkTransactionProof::Dummy);

    let ledger = Ledger {
        utxos: HashMap::new(),
        contract_counters: HashMap::new(),
        utxo_to_coroutine: HashMap::new(),
        ownership_registry: HashMap::new(),
    };

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    assert_eq!(ledger.utxos.len(), 1);
    assert_eq!(ledger.ownership_registry.len(), 0);
}

#[test]
fn test_burn_with_continuation_fails() {
    let (_, input_utxo_1, _, _, _) = mock_genesis();
    let tx = TransactionBuilder::new()
        .with_input(
            input_utxo_1,
            Some(CoroutineState {
                pc: 1,
                last_yield: v(b"burned"),
            }),
            vec![
                WitLedgerEffect::NewRef {
                    size: 1,
                    ret: Ref(0),
                },
                WitLedgerEffect::RefPush { val: v(b"burned") },
                WitLedgerEffect::Burn { ret: Ref(0) },
            ],
        )
        .with_entrypoint(0)
        .build(ZkTransactionProof::Dummy);
    let result = mock_genesis_and_apply_tx(tx);
    assert!(matches!(
        result,
        Err(VerificationError::InterleavingProofError(
            InterleavingError::UtxoShouldNotBurn(_)
        ))
    ));
}

#[test]
fn test_utxo_resumes_utxo_fails() {
    let (_, input_utxo_1, input_utxo_2, _, _) = mock_genesis();
    let tx = TransactionBuilder::new()
        .with_input(
            input_utxo_1,
            None,
            vec![
                WitLedgerEffect::NewRef {
                    size: 1,
                    ret: Ref(0),
                },
                WitLedgerEffect::RefPush { val: v(b"") },
                WitLedgerEffect::Resume {
                    target: ProcessId(1),
                    val: Ref(0),
                    ret: Ref(0),
                    id_prev: None,
                },
            ],
        )
        .with_input(input_utxo_2, None, vec![])
        .with_entrypoint(0)
        .build(ZkTransactionProof::Dummy);
    let result = mock_genesis_and_apply_tx(tx);
    assert!(matches!(
        result,
        Err(VerificationError::InterleavingProofError(
            InterleavingError::UtxoResumesUtxo { .. }
        ))
    ));
}

#[test]
fn test_continuation_without_yield_fails() {
    let (_, input_utxo_1, _, _, _) = mock_genesis();
    let tx = TransactionBuilder::new()
        .with_input(
            input_utxo_1,
            Some(CoroutineState {
                pc: 1,
                last_yield: v(b""),
            }),
            vec![],
        )
        .with_entrypoint(0)
        .build(ZkTransactionProof::Dummy);
    let result = mock_genesis_and_apply_tx(tx);
    assert!(matches!(
        result,
        Err(VerificationError::InterleavingProofError(
            InterleavingError::UtxoNotFinalized(_)
        ))
    ));
}

#[test]
fn test_unbind_not_owner_fails() {
    let (_, input_utxo_1, input_utxo_2, _, _) = mock_genesis();
    let tx = TransactionBuilder::new()
        .with_input(input_utxo_1, None, vec![])
        .with_input(
            input_utxo_2,
            None,
            vec![WitLedgerEffect::Unbind {
                token_id: ProcessId(0),
            }],
        )
        .with_entrypoint(1)
        .build(ZkTransactionProof::Dummy);
    let result = mock_genesis_and_apply_tx(tx);
    assert!(matches!(
        result,
        Err(VerificationError::InterleavingProofError(
            InterleavingError::UnbindNotOwner { .. }
        ))
    ));
}

#[test]
fn test_duplicate_input_utxo_fails() {
    let input_id = UtxoId {
        contract_hash: h(1),
        nonce: 0,
    };

    let mut utxos = HashMap::new();

    utxos.insert(
        input_id.clone(),
        UtxoEntry {
            state: CoroutineState {
                pc: 0,
                last_yield: Value::nil(),
            },
            contract_hash: h(1),
        },
    );

    let mut contract_counters = HashMap::new();

    contract_counters.insert(h(1), 0);

    let mut utxo_to_coroutine = HashMap::new();

    utxo_to_coroutine.insert(
        input_id.clone(),
        CoroutineId {
            creation_tx_hash: Hash([1u8; 32], PhantomData),
            creation_output_index: 0,
        },
    );

    let ledger = Ledger {
        utxos,
        contract_counters,
        utxo_to_coroutine,
        ownership_registry: HashMap::new(),
    };

    let coord_hash = h(42);
    let tx = TransactionBuilder::new()
        .with_input(
            input_id.clone(),
            None,
            vec![
                WitLedgerEffect::NewRef {
                    size: 1,
                    ret: Ref(1),
                },
                WitLedgerEffect::RefPush { val: Value::nil() },
                WitLedgerEffect::Burn { ret: Ref(0) },
            ],
        )
        .with_coord_script(
            coord_hash,
            vec![
                WitLedgerEffect::NewRef {
                    size: 1,
                    ret: Ref(0),
                },
                WitLedgerEffect::RefPush { val: Value::nil() },
                WitLedgerEffect::Resume {
                    target: 0.into(),
                    val: Ref(0),
                    ret: Ref(0),
                    id_prev: None,
                },
            ],
        )
        .with_entrypoint(1)
        .build(ZkTransactionProof::Dummy);

    let _ledger = ledger.apply_transaction(&tx).unwrap();
}
