use super::*;
use crate::{mocked_verifier::InterleavingError, transaction_effects::witness::WitLedgerEffect};

pub fn h<T>(n: u8) -> Hash<T> {
    // TODO: actual hashing
    let mut bytes = [0u8; 32];
    bytes[0] = n;
    Hash(bytes, std::marker::PhantomData)
}

pub fn v(data: &[u8]) -> Value {
    Value(data.to_vec())
}

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
                last_yield: Ref(1),
            },
            contract_hash: input_hash_1.clone(),
        },
    );
    ledger.utxos.insert(
        input_utxo_2.clone(),
        UtxoEntry {
            state: CoroutineState {
                pc: 0,
                last_yield: Ref(2),
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

struct TransactionBuilder {
    body: TransactionBody,
    spending_proofs: Vec<ZkWasmProof>,
    new_output_proofs: Vec<ZkWasmProof>,
    coordination_scripts: Vec<ZkWasmProof>,
}

impl TransactionBuilder {
    fn new() -> Self {
        Self {
            body: TransactionBody {
                inputs: vec![],
                continuations: vec![],
                new_outputs: vec![],
                ownership_out: HashMap::new(),
                coordination_scripts_keys: vec![],
                entrypoint: 0,
            },
            spending_proofs: vec![],
            new_output_proofs: vec![],
            coordination_scripts: vec![],
        }
    }

    fn with_input(
        mut self,
        utxo: UtxoId,
        continuation: Option<CoroutineState>,
        trace: Vec<WitLedgerEffect>,
    ) -> Self {
        self.body.inputs.push(utxo);
        self.body.continuations.push(continuation);
        self.spending_proofs.push(ZkWasmProof {
            host_calls_root: MockedLookupTableCommitment { trace },
        });
        self
    }

    fn with_fresh_output(mut self, output: NewOutput, trace: Vec<WitLedgerEffect>) -> Self {
        self.body.new_outputs.push(output);
        self.new_output_proofs.push(ZkWasmProof {
            host_calls_root: MockedLookupTableCommitment { trace },
        });
        self
    }

    fn with_coord_script(mut self, key: Hash<WasmModule>, trace: Vec<WitLedgerEffect>) -> Self {
        self.body.coordination_scripts_keys.push(key);
        self.coordination_scripts.push(ZkWasmProof {
            host_calls_root: MockedLookupTableCommitment { trace },
        });
        self
    }

    fn with_ownership(mut self, token: OutputRef, owner: OutputRef) -> Self {
        self.body.ownership_out.insert(token, owner);
        self
    }

    fn with_entrypoint(mut self, entrypoint: usize) -> Self {
        self.body.entrypoint = entrypoint;
        self
    }

    fn build(self) -> ProvenTransaction {
        let witness = TransactionWitness {
            spending_proofs: self.spending_proofs,
            new_output_proofs: self.new_output_proofs,
            interleaving_proof: ZkTransactionProof {},
            coordination_scripts: self.coordination_scripts,
        };

        ProvenTransaction {
            body: self.body,
            witness,
        }
    }
}

fn mock_genesis_and_apply_tx(proven_tx: ProvenTransaction) -> Result<Ledger, VerificationError> {
    let (ledger, _, _, _, _) = mock_genesis();
    ledger.apply_transaction(&proven_tx)
}

#[test]
fn test_transaction_with_coord_and_utxos() {
    let (ledger, input_utxo_1, input_utxo_2, _, _) = mock_genesis();

    let coord_hash = h(1);
    let utxo_hash_a = h(2);
    let utxo_hash_b = h(3);

    let input_1_trace = vec![
        WitLedgerEffect::Activation {
            val: v(b"spend_input_1"),
            caller: ProcessId(4),
        },
        WitLedgerEffect::Yield {
            val: Ref(2),
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let input_2_trace = vec![
        WitLedgerEffect::Activation {
            val: v(b"spend_input_2"),
            caller: ProcessId(4),
        },
        WitLedgerEffect::Burn {
            ret: Ref(4),
        },
    ];

    let utxo_a_trace = vec![
        WitLedgerEffect::Init {
            val: v(b"init_a"),
            caller: ProcessId(4),
        },
        WitLedgerEffect::Activation {
            val: v(b"init_a"),
            caller: ProcessId(4),
        },
        WitLedgerEffect::Bind {
            owner_id: ProcessId(3),
        },
        WitLedgerEffect::Yield {
            val: Ref(6),
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let utxo_b_trace = vec![
        WitLedgerEffect::Init {
            val: v(b"init_b"),
            caller: ProcessId(4),
        },
        WitLedgerEffect::Activation {
            val: v(b"init_b"),
            caller: ProcessId(4),
        },
        WitLedgerEffect::Yield {
            val: Ref(8),
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let coord_trace = vec![
        WitLedgerEffect::NewUtxo {
            program_hash: utxo_hash_a.clone(),
            val: v(b"init_a"),
            id: ProcessId(2),
        },
        WitLedgerEffect::NewUtxo {
            program_hash: utxo_hash_b.clone(),
            val: v(b"init_b"),
            id: ProcessId(3),
        },
        WitLedgerEffect::NewRef {
            val: v(b"spend_input_1"),
            ret: Ref(1),
        },
        WitLedgerEffect::NewRef {
            val: v(b"continued_1"),
            ret: Ref(2),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: Ref(1),
            ret: Ref(2),
            id_prev: None,
        },
        WitLedgerEffect::NewRef {
            val: v(b"spend_input_2"),
            ret: Ref(3),
        },
        WitLedgerEffect::NewRef {
            val: v(b"burned_2"),
            ret: Ref(4),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(1),
            val: Ref(3),
            ret: Ref(4),
            id_prev: Some(ProcessId(0)),
        },
        WitLedgerEffect::NewRef {
            val: v(b"init_a"),
            ret: Ref(5),
        },
        WitLedgerEffect::NewRef {
            val: v(b"done_a"),
            ret: Ref(6),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(2),
            val: Ref(5),
            ret: Ref(6),
            id_prev: Some(ProcessId(1)),
        },
        WitLedgerEffect::NewRef {
            val: v(b"init_b"),
            ret: Ref(7),
        },
        WitLedgerEffect::NewRef {
            val: v(b"done_b"),
            ret: Ref(8),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(3),
            val: Ref(7),
            ret: Ref(8),
            id_prev: Some(ProcessId(2)),
        },
    ];

    let proven_tx = TransactionBuilder::new()
        .with_input(
            input_utxo_1,
            Some(CoroutineState {
                pc: 1,
                last_yield: Ref(2),
            }),
            input_1_trace,
        )
        .with_input(input_utxo_2, None, input_2_trace)
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: Ref(6),
                },
                contract_hash: utxo_hash_a.clone(),
            },
            utxo_a_trace,
        )
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: Ref(8),
                },
                contract_hash: utxo_hash_b.clone(),
            },
            utxo_b_trace,
        )
        .with_coord_script(coord_hash, coord_trace)
        .with_ownership(OutputRef(2), OutputRef(3))
        .with_entrypoint(4)
        .build();

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    assert_eq!(ledger.utxos.len(), 3); 
    assert_eq!(ledger.ownership_registry.len(), 1);
}

#[test]
fn test_effect_handlers() {
    let coord_hash = h(1);
    let utxo_hash = h(2);
    let interface_id = h(42);

    let utxo_trace = vec![
        WitLedgerEffect::Init {
            val: v(b"init_utxo"),
            caller: ProcessId(1),
        },
        WitLedgerEffect::Activation {
            val: v(b"init_utxo"),
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
        WitLedgerEffect::Resume {
            target: ProcessId(1),
            val: Ref(2),
            ret: Ref(3),
            id_prev: Some(ProcessId(1)),
        },
        WitLedgerEffect::Yield {
            val: Ref(4),
            ret: None,
            id_prev: Some(ProcessId(1)),
        },
    ];

    let coord_trace = vec![
        WitLedgerEffect::InstallHandler {
            interface_id: interface_id.clone(),
        },
        WitLedgerEffect::NewUtxo {
            program_hash: h(2),
            val: v(b"init_utxo"),
            id: ProcessId(0),
        },
        WitLedgerEffect::NewRef {
            val: v(b"init_utxo"),
            ret: Ref(1),
        },
        WitLedgerEffect::NewRef {
            val: v(b"Interface::Effect(42)"),
            ret: Ref(2),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: Ref(1),
            ret: Ref(2),
            id_prev: None,
        },
        WitLedgerEffect::NewRef {
            val: v(b"Interface::EffectResponse(43)"),
            ret: Ref(3),
        },
        WitLedgerEffect::NewRef {
            val: v(b"utxo_final"),
            ret: Ref(4),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: Ref(3),
            ret: Ref(4),
            id_prev: Some(ProcessId(0)),
        },
        WitLedgerEffect::UninstallHandler { interface_id },
    ];

    let proven_tx = TransactionBuilder::new()
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: Ref(4),
                },
                contract_hash: utxo_hash.clone(),
            },
            utxo_trace,
        )
        .with_coord_script(coord_hash, coord_trace)
        .with_entrypoint(1)
        .build();

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
                last_yield: Ref(1),
            }),
            vec![
                WitLedgerEffect::NewRef { val: v(b"burned"), ret: Ref(1) },
                WitLedgerEffect::Burn { ret: Ref(1) },
            ],
        )
        .with_entrypoint(0)
        .build();
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
                WitLedgerEffect::NewRef { val: v(b""), ret: Ref(1) },
                WitLedgerEffect::Resume {
                    target: ProcessId(1),
                    val: Ref(1),
                    ret: Ref(1),
                    id_prev: None,
                },
            ],
        )
        .with_input(input_utxo_2, None, vec![])
        .with_entrypoint(0)
        .build();
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
                last_yield: Ref(1),
            }),
            vec![],
        )
        .with_entrypoint(0)
        .build();
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
        .build();
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
                last_yield: Ref(0),
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
                WitLedgerEffect::NewRef { val: Value::nil(), ret: Ref(1) },
                WitLedgerEffect::Burn { ret: Ref(1) }
            ],
        )
        .with_coord_script(
            coord_hash,
            vec![
                WitLedgerEffect::NewRef { val: Value::nil(), ret: Ref(1) },
                WitLedgerEffect::Resume {
                    target: 0.into(),
                    val: Ref(1),
                    ret: Ref(1),
                    id_prev: None,
                },
            ],
        )
        .with_entrypoint(1)
        .build();

    let _ledger = ledger.apply_transaction(&tx).unwrap();
}
