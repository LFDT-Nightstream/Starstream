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
                last_yield: v(b"spend_input_1"),
            },
            contract_hash: input_hash_1.clone(),
        },
    );
    ledger.utxos.insert(
        input_utxo_2.clone(),
        UtxoEntry {
            state: CoroutineState {
                pc: 0,
                last_yield: v(b"spend_input_2"),
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
    // This test simulates a complex transaction involving 2 input UTXOs, 2 new UTXOs,
    // and 1 coordination script that orchestrates the control flow.
    // The diagram below shows the lifecycle of each process: `(input)` marks a UTXO
    // that is consumed by the transaction, and `(output)` marks one that is
    // created by the transaction. P1 is burned, so it is an input but not an output.
    //
    //  P4 (Coord)      P0 (In 1)         P1 (In 2)       P2 (New)        P3 (New)
    //      |           (input)           (input)             |               |
    // (entrypoint)       |                 |                 |               |
    //      |---NewUtxo---|-----------------|----------------->(created)      |
    //      |---NewUtxo---|-----------------|-----------------|-------------->|(created)
    //      |             |                 |                 |               |
    //      |---Resume--->| (spend)         |                 |               |
    //      |<--Yield-----| (continue)      |                 |               |
    //      |             |                 |                 |               |
    //      |---Resume--------------------->| (spend)         |               |
    //      |<--Burn------------------------|                 |               |
    //      |             |                 |                 |               |
    //      |---Resume--------------------------------------->|               |
    //      |<--Yield-----------------------------------------|               |
    //      |             |                 |                 |               |
    //      |---Resume------------------------------------------------------->|
    //      |<--Yield---------------------------------------------------------|
    //      |             |                 |                 |               |
    //    (end)        (output)             X (burned)       (output)        (output)

    let (ledger, input_utxo_1, input_utxo_2, _, _) = mock_genesis();

    let coord_hash = h(1);
    let utxo_hash_a = h(2);
    let utxo_hash_b = h(3);

    // Host call traces for each process in canonical order: inputs ++ new_outputs ++ coord_scripts
    // Process 0: Input 1, Process 1: Input 2, Process 2: UTXO A (spawn), Process 3: UTXO B (spawn), Process 4: Coordination script
    let input_1_trace = vec![WitLedgerEffect::Yield {
        val: v(b"continued_1"),
        ret: None,
        id_prev: Some(ProcessId(4)),
    }];

    let input_2_trace = vec![WitLedgerEffect::Burn {
        ret: v(b"burned_2"),
    }];

    let utxo_a_trace = vec![
        WitLedgerEffect::Bind {
            owner_id: ProcessId(3),
        },
        WitLedgerEffect::Yield {
            val: v(b"done_a"),
            ret: None,
            id_prev: Some(ProcessId(4)),
        },
    ];

    let utxo_b_trace = vec![WitLedgerEffect::Yield {
        val: v(b"done_b"),
        ret: None,
        id_prev: Some(ProcessId(4)),
    }];

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
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: v(b"spend_input_1"),
            ret: v(b"continued_1"),
            id_prev: None,
        },
        WitLedgerEffect::Resume {
            target: ProcessId(1),
            val: v(b"spend_input_2"),
            ret: v(b"burned_2"),
            id_prev: Some(ProcessId(0)),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(2),
            val: v(b"init_a"),
            ret: v(b"done_a"),
            id_prev: Some(ProcessId(1)),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(3),
            val: v(b"init_b"),
            ret: v(b"done_b"),
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
                    last_yield: v(b"utxo_a_state"),
                },
                contract_hash: utxo_hash_a.clone(),
            },
            utxo_a_trace,
        )
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: v(b"utxo_b_state"),
                },
                contract_hash: utxo_hash_b.clone(),
            },
            utxo_b_trace,
        )
        .with_coord_script(coord_hash, coord_trace)
        .with_ownership(OutputRef(2), OutputRef(3)) // utxo_a owned by utxo_b
        .with_entrypoint(4)
        .build();

    let ledger = ledger.apply_transaction(&proven_tx).unwrap();

    // Verify final ledger state
    assert_eq!(ledger.utxos.len(), 3); // 1 continuation + 2 new outputs
    assert_eq!(ledger.ownership_registry.len(), 1); // UTXO A is owned by UTXO B
}

#[test]
fn test_effect_handlers() {
    // Create a transaction with:
    // - 1 coordination script (process 1) that acts as an effect handler
    // - 1 new UTXO (process 0) that calls the effect handler
    //
    // Roughly models this:
    //
    // interface Interface {
    //   Effect(int): int
    // }
    //
    // utxo Utxo {
    //   main {
    //     raise Interface::Effect(42);
    //   }
    // }
    //
    // script {
    //   fn main() {
    //     let utxo = Utxo::new();
    //
    //     try {
    //       utxo.resume(utxo);
    //     }
    //     with Interface {
    //       do Effect(x) = {
    //         resume(43)
    //       }
    //     }
    //   }
    // }
    //
    // This test simulates a coordination script acting as an algebraic effect handler
    // for a UTXO. The UTXO "raises" an effect by calling the handler, and the
    // handler resumes the UTXO with the result.
    //
    //  P1 (Coord/Handler)     P0 (UTXO)
    //        |                    |
    //   (entrypoint)              |
    //        |                    |
    // InstallHandler (self)       |
    //        |                    |
    //    NewUtxo ---------------->(P0 created)
    //   (val="init_utxo")         |
    //        |                    |
    //    Resume ----------------->|
    //   (val="init_utxo")         |
    //        |               Input (val="init_utxo", caller=P1)
    //        |               ProgramHash(P1) -> (attest caller)
    //        |               GetHandlerFor -> P1
    //        |<----------------- Resume (Effect call)
    //        |        (val="Interface::Effect(42)")
    //(handles effect)             |
    //        |                    |
    //    Resume ----------------->| (Resume with result)
    //(val="Interface::EffectResponse(43)")
    //        |                    |
    //        |<----------------- Yield
    //        |            (val="utxo_final")
    //UninstallHandler (self)      |
    //        |                    |
    //      (end)                  |

    let coord_hash = h(1);
    let utxo_hash = h(2);
    let interface_id = h(42);

    // Host call traces for each process in canonical order: (no inputs) ++ new_outputs ++ coord_scripts
    // Process 0: UTXO, Process 1: Coordination script
    let utxo_trace = vec![
        WitLedgerEffect::Input {
            val: v(b"init_utxo"),
            caller: ProcessId(1),
        },
        WitLedgerEffect::ProgramHash {
            target: ProcessId(1),
            program_hash: coord_hash.clone(), // assert coord_script hash == h(1)
        },
        WitLedgerEffect::GetHandlerFor {
            interface_id: interface_id.clone(),
            handler_id: ProcessId(1),
        },
        WitLedgerEffect::Resume {
            target: ProcessId(1),
            val: v(b"Interface::Effect(42)"),         // request
            ret: v(b"Interface::EffectResponse(43)"), // expected response
            id_prev: Some(ProcessId(1)),
        },
        WitLedgerEffect::Yield {
            val: v(b"utxo_final"),
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
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: v(b"init_utxo"),
            ret: v(b"Interface::Effect(42)"), // expected request
            id_prev: None,
        },
        WitLedgerEffect::Resume {
            target: ProcessId(0),
            val: v(b"Interface::EffectResponse(43)"), // response sent
            ret: v(b"utxo_final"),
            id_prev: Some(ProcessId(0)),
        },
        WitLedgerEffect::UninstallHandler { interface_id },
    ];

    let proven_tx = TransactionBuilder::new()
        .with_fresh_output(
            NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: v(b"utxo_state"),
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

    assert_eq!(ledger.utxos.len(), 1); // 1 new UTXO
    assert_eq!(ledger.ownership_registry.len(), 0); // No ownership relationships
}

#[test]
fn test_burn_with_continuation_fails() {
    let (_, input_utxo_1, _, _, _) = mock_genesis();
    let tx = TransactionBuilder::new()
        .with_input(
            input_utxo_1,
            Some(CoroutineState {
                pc: 1,
                last_yield: v(b"continued"),
            }),
            vec![WitLedgerEffect::Burn { ret: v(b"burned") }],
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
            vec![WitLedgerEffect::Resume {
                target: ProcessId(1),
                val: v(b""),
                ret: v(b""),
                id_prev: None,
            }],
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
                last_yield: v(b"continued"),
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
            vec![WitLedgerEffect::Burn { ret: Value::nil() }],
        )
        .with_input(
            input_id.clone(),
            None,
            vec![WitLedgerEffect::Burn { ret: Value::nil() }],
        )
        .with_coord_script(
            coord_hash,
            vec![
                WitLedgerEffect::Resume {
                    target: 0.into(),
                    val: Value::nil(),
                    ret: Value::nil(),
                    id_prev: None,
                },
                WitLedgerEffect::Resume {
                    target: 1.into(),
                    val: Value::nil(),
                    ret: Value::nil(),
                    id_prev: Some(0.into()),
                },
            ],
        )
        .with_entrypoint(2)
        .build();

    let result = ledger.apply_transaction(&tx);

    assert!(matches!(result, Err(VerificationError::InputNotFound)));

    let tx = TransactionBuilder::new()
        .with_input(
            input_id.clone(),
            None,
            vec![WitLedgerEffect::Burn { ret: Value::nil() }],
        )
        .with_coord_script(
            coord_hash,
            vec![WitLedgerEffect::Resume {
                target: 0.into(),
                val: Value::nil(),
                ret: Value::nil(),
                id_prev: None,
            }],
        )
        .with_entrypoint(1)
        .build();

    let _ledger = ledger.apply_transaction(&tx).unwrap();
}
