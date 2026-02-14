use super::*;
use crate::transaction_effects::witness::WitLedgerEffect;

pub struct RefGenerator {
    counter: u64,
    map: HashMap<&'static str, Ref>,
}

impl RefGenerator {
    pub fn new() -> Self {
        Self {
            counter: 0,
            map: HashMap::new(),
        }
    }

    pub fn get(&mut self, name: &'static str) -> Ref {
        let entry = self.map.entry(name).or_insert_with(|| {
            let r = Ref(self.counter);
            self.counter += REF_PUSH_WIDTH as u64;
            r
        });
        *entry
    }
}

impl Default for RefGenerator {
    fn default() -> Self {
        Self::new()
    }
}

pub fn h<T>(n: u8) -> Hash<T> {
    // TODO: actual hashing
    let mut bytes = [0u8; 32];
    bytes[0] = n;
    Hash(bytes, std::marker::PhantomData)
}

pub fn v(data: &[u8]) -> Value {
    let mut bytes = [0u8; 8];
    let len = data.len().min(8);
    bytes[..len].copy_from_slice(&data[..len]);
    Value(u64::from_le_bytes(bytes))
}

pub struct TransactionBuilder {
    body: TransactionBody,
    spending_proofs: Vec<ZkWasmProof>,
    new_output_proofs: Vec<ZkWasmProof>,
    coordination_scripts: Vec<ZkWasmProof>,
}

impl TransactionBuilder {
    pub fn new() -> Self {
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

    pub fn with_input(
        self,
        utxo: UtxoId,
        continuation: Option<CoroutineState>,
        trace: Vec<WitLedgerEffect>,
    ) -> Self {
        self.with_input_and_trace_commitment(
            utxo,
            continuation,
            trace,
            LedgerEffectsCommitment::zero(),
        )
    }

    pub fn with_input_and_trace_commitment(
        mut self,
        utxo: UtxoId,
        continuation: Option<CoroutineState>,
        trace: Vec<WitLedgerEffect>,
        host_calls_root: LedgerEffectsCommitment,
    ) -> Self {
        self.body.inputs.push(utxo);
        self.body.continuations.push(continuation);
        self.spending_proofs.push(ZkWasmProof {
            host_calls_root,
            trace,
        });
        self
    }

    pub fn with_fresh_output(self, output: NewOutput, trace: Vec<WitLedgerEffect>) -> Self {
        self.with_fresh_output_and_trace_commitment(output, trace, LedgerEffectsCommitment::zero())
    }

    pub fn with_fresh_output_and_trace_commitment(
        mut self,
        output: NewOutput,
        trace: Vec<WitLedgerEffect>,
        host_calls_root: LedgerEffectsCommitment,
    ) -> Self {
        self.body.new_outputs.push(output);
        self.new_output_proofs.push(ZkWasmProof {
            host_calls_root,
            trace,
        });
        self
    }

    pub fn with_coord_script(self, key: Hash<WasmModule>, trace: Vec<WitLedgerEffect>) -> Self {
        self.with_coord_script_and_trace_commitment(key, trace, LedgerEffectsCommitment::zero())
    }

    pub fn with_coord_script_and_trace_commitment(
        mut self,
        key: Hash<WasmModule>,
        trace: Vec<WitLedgerEffect>,
        host_calls_root: LedgerEffectsCommitment,
    ) -> Self {
        self.body.coordination_scripts_keys.push(key);
        self.coordination_scripts.push(ZkWasmProof {
            host_calls_root,
            trace,
        });
        self
    }

    pub fn with_ownership(mut self, token: OutputRef, owner: OutputRef) -> Self {
        self.body.ownership_out.insert(token, owner);
        self
    }

    pub fn with_entrypoint(mut self, entrypoint: usize) -> Self {
        self.body.entrypoint = entrypoint;
        self
    }

    pub fn build(self, interleaving_proof: ZkTransactionProof) -> ProvenTransaction {
        let witness = TransactionWitness {
            spending_proofs: self.spending_proofs,
            new_output_proofs: self.new_output_proofs,
            interleaving_proof,
            coordination_scripts: self.coordination_scripts,
        };

        ProvenTransaction {
            body: self.body,
            witness,
        }
    }
}

impl Default for TransactionBuilder {
    fn default() -> Self {
        Self::new()
    }
}
