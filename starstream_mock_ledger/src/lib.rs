mod interleaving_semantic_verifier;

use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

use crate::interleaving_semantic_verifier::HostCall;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Hash<T>([u8; 32], PhantomData<T>);

impl<T> std::fmt::Debug for Hash<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Hash({})", hex::encode(&self.0))
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct WasmModule(Vec<u8>);

/// Opaque user data.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Value(pub Vec<u8>);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match std::str::from_utf8(&self.0) {
            Ok(s) => write!(f, "Value(\"{}\")", s),
            Err(_) => write!(f, "Value({:?})", self.0),
        }
    }
}

impl Value {
    pub fn nil() -> Self {
        Value(vec![])
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CoroutineState {
    // For the purpose of this model, we only care *if* the state changed,
    // not what it is. A simple program counter is sufficient to check if a
    // coroutine continued execution in the tests.
    //
    // It's still TBD whether the module would yield its own continuation
    // state (making last_yield just the state), and always executed from the
    // entry-point, or whether those should actually be different things.
    pc: u64,
    last_yield: Value,
}

pub struct ZkTransactionProof {}

impl ZkTransactionProof {
    pub fn verify(
        &self,
        inst: &InterleavingInstance,
        input_states: &[CoroutineState],
    ) -> Result<(), VerificationError> {
        let traces = inst
            .host_calls_roots
            .iter()
            .map(|lt| lt.trace.clone())
            .collect();

        let wit = interleaving_semantic_verifier::InterleavingWitness { traces };

        Ok(interleaving_semantic_verifier::verify_interleaving_semantics(
            inst,
            &wit,
            input_states,
        )?)
    }
}

pub struct ZkWasmProof {
    pub host_calls_root: LookupTableCommitment,
}

impl ZkWasmProof {
    pub fn public_instance(&self) -> WasmInstance {
        WasmInstance {
            host_calls_root: self.host_calls_root.clone(),
            host_calls_len: self.host_calls_root.trace.len() as u32,
        }
    }

    pub fn verify(
        &self,
        _input: Option<CoroutineState>,
        _key: &Hash<WasmModule>,
        _output: Option<CoroutineState>,
    ) -> Result<(), VerificationError> {
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct LookupTableCommitment {
    // obviously the actual commitment shouldn't have this
    // but this is used for the mocked circuit
    trace: Vec<HostCall>,
}

#[derive(thiserror::Error, Debug)]
pub enum VerificationError {
    #[error("Input continuation size mismatch")]
    InputContinuationSizeMismatch,
    #[error("Ownership size mismatch")]
    OwnershipSizeMismatch,
    #[error("Owner has no stable identity")]
    OwnerHasNoStableIdentity,
    #[error("Interleaving proof error: {0}")]
    InterleavingProofError(#[from] interleaving_semantic_verifier::InterleavingError),
}

/// The actual utxo identity.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct UtxoId {
    pub contract_hash: Hash<WasmModule>,
    /// A Global Sequence Number for this specific contract code.
    /// - Assigned by Ledger at creation by keeping track of the utxos with the same wasm.
    /// - Utxo's can't know about it, otherwise it would lead to contention.
    pub nonce: u64,
}

/// Uniquely identifies a "process" or "chain" of states.
/// Defined by the transaction that spawned it (Genesis).
///
/// This is an internal id, transactions don't know/care about this.
///
/// The ledger uses stable identities internally to keep track of ownership
/// without having to rewrite all the tuples in the relation each time a utxo
/// with tokens gets resumed.
///
/// When resuming a utxo, the utxo_to_coroutine mapping gets updated.
///
/// But utxos just refer to each other through relative indexing in the
/// transaction input/outputs.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CoroutineId {
    pub creation_tx_hash: Hash<TransactionBody>,
    pub creation_output_index: u64,
}

/// an index into a table with all the coroutines that are iterated in the
/// current transaction
pub type ProcessId = usize;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TransactionBody {
    pub inputs: Vec<UtxoId>,

    /// Continuation outputs aligned with inputs.
    ///
    /// Must have length == inputs.len().
    /// - continuations[i] = Some(out): input[i] continues with out.state
    /// - continuations[i] = None: input[i] is burned (no continuation)
    pub continuations: Vec<Option<CoroutineState>>,

    /// New spawns created by coordination scripts (no parent input).
    /// Basically the condition for this is that a utxo called `new`, this is
    /// also then used to verify the interleaving proof.
    pub new_outputs: Vec<NewOutput>,

    /// Final ownership snapshot for utxos IN THE TRANSACTION.
    ///
    /// This has len == process_table.len() where process_table is
    /// inputs ++ new_outputs ++ coord scripts.
    ///
    /// ownership_out[p] == Some(q) means process p (token) is owned by process q at the end.
    /// None means unowned.
    ///
    /// (Coord scripts should always be None, and any illegal edges are rejected by the interleaving proof.)
    pub ownership_out: Vec<Option<ProcessId>>,

    pub coordination_scripts_keys: Vec<Hash<WasmModule>>,
    pub entrypoint: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NewOutput {
    pub state: CoroutineState,
    pub contract_hash: Hash<WasmModule>,
}

/// Public instance extracted from a zkWasm proof (per process).
pub struct WasmInstance {
    /// Commitment to the ordered list of host calls performed by this vm. Each
    /// entry encodes opcode + args + return.
    pub host_calls_root: LookupTableCommitment,

    /// Number of host calls (length of the list committed by host_calls_root).
    pub host_calls_len: u32,
}

/// In practice this is going to be aggregated into a single proof (or maybe
/// two).
pub struct TransactionWitness {
    /// ZK Proofs corresponding to inputs (spending).
    ///
    /// Note that to verify these, the matching output state has to be provided
    /// too.
    pub spending_proofs: Vec<ZkWasmProof>,

    /// ZK Proofs corresponding to new coroutines.
    pub new_output_proofs: Vec<ZkWasmProof>,

    /// The global transaction proof.
    ///
    /// This has access to all the operations peformed by each spending_proof
    // that require either talking to another coroutine, or making changes/reads
    // in the ledger (like token ownership).
    //
    /// Plus the attestation capability (getting the hash of one of the
    /// coroutines).
    ///
    /// Note that the circuit for this is fixed in the ledger (just like the
    /// zkwasm one), so in practice this encodes the transaction rules.
    pub interleaving_proof: ZkTransactionProof,

    /// Coordination script proofs.
    pub coordination_scripts: Vec<ZkWasmProof>,
}

/// A transaction that can be applied to the ledger
pub struct ProvenTransaction {
    pub body: TransactionBody,
    pub witness: TransactionWitness,
}

pub struct Ledger {
    pub utxos: HashMap<UtxoId, UtxoEntry>,

    // ContractHash -> NextAvailableNonce
    pub contract_counters: HashMap<Hash<WasmModule>, u64>,

    pub utxo_to_coroutine: HashMap<UtxoId, CoroutineId>,

    // Ownership registry.
    //
    // many to one mapping of inclusion: token -> utxo.
    pub ownership_registry: HashMap<CoroutineId /* token */, CoroutineId /* owner */>,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct UtxoEntry {
    pub state: CoroutineState,
    pub contract_hash: Hash<WasmModule>,
}

impl Ledger {
    pub fn apply_transaction(&mut self, tx: &ProvenTransaction) -> Result<(), VerificationError> {
        self.verify_witness(&tx.body, &tx.witness)?;

        let tx_hash = tx.body.hash();

        // Canonical process order used by the interleaving public instance:
        // processes = inputs ++ new_outputs ++ coordination_scripts_keys
        let n_inputs = tx.body.inputs.len();
        let n_new = tx.body.new_outputs.len();
        let n_coords = tx.body.coordination_scripts_keys.len();
        let n_processes = n_inputs + n_new + n_coords;

        // For translating ProcessId -> stable CoroutineId when applying ownership changes.
        // Coord scripts have no stable identity, so we store None for those slots.
        //
        // reminder:
        //
        // a ProcessId is the offset of that program in the transaction
        // a CoroutineId is the genesis of a chain of utxo states (like a branch name).
        //
        // Coordination scripts are processes (or threads or fibers) from the
        // point of view of the interleaving machine, but don't have an identity
        // in the ledger. The proof just cares about the order, and that's used
        // for addressing.
        //
        // CoroutineIds are mostly kept around to keep relations simpler. If a
        // utxo gets resumed, all the tokens that point to it (are owned by) are
        // technically owned by the new utxo.
        //
        // But there is no reason to go and change all the links instead of just
        // changing a pointer.
        let mut process_to_coroutine: Vec<Option<CoroutineId>> = vec![None; n_processes];

        // Pre-state stable ids for inputs (aligned with inputs/process ids 0..n_inputs-1).
        for (i, utxo_id) in tx.body.inputs.iter().enumerate() {
            let cid = self.utxo_to_coroutine[utxo_id].clone();
            process_to_coroutine[i] = Some(cid);
        }

        // Track which input indices are *not* removed from the ledger because
        // the continuation reuses the same UtxoId.
        let mut is_reference_input: HashSet<usize> = HashSet::new();

        // inputs and continuations have to be aligned, so we just zip over them
        for (i, cont_opt) in tx.body.continuations.iter().enumerate() {
            let Some(cont) = cont_opt else { continue };

            // parent meaning previous state (each continuation is a chain of
            // utxos)
            let parent_utxo_id = &tx.body.inputs[i];

            // A continuation has the same contract hash as the input it resumes
            let contract_hash = self.utxos[parent_utxo_id].contract_hash.clone();

            let parent_state = self.utxos[parent_utxo_id].state.clone();

            // same state we don't change the nonce
            //
            // note that this doesn't include the last_yield claim
            let utxo_id = if cont.pc == parent_state.pc {
                is_reference_input.insert(i);
                parent_utxo_id.clone()
            } else {
                // Allocate new UtxoId for the continued output
                let counter = self
                    .contract_counters
                    .entry(contract_hash.clone())
                    .or_insert(0);

                let utxo_id = UtxoId {
                    contract_hash: contract_hash.clone(),
                    nonce: *counter,
                };
                *counter += 1;

                utxo_id
            };

            // Same stable CoroutineId as the input
            let coroutine_id = process_to_coroutine[i]
                .clone()
                .expect("input must have coroutine id");

            // update index
            self.utxo_to_coroutine.insert(utxo_id.clone(), coroutine_id);

            // actual utxo entry
            self.utxos.insert(
                utxo_id,
                UtxoEntry {
                    state: cont.clone(),
                    contract_hash,
                },
            );
        }

        // new utxos that don't resume anything
        for (j, out) in tx.body.new_outputs.iter().enumerate() {
            // note that the nonce is not 0, this counts instances of the same
            // code, not just resumptions of the same coroutine
            let counter = self
                .contract_counters
                .entry(out.contract_hash.clone())
                .or_insert(0);
            let utxo_id = UtxoId {
                contract_hash: out.contract_hash.clone(),
                nonce: *counter,
            };
            *counter += 1;

            let coroutine_id = CoroutineId {
                creation_tx_hash: tx_hash.clone(),
                // creation_output_index is relative to new_outputs (as before)
                creation_output_index: j as u64,
            };

            // Fill stable ids for processes in the "new_outputs" segment:
            // ProcessId(inputs.len() + j)
            process_to_coroutine[n_inputs + j] = Some(coroutine_id.clone());

            self.utxo_to_coroutine.insert(utxo_id.clone(), coroutine_id);
            self.utxos.insert(
                utxo_id,
                UtxoEntry {
                    state: out.state.clone(),
                    contract_hash: out.contract_hash.clone(),
                },
            );
        }

        // Apply ownership updates for the processes that exist in this transaction's process set.
        //
        // NOTE: we don't check things here, that's part of the
        // interleaving/transaction proof, which we already verified
        //
        // We only translate ProcessId -> stable CoroutineId for utxo processes
        // (inputs and new_outputs). Coord scripts have None and thus can't appear in
        // the on-ledger ownership_registry.
        assert_eq!(tx.body.ownership_out.len(), n_processes);

        for (token_pid, owner_pid_opt) in tx.body.ownership_out.iter().enumerate() {
            // Ignore coord scripts segment (no stable identity).
            let Some(token_cid) = process_to_coroutine[token_pid].clone() else {
                continue;
            };

            if let Some(owner_pid) = owner_pid_opt {
                let Some(owner_cid) = process_to_coroutine[*owner_pid].clone() else {
                    // the proof should reject this in theory
                    return Err(VerificationError::OwnerHasNoStableIdentity);
                };

                self.ownership_registry.insert(token_cid, owner_cid);
            } else {
                self.ownership_registry.remove(&token_cid);
            }
        }

        // 4) Remove spent inputs
        for (i, input_id) in tx.body.inputs.iter().enumerate() {
            if is_reference_input.contains(&i) {
                continue;
            }

            self.utxos.remove(input_id);
            self.utxo_to_coroutine.remove(input_id);
        }

        Ok(())
    }

    pub fn verify_witness(
        &self,
        body: &TransactionBody,
        witness: &TransactionWitness,
    ) -> Result<(), VerificationError> {
        assert_eq!(witness.spending_proofs.len(), body.inputs.len());

        if body.continuations.len() != body.inputs.len() {
            return Err(VerificationError::InputContinuationSizeMismatch);
        }

        let n_inputs = body.inputs.len();
        let n_new = body.new_outputs.len();
        let n_coords = body.coordination_scripts_keys.len();
        let n_processes = n_inputs + n_new + n_coords;

        if body.ownership_out.len() != n_processes {
            return Err(VerificationError::OwnershipSizeMismatch);
        }

        // if a utxo doesn't have a continuation state, we explicitly need to
        // check that it has a call to "burn".
        let mut burned = Vec::with_capacity(n_inputs);

        // verify continuation wasm proofs
        for (i, (utxo_id, proof)) in body.inputs.iter().zip(&witness.spending_proofs).enumerate() {
            let cont = &body.continuations[i];

            burned.push(cont.is_none());

            let utxo_entry = self.utxos[utxo_id].clone();

            proof.verify(
                Some(utxo_entry.state),
                &utxo_entry.contract_hash,
                cont.clone(),
            )?;
        }

        // verify all the coordination script proofs
        for (proof, key) in witness
            .coordination_scripts
            .iter()
            .zip(body.coordination_scripts_keys.iter())
        {
            proof.verify(None, key, None)?;
        }

        for (proof, entry) in witness
            .new_output_proofs
            .iter()
            .zip(body.new_outputs.iter())
        {
            proof.verify(None, &entry.contract_hash, Some(entry.state.clone()))?;
        }

        // Canonical process kind flags (used by the interleaving public instance).
        let is_utxo = (0..n_processes)
            .map(|pid| pid < (n_inputs + n_new))
            .collect::<Vec<_>>();

        // 1. for each input, the verification key (wasm module hash) stored in the ledger.
        // 2. for each new output, the verification key (wasm module hash) included in it.
        // 3. for each coordination script, the verification key (wasm module hash) included in it.
        //
        // note that the order is bound too
        let process_table = body
            .inputs
            .iter()
            .map(|input| self.utxos[input].contract_hash.clone())
            .chain(body.new_outputs.iter().map(|o| o.contract_hash.clone()))
            .chain(body.coordination_scripts_keys.iter().cloned())
            .collect::<Vec<_>>();

        // Initial ownership snapshot for utxos IN THE TRANSACTION.
        // This has len == process_table.len(). Coord scripts are None.
        //
        // token -> owner (both stable ids), projected into ProcessId space by matching
        // the transaction-local processes that correspond to stable ids.
        //
        // (The circuit enforces that ownership_out is derived legally from this.)
        let mut ownership_in: Vec<Option<ProcessId>> = vec![None; n_processes];

        // Build ProcessId -> stable CoroutineId map for inputs/new_outputs.
        // - Inputs: stable ids from ledger
        // - New outputs: have no prior stable id, so they start as unowned in ownership_in
        // - Coord scripts: None
        let mut process_to_coroutine: Vec<Option<CoroutineId>> = vec![None; n_processes];
        for (i, utxo_id) in body.inputs.iter().enumerate() {
            process_to_coroutine[i] = Some(self.utxo_to_coroutine[utxo_id].clone());
        }
        // new_outputs and coord scripts remain None here, which encodes "no prior ownership relation"

        // Invert for the subset of stable ids that appear in inputs (so we can express owner as ProcessId).
        let mut coroutine_to_process: HashMap<CoroutineId, ProcessId> = HashMap::new();
        for (pid, cid_opt) in process_to_coroutine.iter().enumerate() {
            if let Some(cid) = cid_opt {
                coroutine_to_process.insert(cid.clone(), pid);
            }
        }

        // Fill ownership_in only for tokens that are inputs (the only ones that existed before the tx).
        // New outputs are necessarily unowned at start, and coord scripts are None.
        for (token_cid, owner_cid) in self.ownership_registry.iter() {
            let Some(&token_pid) = coroutine_to_process.get(token_cid) else {
                continue;
            };
            let Some(&owner_pid) = coroutine_to_process.get(owner_cid) else {
                continue;
            };
            ownership_in[token_pid] = Some(owner_pid);
        }

        // Build wasm instances in the same canonical order as process_table:
        // inputs ++ new_outputs ++ coord scripts
        let wasm_instances = build_wasm_instances_in_canonical_order(
            &witness.spending_proofs,
            &witness.new_output_proofs,
            &witness.coordination_scripts,
        )?;

        verify_interleaving_public(
            &process_table,
            &is_utxo,
            &burned,
            &ownership_in,
            &body.ownership_out,
            &wasm_instances,
            &witness.interleaving_proof,
            body.inputs.len(),
            body.entrypoint,
            body,
            self,
        )?;

        Ok(())
    }
}

// this mirrors the configuration described in SEMANTICS.md
pub struct InterleavingInstance {
    /// Digest of all per-process host call tables the circuit is wired to.
    /// One per wasm proof.
    pub host_calls_roots: Vec<LookupTableCommitment>,
    #[allow(dead_code)]
    pub host_calls_lens: Vec<u32>,

    /// Process table in canonical order: inputs, new_outputs, coord scripts.
    process_table: Vec<Hash<WasmModule>>,
    is_utxo: Vec<bool>,

    /// Burned/continuation mask for inputs (length = #inputs).
    burned: Vec<bool>,
    n_inputs: usize,

    /// Initial ownership snapshot for inputs IN THE TRANSACTION.
    ///
    /// This has len == process_table
    ///
    /// process[i] == Some(j) means that utxo i is owned by j at the beginning of
    /// the transaction.
    ///
    /// None means not owned.
    ownership_in: Vec<Option<ProcessId>>,

    /// Final ownership snapshot for utxos IN THE TRANSACTION.
    ///
    /// This has len == process_table
    ///
    /// final state of the ownership graph (new ledger state).
    ownership_out: Vec<Option<ProcessId>>,

    entrypoint: usize,
}

pub fn verify_interleaving_public(
    process_table: &[Hash<WasmModule>],
    is_utxo: &[bool],
    burned: &[bool],
    ownership_in: &[Option<ProcessId>],
    ownership_out: &[Option<ProcessId>],
    wasm_instances: &[WasmInstance],
    interleaving_proof: &ZkTransactionProof,
    n_inputs: usize,
    entrypoint: usize,
    body: &TransactionBody,
    ledger: &Ledger,
) -> Result<(), VerificationError> {
    // ---------- derive the public instance that the interleaving proof MUST be verified against ----------
    // We bind the interleaving proof to:
    // - the vector of per-process host call commitments and lengths
    // - process_table (program hashes),
    // - is_utxo
    // - burned/present mask and new_outputs_len
    // - ownership_in and ownership_changes
    let inst = InterleavingInstance {
        host_calls_roots: wasm_instances
            .iter()
            .map(|w| w.host_calls_root.clone())
            .collect(),
        host_calls_lens: wasm_instances.iter().map(|w| w.host_calls_len).collect(),

        process_table: process_table.to_vec(),
        is_utxo: is_utxo.to_vec(),

        burned: burned.to_vec(),

        ownership_in: ownership_in.to_vec(),
        ownership_out: ownership_out.to_vec(),
        n_inputs,
        entrypoint,
    };

    // Collect input states for the interleaving proof
    let input_states: Vec<CoroutineState> = body
        .inputs
        .iter()
        .map(|utxo_id| ledger.utxos[utxo_id].state.clone())
        .collect();

    // ---------- verify interleaving proof ----------
    // All semantics (resume/yield matching, ownership authorization, attestation, etc.)
    // are enforced inside the interleaving circuit relative to inst + the committed tables.
    //
    // See the README.md for the high level description.
    //
    // NOTE: however that this is mocked right now, and it's using a non-zk
    // verifier.
    //
    // but the circuit in theory in theory encode the same machine
    interleaving_proof.verify(&inst, &input_states)?;

    Ok(())
}

// ---------- helper glue (still pseudocode) ----------

pub fn build_wasm_instances_in_canonical_order(
    spending: &[ZkWasmProof],
    new_outputs: &[ZkWasmProof],
    coords: &[ZkWasmProof],
) -> Result<Vec<WasmInstance>, VerificationError> {
    let mut out = Vec::with_capacity(spending.len() + new_outputs.len() + coords.len());

    for p in spending {
        out.push(p.public_instance()); // returns WasmInstance { host_calls_root, host_calls_len }
    }
    for p in new_outputs {
        out.push(p.public_instance());
    }
    for p in coords {
        out.push(p.public_instance());
    }

    Ok(out)
}

impl TransactionBody {
    pub fn hash(&self) -> Hash<TransactionBody> {
        Hash([0u8; 32], PhantomData)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interleaving_semantic_verifier::{HostCall, InterleavingWitness};

    pub fn h(n: u8) -> Hash<WasmModule> {
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
        //      |---NewUtxo---|-----------------|--------------->(created)      |
        //      |---NewUtxo---|-----------------|-----------------|------------->(created)
        //      |             |                 |                 |               |
        //      |---Resume--->| (spend)         |                 |               |
        //      |<--Yield-----| (continue)      |                 |               |
        //      |             |                 |                 |               |
        //      |---Resume-------------------->| (spend)         |               |
        //      |<--Burn----------------------|                 |               |
        //      |             |                 |                 |               |
        //      |---Resume-------------------------------------->|               |
        //      |<--Yield---------------------------------------|               |
        //      |             |                 |                 |               |
        //      |---Resume------------------------------------------------------>|
        //      |<--Yield-------------------------------------------------------|
        //      |             |                 |                 |               |
        //    (end)        (output)             X (burned)       (output)        (output)

        // Create a transaction with:
        // - 2 input UTXOs to spend (processes 0, 1)
        // - 2 new UTXOs (processes 2, 3)
        // - 1 coordination script (process 4)

        let input_hash_1 = h(10);
        let input_hash_2 = h(11);
        let coord_hash = h(1);
        let utxo_hash_a = h(2);
        let utxo_hash_b = h(3);

        // Create input UTXO IDs
        let input_utxo_1 = UtxoId {
            contract_hash: input_hash_1.clone(),
            nonce: 0,
        };
        let input_utxo_2 = UtxoId {
            contract_hash: input_hash_2.clone(),
            nonce: 0,
        };

        // Transaction body
        let tx_body = TransactionBody {
            inputs: vec![input_utxo_1.clone(), input_utxo_2.clone()],
            continuations: vec![
                Some(CoroutineState {
                    pc: 1, // Modified state for input 1
                    last_yield: v(b"continued_1"),
                }),
                None, // Input 2 is burned
            ],
            new_outputs: vec![
                NewOutput {
                    state: CoroutineState {
                        pc: 0,
                        last_yield: v(b"utxo_a_state"),
                    },
                    contract_hash: utxo_hash_a.clone(),
                },
                NewOutput {
                    state: CoroutineState {
                        pc: 0,
                        last_yield: v(b"utxo_b_state"),
                    },
                    contract_hash: utxo_hash_b.clone(),
                },
            ],
            ownership_out: vec![
                None,    // process 0 (input_1 continuation) - unowned
                None,    // process 1 (input_2 burned) - N/A
                Some(3), // process 2 (utxo_a) owned by utxo 3 (utxo_b)
                None,    // process 3 (utxo_b) - unowned
                None,    // process 4 (coord) - no ownership
            ],
            coordination_scripts_keys: vec![coord_hash.clone()],
            entrypoint: 4, // Coordination script is now process 4
        };

        // Host call traces for each process in canonical order: inputs ++ new_outputs ++ coord_scripts
        // Process 0: Input 1, Process 1: Input 2, Process 2: UTXO A (spawn), Process 3: UTXO B (spawn), Process 4: Coordination script

        let input_1_trace = vec![HostCall::Yield {
            val: v(b"continued_1"),
            ret: None,
            id_prev: Some(4), // Coordination script is process 4
        }];

        let input_2_trace = vec![HostCall::Burn {
            ret: v(b"burned_2"),
        }];

        let utxo_a_trace = vec![
            // UTXO A binds itself to UTXO B (making B the owner)
            HostCall::Bind { owner_id: 3 }, // UTXO B is now process 3
            // Yield back to coordinator (end-of-transaction)
            HostCall::Yield {
                val: v(b"done_a"),
                ret: None,
                id_prev: Some(4), // Coordination script is now process 4
            },
        ];

        let utxo_b_trace = vec![
            // UTXO B just yields back (end-of-transaction)
            HostCall::Yield {
                val: v(b"done_b"),
                ret: None,
                id_prev: Some(4), // Coordination script is now process 4
            },
        ];

        let coord_trace = vec![
            // Coordination script creates the two UTXOs
            HostCall::NewUtxo {
                program_hash: h(2),
                val: v(b"init_a"),
                id: 2, // UTXO A
            },
            HostCall::NewUtxo {
                program_hash: h(3),
                val: v(b"init_b"),
                id: 3, // UTXO B
            },
            HostCall::Resume {
                target: 0, // Input 1
                val: v(b"spend_input_1"),
                ret: v(b"continued_1"),
                id_prev: None,
            },
            HostCall::Resume {
                target: 1, // Input 2
                val: v(b"spend_input_2"),
                ret: v(b"burned_2"),
                id_prev: Some(0), // Input 1
            },
            HostCall::Resume {
                target: 2, // UTXO A
                val: v(b"init_a"),
                ret: v(b"done_a"),
                id_prev: Some(1), // Input 2
            },
            HostCall::Resume {
                target: 3, // UTXO B
                val: v(b"init_b"),
                ret: v(b"done_b"),
                id_prev: Some(2), // UTXO A
            },
        ];

        let witness = InterleavingWitness {
            traces: vec![
                input_1_trace,
                input_2_trace,
                utxo_a_trace,
                utxo_b_trace,
                coord_trace,
            ],
        };

        let mock_proofs = TransactionWitness {
            spending_proofs: vec![
                ZkWasmProof {
                    host_calls_root: LookupTableCommitment {
                        trace: witness.traces[0].clone(), // Input 1 trace
                    },
                },
                ZkWasmProof {
                    host_calls_root: LookupTableCommitment {
                        trace: witness.traces[1].clone(), // Input 2 trace
                    },
                },
            ],
            new_output_proofs: vec![
                ZkWasmProof {
                    host_calls_root: LookupTableCommitment {
                        trace: witness.traces[2].clone(), // UTXO A trace
                    },
                },
                ZkWasmProof {
                    host_calls_root: LookupTableCommitment {
                        trace: witness.traces[3].clone(), // UTXO B trace
                    },
                },
            ],
            interleaving_proof: ZkTransactionProof {},
            coordination_scripts: vec![ZkWasmProof {
                host_calls_root: LookupTableCommitment {
                    trace: witness.traces[4].clone(), // Coordination script trace
                },
            }],
        };

        let proven_tx = ProvenTransaction {
            body: tx_body,
            witness: mock_proofs,
        };

        let (mut ledger, _input_utxo_1, _input_utxo_2, _input_1_coroutine, _input_2_coroutine) =
            mock_genesis();

        ledger.apply_transaction(&proven_tx).unwrap();

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
        //    Resume ---------------->|
        //   (val="init_utxo")         |
        //        |               Input (val="init_utxo", caller=P1)
        //        |               ProgramHash(P1) -> (attest caller)
        //        |               GetHandlerFor -> P1
        //        |<----------------- Resume (Effect call)
        //        |        (val="Interface::Effect(42)")
        //(handles effect)             |
        //        |                    |
        //    Resume ---------------->| (Resume with result)
        //(val="Interface::EffectResponse(43)")
        //        |                    |
        //        |<----------------- Yield
        //        |            (val="utxo_final")
        //UninstallHandler (self)      |
        //        |                    |
        //      (end)                  |

        let coord_hash = h(1);
        let utxo_hash = h(2);
        let interface_id = 42u64;

        // Transaction body
        let tx_body = TransactionBody {
            inputs: vec![],
            continuations: vec![],
            new_outputs: vec![NewOutput {
                state: CoroutineState {
                    pc: 0,
                    last_yield: v(b"utxo_state"),
                },
                contract_hash: utxo_hash.clone(),
            }],
            ownership_out: vec![
                None, // process 0 (utxo) - unowned
                None, // process 1 (coord) - no ownership (this can be optimized out)
            ],
            coordination_scripts_keys: vec![coord_hash.clone()],
            entrypoint: 1,
        };

        // Host call traces for each process in canonical order: (no inputs) ++ new_outputs ++ coord_scripts
        // Process 0: UTXO, Process 1: Coordination script

        let coord_trace = vec![
            HostCall::InstallHandler { interface_id },
            HostCall::NewUtxo {
                program_hash: h(2),
                val: v(b"init_utxo"),
                id: 0,
            },
            HostCall::Resume {
                target: 0,
                val: v(b"init_utxo"),
                ret: v(b"Interface::Effect(42)"), // expected request
                id_prev: None,
            },
            HostCall::Resume {
                target: 0,
                val: v(b"Interface::EffectResponse(43)"), // response sent
                ret: v(b"utxo_final"),
                id_prev: Some(0),
            },
            HostCall::UninstallHandler { interface_id },
        ];

        let utxo_trace = vec![
            HostCall::Input {
                val: v(b"init_utxo"),
                caller: 1,
            },
            HostCall::ProgramHash {
                target: 1,
                program_hash: coord_hash.clone(), // assert coord_script hash == h(1)
            },
            HostCall::GetHandlerFor {
                interface_id,
                handler_id: 1,
            },
            HostCall::Resume {
                target: 1,
                val: v(b"Interface::Effect(42)"),         // request
                ret: v(b"Interface::EffectResponse(43)"), // expected response
                id_prev: Some(1),
            },
            HostCall::Yield {
                val: v(b"utxo_final"),
                ret: None,
                id_prev: Some(1),
            },
        ];

        let witness = InterleavingWitness {
            traces: vec![utxo_trace, coord_trace],
        };

        let mock_proofs = TransactionWitness {
            spending_proofs: vec![],
            new_output_proofs: vec![ZkWasmProof {
                host_calls_root: LookupTableCommitment {
                    trace: witness.traces[0].clone(),
                },
            }],
            interleaving_proof: ZkTransactionProof {},
            coordination_scripts: vec![ZkWasmProof {
                host_calls_root: LookupTableCommitment {
                    trace: witness.traces[1].clone(),
                },
            }],
        };

        let proven_tx = ProvenTransaction {
            body: tx_body,
            witness: mock_proofs,
        };

        let mut ledger = Ledger {
            utxos: HashMap::new(),
            contract_counters: HashMap::new(),
            utxo_to_coroutine: HashMap::new(),
            ownership_registry: HashMap::new(),
        };

        ledger.apply_transaction(&proven_tx).unwrap();

        assert_eq!(ledger.utxos.len(), 1); // 1 new UTXO
        assert_eq!(ledger.ownership_registry.len(), 0); // No ownership relationships
    }
}
