pub mod builder;
mod mocked_verifier;
mod transaction_effects;

#[cfg(test)]
mod tests;

pub use crate::{
    mocked_verifier::InterleavingWitness, mocked_verifier::LedgerEffectsCommitment,
    transaction_effects::ProcessId,
};
use imbl::{HashMap, HashSet};
use neo_ajtai::Commitment;
use p3_field::PrimeCharacteristicRing;
use std::{hash::Hasher, marker::PhantomData};
pub use transaction_effects::{
    InterfaceId,
    abi::{ArgName, OPCODE_ARG_COUNT},
    instance::InterleavingInstance,
    witness::{
        EffectDiscriminant, REF_GET_WIDTH, REF_PUSH_WIDTH, REF_WRITE_WIDTH, WitEffectOutput,
        WitLedgerEffect,
    },
};

#[derive(PartialEq, Eq)]
pub struct Hash<T>(pub [u64; 4], pub PhantomData<T>);

impl<T> Copy for Hash<T> {}

impl<T> Clone for Hash<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct WasmModule(Vec<u8>);

/// Opaque user data.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(pub u64);

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value({})", self.0)
    }
}

impl Value {
    pub fn nil() -> Self {
        Value(0)
    }
}

fn encode_hash_to_fields<T>(hash: Hash<T>) -> [neo_math::F; 4] {
    hash.0.map(neo_math::F::from_u64)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Ref(pub u64);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum CoroutineState {
    Utxo {
        // For the purpose of this model, we only care *if* the state changed,
        // not what it is.
        storage: Vec<Value>,
    },
    Token {
        // For the purpose of this model, we only care *if* the state changed,
        // not what it is.
        storage: Vec<Value>,
    },
}

impl CoroutineState {
    pub fn storage(&self) -> &[Value] {
        match self {
            CoroutineState::Utxo { storage } | CoroutineState::Token { storage } => storage,
        }
    }
}

// pub struct ZkTransactionProof {}

#[allow(clippy::large_enum_variant)]
pub enum ZkTransactionProof {
    NeoProof {
        // does the verifier need this?
        session: neo_fold::session::FoldingSession<neo_ajtai::AjtaiSModule>,
        proof: neo_fold::shard::ShardProof,
        mcss_public: Vec<neo_ccs::McsInstance<Commitment, neo_math::F>>,
        steps_public: Vec<neo_memory::StepInstanceBundle<Commitment, neo_math::F, neo_math::K>>,
        // TODO: this shouldn't be here I think, the ccs should be known somehow by
        // the verifier
        ccs: neo_ccs::CcsStructure<neo_math::F>,
    },
    Dummy,
}

impl ZkTransactionProof {
    #[allow(clippy::result_large_err)]
    pub fn verify(
        &self,
        inst: &InterleavingInstance,
        wit: &InterleavingWitness,
    ) -> Result<(), VerificationError> {
        match self {
            ZkTransactionProof::NeoProof {
                session,
                proof,
                mcss_public,
                steps_public,
                ccs,
            } => {
                let output_binding_config = inst.output_binding_config();

                let ok = session
                    .verify_with_output_binding_simple(
                        ccs,
                        mcss_public,
                        proof,
                        &output_binding_config,
                    )
                    .expect("verify should run");

                assert!(ok, "optimized verification should pass");

                // dbg!(&self.steps_public[0].lut_insts[0].table);

                // NOTE: the indices in steps_public match the memory initializations
                // ordered by MemoryTag in the circuit
                let mut expected_fields = Vec::with_capacity(inst.process_table.len() * 4);
                for hash in &inst.process_table {
                    let hash_fields = encode_hash_to_fields(*hash);
                    expected_fields.extend(hash_fields.iter().copied());
                }
                // TODO: review if this is correct, I think all ROM's need to be
                // of the same size, so we have some extra padding.
                //
                // we may need to check the length or something as a new check,
                // or maybe try to just use a sparse definition?
                let process_table = &steps_public[0].lut_insts[0].table[0..expected_fields.len()];
                assert!(
                    expected_fields
                        .iter()
                        .zip(process_table.iter())
                        .all(|(expected, found)| *expected == *found),
                    "program hash table mismatch"
                );

                assert!(
                    inst.must_burn
                        .iter()
                        .zip(steps_public[0].lut_insts[1].table.iter())
                        .all(|(expected, found)| {
                            neo_math::F::from_u64(if *expected { 1 } else { 0 }) == *found
                        }),
                    "must burn table mismatch"
                );

                assert!(
                    inst.is_utxo
                        .iter()
                        .zip(steps_public[0].lut_insts[2].table.iter())
                        .all(|(expected, found)| {
                            neo_math::F::from_u64(if *expected { 1 } else { 0 }) == *found
                        }),
                    "is_utxo table mismatch"
                );
                assert!(
                    inst.is_token
                        .iter()
                        .zip(steps_public[0].lut_insts[4].table.iter())
                        .all(|(expected, found)| {
                            neo_math::F::from_u64(if *expected { 1 } else { 0 }) == *found
                        }),
                    "is_token table mismatch"
                );

                // TODO: check interfaces? but I think this can be private
                // dbg!(&self.steps_public[0].lut_insts[4].table);

                // dbg!(&steps_public[0].mcs_inst.x);
            }
            ZkTransactionProof::Dummy => {}
        }

        Ok(mocked_verifier::verify_interleaving_semantics(inst, wit)?)
    }
}

pub struct ZkWasmProof {
    pub host_calls_root: LedgerEffectsCommitment,
    pub trace: Vec<WitLedgerEffect>,
}

impl ZkWasmProof {
    pub fn public_instance(&self) -> WasmInstance {
        WasmInstance {
            host_calls_root: self.host_calls_root.clone(),
            host_calls_len: self.trace.len() as u32,
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn verify(
        &self,
        _input: Option<CoroutineState>,
        _key: &Hash<WasmModule>,
        _output: Option<CoroutineState>,
    ) -> Result<(), VerificationError> {
        Ok(())
    }
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
    InterleavingProofError(#[from] mocked_verifier::InterleavingError),
    #[error("Transaction input not found")]
    InputNotFound,
    #[error("Invalid token storage shape: expected {max}, got {actual}")]
    InvalidShapeTokenStorage { actual: usize, max: usize },
    #[error(
        "Invalid continuation state kind transition (input={input_kind}, output={output_kind})"
    )]
    InvalidStateKindTransition {
        input_kind: &'static str,
        output_kind: &'static str,
    },
}

/// The actual utxo identity.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct CoroutineId {
    pub creation_tx_hash: Hash<TransactionBody>,
    pub creation_output_index: u64,
}

#[derive(Clone, PartialEq, Eq)]
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
    /// ownership_out[p] == Some(q) means process p (token) is owned by process q at the end.
    ///
    /// Note that absence here means the token shouldn't have an owner.
    ///
    /// So this is a delta, where None means "remove the owner".
    pub ownership_out: HashMap<OutputRef, OutputRef>,

    pub coordination_scripts_keys: Vec<Hash<WasmModule>>,
    pub entrypoint: usize,
}

// and OutputRef is an index into the output segment of the transaction
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct OutputRef(usize);

impl From<usize> for OutputRef {
    fn from(v: usize) -> Self {
        OutputRef(v)
    }
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
    pub host_calls_root: LedgerEffectsCommitment,

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
    ///
    // NOTE: this is optional for now just for testing purposes
    pub interleaving_proof: ZkTransactionProof,

    /// Coordination script proofs.
    pub coordination_scripts: Vec<ZkWasmProof>,
}

/// A transaction that can be applied to the ledger
pub struct ProvenTransaction {
    pub body: TransactionBody,
    pub witness: TransactionWitness,
}

#[derive(Clone)]
#[must_use]
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
    const TOKEN_STORAGE_SIZE: usize = 3;

    fn state_kind(state: &CoroutineState) -> &'static str {
        match state {
            CoroutineState::Utxo { .. } => "utxo",
            CoroutineState::Token { .. } => "token",
        }
    }

    fn validate_token_state(state: &CoroutineState) -> Result<(), VerificationError> {
        if let CoroutineState::Token { storage } = state
            && storage.len() != Self::TOKEN_STORAGE_SIZE
        {
            return Err(VerificationError::InvalidShapeTokenStorage {
                actual: storage.len(),
                max: Self::TOKEN_STORAGE_SIZE,
            });
        }
        Ok(())
    }

    pub fn new() -> Self {
        Ledger {
            utxos: HashMap::new(),
            contract_counters: HashMap::new(),
            utxo_to_coroutine: HashMap::new(),
            ownership_registry: HashMap::new(),
        }
    }

    /// Returns transaction-local input ownership for the given input order.
    ///
    /// For each input at process id `pid`, the value is:
    /// - `Some(owner_pid)` if the input's stable coroutine is currently owned by
    ///   another input coroutine in this same list.
    /// - `None` otherwise.
    pub fn input_ownership_for_inputs(&self, inputs: &[UtxoId]) -> Vec<Option<ProcessId>> {
        let mut ownership = vec![None; inputs.len()];
        let mut coroutine_to_pid: HashMap<CoroutineId, ProcessId> = HashMap::new();

        for (pid, utxo_id) in inputs.iter().enumerate() {
            let Some(cid) = self.utxo_to_coroutine.get(utxo_id) else {
                continue;
            };
            coroutine_to_pid.insert(cid.clone(), ProcessId(pid));
        }

        for (pid, utxo_id) in inputs.iter().enumerate() {
            let Some(token_cid) = self.utxo_to_coroutine.get(utxo_id) else {
                continue;
            };
            let Some(owner_cid) = self.ownership_registry.get(token_cid) else {
                continue;
            };
            ownership[pid] = coroutine_to_pid.get(owner_cid).copied();
        }

        ownership
    }

    #[allow(clippy::result_large_err)]
    pub fn apply_transaction(&self, tx: &ProvenTransaction) -> Result<Ledger, VerificationError> {
        let mut new_ledger = self.clone();

        self.verify_witness(&tx.body, &tx.witness)?;

        let tx_hash = tx.body.hash();

        // Canonical process order used by the interleaving public instance:
        // processes = inputs ++ new_outputs ++ coordination_scripts_keys
        let n_inputs = tx.body.inputs.len();
        let n_new = tx.body.new_outputs.len();

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
        let mut process_to_coroutine: Vec<Option<CoroutineId>> = vec![None; n_inputs + n_new];

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
            let contract_hash = self.utxos[parent_utxo_id].contract_hash;

            let parent_state = self.utxos[parent_utxo_id].state.clone();

            // same state we don't change the nonce
            //
            let utxo_id = if cont == &parent_state {
                is_reference_input.insert(i);
                parent_utxo_id.clone()
            } else {
                // Allocate new UtxoId for the continued output
                let counter = new_ledger
                    .contract_counters
                    .entry(contract_hash)
                    .or_insert(0);

                let utxo_id = UtxoId {
                    contract_hash,
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
            new_ledger
                .utxo_to_coroutine
                .insert(utxo_id.clone(), coroutine_id);

            // actual utxo entry
            new_ledger.utxos.insert(
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
            let counter = new_ledger
                .contract_counters
                .entry(out.contract_hash)
                .or_insert(0);
            let utxo_id = UtxoId {
                contract_hash: out.contract_hash,
                nonce: *counter,
            };
            *counter += 1;

            let coroutine_id = CoroutineId {
                creation_tx_hash: tx_hash,
                // creation_output_index is relative to new_outputs (as before)
                creation_output_index: j as u64,
            };

            // Fill stable ids for processes in the "new_outputs" segment:
            // ProcessId(inputs.len() + j)
            process_to_coroutine[n_inputs + j] = Some(coroutine_id.clone());

            new_ledger
                .utxo_to_coroutine
                .insert(utxo_id.clone(), coroutine_id);
            new_ledger.utxos.insert(
                utxo_id,
                UtxoEntry {
                    state: out.state.clone(),
                    contract_hash: out.contract_hash,
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
        // assert_eq!(tx.body.ownership_out.len(), n_processes);

        for token_pid in 0..n_inputs + n_new {
            let token_cid = process_to_coroutine[token_pid].clone().unwrap();

            if let Some(owner_pid) = tx.body.ownership_out.get(&OutputRef(token_pid)) {
                let owner_cid = process_to_coroutine[owner_pid.0].clone().unwrap();

                new_ledger.ownership_registry.insert(token_cid, owner_cid);
            } else {
                new_ledger.ownership_registry.remove(&token_cid);
            }
        }

        // 4) Remove spent inputs
        for (i, input_id) in tx.body.inputs.iter().enumerate() {
            if is_reference_input.contains(&i) {
                continue;
            }

            if new_ledger.utxos.remove(input_id).is_none() {
                return Err(VerificationError::InputNotFound);
            }

            new_ledger.utxo_to_coroutine.remove(input_id);
        }

        Ok(new_ledger)
    }

    #[allow(clippy::result_large_err)]
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

        // if a utxo doesn't have a continuation state, we explicitly need to
        // check that it has a call to "burn".
        let mut burned = Vec::with_capacity(n_inputs);

        // verify continuation wasm proofs
        for (i, (utxo_id, proof)) in body.inputs.iter().zip(&witness.spending_proofs).enumerate() {
            let cont = &body.continuations[i];

            burned.push(cont.is_none());

            let Some(utxo_entry) = self.utxos.get(utxo_id) else {
                return Err(VerificationError::InputNotFound);
            };

            Self::validate_token_state(&utxo_entry.state)?;

            if let Some(cont_state) = cont {
                Self::validate_token_state(cont_state)?;
                if std::mem::discriminant(&utxo_entry.state) != std::mem::discriminant(cont_state) {
                    return Err(VerificationError::InvalidStateKindTransition {
                        input_kind: Self::state_kind(&utxo_entry.state),
                        output_kind: Self::state_kind(cont_state),
                    });
                }
            }

            proof.verify(
                Some(utxo_entry.state.clone()),
                &utxo_entry.contract_hash,
                cont.clone(),
            )?;
        }

        for (proof, entry) in witness
            .new_output_proofs
            .iter()
            .zip(body.new_outputs.iter())
        {
            Self::validate_token_state(&entry.state)?;
            proof.verify(None, &entry.contract_hash, Some(entry.state.clone()))?;
        }

        // verify all the coordination script proofs
        for (proof, key) in witness
            .coordination_scripts
            .iter()
            .zip(body.coordination_scripts_keys.iter())
        {
            proof.verify(None, key, None)?;
        }

        // Canonical process kind flags (used by the interleaving public instance).
        let is_utxo = (0..n_processes)
            .map(|pid| pid < (n_inputs + n_new))
            .collect::<Vec<_>>();
        let is_token = body
            .inputs
            .iter()
            .map(|input| matches!(self.utxos[input].state, CoroutineState::Token { .. }))
            .chain(
                body.new_outputs
                    .iter()
                    .map(|o| matches!(o.state, CoroutineState::Token { .. })),
            )
            .chain(std::iter::repeat_n(false, n_coords))
            .collect::<Vec<_>>();

        // 1. for each input, the verification key (wasm module hash) stored in the ledger.
        // 2. for each new output, the verification key (wasm module hash) included in it.
        // 3. for each coordination script, the verification key (wasm module hash) included in it.
        //
        // note that the order is bound too
        let process_table = body
            .inputs
            .iter()
            .map(|input| self.utxos[input].contract_hash)
            .chain(body.new_outputs.iter().map(|o| o.contract_hash))
            .chain(body.coordination_scripts_keys.iter().cloned())
            .collect::<Vec<_>>();

        // Initial ownership snapshot for utxos IN THE TRANSACTION.
        // This has len == process_table.len(). Coord scripts are None.
        //
        // token -> owner (both stable ids), projected into ProcessId space by matching
        // the transaction-local processes that correspond to stable ids.
        //
        // (The circuit enforces that ownership_out is derived legally from this.)
        let mut ownership_in = self.input_ownership_for_inputs(&body.inputs);
        ownership_in.extend(std::iter::repeat(None).take(n_new));

        // Build wasm instances in the same canonical order as process_table:
        // inputs ++ new_outputs ++ coord scripts
        let wasm_instances = build_wasm_instances_in_canonical_order(
            &witness.spending_proofs,
            &witness.new_output_proofs,
            &witness.coordination_scripts,
        )?;

        let ownership_out = (0..witness.spending_proofs.len() + witness.new_output_proofs.len())
            .map(|i| {
                body.ownership_out
                    .get(&OutputRef(i))
                    .copied()
                    .map(ProcessId::from)
            })
            .collect::<Vec<_>>();

        let input_states: Vec<CoroutineState> = body
            .inputs
            .iter()
            .map(|utxo_id| self.utxos[utxo_id].state.clone())
            .collect();

        let inst = InterleavingInstance {
            host_calls_roots: wasm_instances
                .iter()
                .map(|w| w.host_calls_root.clone())
                .collect(),
            process_table: process_table.to_vec(),

            is_utxo: is_utxo.to_vec(),
            is_token,
            must_burn: burned.to_vec(),

            n_inputs,
            n_new,
            n_coords,

            ownership_in: ownership_in.to_vec(),
            ownership_out: ownership_out.to_vec(),

            entrypoint: ProcessId(body.entrypoint),
            input_states,
        };

        let interleaving_proof: &ZkTransactionProof = &witness.interleaving_proof;

        let wit = InterleavingWitness {
            traces: witness
                .spending_proofs
                .iter()
                .map(|p| p.trace.clone())
                .chain(witness.new_output_proofs.iter().map(|p| p.trace.clone()))
                .chain(witness.coordination_scripts.iter().map(|p| p.trace.clone()))
                .collect(),
        };

        // note however that this is mocked right now, and it's using a non-zk
        // verifier.
        //
        // but the circuit in theory in theory encode the same machine
        interleaving_proof.verify(&inst, &wit)?;

        Ok(())
    }
}

impl Default for Ledger {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(clippy::result_large_err)]
pub fn build_wasm_instances_in_canonical_order(
    spending: &[ZkWasmProof],
    new_outputs: &[ZkWasmProof],
    coords: &[ZkWasmProof],
) -> Result<Vec<WasmInstance>, VerificationError> {
    Ok(spending
        .iter()
        .map(|p| p.public_instance())
        .chain(new_outputs.iter().map(|p| p.public_instance()))
        .chain(coords.iter().map(|p| p.public_instance()))
        .collect())
}

impl TransactionBody {
    pub fn hash(&self) -> Hash<TransactionBody> {
        Hash([0u64; 4], PhantomData)
    }
}

impl From<OutputRef> for ProcessId {
    fn from(val: OutputRef) -> Self {
        ProcessId(val.0)
    }
}

impl<T> std::hash::Hash for Hash<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> std::fmt::Debug for Hash<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Hash({:016x?})", self.0)
    }
}
