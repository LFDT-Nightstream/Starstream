use crate::{
    CoroutineState, Hash, mocked_verifier::MockedLookupTableCommitment, WasmModule,
    mocked_verifier::InterleavingError, transaction_effects::ProcessId,
};

// this mirrors the configuration described in SEMANTICS.md
#[derive(Clone)]
pub struct InterleavingInstance {
    /// Digest of all per-process host call tables the circuit is wired to.
    /// One per wasm proof.
    pub host_calls_roots: Vec<MockedLookupTableCommitment>,
    #[allow(dead_code)]
    pub host_calls_lens: Vec<u32>,

    /// Process table in canonical order: inputs, new_outputs, coord scripts.
    pub process_table: Vec<Hash<WasmModule>>,
    pub is_utxo: Vec<bool>,

    /// Burned/continuation mask for inputs (length = #inputs).
    pub must_burn: Vec<bool>,

    /// Offsets into the process table
    pub n_inputs: usize,
    pub n_new: usize,
    pub n_coords: usize,

    /// Initial ownership snapshot for inputs IN THE TRANSACTION.
    ///
    /// This has len == process_table
    ///
    /// process[i] == Some(j) means that utxo i is owned by j at the beginning of
    /// the transaction.
    ///
    /// None means not owned.
    pub ownership_in: Vec<Option<ProcessId>>,

    /// Final ownership snapshot for utxos IN THE TRANSACTION.
    ///
    /// This has len == process_table
    ///
    /// final state of the ownership graph (new ledger state).
    pub ownership_out: Vec<Option<ProcessId>>,

    /// First coordination script
    pub entrypoint: ProcessId,

    pub input_states: Vec<CoroutineState>,
}

impl InterleavingInstance {
    pub fn check_shape(&self) -> Result<(), InterleavingError> {
        // ---------- shape checks ----------
        // TODO: a few of these may be redundant
        //
        // the data layout is still a bit weird
        let n = self.process_table.len();

        if self.is_utxo.len() != n {
            return Err(InterleavingError::Shape("is_utxo len != process_table len"));
        }

        if self.ownership_in.len() != dbg!(dbg!(self.n_inputs) + dbg!(self.n_new))
            || self.ownership_out.len() != dbg!(self.n_inputs + self.n_new)
        {
            return Err(InterleavingError::Shape(
                "ownership_* len != self.n_inputs len + self.n_new len",
            ));
        }

        if self.entrypoint.0 >= n {
            return Err(InterleavingError::BadPid(self.entrypoint));
        }

        if self.must_burn.len() != self.n_inputs {
            return Err(InterleavingError::Shape("burned len != n_inputs"));
        }

        Ok(())
    }
}
