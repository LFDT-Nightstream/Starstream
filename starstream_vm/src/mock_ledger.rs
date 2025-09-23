use serde::{Deserialize, Serialize};

use crate::CodeHash;

// TODO: Allow choosing between real public value and fake hash.
type MaybePublic<T> = T;
type Memory = Vec<u8>;
type IncrementalCommitment = Vec<u8>;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Token {
    pub code_address: CodeHash,
    pub entry_point: String, // Function of contract that is this token's main. TODO: use a hash?
    pub memory: Memory,

    // Amount is special because we want the VM to be able to automatically
    // trash tokens with an amount of zero.
    pub amount: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Utxo {
    pub code_address: CodeHash,
    pub entry_point: String, // Function of contract that is this UTXO's main. TODO: use a hash?
    pub memory: MaybePublic<Memory>,

    pub tokens: MaybePublic<Vec<Token>>,

    // TODO: some dApps need to force that the data is always unshielded to avoid
    // somebody not sharing the data, which livelocks any public dApp.
    // pub force_public: bool,
    pub incremental_commitment: IncrementalCommitment,
}

// May become a wrapper for an MCC type, which will have get_with_proof functionality that we'll re-export.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UtxoSet {
    pub utxos: Vec<Utxo>,
    pub incremental_commitment: IncrementalCommitment,
}

impl UtxoSet {
    // Private function that does the job of actually updating `utxos` vec.
    // TODO: what about the incremental commitment?
    fn apply(&mut self, body: &TransactionBody) -> Result<(), String> {
        todo!()
    }
}

#[derive(Default)]
pub struct TransactionBody {
    // Note: coordination script identity does not have to be revealed.
    pub inputs: Vec<Utxo>,
    pub referred: Vec<Utxo>,
    pub created: Vec<Utxo>,
    // Fee might go here conceptually, but the mock ledger doesn't implement it.
}

type TransactionProof = Vec<u8>;

pub struct Transaction {
    pub body: TransactionBody,
    pub proof: TransactionProof,
}

impl Transaction {
    /// Combine two transactions into one, folding their proofs as well.
    pub fn combine(&self, other: &Transaction) -> Result<Transaction, String> {
        // TODO: Check that self.outputs and other.inputs + referred are an acceptable combination.
        todo!()
    }
}

/// A simple test ledger that tracks UTXOs and their memories and can spawn and apply transactions.
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Ledger {
    utxos: UtxoSet,
}

impl Ledger {
    // UTXOs in the genesis block do not need valid proofs?
    pub fn new(genesis_utxos: Vec<Utxo>) -> Self {
        Ledger {
            utxos: UtxoSet {
                utxos: genesis_utxos,
                incremental_commitment: Default::default(),
            },
        }
    }

    // Debug functionality? Need to expose proofs if you're being serious.
    pub fn utxos(&self) -> &[Utxo] {
        &self.utxos.utxos[..]
    }

    // If you want to do extra checks before applying, or do folding yourself(?)
    pub fn get_utxo_in_set_proof(&self, utxo: &Utxo) -> Vec<u8> {
        todo!()
    }

    pub fn apply_without_verifying(&mut self, tx: Transaction) -> Result<(), String> {
        self.utxos.apply(&tx.body)?;
        todo!()
    }
}
