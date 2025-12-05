enum MaybePublic<T> {
    Public { data: T },
    // Hash must be masked/salted to prevent preimage attacks.
    // In practice there is risk of asset loss if the user forgets the salt.
    Private { salted_hash: u64 },
}

pub struct Token {
    pub code_address: String,
    pub memory: Vec<u8>,

    // special because we want the VM to be able to trash this if it's zero
    pub amount: u64,
}

#[derive(Debug, Clone)]
pub struct Utxo {
    pub code_address: String,
    pub memory: MaybePublic<Vec<u8>>,

    // should tokens be a Merkle tree?
    // - it's slower and takes more space and is harder to prove
    // - but means you can prove you hold some tokens w/o revealing the rest of your balance
    // too expensive... let's not if we can get away with it.
    pub tokens: MaybePublic<Vec<Token>>,

    // TODO: some dApps need to force the data is always unshielded
    // to avoid somebody not sharing the data (which livelocks any public dApp)
    pub force_public: bool,

    pub incremental_commitment: Vec<u8>,
    // pub starstream_utxo: starstream_dsl::Utxo,
}

// May become a wrapper for an MCC type, which will have get_with_proof functionality that we'll simply expose directly.
#[derive(Default)]
pub struct UtxoSet {
    pub utxos: Vec<Utxo>,
    pub incremental_commitment: Vec<u8>,
}

impl UtxoSet {
    // private function that does the job of actually updating self.utxos
    // and the incremental commitment?
    fn apply(&mut self, transaction: &Transaction) -> Result<(), String> {
        // todo: check that all inputs and referred are in the set
        // todo: erase inputs from the set
        // todo: add created to the set
        // ??? update incremental commitment?
        todo!()
    }
}

#[derive(Default)]
pub struct TransactionBody {
    // note: coordination script does not have to be revealed
    pub inputs: Vec<Utxo>,
    pub referred: Vec<Utxo>,
    pub created: Vec<Utxo>,
    // no fee for the mock ledger
}

pub struct Transaction {
    pub body: TransactionBody,
    pub proof: Vec<u8>,
}

impl Transaction {
    pub fn chain(&self, other: &Transaction) -> Result<Transaction, String> {
        // merge both bodies and fold proofs together
        todo!()
    }
}

#[derive(Default)]
pub struct Ledger {
    pub utxos: UtxoSet,
    // TODO: this could be removed, since we have an incremental commitment to the ledger state
    pub transactions: Vec<Transaction>,
    pub incremental_commitment: Vec<u8>,
}

impl Ledger {
    pub fn new(genesis_utxos: Vec<Utxo>) -> Self {
        todo!()
    }

    // debug functionality? need a proof if you're being serious
    pub fn utxo_set(&self) -> &[Utxo] {
        &self.utxos.utxos[..]
    }

    // if you want to do extra checks before applying, or do folding yourself(?)
    pub fn get_utxo_in_set_proof(&self, utxo: &Utxo) -> Vec<u8> {
        todo!()
    }

    pub fn apply(&mut self, transaction: Transaction) -> Result<(), String> {
        // apply to the utxo set, which checks validity
        self.utxos.apply(&transaction)?;
        // append the transaction to the ledger history
        self.transactions.push(transaction);
        // update the incremental commitment
        todo!()
    }
}

fn main() {
    println!("Created ledger: {:?}", 4);
}
