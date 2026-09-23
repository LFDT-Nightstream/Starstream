pub mod events;
pub mod interleaver;
pub mod quint;
pub mod trace;

pub use quint::{QuintError, QuintVerifier, VerificationFailure};
pub use trace::{
    InputUtxo, MethodHash, Out, OutputUtxo, ResourceHandle, StarstreamValue, Step, Trace,
    TransactionStatement,
};

// this needs to match neo_wasm's event commitment block size
// (but this crate does not depend on neo_wasm)
pub const BLOCK_SIZE: usize = 8;

/// Goldilocks' canonical modulus.
pub const FIELD_MODULUS: u64 = 0xffff_ffff_0000_0001;

/// Number of words in a method identity.
///
/// Right now this is sha256 split into 8 (32bit) limbs for exact
/// representation (Goldilocks can't represent full 64 bits)
///
/// A further optimization could be a more compact encoding, or dropping two
/// bits
pub const METHOD_WORDS: usize = 8;
