use p3_goldilocks::{Goldilocks, Poseidon2Goldilocks};
use p3_field::{PrimeCharacteristicRing};
use p3_symmetric::PaddingFreeSponge;
use p3_symmetric::CryptographicHasher;
use rand::{rngs::StdRng, SeedableRng};
use once_cell::sync::Lazy;

// capacity = WIDTH - RATE = 4 bytes = 256 bytes of pre-image security = 128 bits of collision resistance
const WIDTH: usize = 8; // recommended parameter
const RATE: usize = 4;  // Run permutation on 4 elements at a time (~256 bits). Must be < WIDTH
const OUT: usize = 4;   // 4*64= ~256 bit output (slightly lower as goldilocks is not quite 64-bits)

static HASHER: Lazy<Hash> = Lazy::new(|| {
  // constant seed so contract hashes are deterministic
  let mut rng = StdRng::seed_from_u64(0);
  let perm = Perm::new_from_rng_128(&mut rng);
  Hash::new(perm)
});

type Perm = Poseidon2Goldilocks<WIDTH>;
type Hash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

/**
 * Outputs a 256-bit hash from input bytes.
 */
pub fn poseidon2_hash_bytes(bytes: &[u8]) -> [Goldilocks; OUT] {
  let field_iter = bytes
        // Goldilocks field modulus: p = 2^64 - 2^32 + 1
        // therefore 8 bytes may not fit, so we use 7 instead
        .chunks(7)
        .map(|chunk| {
            // Interpret up to 8 bytes as little-endian u64
            let mut buf = [0u8; 8];
            // Interpret up to 7 bytes as little-endian u64,
            // padded with zeros in the high bytes.
            buf[..chunk.len()].copy_from_slice(chunk);
            let word = u64::from_le_bytes(buf);
            Goldilocks::from_u64(word)
        });

  HASHER.hash_iter(field_iter)
}
