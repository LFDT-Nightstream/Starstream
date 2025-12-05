use p3_goldilocks::{Goldilocks, Poseidon2Goldilocks};
use p3_symmetric::PaddingFreeSponge;
use p3_symmetric::CryptographicHasher; // the trait
use p3_field::PrimeCharacteristicRing;
use rand::{rngs::StdRng, SeedableRng};

const WIDTH: usize = 8;
const RATE: usize = 4;      // must be < WIDTH
const OUT: usize = 1;       // number of field elements in the digest

type Perm = Poseidon2Goldilocks<WIDTH>;
type Hash = PaddingFreeSponge<Perm, WIDTH, RATE, OUT>;

fn make_hasher() -> Hash {
    // Deterministic RNG → deterministic Poseidon2 parameters
    let mut rng = StdRng::seed_from_u64(0);
    let perm = Perm::new_from_rng_128(&mut rng);
    Hash::new(perm)
}

pub fn poseidon2_hash_bytes(s: &[u8]) -> [Goldilocks; OUT] {
  let hasher = make_hasher();

  // Map each byte → Goldilocks field element
  let field_iter = s.iter().map(|b| Goldilocks::from_u8(*b));

  // PaddingFreeSponge implements CryptographicHasher<T, [T; OUT]>
  // where T = Goldilocks here.
  hasher.hash_iter(field_iter)
}
