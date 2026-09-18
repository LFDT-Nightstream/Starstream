use sha2::Digest;

wit_bindgen::generate!({
    world: "sha256lib",
});

struct World;

impl Guest for World {
    fn sha256_u64(input: u64) -> (u64, u64, u64, u64) {
        let digest = sha2::Sha256::digest(&input.to_le_bytes());
        let digest = digest.as_slice().as_chunks::<8>();
        (
            u64::from_le_bytes(digest.0[0]),
            u64::from_le_bytes(digest.0[1]),
            u64::from_le_bytes(digest.0[2]),
            u64::from_le_bytes(digest.0[3]),
        )
    }
}

export!(World);
