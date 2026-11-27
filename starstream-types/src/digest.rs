use thiserror::Error;

/// The [multihash] code of sha2-256.
///
/// [multihash]: https://github.com/multiformats/multihash
const MULTIHASH_SHA2_256: u64 = 0x12;

#[derive(Debug, Error)]
pub enum DigestParseError {
    #[error(transparent)]
    Base(multibase::Error),
    #[error(transparent)]
    Hash(multihash::Error),
    #[error("unexpected multihash code `{0}`")]
    Code(u64),
    #[error("unexpected multihash size {0}")]
    Size(u8),
}

pub fn parse_digest(s: &str) -> Result<[u8; 32], DigestParseError> {
    let (_, buf) = multibase::decode(s).map_err(DigestParseError::Base)?;
    let mh = multihash::Multihash::<32>::from_bytes(&buf).map_err(DigestParseError::Hash)?;
    let (code, digest, size) = mh.into_inner();
    if code != MULTIHASH_SHA2_256 {
        return Err(DigestParseError::Code(code));
    }
    if size != 32 {
        return Err(DigestParseError::Size(size));
    }
    Ok(digest)
}

/// Encode a raw SHA-256 contract digest in the canonical ledger form: the
/// multibase base32-lower encoding of its sha2-256 multihash. The result is
/// always a 56-character lowercase alphanumeric string starting with `b` — a
/// valid component-model label and URL path segment.
pub fn encode_digest(digest: &[u8; 32]) -> String {
    let Ok(digest) = multihash::Multihash::<32>::wrap(MULTIHASH_SHA2_256, digest) else {
        unreachable!();
    };
    multibase::encode(multibase::Base::Base32Lower, digest.to_bytes())
}
