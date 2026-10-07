//! COSE envelope verification.

use coset::{CoseSign, CoseSign1, TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signature, VerifyingKey};
use thiserror::Error;

use crate::Envelope;

#[derive(Debug, Error)]
pub enum EnvelopeReadError {
    #[error("failed to decode COSE tag: {0}")]
    TagDecoding(minicbor::decode::Error),
    #[error("unsupported COSE tag `{0}`")]
    UnsupportedTag(u64),
    #[error("body is not a valid COSE_Sign1: {0}")]
    CoseSign1Parsing(coset::CoseError),
    #[error("body is not a valid COSE_Sign: {0}")]
    CoseSignParsing(coset::CoseError),
    #[error("failed to reencode envelope: {0}")]
    Reencoding(coset::CoseError),
    #[error("envelope is not canonically encoded")]
    NonCanonical,
    #[error("envelope payload missing")]
    PayloadMissing,
    #[error("failed to decode envelope: {0}")]
    Decoding(minicbor::decode::Error),
    #[error("envelope must contain at least one signature")]
    SignatureMissing,
    #[error("envelope must not contain unprotected headers")]
    UnprotectedHeader,
    #[error("envelope must not contain critical headers")]
    CriticalHeader,
    #[error("protected `alg` header must be EdDSA")]
    Algorithm,
    #[error("protected `kid` header must be a raw 32-byte Ed25519 public key")]
    KeyIdFormat,
    #[error("`kid` is not a valid Ed25519 public key: {0}")]
    Key(ed25519_dalek::SignatureError),
    #[error("signature verification failed: {0}")]
    SignatureVerification(ed25519_dalek::SignatureError),
}

fn verify_headers(
    protected: &coset::ProtectedHeader,
    unprotected: &coset::Header,
) -> Result<VerifyingKey, EnvelopeReadError> {
    if !unprotected.is_empty() {
        return Err(EnvelopeReadError::UnprotectedHeader);
    }
    if !protected.header.crit.is_empty() {
        return Err(EnvelopeReadError::CriticalHeader);
    }
    if protected.header.alg != Some(coset::Algorithm::Assigned(iana::Algorithm::EdDSA)) {
        return Err(EnvelopeReadError::Algorithm);
    }
    let key = <[u8; 32]>::try_from(protected.header.key_id.as_slice())
        .map_err(|_| EnvelopeReadError::KeyIdFormat)?;
    VerifyingKey::from_bytes(&key).map_err(EnvelopeReadError::Key)
}

/// Verify a `COSE_Sign` or `COSE_Sign1` envelope and return its signers and decoded payload.
pub fn read_envelope(envelope: &[u8]) -> Result<(Vec<VerifyingKey>, Envelope), EnvelopeReadError> {
    let mut dec = minicbor::Decoder::new(envelope);
    let tag = dec.tag().map_err(EnvelopeReadError::TagDecoding)?;
    match tag.as_u64() {
        CoseSign::TAG => {
            let sign = CoseSign::from_tagged_slice(envelope)
                .map_err(EnvelopeReadError::CoseSignParsing)?;
            if !sign.unprotected.is_empty() {
                return Err(EnvelopeReadError::UnprotectedHeader);
            }
            if !sign.protected.header.crit.is_empty() {
                return Err(EnvelopeReadError::CriticalHeader);
            }
            if sign.signatures.is_empty() {
                return Err(EnvelopeReadError::SignatureMissing);
            }
            let mut keys = Vec::with_capacity(sign.signatures.len());
            for (i, signature) in sign.signatures.iter().enumerate() {
                let key = verify_headers(&signature.protected, &signature.unprotected)?;
                sign.verify_signature(i, b"", |sig, data| {
                    Signature::from_slice(sig).and_then(|sig| key.verify_strict(data, &sig))
                })
                .map_err(EnvelopeReadError::SignatureVerification)?;
                keys.push(key);
            }
            let Some(payload) = sign.payload.as_deref() else {
                return Err(EnvelopeReadError::PayloadMissing);
            };
            let payload = minicbor::decode(payload).map_err(EnvelopeReadError::Decoding)?;
            let canonical = sign
                .to_tagged_vec()
                .map_err(EnvelopeReadError::Reencoding)?;
            if canonical != envelope {
                return Err(EnvelopeReadError::NonCanonical);
            }
            Ok((keys, payload))
        }
        CoseSign1::TAG => {
            let sign1 = CoseSign1::from_tagged_slice(envelope)
                .map_err(EnvelopeReadError::CoseSign1Parsing)?;
            let key = verify_headers(&sign1.protected, &sign1.unprotected)?;
            sign1
                .verify_signature(b"", |sig, data| {
                    Signature::from_slice(sig).and_then(|sig| key.verify_strict(data, &sig))
                })
                .map_err(EnvelopeReadError::SignatureVerification)?;
            let Some(payload) = sign1.payload.as_deref() else {
                return Err(EnvelopeReadError::PayloadMissing);
            };
            let payload = minicbor::decode(payload).map_err(EnvelopeReadError::Decoding)?;
            let canonical = sign1
                .to_tagged_vec()
                .map_err(EnvelopeReadError::Reencoding)?;
            if canonical != envelope {
                return Err(EnvelopeReadError::NonCanonical);
            }
            Ok((vec![key], payload))
        }
        tag => Err(EnvelopeReadError::UnsupportedTag(tag)),
    }
}
