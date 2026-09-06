//! Starstream ledger client.

use anyhow::Context as _;
use coset::{TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signer as _, SigningKey, VerifyingKey};

use crate::{FUND_CONTEXT, PUBLISH_CONTEXT};

pub mod http;

/// Build a signed envelope.
fn build_envelope(key: SigningKey, payload: impl Into<Vec<u8>>) -> anyhow::Result<Vec<u8>> {
    let protected = coset::HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(key.verifying_key().to_bytes().into())
        .build();
    coset::CoseSign1Builder::new()
        .protected(protected)
        .payload(payload.into())
        .create_signature(b"", |data| key.sign(data).to_bytes().into())
        .build()
        .to_tagged_vec()
        .context("failed to serialize envelope")
}

/// Build and sign [FUND_CONTEXT] envelope.
pub fn build_fund_envelope(
    key: SigningKey,
    network: impl Into<String>,
    nonce: u64,
    account: &VerifyingKey,
    amount: u64,
) -> anyhow::Result<Vec<u8>> {
    let mut payload = Vec::default();
    ciborium::into_writer(
        &ciborium::Value::Array(vec![
            ciborium::Value::Text(FUND_CONTEXT.into()),
            ciborium::Value::Text(network.into()),
            ciborium::Value::Integer(nonce.into()),
            ciborium::Value::Bytes(account.to_bytes().into()),
            ciborium::Value::Integer(amount.into()),
        ]),
        &mut payload,
    )
    .context("failed to encode CBOR")?;
    build_envelope(key, payload)
}

/// Build and sign [PUBLISH_CONTEXT] envelope.
pub fn build_publish_envelope(
    key: SigningKey,
    network: impl Into<String>,
    nonce: u64,
    wasm: impl Into<Vec<u8>>,
) -> anyhow::Result<Vec<u8>> {
    let mut payload = Vec::default();
    ciborium::into_writer(
        &ciborium::Value::Array(vec![
            ciborium::Value::Text(PUBLISH_CONTEXT.into()),
            ciborium::Value::Text(network.into()),
            ciborium::Value::Integer(nonce.into()),
            ciborium::Value::Bytes(wasm.into()),
        ]),
        &mut payload,
    )
    .context("failed to encode CBOR")?;
    build_envelope(key, payload)
}
