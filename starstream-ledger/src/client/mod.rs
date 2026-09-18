//! Starstream ledger client.

use anyhow::Context as _;
use coset::{
    CoseSign1Builder, CoseSignBuilder, CoseSignatureBuilder, TaggedCborSerializable as _, iana,
};
use ed25519_dalek::{Signer as _, SigningKey};
use wasmtime::component::Val;

use crate::wrpc::UTXO_PACKAGE;
use crate::{
    Envelope, EnvelopeContext, Fund, Publish, Transaction, TransactionInput, encode_digest,
};

pub mod http;
pub mod runtime;

/// Ledger wRPC bindings.
pub mod bindings {
    wit_bindgen_wrpc::generate!();
}

/// The wRPC instance name of the UTXO identified by `digest`.
fn utxo_instance(digest: &[u8; 32]) -> String {
    format!("{UTXO_PACKAGE}/{}", encode_digest(digest))
}

/// Build a signed `COSE_Sign` envelope.
fn build_sign_envelope(key: SigningKey, payload: impl Into<Vec<u8>>) -> anyhow::Result<Vec<u8>> {
    let protected = coset::HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(key.verifying_key().to_bytes().into())
        .build();
    let sig = CoseSignatureBuilder::new().protected(protected).build();
    CoseSignBuilder::new()
        .payload(payload.into())
        .add_created_signature(sig, b"", |data| key.sign(data).to_bytes().into())
        .build()
        .to_tagged_vec()
        .context("failed to serialize envelope")
}

/// Build a signed `COSE_Sign1` envelope.
fn build_sign1_envelope(key: SigningKey, payload: impl Into<Vec<u8>>) -> anyhow::Result<Vec<u8>> {
    let protected = coset::HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(key.verifying_key().to_bytes().into())
        .build();
    CoseSign1Builder::new()
        .protected(protected)
        .payload(payload.into())
        .create_signature(b"", |data| key.sign(data).to_bytes().into())
        .build()
        .to_tagged_vec()
        .context("failed to serialize envelope")
}

/// Argument of a coordination script call.
#[derive(Debug, Clone)]
pub enum CoordinationScriptArg {
    /// UTXO loaded from the referenced transaction output.
    Utxo(TransactionInput),
    /// Plain value.
    Val(Val),
}

impl From<TransactionInput> for CoordinationScriptArg {
    fn from(input: TransactionInput) -> Self {
        Self::Utxo(input)
    }
}

impl From<Val> for CoordinationScriptArg {
    fn from(v: Val) -> Self {
        Self::Val(v)
    }
}

/// Build and sign [FUND_CONTEXT] envelope.
pub fn build_fund_envelope(
    key: SigningKey,
    network: impl Into<Box<str>>,
    payload: Fund,
) -> anyhow::Result<Vec<u8>> {
    let payload = minicbor::to_vec(Envelope {
        context: EnvelopeContext::Fund,
        network: network.into(),
        payload,
    })
    .context("failed to encode CBOR")?;
    build_sign1_envelope(key, payload)
}

/// Build and sign [PUBLISH_CONTEXT] envelope.
pub fn build_publish_envelope(
    key: SigningKey,
    network: impl Into<Box<str>>,
    payload: Publish,
) -> anyhow::Result<Vec<u8>> {
    let payload = minicbor::to_vec(Envelope {
        context: EnvelopeContext::Publish,
        network: network.into(),
        payload,
    })
    .context("failed to encode CBOR")?;
    build_sign1_envelope(key, payload)
}

/// Encode a [TRANSACTION_CONTEXT] transaction payload.
pub fn encode_transaction(
    network: impl Into<Box<str>>,
    tx: Transaction,
) -> anyhow::Result<Vec<u8>> {
    minicbor::to_vec(Envelope {
        context: EnvelopeContext::Transaction,
        network: network.into(),
        payload: tx,
    })
    .context("failed to encode CBOR")
}

/// Build and sign [TRANSACTION_CONTEXT] envelope.
pub fn build_transaction_envelope(
    key: SigningKey,
    network: impl Into<Box<str>>,
    tx: Transaction,
) -> anyhow::Result<Vec<u8>> {
    let payload = encode_transaction(network, tx)?;
    build_sign_envelope(key, payload)
}
