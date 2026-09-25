//! Starstream ledger

use core::fmt;

use std::collections::BTreeSet;

use bytes::Bytes;
use mediatype::MediaType;
use minicbor::{Decode, Encode};
use serde::Serialize;
use thiserror::Error;

use crate::runtime::ModuleState;

#[cfg(feature = "client")]
pub mod client;
#[cfg(feature = "server")]
pub mod server;

pub mod runtime;
pub mod wrpc;

pub const FUND_CONTEXT: &str = "starstream:fund";
pub const PUBLISH_CONTEXT: &str = "starstream:publish";
pub const TRANSACTION_CONTEXT: &str = "starstream:transaction";

/// COSE media type
pub const APPLICATION_COSE: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::COSE);

/// Wasm media type
pub const APPLICATION_WASM: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::WASM);

/// CBOR media type
pub const APPLICATION_CBOR: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::CBOR);

/// The [multihash] code of sha2-256.
///
/// [multihash]: https://github.com/multiformats/multihash
const MULTIHASH_SHA2_256: u64 = 0x12;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct Fund {
    #[n(0)]
    pub nonce: u64,
    #[cbor(n(1), with = "minicbor::bytes")]
    #[serde(serialize_with = "serialize_bytes")]
    pub account: [u8; 32],
    #[n(2)]
    pub amount: u64,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct Publish {
    #[n(0)]
    pub nonce: u64,
    #[cbor(n(1), with = "minicbor::bytes")]
    #[serde(serialize_with = "serialize_wasm")]
    pub wasm: Box<[u8]>,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct TransactionEvent {
    #[n(0)]
    pub abi_name: Box<str>,
    #[n(1)]
    pub name: Box<str>,
    #[cbor(n(2), with = "minicbor::bytes")]
    #[serde(serialize_with = "serialize_bytes")]
    pub params: Box<[u8]>,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct TransactionInput {
    #[n(0)]
    pub transaction: Box<str>,
    #[n(1)]
    pub index: u32,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct TransactionOutput {
    #[n(0)]
    pub contract: Box<str>,
    #[n(1)]
    pub instance: Box<str>,
    #[n(2)]
    #[serde(serialize_with = "serialize_methods")]
    pub methods: BTreeSet<(u64, u64, u64, u64)>,
    #[cbor(n(3), with = "minicbor::bytes")]
    #[serde(serialize_with = "serialize_bytes")]
    pub storage: Box<[u8]>,
    #[n(4)]
    pub state: Vec<ModuleState>,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct Transaction {
    #[n(0)]
    pub inputs: Vec<TransactionInput>,
    #[n(1)]
    pub outputs: Vec<TransactionOutput>,
    #[n(2)]
    pub events: Vec<TransactionEvent>,
    #[cbor(n(3), with = "minicbor::bytes")]
    #[serde(serialize_with = "serialize_bytes")]
    pub proof: Box<[u8]>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Serialize)]
pub enum EnvelopeContext {
    #[serde(rename = "starstream:fund")]
    Fund,
    #[serde(rename = "starstream:publish")]
    Publish,
    #[serde(rename = "starstream:transaction")]
    Transaction,
}

impl EnvelopeContext {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Fund => FUND_CONTEXT,
            Self::Publish => PUBLISH_CONTEXT,
            Self::Transaction => TRANSACTION_CONTEXT,
        }
    }
}

impl fmt::Display for EnvelopeContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

fn serialize_bytes<S: serde::Serializer>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
    if serializer.is_human_readable() {
        serializer.serialize_str(&hex::encode(bytes))
    } else {
        serializer.serialize_bytes(bytes)
    }
}

fn serialize_methods<S: serde::Serializer>(
    methods: &BTreeSet<(u64, u64, u64, u64)>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    if !serializer.is_human_readable() {
        return methods.serialize(serializer);
    }
    serializer.collect_seq(methods.iter().map(|(a, b, c, d)| {
        let mut digest = [0; 32];
        digest[..8].copy_from_slice(&a.to_le_bytes());
        digest[8..16].copy_from_slice(&b.to_le_bytes());
        digest[16..24].copy_from_slice(&c.to_le_bytes());
        digest[24..].copy_from_slice(&d.to_le_bytes());
        hex::encode(digest)
    }))
}

fn serialize_wasm<S: serde::Serializer>(wasm: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
    if !serializer.is_human_readable() {
        return serializer.serialize_bytes(wasm);
    }
    let wat = wasmprinter::print_bytes(wasm).map_err(serde::ser::Error::custom)?;
    serializer.serialize_str(&wat)
}

impl<C> Encode<C> for EnvelopeContext {
    fn encode<W: minicbor::encode::Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        _: &mut C,
    ) -> Result<(), minicbor::encode::Error<W::Error>> {
        e.str(self.as_str())?;
        Ok(())
    }
}

impl<'b, C> Decode<'b, C> for EnvelopeContext {
    fn decode(d: &mut minicbor::Decoder<'b>, _: &mut C) -> Result<Self, minicbor::decode::Error> {
        let p = d.position();
        match d.str()? {
            FUND_CONTEXT => Ok(Self::Fund),
            PUBLISH_CONTEXT => Ok(Self::Publish),
            TRANSACTION_CONTEXT => Ok(Self::Transaction),
            _ => Err(minicbor::decode::Error::message("unknown envelope context").at(p)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, Serialize)]
pub struct Envelope<T> {
    #[n(0)]
    pub context: EnvelopeContext,
    #[n(1)]
    pub network: Box<str>,
    #[n(2)]
    pub payload: T,
}

pub enum Action {
    UploadContract(Bytes),
    FundAccount(Bytes),
    Transaction(Bytes),
}

pub struct Block {
    pub actions: Box<[Action]>,
}

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
