//! Starstream ledger

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

pub mod cose;
pub mod runtime;
pub mod wrpc;

pub const FUND_CONTEXT: u8 = 0;
pub const PUBLISH_CONTEXT: u8 = 1;
pub const TRANSACTION_CONTEXT: u8 = 2;

/// COSE media type
pub const APPLICATION_COSE: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::COSE);

/// Wasm media type
pub const APPLICATION_WASM: MediaType =
    MediaType::new(mediatype::names::APPLICATION, mediatype::names::WASM);

/// wRPC media type
pub const APPLICATION_WRPC: MediaType = MediaType::new(
    mediatype::names::APPLICATION,
    mediatype::Name::new_unchecked("x.wrpc"),
);

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

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Message {
    Fund(Fund),
    Publish(Publish),
    Transaction(Transaction),
}

impl Message {
    pub const fn context(&self) -> u8 {
        match self {
            Self::Fund(..) => FUND_CONTEXT,
            Self::Publish(..) => PUBLISH_CONTEXT,
            Self::Transaction(..) => TRANSACTION_CONTEXT,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Envelope {
    pub network: Box<str>,
    pub message: Message,
}

impl<C> Encode<C> for Envelope {
    fn encode<W: minicbor::encode::Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        ctx: &mut C,
    ) -> Result<(), minicbor::encode::Error<W::Error>> {
        e.array(3)?;
        e.str(&self.network)?;
        e.u8(self.message.context())?;
        match &self.message {
            Message::Fund(pld) => pld.encode(e, ctx),
            Message::Publish(pld) => pld.encode(e, ctx),
            Message::Transaction(pld) => pld.encode(e, ctx),
        }
    }
}

impl<'b, C> Decode<'b, C> for Envelope {
    fn decode(d: &mut minicbor::Decoder<'b>, ctx: &mut C) -> Result<Self, minicbor::decode::Error> {
        let pos = d.position();
        let n = d.array()?;
        if n != Some(3) {
            return Err(
                minicbor::decode::Error::message("envelope must be a 3-element array").at(pos),
            );
        }
        let network = d.str()?;
        let pos = d.position();
        let context = d.u8()?;
        let message = match context {
            FUND_CONTEXT => {
                let pld = d.decode_with(ctx)?;
                Message::Fund(pld)
            }
            PUBLISH_CONTEXT => {
                let pld = d.decode_with(ctx)?;
                Message::Publish(pld)
            }
            TRANSACTION_CONTEXT => {
                let pld = d.decode_with(ctx)?;
                Message::Transaction(pld)
            }
            _ => return Err(minicbor::decode::Error::message("unknown envelope context").at(pos)),
        };
        Ok(Self {
            network: network.into(),
            message,
        })
    }
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
