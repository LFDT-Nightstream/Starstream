use std::sync::Arc;

use ed25519_dalek::VerifyingKey;
use mediatype::MediaType;
use thiserror::Error;

use crate::{
    APPLICATION_COSE, DigestParseError, EnvelopeContext, FUND_CONTEXT, PUBLISH_CONTEXT,
    TRANSACTION_CONTEXT, encode_digest,
};

#[derive(Debug, Error)]
pub enum ContractGetError {
    #[error("failed to parse contract digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("contract not found")]
    ContractNotFound,
    #[error(transparent)]
    AcceptHeader(AcceptHeaderError),
    #[error(transparent)]
    Http(http::Error),
}

impl ContractGetError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..) => http::StatusCode::BAD_REQUEST,
            Self::ContractNotFound => http::StatusCode::NOT_FOUND,
            Self::AcceptHeader(err) => err.http_status_code(),
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum EnvelopeReadError {
    #[error(transparent)]
    ContentTypeToStr(http::header::ToStrError),
    #[error(transparent)]
    ContentTypeParsing(mediatype::MediaTypeError),
    #[error("expected `{APPLICATION_COSE}` content-type, got `{0}`")]
    UnsupportedContentType(Box<str>),
    #[error("missing content-type, expected `{APPLICATION_COSE}`")]
    ContentTypeMissing,
    #[error("body exceeds {0}-byte limit")]
    BodyTooLarge(u64),
    #[error(transparent)]
    Body(Box<dyn std::error::Error + Send + Sync>),
    #[error("body is not a valid COSE_Sign1: {0}")]
    CoseSign1Parsing(coset::CoseError),
    #[error("body is not a valid COSE_Sign: {0}")]
    CoseSignParsing(coset::CoseError),
    #[error("envelope must contain at least one signature")]
    SignatureMissing,
    #[error("failed to reencode envelope: {0}")]
    Reencode(coset::CoseError),
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

impl EnvelopeReadError {
    fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::ContentTypeToStr(..)
            | Self::ContentTypeParsing(..)
            | Self::Body(..)
            | Self::CoseSign1Parsing(..)
            | Self::CoseSignParsing(..)
            | Self::SignatureMissing
            | Self::UnprotectedHeader
            | Self::CriticalHeader
            | Self::Algorithm
            | Self::KeyIdFormat
            | Self::Key(..) => http::StatusCode::BAD_REQUEST,
            Self::BodyTooLarge(..) => http::StatusCode::PAYLOAD_TOO_LARGE,
            Self::UnsupportedContentType(..) | Self::ContentTypeMissing => {
                http::StatusCode::UNSUPPORTED_MEDIA_TYPE
            }
            Self::SignatureVerification(..) => http::StatusCode::UNAUTHORIZED,
            Self::Reencode(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum ContractPutError {
    #[error(transparent)]
    DigestParsing(DigestParseError),
    #[error(transparent)]
    Envelope(EnvelopeReadError),
    #[error("COSE_Sign1 payload missing")]
    PayloadMissing,
    #[error("COSE_Sign1 payload is not valid CBOR: {0}")]
    PayloadParsing(minicbor::decode::Error),
    #[error("unexpected context `{0}`, expected `{PUBLISH_CONTEXT}`")]
    Context(EnvelopeContext),
    #[error("unexpected network `{got}`, expected `{expected}`")]
    Network { got: Box<str>, expected: Arc<str> },
    #[error("digest mismatch, got: `{}`", encode_digest(.0))]
    DigestMismatch([u8; 32]),
    #[error("invalid Wasm: {0}")]
    Wasm(wasmparser::BinaryReaderError),
    #[error("account ID `{}` not found", hex::encode(.0))]
    AccountNotFound(VerifyingKey),
    #[error("nonce must be higher than {last_nonce}, got {nonce}")]
    NonceTooLow { last_nonce: u64, nonce: u64 },
    #[error("balance insufficient, required at least {required}, available {available}")]
    InsufficientBalance { required: u64, available: u64 },
    #[error(transparent)]
    Http(http::Error),
}

impl ContractPutError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..)
            | Self::PayloadMissing
            | Self::PayloadParsing(..)
            | Self::Context(..)
            | Self::Network { .. }
            | Self::DigestMismatch(..)
            | Self::Wasm(..) => http::StatusCode::BAD_REQUEST,
            Self::Envelope(err) => err.http_status_code(),
            Self::NonceTooLow { .. } => http::StatusCode::CONFLICT,
            Self::AccountNotFound(..) | Self::InsufficientBalance { .. } => {
                http::StatusCode::PAYMENT_REQUIRED
            }
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum AccountFundError {
    #[error(transparent)]
    Envelope(EnvelopeReadError),
    #[error("COSE_Sign1 payload missing")]
    PayloadMissing,
    #[error("COSE_Sign1 payload is not valid CBOR: {0}")]
    PayloadParsing(minicbor::decode::Error),
    #[error("unexpected context `{0}`, expected `{FUND_CONTEXT}`")]
    Context(EnvelopeContext),
    #[error("unexpected network `{got}`, expected `{expected}`")]
    Network { got: Box<str>, expected: Arc<str> },
    #[error("fund transaction account is not a valid Ed25519 public key: {0}")]
    Key(ed25519_dalek::SignatureError),
    #[error("fund transaction account is a weak Ed25519 public key")]
    WeakKey,
    #[error("signer `{}` is not the admin account", hex::encode(.0))]
    NotAdmin(VerifyingKey),
    #[error("nonce must be higher than {last_nonce}, got {nonce}")]
    NonceTooLow { last_nonce: u64, nonce: u64 },
    #[error(transparent)]
    Http(http::Error),
}

impl AccountFundError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::PayloadMissing
            | Self::PayloadParsing(..)
            | Self::Context(..)
            | Self::Network { .. }
            | Self::Key(..)
            | Self::WeakKey => http::StatusCode::BAD_REQUEST,
            Self::Envelope(err) => err.http_status_code(),
            Self::NotAdmin(..) => http::StatusCode::FORBIDDEN,
            Self::NonceTooLow { .. } => http::StatusCode::CONFLICT,
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum TransactionGetError {
    #[error("failed to parse transaction digest: {0}")]
    DigestParsing(DigestParseError),
    #[error("transaction not found")]
    TransactionNotFound,
    #[error(transparent)]
    Http(http::Error),
}

impl TransactionGetError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..) => http::StatusCode::BAD_REQUEST,
            Self::TransactionNotFound => http::StatusCode::NOT_FOUND,
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum GenesisGetError {
    #[error(transparent)]
    Http(http::Error),
}

impl GenesisGetError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum TransactionPutError {
    #[error(transparent)]
    DigestParsing(DigestParseError),
    #[error(transparent)]
    Envelope(EnvelopeReadError),
    #[error("COSE_Sign payload missing")]
    PayloadMissing,
    #[error("digest mismatch, got: `{}`", encode_digest(.0))]
    DigestMismatch([u8; 32]),
    #[error("COSE_Sign payload is not valid CBOR: {0}")]
    PayloadParsing(minicbor::decode::Error),
    #[error("unexpected context `{0}`, expected `{TRANSACTION_CONTEXT}`")]
    Context(EnvelopeContext),
    #[error("unexpected network `{got}`, expected `{expected}`")]
    Network { got: Box<str>, expected: Arc<str> },
    #[error("transaction must have at least one input")]
    InputsEmpty,
    #[error("duplicate input")]
    InputDuplicate,
    #[error("failed to parse input transaction digest `{0}`: {1}")]
    InputTransactionDigestParsing(Box<str>, DigestParseError),
    #[error("input not found")]
    InputNotFound,
    #[error("input not authorized")]
    InputUnauthorized,
    #[error("transaction input index does not fit in usize")]
    InputIndexOverflow,
    #[error(transparent)]
    Http(http::Error),
}

impl TransactionPutError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..)
            | Self::PayloadMissing
            | Self::DigestMismatch(..)
            | Self::PayloadParsing(..)
            | Self::Context(..)
            | Self::Network { .. }
            | Self::InputsEmpty
            | Self::InputDuplicate
            | Self::InputTransactionDigestParsing(..)
            | Self::InputIndexOverflow => http::StatusCode::BAD_REQUEST,
            Self::Envelope(err) => err.http_status_code(),
            Self::InputNotFound => http::StatusCode::NOT_FOUND,
            Self::InputUnauthorized => http::StatusCode::FORBIDDEN,
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

#[derive(Debug, Error)]
pub enum RpcPostError {
    #[error("failed to read wRPC invocation header: {0}")]
    Header(wrpc_transport::frame::HeaderReadError),
    #[error("instance `{0}` not found")]
    InstanceNotFound(String),
    #[error("function `{name}` not found in instance `{instance}`")]
    FunctionNotFound { instance: String, name: String },
    #[error("failed to parse transaction digest: {0}")]
    TransactionDigestParsing(DigestParseError),
    #[error("UTXO index does not fit in usize")]
    UtxoIndexOverflow,
    #[error("UTXO not found")]
    UtxoNotFound,
    #[error("UTXO is an instance of `{instance}`, not `{name}`")]
    UtxoInstanceMismatch { name: Box<str>, instance: Box<str> },
    #[error("failed to parse contract digest: {0}")]
    ContractDigestParsing(DigestParseError),
    #[error("contract not found")]
    ContractNotFound,
    #[error("failed to merge UTXO state into contract: {0:#}")]
    StateMerge(anyhow::Error),
    #[error("failed to decode UTXO storage: {0}")]
    StorageDecoding(std::io::Error),
    #[error("UTXO instance `{instance}` not found: {source:#}")]
    UtxoInstanceNotFound {
        instance: String,
        source: wasmtime::Error,
    },
    #[error("method `{name}` not found in UTXO instance `{instance}`: {source:#}")]
    UtxoMethodNotFound {
        instance: String,
        name: String,
        source: wasmtime::Error,
    },
    #[error("UTXO storage missing")]
    UtxoStorageMissing,
    #[error("failed to decode parameters: {0}")]
    ParameterDecoding(std::io::Error),
    #[error("runtime failed: {0:#}")]
    Runtime(wasmtime::Error),
    #[error("resource table error: {0}")]
    ResourceTable(wasmtime::component::ResourceTableError),
    #[error("failed to encode result: {0}")]
    ResultEncoding(std::io::Error),
    #[error("failed to encode call result: {0:#}")]
    CallResultEncoding(wasmtime::Error),
    #[error("failed to encode response frame: {0}")]
    FrameEncoding(std::io::Error),
    #[error(transparent)]
    Http(http::Error),
}

impl RpcPostError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::Header(..)
            | Self::TransactionDigestParsing(..)
            | Self::UtxoIndexOverflow
            | Self::ParameterDecoding(..)
            | Self::UtxoStorageMissing
            | Self::ResourceTable(..) => http::StatusCode::BAD_REQUEST,
            Self::InstanceNotFound(..)
            | Self::FunctionNotFound { .. }
            | Self::UtxoNotFound
            | Self::UtxoInstanceMismatch { .. }
            | Self::ContractNotFound
            | Self::UtxoInstanceNotFound { .. }
            | Self::UtxoMethodNotFound { .. } => http::StatusCode::NOT_FOUND,
            Self::ContractDigestParsing(..)
            | Self::StateMerge(..)
            | Self::StorageDecoding(..)
            | Self::Runtime(..)
            | Self::ResultEncoding(..)
            | Self::CallResultEncoding(..)
            | Self::FrameEncoding(..)
            | Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

fn format_media_types(available: &[MediaType<'_>]) -> String {
    available
        .iter()
        .map(|mt| format!("`{mt}`"))
        .collect::<Vec<_>>()
        .join(", ")
}

#[derive(Debug, Error)]
pub enum AcceptHeaderError {
    #[error("failed to decode `Accept` header: {0}")]
    Decoding(headers_core::Error),
    #[error("no acceptable media type, available: {}", format_media_types(.0))]
    NotAcceptable(&'static [MediaType<'static>]),
}

impl AcceptHeaderError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::Decoding(..) => http::StatusCode::BAD_REQUEST,
            Self::NotAcceptable(..) => http::StatusCode::NOT_ACCEPTABLE,
        }
    }
}
