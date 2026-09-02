use std::sync::Arc;

use ed25519_dalek::VerifyingKey;
use mediatype::MediaType;
use thiserror::Error;

use crate::server::http::APPLICATION_COSE;
use crate::{DigestParseError, FUND_CONTEXT, PUBLISH_CONTEXT, encode_digest};

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
    #[error("failed to reencode envelope: {0}")]
    Reencode(coset::CoseError),
    #[error("envelope must not contain unprotected headers")]
    UnprotectedHeader,
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
            | Self::UnprotectedHeader
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
    PayloadParsing(ciborium::de::Error<std::io::Error>),
    #[error("publish transaction must be a `[context, network, nonce, wasm]` array")]
    TransactionFormat,
    #[error("unexpected context `{0}`, expected `{PUBLISH_CONTEXT}`")]
    Context(Box<str>),
    #[error("unexpected network `{got}`, expected `{expected}`")]
    Network { got: Box<str>, expected: Arc<str> },
    #[error("publish transaction nonce does not fit in u64")]
    NonceOverflow,
    #[error("digest mismatch, got: `{}`", encode_digest(.0))]
    DigestMismatch([u8; 32]),
    #[error("account ID `{}` not found", hex::encode(.0))]
    AccountNotFound(VerifyingKey),
    #[error("nonce must be higher than {last_nonce}, got {nonce}")]
    NonceTooLow { last_nonce: u64, nonce: u64 },
    #[error("balance insufficient, required at least {required}, available {available}")]
    InsufficientBalance { required: u64, available: u64 },
    #[error("{0:#}")]
    Runtime(wasmtime::Error),
    #[error(transparent)]
    Http(http::Error),
    #[error("instrumentation failed: {0:#}")]
    Wizer(wasmtime::Error),
}

impl ContractPutError {
    pub fn http_status_code(&self) -> http::StatusCode {
        match self {
            Self::DigestParsing(..)
            | Self::PayloadMissing
            | Self::PayloadParsing(..)
            | Self::TransactionFormat
            | Self::Context(..)
            | Self::Network { .. }
            | Self::NonceOverflow
            | Self::DigestMismatch(..)
            | Self::Runtime(..)
            | Self::Wizer(..) => http::StatusCode::BAD_REQUEST,
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
    PayloadParsing(ciborium::de::Error<std::io::Error>),
    #[error("fund transaction must be a `[context, network, nonce, account, amount]` array")]
    TransactionFormat,
    #[error("unexpected context `{0}`, expected `{FUND_CONTEXT}`")]
    Context(Box<str>),
    #[error("unexpected network `{got}`, expected `{expected}`")]
    Network { got: Box<str>, expected: Arc<str> },
    #[error("fund transaction nonce does not fit in u64")]
    NonceOverflow,
    #[error("fund transaction account must be a raw 32-byte Ed25519 public key")]
    KeyIdFormat,
    #[error("fund transaction account is not a valid Ed25519 public key: {0}")]
    Key(ed25519_dalek::SignatureError),
    #[error("fund transaction account is a weak Ed25519 public key")]
    WeakKey,
    #[error("fund transaction amount does not fit in u64")]
    AmountOverflow,
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
            | Self::TransactionFormat
            | Self::Context(..)
            | Self::Network { .. }
            | Self::NonceOverflow
            | Self::KeyIdFormat
            | Self::Key(..)
            | Self::WeakKey
            | Self::AmountOverflow => http::StatusCode::BAD_REQUEST,
            Self::Envelope(err) => err.http_status_code(),
            Self::NotAdmin(..) => http::StatusCode::FORBIDDEN,
            Self::NonceTooLow { .. } => http::StatusCode::CONFLICT,
            Self::Http(..) => http::StatusCode::INTERNAL_SERVER_ERROR,
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
