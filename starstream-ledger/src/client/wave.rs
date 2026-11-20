//! WAVE encoding of coordination script arguments and UTXO method values.

use core::iter::zip;

use anyhow::{Context as _, bail, ensure};
use bytes::{Bytes, BytesMut};
use tokio::io::AsyncRead;
use tokio_util::codec::Encoder as _;
use wasm_wave::value::{FuncType, Value};
use wasm_wave::wasm::WasmFunc as _;
use wasmtime::component::{Type, Val, types};

use crate::TransactionInput;
use crate::client::CoordinationScriptArg;

/// Parse a digest given either as multibase multihash or as `sha256:HEX`.
pub fn parse_digest(s: &str) -> anyhow::Result<[u8; 32]> {
    if let Some(hex) = s.strip_prefix("sha256:") {
        let mut buf = [0u8; 32];
        hex::decode_to_slice(hex, &mut buf).context("sha256 digest hex is not valid")?;
        return Ok(buf);
    }
    let digest = crate::parse_digest(s).context("digest is not a valid multibase multihash")?;
    Ok(digest)
}

/// Parse a transaction output reference formatted as `[TRANSACTION]:INDEX`, with
/// `TRANSACTION` a digest accepted by [`parse_digest`], or empty for genesis.
pub fn parse_input(s: &str) -> anyhow::Result<TransactionInput> {
    let (transaction, index) = s
        .rsplit_once(':')
        .context("input must be formatted as `[TRANSACTION]:INDEX`")?;
    let index = index.parse().context("input index is not valid")?;
    if transaction.is_empty() {
        return Ok(TransactionInput::genesis(index));
    }
    let transaction = parse_digest(transaction).context("transaction digest is not valid")?;
    Ok(TransactionInput::new(transaction, index))
}

/// Parse the arguments of a coordination script of type `ty`, UTXO parameters
/// as references accepted by [`parse_input`] and all others WAVE-encoded.
pub fn parse_script_args(
    ty: &types::ComponentFunc,
    args: impl IntoIterator<Item = impl AsRef<str>>,
) -> anyhow::Result<Vec<CoordinationScriptArg>> {
    let mut args = args.into_iter();
    let mut params = Vec::with_capacity(ty.params().len());
    for (name, ty) in ty.params() {
        let v = args
            .next()
            .with_context(|| format!("missing value for parameter `{name}`"))?;
        let v = v.as_ref();
        match ty {
            Type::Own(..) | Type::Borrow(..) => {
                let input = parse_input(v).with_context(|| {
                    format!("failed to parse UTXO input for parameter `{name}`")
                })?;
                params.push(input.into());
            }
            ty => {
                let v = wasm_wave::from_str::<Val>(&ty, v)
                    .with_context(|| format!("failed to parse value for parameter `{name}`"))?;
                params.push(v.into());
            }
        }
    }
    ensure!(args.next().is_none(), "trailing arguments");
    Ok(params)
}

/// Get the type of the method `method` of a UTXO from `wit`, its WIT package
/// encoded as Wasm as served by the ledger, without the leading output
/// reference parameters.
pub fn utxo_method_type(wit: &[u8], method: &str) -> anyhow::Result<FuncType> {
    let wasm = wit_parser::decoding::decode(wit).context("failed to decode UTXO WIT")?;
    let wit_parser::decoding::DecodedWasm::WitPackage(resolve, pkg) = wasm else {
        bail!("UTXO WIT is not a WIT package")
    };
    let utxo = resolve.packages[pkg]
        .interfaces
        .get("utxo")
        .context("`utxo` interface not found")?;
    let ty = resolve.interfaces[*utxo]
        .functions
        .get(method)
        .with_context(|| format!("method `{method}` not found"))?;
    let ty = wit_parser::Function {
        params: ty.params.iter().skip(2).cloned().collect(),
        ..ty.clone()
    };
    wasm_wave::value::resolve_wit_func_type(&resolve, &ty).context("failed to resolve method type")
}

/// Encode WAVE-encoded `args` as wRPC-encoded parameters of a UTXO method of
/// type `ty`.
pub fn encode_utxo_args(
    ty: &FuncType,
    args: impl IntoIterator<Item = impl AsRef<str>>,
) -> anyhow::Result<Bytes> {
    let mut args = args.into_iter();
    let mut buf = BytesMut::default();
    for (name, ty) in zip(ty.param_names(), ty.params()) {
        let v = args
            .next()
            .with_context(|| format!("missing value for parameter `{name}`"))?;
        let v = wasm_wave::from_str::<Value>(&ty, v.as_ref())
            .with_context(|| format!("failed to parse value for parameter `{name}`"))?;
        wrpc_wave::ValueEncoder::new(&ty)
            .encode(&v, &mut buf)
            .with_context(|| format!("failed to encode value of parameter `{name}`"))?;
    }
    ensure!(args.next().is_none(), "trailing arguments");
    Ok(buf.freeze())
}

/// Read the wRPC-encoded results of a UTXO method of type `ty` from `rx` and
/// return them as a WAVE-encoded tuple.
pub async fn read_utxo_results(
    ty: &FuncType,
    rx: &mut (impl AsyncRead + Unpin),
) -> anyhow::Result<String> {
    let Some(ty) = wasm_wave::value::Type::tuple(ty.results().collect::<Box<_>>()) else {
        return Ok("()".into());
    };
    let v = wrpc_wave::read_value::<Value, _>(rx, &ty)
        .await
        .context("failed to read result tuple")?;
    wasm_wave::to_string(&v).context("failed to encode result tuple")
}
