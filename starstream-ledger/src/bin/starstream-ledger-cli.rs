//! Client CLI for the `starstream-ledger` HTTP API specified in
//! `docs/ledger.md`: publish contracts and invoke coordination scripts
//! and persisted-UTXO methods; invoked without a function name, the
//! target's ABI is printed as WIT instead.
//!
//! Invocation parameters are given in WAVE (WebAssembly Value Encoding)
//! and results are printed back as WAVE. The parameter and result types
//! come from the WIT the ledger itself serves for the invocation target
//! (`GET /contracts/<digest>/rpc` and `GET /utxos/<digest>/rpc`); the
//! invocation itself is `POST`ed to `/rpc`, addressed by the
//! `starstream:contract/<digest>` or `starstream:utxo/<digest>` wRPC
//! instance — the very interface ID the served WIT declares.

use core::iter::zip;

use std::io::stderr;
use std::path::PathBuf;

use anyhow::{Context as _, ensure};
use bytes::{Bytes, BytesMut};
use clap::{Parser, Subcommand};
use coset::{TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signer as _, SigningKey};
use http::header::CONTENT_TYPE;
use http::{Method, Request, StatusCode};
use http_body_util::{BodyExt as _, Full};
use hyper_util::client::legacy::Client;
use hyper_util::client::legacy::connect::HttpConnector;
use hyper_util::rt::TokioExecutor;
use sha2::{Digest as _, Sha256};
use starstream_ledger::codec::{ValEncoder, read_value};
use starstream_ledger::{
    PUBLISH_CONTEXT, X_STARSTREAM_BLOCK, X_STARSTREAM_TRANSACTION, X_STARSTREAM_UTXO,
    encode_digest, parse_digest,
};
use tokio_util::codec::{Decoder as _, Encoder as _};
use tracing::info;
use wasmtime::component::{Component, Type, Val, types};

#[derive(Debug, Parser)]
#[command(version, about)]
struct Args {
    /// Base URL of the ledger HTTP API.
    #[arg(
        long,
        global = true,
        value_name = "URL",
        default_value = "http://[::1]:9000"
    )]
    url: String,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Sign and publish a contract, printing its digest.
    Publish {
        /// Hex-encoded 32-byte Ed25519 signing key.
        #[arg(long, value_parser = parse_key)]
        key: SigningKey,

        /// Network identifier the publish transaction is bound to.
        #[arg(long, default_value = "dev")]
        network: String,

        /// Publish transaction nonce; must be strictly greater than the
        /// last nonce the account published with.
        #[arg(long)]
        nonce: u64,

        /// Path to the contract Wasm.
        wasm: PathBuf,
    },
    /// Invoke a coordination script of a published contract.
    Script {
        /// UTXO import mapping (`<instance>=<contract-digest>`), one per
        /// UTXO import of the contract, repeatable.
        #[arg(long = "utxo", value_name = "INSTANCE=DIGEST")]
        utxos: Vec<String>,

        /// Digest of the published contract.
        contract: String,

        /// Coordination script name; when omitted, the contract's
        /// script ABI is printed as WIT instead.
        name: Option<String>,

        /// WAVE-encoded arguments, one per parameter.
        args: Vec<String>,
    },
    /// Invoke a method on a persisted UTXO.
    Method {
        /// Digest of the persisted UTXO, as reported by `script`.
        utxo: String,

        /// Method name as served in the UTXO WIT (kebab-case); when
        /// omitted, the UTXO's ABI is printed as WIT instead.
        name: Option<String>,

        /// WAVE-encoded arguments, one per parameter.
        args: Vec<String>,
    },
}

fn parse_key(s: &str) -> Result<SigningKey, String> {
    let mut buf = [0u8; 32];
    hex::decode_to_slice(s, &mut buf)
        .map_err(|err| format!("key is not a valid hex-encoded 32 bytes: {err}"))?;
    Ok(SigningKey::from_bytes(&buf))
}

type HttpClient = Client<HttpConnector, Full<Bytes>>;

async fn send(
    client: &HttpClient,
    req: Request<Full<Bytes>>,
) -> anyhow::Result<http::Response<Bytes>> {
    let res = client
        .request(req)
        .await
        .context("failed to send HTTP request")?;
    let (parts, body) = res.into_parts();
    let body = body
        .collect()
        .await
        .context("failed to read HTTP response body")?
        .to_bytes();
    Ok(http::Response::from_parts(parts, body))
}

/// Fetch the WIT served at `rpc`.
async fn fetch_wit(client: &HttpClient, rpc: &str) -> anyhow::Result<String> {
    let req = Request::builder().uri(rpc).body(Full::new(Bytes::new()))?;
    let res = send(client, req).await?;
    ensure!(
        res.status() == StatusCode::OK,
        "failed to fetch WIT from `{rpc}`: {} {}",
        res.status(),
        String::from_utf8_lossy(res.body()),
    );
    String::from_utf8(res.into_body().into()).context("served WIT is not valid utf-8")
}

/// Recover each function of the `iface` interface of the served `wit`
/// along with its type, in served order: a world importing the interface
/// is appended, a dummy component implementing it is synthesized and
/// compiled, and the types are read off the resulting instance import.
fn parse_funcs(
    engine: &wasmtime::Engine,
    wit: &str,
    iface: &str,
) -> anyhow::Result<Vec<(String, types::ComponentFunc)>> {
    let mut resolve = wit_parser::Resolve::new();
    let pkg = resolve
        .push_str(
            "served.wit",
            &format!("{wit}world client {{ import {iface}; }}\n"),
        )
        .context("failed to parse served WIT")?;
    let world = resolve
        .select_world(&[pkg], None)
        .context("failed to select the served WIT world")?;
    let mut module =
        wit_component::dummy_module(&resolve, world, wit_parser::ManglingAndAbi::Standard32);
    wit_component::embed_component_metadata(
        &mut module,
        &resolve,
        world,
        wit_component::StringEncoding::UTF8,
    )
    .context("failed to embed WIT metadata in the dummy module")?;
    let wasm = wit_component::ComponentEncoder::default()
        .module(&module)
        .context("failed to set the dummy component module")?
        .encode()
        .context("failed to encode the dummy component")?;
    let component = Component::from_binary(engine, &wasm)
        .map_err(anyhow::Error::from)
        .context("failed to compile the dummy component")?;
    Ok(component
        .component_type()
        .imports(engine)
        .filter_map(|(_, types::ComponentExtern { ty, .. })| match ty {
            types::ComponentItem::ComponentInstance(instance) => Some(
                instance
                    .exports(engine)
                    .filter_map(|(name, types::ComponentExtern { ty, .. })| match ty {
                        types::ComponentItem::ComponentFunc(ty) => Some((name.to_string(), ty)),
                        _ => None,
                    })
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        })
        .flatten()
        .collect())
}

/// Parse one WAVE-encoded argument per parameter type.
fn parse_params(tys: &[Type], args: &[String]) -> anyhow::Result<Vec<Val>> {
    ensure!(
        tys.len() == args.len(),
        "expected {} arguments, got {}",
        tys.len(),
        args.len(),
    );
    zip(tys, args)
        .map(|(ty, arg)| {
            Val::from_wave(ty, arg)
                .map_err(anyhow::Error::from)
                .with_context(|| format!("failed to parse argument `{arg}`"))
        })
        .collect()
}

/// The wRPC invocation request body: the invocation header naming `func`
/// on `instance`, followed by the encoded parameters.
fn encode_invocation(
    instance: &str,
    func: &str,
    params: &[Val],
    tys: &[Type],
) -> anyhow::Result<Bytes> {
    let mut data = BytesMut::new();
    for (v, ty) in zip(params, tys) {
        ValEncoder::new(ty)
            .encode(v, &mut data)
            .map_err(anyhow::Error::from)
            .context("failed to encode parameter")?;
    }
    let mut buf = BytesMut::new();
    wrpc_transport::frame::encode_invocation(&mut buf, instance, func, &data)
        .context("failed to encode invocation")?;
    Ok(buf.freeze())
}

/// Decode the wRPC-framed results carried by an invocation response body
/// and print each as WAVE.
async fn print_results(body: Bytes, tys: impl IntoIterator<Item = Type>) -> anyhow::Result<()> {
    let mut src = BytesMut::from(&body[..]);
    let mut decoder = wrpc_transport::FrameDecoder::default();
    let mut data = BytesMut::new();
    while let Some(wrpc_transport::Frame { path, data: frame }) = decoder
        .decode_eof(&mut src)
        .context("failed to decode result frame")?
    {
        ensure!(path.is_empty(), "async values not supported");
        data.extend_from_slice(&frame);
    }
    let mut r: &[u8] = &data;
    for ty in tys {
        let mut v = Val::Bool(false);
        read_value(&mut r, &mut v, &ty)
            .await
            .context("failed to read result value")?;
        let v = v
            .to_wave()
            .map_err(anyhow::Error::from)
            .context("failed to encode result as WAVE")?;
        println!("{v}");
    }
    Ok(())
}

async fn publish(
    client: &HttpClient,
    url: &str,
    key: SigningKey,
    network: String,
    nonce: u64,
    wasm: PathBuf,
) -> anyhow::Result<()> {
    let wasm =
        std::fs::read(&wasm).with_context(|| format!("failed to read `{}`", wasm.display()))?;
    let digest = encode_digest(&Sha256::digest(&wasm).into());
    let mut payload = Vec::new();
    ciborium::into_writer(
        &ciborium::Value::Array(vec![
            ciborium::Value::Text(PUBLISH_CONTEXT.into()),
            ciborium::Value::Text(network),
            ciborium::Value::Integer(nonce.into()),
            ciborium::Value::Bytes(wasm),
        ]),
        &mut payload,
    )
    .context("failed to encode publish transaction")?;
    let protected = coset::HeaderBuilder::new()
        .algorithm(iana::Algorithm::EdDSA)
        .key_id(key.verifying_key().to_bytes().to_vec())
        .build();
    let envelope = coset::CoseSign1Builder::new()
        .protected(protected)
        .payload(payload)
        .create_signature(b"", |data| key.sign(data).to_bytes().to_vec())
        .build()
        .to_tagged_vec()
        .map_err(|err| anyhow::anyhow!("failed to encode COSE_Sign1 envelope: {err}"))?;
    let req = Request::builder()
        .method(Method::PUT)
        .uri(format!("{url}/contracts/{digest}"))
        .header(CONTENT_TYPE, "application/cose")
        .body(Full::new(envelope.into()))?;
    let res = send(client, req).await?;
    ensure!(
        res.status() == StatusCode::OK,
        "publish failed: {} {}",
        res.status(),
        String::from_utf8_lossy(res.body()),
    );
    println!("{digest}");
    Ok(())
}

async fn script(
    client: &HttpClient,
    url: &str,
    utxos: Vec<String>,
    contract: String,
    name: Option<String>,
    args: Vec<String>,
) -> anyhow::Result<()> {
    let contract = parse_digest(&contract)
        .map(|digest| encode_digest(&digest))
        .map_err(|err| anyhow::anyhow!("invalid contract digest: {err}"))?;
    let wit = fetch_wit(client, &format!("{url}/contracts/{contract}/rpc")).await?;
    let Some(name) = name else {
        print!("{wit}");
        return Ok(());
    };
    let engine = wasmtime::Engine::default();
    let funcs = parse_funcs(&engine, &wit, &contract)?;
    let (_, func) = funcs
        .iter()
        .find(|(func, ..)| *func == name)
        .with_context(|| format!("coordination script `{name}` not found"))?;
    let tys: Vec<Type> = func.params().map(|(_, ty)| ty).collect();
    let params = parse_params(&tys, &args)?;
    let body = encode_invocation(
        &format!("starstream:contract/{contract}"),
        &name,
        &params,
        &tys,
    )?;
    let mut req = Request::builder()
        .method(Method::POST)
        .uri(format!("{url}/rpc"));
    for utxo in utxos {
        req = req.header(X_STARSTREAM_UTXO, utxo);
    }
    let res = send(client, req.body(Full::new(body))?).await?;
    ensure!(
        res.status() == StatusCode::OK,
        "invocation failed: {} {}",
        res.status(),
        String::from_utf8_lossy(res.body()),
    );
    for (index, utxo) in res.headers().get_all(X_STARSTREAM_UTXO).iter().enumerate() {
        info!(index, utxo = %String::from_utf8_lossy(utxo.as_bytes()), "UTXO persisted");
    }
    if let (Some(transaction), Some(block)) = (
        res.headers().get(X_STARSTREAM_TRANSACTION),
        res.headers().get(X_STARSTREAM_BLOCK),
    ) {
        info!(
            transaction = %String::from_utf8_lossy(transaction.as_bytes()),
            block = %String::from_utf8_lossy(block.as_bytes()),
            "transaction recorded",
        );
    }
    print_results(res.into_body(), func.results()).await
}

async fn method(
    client: &HttpClient,
    url: &str,
    utxo: String,
    name: Option<String>,
    args: Vec<String>,
) -> anyhow::Result<()> {
    let utxo = parse_digest(&utxo)
        .map(|digest| encode_digest(&digest))
        .map_err(|err| anyhow::anyhow!("invalid UTXO digest: {err}"))?;
    let wit = fetch_wit(client, &format!("{url}/utxos/{utxo}/rpc")).await?;
    let Some(name) = name else {
        print!("{wit}");
        return Ok(());
    };
    let engine = wasmtime::Engine::default();
    let funcs = parse_funcs(&engine, &wit, &utxo)?;
    let (_, func) = funcs
        .iter()
        .find(|(func, ..)| *func == name)
        .with_context(|| format!("method `{name}` not found"))?;
    // The implicit `self` receiver is absent from the served WIT; the
    // ledger supplies it.
    let tys: Vec<Type> = func.params().map(|(_, ty)| ty).collect();
    let params = parse_params(&tys, &args)?;
    let body = encode_invocation(&format!("starstream:utxo/{utxo}"), &name, &params, &tys)?;
    let req = Request::builder()
        .method(Method::POST)
        .uri(format!("{url}/rpc"))
        .body(Full::new(body))?;
    let res = send(client, req).await?;
    ensure!(
        res.status() == StatusCode::OK,
        "invocation failed: {} {}",
        res.status(),
        String::from_utf8_lossy(res.body()),
    );
    print_results(res.into_body(), func.results()).await
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let Args { url, command } = Args::parse();

    tracing_subscriber::fmt()
        .with_writer(stderr)
        .with_env_filter(
            tracing_subscriber::EnvFilter::builder()
                .with_default_directive(tracing::level_filters::LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .init();

    let url = url.trim_end_matches('/');
    let client = Client::builder(TokioExecutor::new()).build_http();
    match command {
        Command::Publish {
            key,
            network,
            nonce,
            wasm,
        } => publish(&client, url, key, network, nonce, wasm).await,
        Command::Script {
            utxos,
            contract,
            name,
            args,
        } => script(&client, url, utxos, contract, name, args).await,
        Command::Method { utxo, name, args } => method(&client, url, utxo, name, args).await,
    }
}
