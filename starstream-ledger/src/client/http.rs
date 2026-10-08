use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{Context as _, bail, ensure};
use bytes::{Bytes, BytesMut};
use ed25519_dalek::{SigningKey, VerifyingKey};
use http::header::{ACCEPT, CONTENT_TYPE};
use http::{Method, Request, Uri};
use http_body_util::{BodyExt as _, Full};
use hyper_util::client::legacy::connect::Connect;
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use starstream_runtime::{CoordinationScriptExport, Utxo};
use tokio_util::codec::Encoder as _;
use tracing::{instrument, warn};
use wasm_tokio::cm::OptionEncoder;
use wasm_tokio::{CoreNameEncoder, Leb128Encoder};
use wasmtime::Store;
use wasmtime::component::Val;
use wasmtime_wizer::Wizer;
use wrpc_transport::Invoke as _;

use crate::client::runtime::{CompiledContract, Contract, Ctx, UtxoCtx, call_coordination_script};
use crate::client::{
    CoordinationScriptArg, build_fund_envelope, build_publish_envelope, build_transaction_envelope,
    runtime,
};
use crate::cose::read_envelope;
use crate::wrpc::{LEDGER_UTXO_INSTANCE, bindings};
use crate::{
    APPLICATION_COSE, APPLICATION_WRPC, Fund, Message, Publish, Transaction, TransactionInput,
    TransactionOutput, encode_digest, parse_digest,
};

/// Default network used by the client
pub const DEFAULT_NETWORK: &str = "dev";

fn endpoint_uri(base: &Uri, endpoint: impl AsRef<str>) -> anyhow::Result<String> {
    ensure!(
        base.query().is_none(),
        "base URL `{base}` must not contain a query"
    );
    let endpoint = endpoint.as_ref();
    let base = base.to_string();
    let base = base.trim_end_matches('/');
    Ok(format!("{base}/{endpoint}"))
}

fn wrpc_context(base: &Uri) -> anyhow::Result<http::request::Parts> {
    let uri = endpoint_uri(base, "rpc")?;
    let req = Request::builder()
        .uri(uri)
        .header(CONTENT_TYPE, APPLICATION_WRPC.to_string())
        .body(())
        .context("failed to build request")?;
    let (cx, ()) = req.into_parts();
    Ok(cx)
}

fn build_envelope_request(
    base: &Uri,
    envelope: Vec<u8>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let uri = endpoint_uri(base, "rpc")?;
    Request::builder()
        .method(Method::POST)
        .uri(uri)
        .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
        .body(Full::new(Bytes::from(envelope)))
        .context("failed to build request")
}

/// Build a signed fund request.
pub fn build_fund_request(
    base: &Uri,
    key: SigningKey,
    network: impl Into<Box<str>>,
    payload: Fund,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let envelope = build_fund_envelope(key, network, payload)?;
    build_envelope_request(base, envelope)
}

/// Build a signed contract publish request.
pub fn build_contract_publish_request(
    base: &Uri,
    key: SigningKey,
    network: impl Into<Box<str>>,
    payload: Publish,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let envelope = build_publish_envelope(key, network, payload)?;
    build_envelope_request(base, envelope)
}

/// Build a signed transaction request.
pub fn build_transaction_request(
    base: &Uri,
    key: SigningKey,
    network: impl Into<Box<str>>,
    tx: Transaction,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let envelope = build_transaction_envelope(key, network, tx)?;
    build_envelope_request(base, envelope)
}

/// Build a contract get request.
pub fn build_contract_get_request(
    base: &Uri,
    digest: &[u8; 32],
    accept: Option<MediaType>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let digest = encode_digest(digest);
    let uri = endpoint_uri(base, format!("contracts/{digest}"))?;
    let req = Request::builder().method(Method::GET).uri(uri);
    let req = if let Some(accept) = accept {
        req.header(ACCEPT, accept.to_string())
    } else {
        req
    };
    req.body(Full::default()).context("failed to build request")
}

/// Build a transaction get request.
pub fn build_transaction_get_request(
    base: &Uri,
    digest: &[u8; 32],
    accept: Option<MediaType>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let digest = encode_digest(digest);
    let uri = endpoint_uri(base, format!("transactions/{digest}"))?;
    let req = Request::builder().method(Method::GET).uri(uri);
    let req = if let Some(accept) = accept {
        req.header(ACCEPT, accept.to_string())
    } else {
        req
    };
    req.body(Full::default()).context("failed to build request")
}

#[derive(Clone)]
pub struct ClientBuilder<C> {
    http: hyper_util::client::legacy::Builder,
    connect: C,
    api_base: Uri,
    network: Box<str>,
    engine: wasmtime::Engine,
    wizer: Wizer,
}

impl<C> ClientBuilder<C> {
    pub fn new(
        http: hyper_util::client::legacy::Builder,
        connect: C,
        api_base: impl Into<Uri>,
    ) -> Self {
        let mut engine_config = wasmtime::Config::new();
        engine_config.wasm_component_model_implements(true);
        let engine = wasmtime::Engine::new(&engine_config).unwrap();
        Self {
            http,
            connect,
            api_base: api_base.into(),
            network: DEFAULT_NETWORK.into(),
            engine,
            wizer: Wizer::new(),
        }
    }

    pub fn network(mut self, network: impl Into<Box<str>>) -> Self {
        self.network = network.into();
        self
    }

    pub fn engine(mut self, engine: wasmtime::Engine) -> Self {
        self.engine = engine;
        self
    }

    pub fn wizer(mut self, wizer: Wizer) -> Self {
        self.wizer = wizer;
        self
    }

    pub fn build(self) -> Client<C>
    where
        C: Connect + Clone + Send + Sync + 'static,
    {
        self.into()
    }
}

pub struct Client<C> {
    wrpc: wrpc_http::Client<hyper_util::client::legacy::Client<C, wrpc_http::OutgoingBody>>,
    http: hyper_util::client::legacy::Client<C, Full<Bytes>>,
    api_base: Uri,
    network: Box<str>,
    engine: wasmtime::Engine,
    wizer: Wizer,
}

impl<C> From<ClientBuilder<C>> for Client<C>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    fn from(
        ClientBuilder {
            http,
            connect,
            api_base,
            network,
            engine,
            wizer,
        }: ClientBuilder<C>,
    ) -> Self {
        let wrpc = wrpc_http::Client::new(http.build(connect.clone()));
        let http = http.build(connect.clone());
        Self {
            wrpc,
            http,
            api_base,
            network,
            engine,
            wizer,
        }
    }
}

impl<C> runtime::Client for Client<C>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        Client::get_contract(self, digest).await
    }

    /// Get the UTXO referenced by the input.
    async fn get_input_utxo(
        &self,
        TransactionInput { transaction, index }: &TransactionInput,
    ) -> anyhow::Result<TransactionOutput> {
        let index = usize::try_from(*index).context("index does not fit in usize")?;
        if transaction.is_empty() {
            self.get_genesis_utxo(index).await
        } else {
            let transaction = parse_digest(transaction).with_context(|| {
                format!("failed to parse `{transaction}` as multibase multihash")
            })?;
            self.get_transaction_utxo(transaction, index).await
        }
    }
}

impl<C> Client<C>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    pub fn new(
        http: hyper_util::client::legacy::Builder,
        connect: C,
        api_base: impl Into<Uri>,
    ) -> Self {
        ClientBuilder::new(http, connect, api_base).build()
    }

    pub fn engine(&self) -> &wasmtime::Engine {
        &self.engine
    }

    pub fn wizer(&self) -> &Wizer {
        &self.wizer
    }

    /// Get the height of the latest ledger block.
    #[instrument(skip_all)]
    pub async fn block_height(&self) -> anyhow::Result<u64> {
        let cx = wrpc_context(&self.api_base)?;
        bindings::starstream::ledger::block::get_height(&self.wrpc, cx).await
    }

    /// Get the transaction UTXO referenced by `digest` and `idx`.
    #[instrument(skip_all)]
    pub async fn get_transaction_utxo(
        &self,
        digest: [u8; 32],
        idx: usize,
    ) -> anyhow::Result<TransactionOutput> {
        let Transaction { outputs, .. } = self.get_transaction(digest).await?;
        outputs.into_iter().nth(idx).context("output not found")
    }

    /// Get the genesis UTXO referenced by `idx`.
    #[instrument(skip_all)]
    pub async fn get_genesis_utxo(&self, idx: usize) -> anyhow::Result<TransactionOutput> {
        let outputs = self.get_genesis().await?;
        outputs.into_iter().nth(idx).context("output not found")
    }

    /// Call the coordination script `export` exported by the contract `contract` with `args`,
    /// loading UTXO arguments from the ledger.
    /// Contracts in `imports` are used to resolve imports instead of the ledger.
    /// `wasm` must be the original component bytes `contract` was compiled from.
    #[instrument(skip_all)]
    #[allow(clippy::too_many_arguments)]
    pub async fn call_coordination_script(
        &self,
        store: &mut Store<Ctx>,
        contract: &CompiledContract,
        wasm: &[u8],
        export: &CoordinationScriptExport,
        imports: &mut HashMap<[u8; 32], Contract>,
        args: Vec<CoordinationScriptArg>,
        results: &mut [Val],
        utxos: &mut Vec<Utxo<Arc<std::sync::Mutex<UtxoCtx>>>>,
    ) -> anyhow::Result<Transaction> {
        let (tx, ..) = call_coordination_script(
            store,
            self,
            &self.wizer,
            contract,
            wasm,
            export,
            imports,
            args,
            results,
            utxos,
        )
        .await?;
        Ok(tx)
    }

    /// Call the method `name` exported by the UTXO
    /// created by the transaction output `input` with encoded `args`.
    #[instrument(skip_all)]
    pub async fn call_utxo_method(
        &self,
        TransactionInput { transaction, index }: &TransactionInput,
        name: &str,
        args: &[u8],
    ) -> anyhow::Result<wrpc_transport::frame::Incoming> {
        let cx = wrpc_context(&self.api_base)?;
        let mut params = BytesMut::with_capacity(1 + 5 + transaction.len() + 5 + args.len());
        let transaction = if transaction.is_empty() {
            None
        } else {
            Some(transaction)
        };
        OptionEncoder(CoreNameEncoder)
            .encode(transaction, &mut params)
            .context("failed to encode transaction digest")?;
        Leb128Encoder
            .encode(*index, &mut params)
            .context("failed to encode output index")?;
        params.extend_from_slice(args);
        let (tx, rx) = self
            .wrpc
            .invoke(cx, LEDGER_UTXO_INSTANCE, name, params.freeze(), [[]])
            .await?;
        drop(tx);
        Ok(rx)
    }

    #[instrument(skip_all)]
    async fn request(
        &self,
        req: http::Request<Full<Bytes>>,
    ) -> anyhow::Result<(http::response::Parts, Bytes)> {
        let res = self
            .http
            .request(req)
            .await
            .context("failed to send request")?;
        let (parts, body) = res.into_parts();
        let body = body
            .collect()
            .await
            .context("failed to receive response body")?;
        Ok((parts, body.to_bytes()))
    }

    #[instrument(skip_all)]
    pub async fn fund(
        &self,
        key: SigningKey,
        nonce: u64,
        account: &VerifyingKey,
        amount: u64,
    ) -> anyhow::Result<()> {
        let req = build_fund_request(
            &self.api_base,
            key,
            self.network.as_ref(),
            Fund {
                nonce,
                account: account.to_bytes(),
                amount,
            },
        )?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        if !body.is_empty() {
            warn!("received unexpected body: {body}")
        }
        Ok(())
    }

    /// Publish the contract `wasm` and return its digest.
    #[instrument(skip_all)]
    pub async fn publish_contract(
        &self,
        key: SigningKey,
        nonce: u64,
        wasm: impl Into<Box<[u8]>>,
    ) -> anyhow::Result<[u8; 32]> {
        let req = build_contract_publish_request(
            &self.api_base,
            key,
            self.network.as_ref(),
            Publish {
                nonce,
                wasm: wasm.into(),
            },
        )?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        parse_digest(&body).context("failed to parse contract digest")
    }

    /// Submit the transaction `tx` and return its digest.
    #[instrument(skip_all)]
    pub async fn transact(&self, key: SigningKey, tx: Transaction) -> anyhow::Result<[u8; 32]> {
        let req = build_transaction_request(&self.api_base, key, self.network.as_ref(), tx)?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        parse_digest(&body).context("failed to parse transaction digest")
    }

    /// Get the transaction identified by `digest`.
    #[instrument(skip_all)]
    pub async fn get_transaction(&self, digest: [u8; 32]) -> anyhow::Result<Transaction> {
        let cx = wrpc_context(&self.api_base)?;
        let tx = bindings::starstream::ledger::transaction::get_transaction(
            &self.wrpc,
            cx,
            &encode_digest(&digest),
        )
        .await?;
        let tx = tx.context("failed to get transaction")?;
        let tx = Transaction::from(tx);
        let tx_cbor = minicbor::to_vec(&tx).context("failed to encode transaction")?;
        let tx_digest: [u8; 32] = Sha256::digest(&tx_cbor).into();
        ensure!(
            tx_digest == digest,
            "transaction digest mismatch, got `{}`",
            encode_digest(&tx_digest)
        );
        Ok(tx)
    }

    /// Get the genesis outputs.
    #[instrument(skip_all)]
    pub async fn get_genesis(&self) -> anyhow::Result<Vec<TransactionOutput>> {
        let cx = wrpc_context(&self.api_base)?;
        let outputs = bindings::starstream::ledger::genesis::get_outputs(&self.wrpc, cx).await?;
        Ok(outputs.into_iter().map(Into::into).collect())
    }

    /// Get the Wasm bytes of the contract identified by `digest`.
    #[instrument(skip_all)]
    pub async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        let cx = wrpc_context(&self.api_base)?;
        let wasm = bindings::starstream::ledger::contract::get_wasm(
            &self.wrpc,
            cx,
            &encode_digest(&digest),
        )
        .await?;
        let wasm = wasm.context("failed to get contract Wasm")?;
        let wasm_digest: [u8; 32] = Sha256::digest(&wasm).into();
        ensure!(
            wasm_digest == digest,
            "contract digest mismatch, got `{}`",
            encode_digest(&wasm_digest)
        );
        Ok(wasm)
    }

    #[instrument(skip_all)]
    pub async fn get_contract_envelope(&self, digest: &[u8; 32]) -> anyhow::Result<Bytes> {
        let cx = wrpc_context(&self.api_base)?;
        let body = bindings::starstream::ledger::contract::get_envelope(
            &self.wrpc,
            cx,
            &encode_digest(digest),
        )
        .await?;
        let body = body.context("failed to get contract envelope")?;
        let (_, envelope) = read_envelope(&body).context("invalid envelope")?;
        ensure!(
            envelope.network == self.network,
            "unexpected network `{}`, expected `{}`",
            envelope.network,
            self.network
        );
        let Message::Publish(publish) = envelope.message else {
            bail!("unexpected context `{}`", envelope.message.context());
        };
        let wasm_digest: [u8; 32] = Sha256::digest(&publish.wasm).into();
        ensure!(
            wasm_digest == *digest,
            "contract digest mismatch, got `{}`",
            encode_digest(&wasm_digest)
        );
        Ok(body)
    }
}
