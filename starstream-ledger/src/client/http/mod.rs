use core::task::{Context, Poll};

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{Context as _, ensure};
use bytes::Bytes;
use ed25519_dalek::{SigningKey, VerifyingKey};
use futures::future::BoxFuture;
use http::header::{ACCEPT, CONTENT_TYPE};
use http::{Method, Request, Uri};
use http_body_util::{BodyExt as _, Full};
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use starstream_runtime::{CoordinationScriptExport, Utxo};
use tracing::{instrument, warn};
use wasmtime::Store;
use wasmtime::component::Val;
use wasmtime_wizer::Wizer;
use wrpc::http::{OutgoingParams, Service};
use wrpc::{Codec, Invoke, RawCodec};

use crate::client::runtime::{CompiledContract, Contract, Ctx, UtxoCtx, call_coordination_script};
use crate::client::{
    CoordinationScriptArg, build_fund_envelope, build_publish_envelope, build_transaction_envelope,
    runtime,
};
use crate::cose::read_envelope;
use crate::wrpc::{LEDGER_UTXO_INSTANCE, bindings};
use crate::{
    APPLICATION_COSE, Fund, PUBLISH_CONTEXT, Publish, TRANSACTION_CONTEXT, Transaction,
    TransactionInput, TransactionOutput, encode_digest, parse_digest,
};

#[cfg(feature = "tcp")]
pub mod tcp;

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

fn wrpc_client<T>(
    base: &Uri,
    endpoint: impl AsRef<str>,
    service: T,
) -> anyhow::Result<wrpc::http::Client<T>> {
    let uri = endpoint_uri(base, endpoint)?;
    let uri = Uri::try_from(uri).context("failed to parse wRPC endpoint URI")?;
    Ok(wrpc::http::Client::new(uri, service))
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

fn build_get_request(
    uri: String,
    accept: Option<MediaType>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let req = Request::builder().method(Method::GET).uri(uri);
    let req = if let Some(accept) = accept {
        req.header(ACCEPT, accept.to_string())
    } else {
        req
    };
    req.body(Full::default()).context("failed to build request")
}

/// Build a contract get request.
pub fn build_contract_get_request(
    base: &Uri,
    digest: &[u8; 32],
    accept: Option<MediaType>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let digest = encode_digest(digest);
    let uri = endpoint_uri(base, format!("contracts/{digest}"))?;
    build_get_request(uri, accept)
}

/// Build a transaction get request.
pub fn build_transaction_get_request(
    base: &Uri,
    digest: &[u8; 32],
    accept: Option<MediaType>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let digest = encode_digest(digest);
    let uri = endpoint_uri(base, format!("transactions/{digest}"))?;
    build_get_request(uri, accept)
}

fn utxo_endpoint(TransactionInput { transaction, index }: &TransactionInput) -> String {
    let mut query = form_urlencoded::Serializer::new(String::default());
    if !transaction.is_empty() {
        query.append_pair("transaction", transaction);
    }
    query.append_pair("index", &index.to_string());
    let query = query.finish();
    format!("rpc?{query}")
}

/// Build a request for the WIT of the UTXO created by the transaction output `input`.
pub fn build_utxo_wit_request(
    base: &Uri,
    input: &TransactionInput,
    accept: Option<MediaType>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let uri = endpoint_uri(base, utxo_endpoint(input))?;
    build_get_request(uri, accept)
}

fn verify_envelope(
    body: Result<Bytes, bindings::starstream::ledger::types::GetError>,
    digest: &[u8; 32],
    context: u8,
) -> anyhow::Result<Option<Bytes>> {
    let body = match body {
        Ok(body) => body,
        Err(bindings::starstream::ledger::types::GetError::NotFound) => return Ok(None),
        Err(err) => return Err(err.into()),
    };
    let (_, envelope) = read_envelope(&body).context("invalid envelope")?;
    ensure!(
        envelope.message.context() == context,
        "unexpected context `{}`, expected `{context}`",
        envelope.message.context()
    );
    let message_digest = envelope
        .message
        .payload_digest()
        .context("failed to compute payload digest")?;
    ensure!(
        message_digest == *digest,
        "digest mismatch, got `{}`",
        encode_digest(&message_digest)
    );
    Ok(Some(body))
}

/// HTTP transport of the ledger client.
pub trait Transport: Send + Sync {
    /// [`Service`] sending wRPC requests over this transport.
    type Wrpc;

    fn wrpc(&self) -> Self::Wrpc;

    /// Send `req` and return the response with its body collected.
    fn request(
        &self,
        req: http::Request<Full<Bytes>>,
    ) -> impl Future<Output = anyhow::Result<(http::response::Parts, Bytes)>> + Send;
}

/// [`Service`] sending wRPC requests through a [`Transport`], which buffers their parameters,
/// failing on responses with an unsuccessful status.
#[derive(Clone)]
pub struct TransportService<T>(pub T);

impl<T> Service<http::Request<OutgoingParams>> for TransportService<T>
where
    T: Transport + Clone + 'static,
{
    type Response = http::Response<Full<Bytes>>;
    type Error = anyhow::Error;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: http::Request<OutgoingParams>) -> Self::Future {
        let http = self.0.clone();
        Box::pin(async move {
            let (parts, body) = req.into_parts();
            let body = body
                .collect()
                .await
                .context("failed to encode parameters")?;
            let req = http::Request::from_parts(parts, Full::new(body.to_bytes()));
            let (parts, body) = http.request(req).await?;
            ensure!(
                parts.status.is_success(),
                "HTTP request failed with status `{}`: {}",
                parts.status,
                String::from_utf8_lossy(&body)
            );
            Ok(http::Response::from_parts(parts, Full::new(body)))
        })
    }
}

#[derive(Clone)]
pub struct ClientBuilder<T> {
    http: T,
    api_base: Uri,
    engine: wasmtime::Engine,
    wizer: Wizer,
}

impl<T> ClientBuilder<T> {
    /// `engine` must be configured by [`crate::configure_wasmtime`].
    pub fn new(http: T, api_base: impl Into<Uri>, engine: wasmtime::Engine) -> Self {
        Self {
            http,
            api_base: api_base.into(),
            engine,
            wizer: Wizer::new(),
        }
    }

    pub fn wizer(mut self, wizer: Wizer) -> Self {
        self.wizer = wizer;
        self
    }

    pub fn build(self) -> Client<T> {
        self.into()
    }
}

pub struct Client<T> {
    http: T,
    api_base: Uri,
    engine: wasmtime::Engine,
    wizer: Wizer,
}

impl<T> From<ClientBuilder<T>> for Client<T> {
    fn from(
        ClientBuilder {
            http,
            api_base,
            engine,
            wizer,
        }: ClientBuilder<T>,
    ) -> Self {
        Self {
            http,
            api_base,
            engine,
            wizer,
        }
    }
}

impl<T> runtime::Client for Client<T>
where
    T: Transport,
    wrpc::http::Client<T::Wrpc>: Invoke,
{
    async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Bytes> {
        let wasm = Client::get_contract(self, digest).await?;
        wasm.context("contract not found")
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

impl<T> Client<T>
where
    T: Transport,
    wrpc::http::Client<T::Wrpc>: Invoke,
{
    /// `engine` must be configured by [`crate::configure_wasmtime`].
    pub fn new(http: T, api_base: impl Into<Uri>, engine: wasmtime::Engine) -> Self {
        ClientBuilder::new(http, api_base, engine).build()
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
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        bindings::starstream::ledger::block::get_height(clt)
            .await
            .map_err(anyhow::Error::from)
    }

    /// Get the envelopes of the block at `height`, in order.
    #[instrument(skip_all)]
    pub async fn get_block(&self, height: u64) -> anyhow::Result<Option<Vec<Bytes>>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let envelopes = bindings::starstream::ledger::block::get_block(clt, height).await?;
        let Some(envelopes) = envelopes else {
            return Ok(None);
        };
        for envelope in &envelopes {
            read_envelope(envelope).context("invalid envelope")?;
        }
        Ok(Some(envelopes))
    }

    /// Get the name of the network the ledger accepts envelopes for.
    #[instrument(skip_all)]
    pub async fn get_network(&self) -> anyhow::Result<String> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        bindings::starstream::ledger::network::get_name(clt)
            .await
            .map_err(anyhow::Error::from)
    }

    /// Return `network`, defaulting to the name of the network the ledger accepts envelopes for.
    pub async fn resolve_network(&self, network: Option<String>) -> anyhow::Result<String> {
        match network {
            Some(network) => Ok(network),
            None => self.get_network().await,
        }
    }

    /// Get the last nonce used by the admin key to fund accounts.
    #[instrument(skip_all)]
    pub async fn get_admin_nonce(&self) -> anyhow::Result<u64> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        bindings::starstream::ledger::admin::get_nonce(clt)
            .await
            .map_err(anyhow::Error::from)
    }

    /// Get the nonce following the last one used by the admin key to fund accounts.
    #[instrument(skip_all)]
    pub async fn next_admin_nonce(&self) -> anyhow::Result<u64> {
        let nonce = self.get_admin_nonce().await?;
        nonce.checked_add(1).context("admin nonce exhausted")
    }

    /// Get the balance of the account `key`.
    #[instrument(skip_all)]
    pub async fn get_account_balance(&self, key: &VerifyingKey) -> anyhow::Result<Option<u64>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let key = Bytes::copy_from_slice(key.as_bytes());
        let balance = bindings::starstream::ledger::account::get_balance(clt, &key).await?;
        match balance {
            Ok(balance) => Ok(Some(balance)),
            Err(bindings::starstream::ledger::types::GetError::NotFound) => Ok(None),
            Err(err) => Err(err).context("failed to get account balance"),
        }
    }

    /// Get the last nonce used by the account `key` to publish contracts or sign transactions.
    #[instrument(skip_all)]
    pub async fn get_account_nonce(&self, key: &VerifyingKey) -> anyhow::Result<Option<u64>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let key = Bytes::copy_from_slice(key.as_bytes());
        let nonce = bindings::starstream::ledger::account::get_nonce(clt, &key).await?;
        match nonce {
            Ok(nonce) => Ok(Some(nonce)),
            Err(bindings::starstream::ledger::types::GetError::NotFound) => Ok(None),
            Err(err) => Err(err).context("failed to get account nonce"),
        }
    }

    /// Get the nonce following the last one used by the account `key` to publish contracts or
    /// sign transactions.
    #[instrument(skip_all)]
    pub async fn next_account_nonce(&self, key: &VerifyingKey) -> anyhow::Result<u64> {
        let nonce = self.get_account_nonce(key).await?;
        let nonce = nonce
            .with_context(|| format!("account `{}` not found", hex::encode(key.as_bytes())))?;
        nonce.checked_add(1).context("account nonce exhausted")
    }

    /// Get the transaction UTXO referenced by `digest` and `idx`.
    #[instrument(skip_all)]
    pub async fn get_transaction_utxo(
        &self,
        digest: [u8; 32],
        idx: usize,
    ) -> anyhow::Result<TransactionOutput> {
        let tx = self.get_transaction(digest).await?;
        let Transaction { outputs, .. } = tx.context("transaction not found")?;
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
        prover: Option<&mut crate::client::runtime::TransactionProver>,
    ) -> anyhow::Result<Transaction> {
        let (tx, execution) = call_coordination_script(
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
            prover,
        )
        .await?;
        if let Err(err) = execution {
            warn!(?err, "failed to check execution trace");
        }
        Ok(tx)
    }

    /// Call the method `name` exported by the UTXO
    /// created by the transaction output `input` with encoded `args`,
    /// returning the encoded results.
    #[instrument(skip_all)]
    pub async fn call_utxo_method(
        &self,
        input: &TransactionInput,
        name: &str,
        args: &[u8],
    ) -> anyhow::Result<Bytes> {
        let clt = wrpc_client(&self.api_base, utxo_endpoint(input), self.http.wrpc())?;
        let (results, io) = Invoke::invoke_with(
            clt,
            Codec {
                enc: RawCodec,
                dec: RawCodec,
            },
            LEDGER_UTXO_INSTANCE,
            name,
            args,
        )
        .await?;
        io.await?;
        Ok(results)
    }

    /// Get the WIT of the UTXO created by the transaction output `input`,
    /// describing its methods as `call_utxo_method` invokes them,
    /// as WIT text or, if `accept` is `application/wasm`, encoded as Wasm.
    #[instrument(skip_all)]
    pub async fn get_utxo_wit(
        &self,
        input: &TransactionInput,
        accept: Option<MediaType<'_>>,
    ) -> anyhow::Result<Bytes> {
        let req = build_utxo_wit_request(&self.api_base, input, accept)?;
        let (http::response::Parts { status, .. }, body) = self.http.request(req).await?;
        ensure!(status.is_success(), "{}", String::from_utf8_lossy(&body));
        Ok(body)
    }

    #[instrument(skip_all)]
    pub async fn fund(
        &self,
        key: SigningKey,
        network: &str,
        nonce: u64,
        account: &VerifyingKey,
        amount: u64,
    ) -> anyhow::Result<()> {
        let req = build_fund_request(
            &self.api_base,
            key,
            network,
            Fund {
                nonce,
                account: account.to_bytes(),
                amount,
            },
        )?;
        let (http::response::Parts { status, .. }, body) = self.http.request(req).await?;
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
        network: &str,
        nonce: u64,
        wasm: impl Into<Box<[u8]>>,
    ) -> anyhow::Result<[u8; 32]> {
        let req = build_contract_publish_request(
            &self.api_base,
            key,
            network,
            Publish {
                nonce,
                wasm: wasm.into(),
            },
        )?;
        let (http::response::Parts { status, .. }, body) = self.http.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        parse_digest(&body).context("failed to parse contract digest")
    }

    /// Submit the transaction `tx` and return its digest.
    #[instrument(skip_all)]
    pub async fn transact(
        &self,
        key: SigningKey,
        network: &str,
        tx: Transaction,
    ) -> anyhow::Result<[u8; 32]> {
        let req = build_transaction_request(&self.api_base, key, network, tx)?;
        let (http::response::Parts { status, .. }, body) = self.http.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        parse_digest(&body).context("failed to parse transaction digest")
    }

    /// Get the transaction identified by `digest`.
    #[instrument(skip_all)]
    pub async fn get_transaction(&self, digest: [u8; 32]) -> anyhow::Result<Option<Transaction>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let tx = bindings::starstream::ledger::transaction::get_transaction(
            clt,
            &encode_digest(&digest),
        )
        .await?;
        let tx = match tx {
            Ok(tx) => tx,
            Err(bindings::starstream::ledger::types::GetError::NotFound) => return Ok(None),
            Err(err) => return Err(err).context("failed to get transaction"),
        };
        let tx = Transaction::from(tx);
        let tx_digest = tx.digest().context("failed to encode transaction")?;
        ensure!(
            tx_digest == digest,
            "transaction digest mismatch, got `{}`",
            encode_digest(&tx_digest)
        );
        Ok(Some(tx))
    }

    /// Get the envelope of the transaction identified by `digest`.
    #[instrument(skip_all)]
    pub async fn get_transaction_envelope(
        &self,
        digest: &[u8; 32],
    ) -> anyhow::Result<Option<Bytes>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let body =
            bindings::starstream::ledger::transaction::get_envelope(clt, &encode_digest(digest))
                .await?;
        verify_envelope(body, digest, TRANSACTION_CONTEXT)
            .context("failed to get transaction envelope")
    }

    /// Get the genesis outputs.
    #[instrument(skip_all)]
    pub async fn get_genesis(&self) -> anyhow::Result<Vec<TransactionOutput>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let outputs = bindings::starstream::ledger::genesis::get_outputs(clt).await?;
        Ok(outputs.into_iter().map(Into::into).collect())
    }

    /// Get the Wasm bytes of the contract identified by `digest`.
    #[instrument(skip_all)]
    pub async fn get_contract(&self, digest: [u8; 32]) -> anyhow::Result<Option<Bytes>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let wasm =
            bindings::starstream::ledger::contract::get_wasm(clt, &encode_digest(&digest)).await?;
        let wasm = match wasm {
            Ok(wasm) => wasm,
            Err(bindings::starstream::ledger::types::GetError::NotFound) => return Ok(None),
            Err(err) => return Err(err).context("failed to get contract Wasm"),
        };
        let wasm_digest: [u8; 32] = Sha256::digest(&wasm).into();
        ensure!(
            wasm_digest == digest,
            "contract digest mismatch, got `{}`",
            encode_digest(&wasm_digest)
        );
        Ok(Some(wasm))
    }

    #[instrument(skip_all)]
    pub async fn get_contract_envelope(&self, digest: &[u8; 32]) -> anyhow::Result<Option<Bytes>> {
        let clt = wrpc_client(&self.api_base, "rpc", self.http.wrpc())?;
        let body =
            bindings::starstream::ledger::contract::get_envelope(clt, &encode_digest(digest))
                .await?;
        verify_envelope(body, digest, PUBLISH_CONTEXT).context("failed to get contract envelope")
    }
}
