use anyhow::{Context as _, bail, ensure};
use bytes::Bytes;
use coset::{CborSerializable as _, CoseSign1, TaggedCborSerializable as _, iana};
use ed25519_dalek::{Signature, SigningKey, VerifyingKey};
use http::header::{ACCEPT, CONTENT_TYPE};
use http::{Method, Request, Uri};
use http_body_util::{BodyExt as _, Full};
use mediatype::MediaType;
use sha2::{Digest as _, Sha256};
use tracing::{instrument, warn};

use crate::client::{build_fund_envelope, build_publish_envelope};
use crate::{APPLICATION_COSE, APPLICATION_WASM, PUBLISH_CONTEXT, encode_digest};

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

/// Build a signed fund request.
pub fn build_fund_request(
    base: &Uri,
    key: SigningKey,
    network: impl Into<String>,
    nonce: u64,
    account: &VerifyingKey,
    amount: u64,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let envelope = build_fund_envelope(key, network, nonce, account, amount)?;
    let uri = endpoint_uri(base, "fund")?;
    Request::builder()
        .method(Method::POST)
        .uri(uri)
        .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
        .body(Full::new(Bytes::from(envelope)))
        .context("failed to build request")
}

/// Build a signed contract publish request.
pub fn build_contract_publish_request(
    base: &Uri,
    key: SigningKey,
    network: impl Into<String>,
    nonce: u64,
    wasm: impl Into<Vec<u8>>,
) -> anyhow::Result<http::Request<Full<Bytes>>> {
    let wasm = wasm.into();
    let digest = Sha256::digest(&wasm);
    let digest = encode_digest(&digest.into());
    let envelope = build_publish_envelope(key, network, nonce, wasm)?;
    let uri = endpoint_uri(base, format!("contracts/{digest}"))?;
    Request::builder()
        .method(Method::PUT)
        .uri(uri)
        .header(CONTENT_TYPE, APPLICATION_COSE.to_string())
        .body(Full::new(Bytes::from(envelope)))
        .context("failed to build request")
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

#[derive(Clone, Debug)]
pub struct ClientBuilder<C> {
    http: hyper_util::client::legacy::Client<C, Full<Bytes>>,
    api_base: Uri,
    network: Box<str>,
}

impl<C> ClientBuilder<C> {
    pub fn new(
        http: hyper_util::client::legacy::Client<C, Full<Bytes>>,
        api_base: impl Into<Uri>,
    ) -> Self {
        Self {
            http,
            api_base: api_base.into(),
            network: DEFAULT_NETWORK.into(),
        }
    }

    pub fn network(mut self, network: impl Into<Box<str>>) -> Self {
        self.network = network.into();
        self
    }

    pub fn build(self) -> Client<C> {
        self.into()
    }
}

pub struct Client<C> {
    http: hyper_util::client::legacy::Client<C, Full<Bytes>>,
    api_base: Uri,
    network: Box<str>,
}

impl<C> From<ClientBuilder<C>> for Client<C> {
    fn from(
        ClientBuilder {
            http,
            api_base,
            network,
        }: ClientBuilder<C>,
    ) -> Self {
        Self {
            http,
            api_base,
            network,
        }
    }
}

impl<C> Client<C> {
    pub fn new(
        http: hyper_util::client::legacy::Client<C, Full<Bytes>>,
        api_base: impl Into<Uri>,
    ) -> Self {
        ClientBuilder::new(http, api_base).build()
    }
}

impl<C> Client<C>
where
    C: hyper_util::client::legacy::connect::Connect + Clone + Send + Sync + 'static,
{
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
            nonce,
            account,
            amount,
        )?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        if !body.is_empty() {
            warn!("received unexpected body: {body}")
        }
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn publish_contract(
        &self,
        key: SigningKey,
        nonce: u64,
        wasm: impl Into<Vec<u8>>,
    ) -> anyhow::Result<()> {
        let req = build_contract_publish_request(
            &self.api_base,
            key,
            self.network.as_ref(),
            nonce,
            wasm,
        )?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        let body = String::from_utf8_lossy(&body);
        ensure!(status.is_success(), "{body}");
        if !body.is_empty() {
            warn!("received unexpected body: {body}")
        }
        Ok(())
    }

    #[instrument(skip_all)]
    pub async fn get_contract_wasm(&self, digest: &[u8; 32]) -> anyhow::Result<Bytes> {
        let req = build_contract_get_request(&self.api_base, digest, Some(APPLICATION_WASM))?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        ensure!(status.is_success(), "{}", String::from_utf8_lossy(&body));
        let wasm_digest: [u8; 32] = Sha256::digest(&body).into();
        ensure!(
            wasm_digest == *digest,
            "contract digest mismatch, got `{}`",
            encode_digest(&wasm_digest)
        );
        Ok(body)
    }

    #[instrument(skip_all)]
    pub async fn get_contract_envelope(&self, digest: &[u8; 32]) -> anyhow::Result<Bytes> {
        let req = build_contract_get_request(&self.api_base, digest, Some(APPLICATION_COSE))?;
        let (http::response::Parts { status, .. }, body) = self.request(req).await?;
        ensure!(status.is_success(), "{}", String::from_utf8_lossy(&body));
        let sign1 = CoseSign1::from_tagged_slice(&body)
            .or_else(|_| CoseSign1::from_slice(&body))
            .context("invalid COSE_Sign1")?;
        ensure!(
            sign1.protected.header.alg == Some(coset::Algorithm::Assigned(iana::Algorithm::EdDSA)),
            "unsupported signature algorithm"
        );
        let key = <[u8; 32]>::try_from(sign1.protected.header.key_id.as_slice())
            .context("invalid `kid` header")?;
        let key = VerifyingKey::from_bytes(&key).context("invalid Ed25519 key")?;
        sign1
            .verify_signature(b"", |signature, data| {
                Signature::from_slice(signature)
                    .and_then(|signature| key.verify_strict(data, &signature))
            })
            .context("signature verification failed")?;
        let payload = sign1.payload.as_deref().context("payload missing")?;
        let ciborium::Value::Array(payload) =
            ciborium::from_reader(payload).context("invalid payload")?
        else {
            bail!("invalid publish transaction");
        };
        let Ok(
            [
                ciborium::Value::Text(context),
                _,
                _,
                ciborium::Value::Bytes(wasm),
            ],
        ) = <[_; _]>::try_from(payload)
        else {
            bail!("invalid publish transaction");
        };
        ensure!(context == PUBLISH_CONTEXT, "unexpected context `{context}`");
        let wasm_digest: [u8; 32] = Sha256::digest(&wasm).into();
        ensure!(
            wasm_digest == *digest,
            "contract digest mismatch, got `{}`",
            encode_digest(&wasm_digest)
        );
        Ok(body)
    }
}
