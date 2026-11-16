use core::task::{Context, Poll};

use anyhow::{Context as _, anyhow};
use bytes::Bytes;
use futures::future::BoxFuture;
use http::Uri;
use http_body_util::combinators::UnsyncBoxBody;
use http_body_util::{BodyExt as _, Full, Limited};
use hyper::body::Incoming;
use hyper_util::client::legacy::connect::Connect;
use tracing::instrument;
use wrpc::http::{OutgoingParams, Service};

use crate::client::http::{ClientBuilder, Transport};
use crate::configure_wasmtime;

const MAX_ERROR_BODY_SIZE: usize = 1 << 16;

/// Request body of the `hyper-util` legacy client backing [Client].
pub type Body = UnsyncBoxBody<Bytes, std::io::Error>;

/// [Client](crate::client::http::Client) backed by a `hyper-util` legacy client.
pub type Client<C> =
    crate::client::http::Client<WrpcService<C>, hyper_util::client::legacy::Client<C, Body>>;

/// Service sending wRPC requests with a `hyper-util` legacy client, failing on responses with an
/// unsuccessful status.
#[derive(Clone)]
pub struct WrpcService<C>(pub hyper_util::client::legacy::Client<C, Body>);

impl<C> Service<http::Request<OutgoingParams>> for WrpcService<C>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    type Response = http::Response<Incoming>;
    type Error = anyhow::Error;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: http::Request<OutgoingParams>) -> Self::Future {
        let res = self.0.request(req.map(|body| body.boxed_unsync()));
        Box::pin(async move {
            let res = res.await?;
            if res.status().is_success() {
                return Ok(res);
            }
            let (http::response::Parts { status, .. }, body) = res.into_parts();
            let Ok(body) = Limited::new(body, MAX_ERROR_BODY_SIZE).collect().await else {
                return Err(anyhow!("HTTP request failed with status `{status}`"));
            };
            let body = body.to_bytes();
            Err(anyhow!(
                "HTTP request failed with status `{status}`: {}",
                String::from_utf8_lossy(&body)
            ))
        })
    }
}

impl<C> ClientBuilder<WrpcService<C>, hyper_util::client::legacy::Client<C, Body>>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    pub fn from_hyper(
        http: hyper_util::client::legacy::Builder,
        connect: C,
        api_base: impl Into<Uri>,
    ) -> Self {
        let http = http.build(connect);
        let mut engine_config = wasmtime::Config::default();
        configure_wasmtime(&mut engine_config);
        let engine = wasmtime::Engine::new(&engine_config).unwrap();
        Self::new(WrpcService(http.clone()), http, api_base, engine)
    }
}

impl<C> Transport for hyper_util::client::legacy::Client<C, Body>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    #[instrument(skip_all)]
    async fn request(
        &self,
        req: http::Request<Full<Bytes>>,
    ) -> anyhow::Result<(http::response::Parts, Bytes)> {
        let req = req.map(|body| body.map_err(|err| match err {}).boxed_unsync());
        let res = hyper_util::client::legacy::Client::request(self, req)
            .await
            .context("failed to send request")?;
        let (parts, body) = res.into_parts();
        let body = body
            .collect()
            .await
            .context("failed to receive response body")?;
        Ok((parts, body.to_bytes()))
    }
}
