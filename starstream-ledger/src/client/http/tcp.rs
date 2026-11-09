use anyhow::Context as _;
use bytes::Bytes;
use http::Uri;
use http_body_util::{BodyExt as _, Full};
use hyper_util::client::legacy::connect::Connect;
use tracing::instrument;

use crate::client::http::{ClientBuilder, Transport};

/// [Client](crate::client::http::Client) backed by the `hyper-util` legacy client.
pub type Client<C> = crate::client::http::Client<
    wrpc_http::Client<hyper_util::client::legacy::Client<C, wrpc_http::OutgoingBody>>,
    hyper_util::client::legacy::Client<C, Full<Bytes>>,
>;

impl<C>
    ClientBuilder<
        wrpc_http::Client<hyper_util::client::legacy::Client<C, wrpc_http::OutgoingBody>>,
        hyper_util::client::legacy::Client<C, Full<Bytes>>,
    >
where
    C: Connect + Clone + Send + Sync + 'static,
{
    pub fn from_hyper(
        http: hyper_util::client::legacy::Builder,
        connect: C,
        api_base: impl Into<Uri>,
    ) -> Self {
        let wrpc = wrpc_http::Client::new(http.build(connect.clone()));
        let http = http.build(connect);
        Self::new(wrpc, http, api_base)
    }
}

impl<C> Transport for hyper_util::client::legacy::Client<C, Full<Bytes>>
where
    C: Connect + Clone + Send + Sync + 'static,
{
    #[instrument(skip_all)]
    async fn request(
        &self,
        req: http::Request<Full<Bytes>>,
    ) -> anyhow::Result<(http::response::Parts, Bytes)> {
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
