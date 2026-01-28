mod adapter;
use std::io::Cursor;

use anyhow::Context as _;
use bytes::{Bytes, BytesMut};
use wrpc_transport::Invoke;

use crate::adapter::{AdapterIncoming, AdapterOutgoing};

mod bindings {
    wit_bindgen_wrpc::generate!({
        world: "client",
        with: {
            "starstream:wrpc-multiplexer/handler": generate,
        }
    });
}

pub struct InvocationContext<TCtx: Send + Sync> {
    contract_hash: String,
    cx: TCtx,
}

impl<TCtx: Send + Sync> InvocationContext<TCtx> {
    pub fn new(cx: TCtx, contract_hash: String) -> Self {
        Self { cx, contract_hash }
    }
}

/// Our proxy makes a call to another WIT, which (for all we know) could be remote and require a different transport
/// We don't have to care about which transport it uses for our case (we want to support any possible transport it may use)
#[derive(Clone)]
pub struct MultiplexClient<TClient: Invoke> {
    client: TClient,
}

impl<TClient: Invoke> MultiplexClient<TClient> {
    pub fn new(client: TClient) -> Self {
        Self { client }
    }
}

impl<TClient: Invoke> wrpc_transport::Invoke for MultiplexClient<TClient> {
    type Context = InvocationContext<TClient::Context>;
    type Outgoing = AdapterOutgoing;
    type Incoming = AdapterIncoming;

    async fn invoke<P>(
        &self,
        cx: Self::Context,
        _instance: &str,
        func: &str,
        params: Bytes,
        _paths: impl AsRef<[P]> + Send,
    ) -> anyhow::Result<(Self::Outgoing, Self::Incoming)>
    where
        P: AsRef<[Option<usize>]> + Send + Sync,
    {
        let contract_hash = cx.contract_hash.clone();
        // Note: there is no way to connect host/guest resources across this interface
        //       do we want to enable that? I'm not sure if there is a good way to do this given we're doing 2 hops of wRPC calls
        let result = bindings::starstream::wrpc_multiplexer::handler::call(
            &self.client,
            cx.cx,
            &contract_hash,
            &func,
            &params,
        )
        .await
        .with_context(|| format!("failed to invoke `{contract_hash}.{func}`"))?;

        // Convert Vec<u8> result to (Outgoing, Incoming) streams
        Ok((
            AdapterOutgoing,
            AdapterIncoming {
                buffer: BytesMut::default(),
                inner: Cursor::new(result.to_vec()),
            },
        ))
    }
}
