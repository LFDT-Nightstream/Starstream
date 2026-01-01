//! Implementation of a custom wRPC transport proxy that routes invocations through a WIT interface.
//!
//! This module implements Approach #3 from APPROACHES.md: Custom Wrappers.
//! It converts the Vec<u8> result from the proxy into wRPC transport streams by:
//! 1. Wrapping the result in `ProxyIncoming` which uses `wrpc_transport::Incoming<Cursor<Vec<u8>>>`
//! 2. Providing a no-op `ProxyOutgoing` since params are already sent through the proxy
//! 3. Implementing the required `Index` trait for stream multiplexing support

use anyhow::Context as _;
use bytes::{Bytes, BytesMut};
use wrpc_transport::{Invoke, Index};
use std::io::Cursor;
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};

mod bindings {
    wit_bindgen_wrpc::generate!({
        world: "client",
        with: {
            "starstream:wrpc-multiplexer/handler": generate,
        }
    });
}

pub struct InvocationContext<TCtx: Send + Sync> {
    cx: TCtx
}

impl<TCtx: Send + Sync> InvocationContext<TCtx> {
    pub fn new(cx: TCtx) -> Self {
        Self { cx }
    }
}

/// Incoming stream that wraps the Vec<u8> result from the proxy
/// 
/// This implements buffering similar to `wrpc_transport::Incoming<T>` but without
/// relying on private fields. For simple cases without multiplexing, we just wrap
/// the cursor directly.
#[derive(Debug)]
pub struct ProxyIncoming {
    buffer: BytesMut,
    inner: Cursor<Vec<u8>>,
}

impl Unpin for ProxyIncoming {}

impl AsyncRead for ProxyIncoming {
    fn poll_read(
        mut self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let cap = buf.remaining();
        if cap == 0 {
            return Poll::Ready(Ok(()));
        }
        
        // First, read from buffer if available
        if !self.buffer.is_empty() {
            if self.buffer.len() > cap {
                buf.put_slice(&self.buffer.split_to(cap));
            } else {
                buf.put_slice(&std::mem::take(&mut self.buffer));
            }
            return Poll::Ready(Ok(()));
        }
        
        // Then read from inner cursor
        // Cursor::read is synchronous, so we can call it directly
        let mut temp_buf = vec![0u8; cap];
        let n = std::io::Read::read(&mut self.inner, &mut temp_buf)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        
        if n > 0 {
            buf.put_slice(&temp_buf[..n]);
        }
        
        Poll::Ready(Ok(()))
    }
}

impl Index<Self> for ProxyIncoming {
    fn index(&self, _path: &[usize]) -> anyhow::Result<Self> {
        // For simple cases without multiplexing, create a new cursor with the same data
        // This allows the Index trait to work for stream multiplexing if needed
        Ok(Self {
            buffer: BytesMut::default(),
            inner: Cursor::new(self.inner.get_ref().clone()),
        })
    }
}

/// Outgoing stream that's a no-op since params are already sent through the proxy
#[derive(Debug, Clone, Copy)]
pub struct ProxyOutgoing;

impl AsyncWrite for ProxyOutgoing {
    fn poll_write(
        self: Pin<&mut Self>,
        _cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        // No-op: params are already sent through the proxy
        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}

impl Index<Self> for ProxyOutgoing {
    fn index(&self, _path: &[usize]) -> anyhow::Result<Self> {
        // For simple cases without multiplexing, just return self
        Ok(ProxyOutgoing)
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
    type Outgoing = ProxyOutgoing;
    type Incoming = ProxyIncoming;

    async fn invoke<P>(
        &self,
        cx: Self::Context,
        instance: &str,
        func: &str,
        params: Bytes,
        paths: impl AsRef<[P]> + Send,
    ) -> anyhow::Result<(Self::Outgoing, Self::Incoming)>
    where
        P: AsRef<[Option<usize>]> + Send + Sync,
    {
        let contract_hash = "0x1170FAD15BECBB08C00B29067171110B34E8B4CEBC648BA662147BA0F2F1224F"; // TODO: properly set later
        let result = bindings::starstream::wrpc_multiplexer::handler::call(
            &self.client,
            cx.cx,
            &contract_hash.clone(),
            &func.clone(),
            &params,
        )
        .await
        .with_context(|| format!("failed to call `{contract_hash}.{func}`"))?;

        // Convert Vec<u8> result to ProxyIncoming stream
        Ok((
            ProxyOutgoing,
            ProxyIncoming {
                buffer: BytesMut::default(),
                inner: Cursor::new(result.to_vec()),
            },
        ))
    }
}
