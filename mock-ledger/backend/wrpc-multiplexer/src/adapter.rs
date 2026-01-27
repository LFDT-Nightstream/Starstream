use bytes::BytesMut;
use std::io::Cursor;
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use wrpc_transport::Index;

/// Incoming stream that wraps the Vec<u8> result with an adapter
///
/// This implements buffering similar to `wrpc_transport::Incoming<T>` but without
/// relying on private fields.
/// For simple cases without multiplexing, we just wrap the cursor directly.
#[derive(Debug)]
pub struct AdapterIncoming {
    pub(crate) buffer: BytesMut,
    pub(crate) inner: Cursor<Vec<u8>>,
}

impl Unpin for AdapterIncoming {}

impl AsyncRead for AdapterIncoming {
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

impl Index<Self> for AdapterIncoming {
    fn index(&self, _path: &[usize]) -> anyhow::Result<Self> {
        // For simple cases without multiplexing, create a new cursor with the same data
        // This allows the Index trait to work for stream multiplexing if needed
        Ok(Self {
            buffer: BytesMut::default(),
            inner: Cursor::new(self.inner.get_ref().clone()),
        })
    }
}

/// Outgoing stream that's a no-op since params are already sent to the proxy
#[derive(Debug, Clone, Copy)]
pub struct AdapterOutgoing;

impl AsyncWrite for AdapterOutgoing {
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

impl Index<Self> for AdapterOutgoing {
    fn index(&self, _path: &[usize]) -> anyhow::Result<Self> {
        // For simple cases without multiplexing, just return self
        Ok(AdapterOutgoing)
    }
}
