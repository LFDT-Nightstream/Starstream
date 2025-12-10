use std::cell::RefCell;
use wasmtime::component::{ResourceTable};
use wrpc_runtime_wasmtime::{WrpcView, WrpcCtx, WrpcCtxView, SharedResourceTable};
use wrpc_transport::{Invoke, Index};
use bytes::Bytes;
use tokio::io::DuplexStream;

/// Host any context that can be fetched from Starstream contracts through an effect handler
///
/// Think of this similar to a React Context: Starstream programs can raise an effect to receive context from the runtime
/// This is represented as a Resource (in the WIT definition of the word) that the host provides when it calls into the UTXO
/// (pseudocode as the Starstream syntax isn't decided yet): `raise Ctx.Caller()`
/// 
/// Here, the ledger is the "host" in the WASM sense
/// and it exposes this data to guests via host functions added to the Linker
/// 
/// Note: blockchain execution doesn't involve stateful handles like file descriptors or network connections
/// When adding fields, also add corresponding host functions in the Linker setup
/// (see `chain.rs`) so guests can access this data.
pub struct ChainContext {
    // TODO: Add ledger context fields as needed:
    // pub block_range: (u64, u64), // validity interval of tx
    // pub timestamp_range: (u64, u64), // validity interval of tx
    // pub caller_address: String,
    wrpc_table: RefCell<ResourceTable>,
    wrpc_shared_resources: RefCell<SharedResourceTable>,
    wrpc_ctx: RefCell<ChainContextWrpcCtx>,
}

/// A wrapper around DuplexStream that implements Index
pub struct IndexedStream(pub DuplexStream);

// IndexedStream needs to be Unpin, Send, Sync, and 'static
unsafe impl Send for IndexedStream {}
unsafe impl Sync for IndexedStream {}
impl Unpin for IndexedStream {}

impl Index<IndexedStream> for IndexedStream {
    fn index(&self, _path: &[usize]) -> anyhow::Result<IndexedStream> {
        // For encoding purposes, we don't need actual indexing
        // Create a new stream pair
        let (outgoing, incoming) = tokio::io::duplex(1024);
        Ok(IndexedStream(outgoing))
    }
}

impl tokio::io::AsyncRead for IndexedStream {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        std::pin::Pin::new(&mut self.get_mut().0).poll_read(cx, buf)
    }
}

impl tokio::io::AsyncWrite for IndexedStream {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        std::pin::Pin::new(&mut self.get_mut().0).poll_write(cx, buf)
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        std::pin::Pin::new(&mut self.get_mut().0).poll_flush(cx)
    }

    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        std::pin::Pin::new(&mut self.get_mut().0).poll_shutdown(cx)
    }
}

/// A no-op Invoke implementation for encoding values without actual RPC calls
pub struct NoOpInvoke;

impl Invoke for NoOpInvoke {
    type Context = ();
    type Incoming = IndexedStream;
    type Outgoing = IndexedStream;

    fn invoke<P>(
        &self,
        _cx: Self::Context,
        _instance: &str,
        _func: &str,
        _params: Bytes,
        _paths: impl AsRef<[P]> + Send,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = anyhow::Result<(Self::Outgoing, Self::Incoming)>> + Send>>
    where
        P: AsRef<[Option<usize>]> + Send + Sync,
    {
        Box::pin(async move {
            // Create a pair of connected streams
            let (outgoing, incoming) = tokio::io::duplex(1024);
            Ok((IndexedStream(outgoing), IndexedStream(incoming)))
        })
    }
}

#[derive(Default)]
struct ChainContextWrpcCtx {
    shared_resources: RefCell<SharedResourceTable>,
}

impl WrpcCtx<NoOpInvoke> for ChainContextWrpcCtx {
    fn context(&self) -> () {
        ()
    }

    fn client(&self) -> &NoOpInvoke {
        static NOOP: NoOpInvoke = NoOpInvoke;
        &NOOP
    }

    fn shared_resources(&mut self) -> &mut SharedResourceTable {
        self.shared_resources.get_mut()
    }
}

impl Default for ChainContext {
  fn default() -> Self {
      let shared_resources = RefCell::new(SharedResourceTable::default());
      Self {
          wrpc_table: RefCell::new(ResourceTable::default()),
          wrpc_shared_resources: RefCell::new(SharedResourceTable::default()),
          wrpc_ctx: RefCell::new(ChainContextWrpcCtx {
              shared_resources,
          }),
      }
  }
}

impl WrpcView for ChainContext {
  type Invoke = NoOpInvoke;

  fn wrpc(&mut self) -> WrpcCtxView<'_, Self::Invoke> {
      let table = self.wrpc_table.get_mut();
      let ctx = self.wrpc_ctx.get_mut();
      WrpcCtxView {
          ctx,
          table,
      }
  }
}