use core::error::Error;
use core::fmt;
use core::pin::Pin;
use core::task::{Context, Poll, ready};

use bytes::Bytes;
use futures::channel::oneshot;
use http_body::{Body, Frame, SizeHint};

use crate::take;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Reads the next chunk of the body of request `id`, then passes it to
    /// `ledger_body_data(read, ..)`, or calls `ledger_body_end(read)` at the
    /// end of the body or `ledger_body_error(read)` if reading it failed.
    fn ledger_body_read(id: u32, read: *mut u8);
}

#[derive(Debug)]
pub struct BodyError;

impl fmt::Display for BodyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("failed to read the request body")
    }
}

impl Error for BodyError {}

/// Request body read from the embedder through `ledger_body_read`, failing
/// unless it is `len` bytes long if set.
pub struct RequestBody {
    id: u32,
    len: Option<u64>,
    ended: bool,
    read: Option<oneshot::Receiver<Option<Result<Bytes, BodyError>>>>,
}

impl RequestBody {
    pub fn new(id: u32, len: Option<u64>) -> Self {
        Self {
            id,
            len,
            ended: false,
            read: None,
        }
    }
}

impl Body for RequestBody {
    type Data = Bytes;
    type Error = BodyError;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        if self.is_end_stream() {
            return Poll::Ready(None);
        }
        let id = self.id;
        let read = self.read.get_or_insert_with(|| {
            let (tx, rx) = oneshot::channel();
            unsafe { ledger_body_read(id, Box::into_raw(Box::new(tx)).cast()) };
            rx
        });
        let res = ready!(Pin::new(read).poll(cx));
        self.read = None;
        match res.unwrap_or(Some(Err(BodyError))) {
            Some(Ok(data)) => {
                if let Some(len) = &mut self.len {
                    let n = u64::try_from(data.len()).unwrap_or(u64::MAX);
                    let Some(rest) = len.checked_sub(n) else {
                        self.ended = true;
                        return Poll::Ready(Some(Err(BodyError)));
                    };
                    *len = rest;
                }
                Poll::Ready(Some(Ok(Frame::data(data))))
            }
            Some(Err(err)) => {
                self.ended = true;
                Poll::Ready(Some(Err(err)))
            }
            None => {
                self.ended = true;
                if self.len.is_some_and(|len| len > 0) {
                    Poll::Ready(Some(Err(BodyError)))
                } else {
                    Poll::Ready(None)
                }
            }
        }
    }

    fn is_end_stream(&self) -> bool {
        self.ended
    }

    fn size_hint(&self) -> SizeHint {
        match self.len {
            Some(len) => SizeHint::with_exact(len),
            None => SizeHint::default(),
        }
    }
}

unsafe fn send(read: *mut u8, frame: Option<Result<Bytes, BodyError>>) {
    let read = unsafe { Box::from_raw(read.cast::<oneshot::Sender<_>>()) };
    _ = read.send(frame);
}

/// Pass the next chunk of the request body to the read started by
/// `ledger_body_read(.., read)`.
///
/// # Safety
///
/// `read` must be passed to `ledger_body_read` and to exactly one
/// `ledger_body_*` export, and `data` must be the result of
/// `ledger_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ledger_body_data(read: *mut u8, data: *mut u8, len: usize) {
    let data = unsafe { take(data, len) };
    unsafe { send(read, Some(Ok(Bytes::from(data)))) }
}

/// End the request body, read by `ledger_body_read(.., read)`.
///
/// # Safety
///
/// `read` must be passed to `ledger_body_read` and to exactly one
/// `ledger_body_*` export.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ledger_body_end(read: *mut u8) {
    unsafe { send(read, None) }
}

/// Fail the read started by `ledger_body_read(.., read)`.
///
/// # Safety
///
/// `read` must be passed to `ledger_body_read` and to exactly one
/// `ledger_body_*` export.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ledger_body_error(read: *mut u8) {
    unsafe { send(read, Some(Err(BodyError))) }
}
