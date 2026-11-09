use core::error::Error;
use core::pin::Pin;
use core::task::{Context, Poll, ready};

use http_body::{Body, Frame, SizeHint};
use pin_project_lite::pin_project;

#[derive(Debug, thiserror::Error)]
#[error("timed out waiting for a body frame")]
pub struct TimeoutError;

pin_project! {
    /// Body failing with [`TimeoutError`] if its inner body sends no frame
    /// before a future returned by `timeout` completes.
    pub struct TimeoutBody<B, F, S> {
        #[pin]
        body: B,
        timeout: F,
        #[pin]
        timer: Option<S>,
    }
}

impl<B, F, S> TimeoutBody<B, F, S> {
    pub fn new(timeout: F, body: B) -> Self {
        Self {
            body,
            timeout,
            timer: None,
        }
    }
}

impl<B, F, S> Body for TimeoutBody<B, F, S>
where
    B: Body,
    B::Error: Into<Box<dyn Error + Send + Sync>>,
    F: Fn() -> S,
    S: Future<Output = ()>,
{
    type Data = B::Data;
    type Error = Box<dyn Error + Send + Sync>;

    fn poll_frame(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        let mut this = self.project();
        if let Poll::Ready(frame) = this.body.poll_frame(cx) {
            this.timer.set(None);
            return Poll::Ready(frame.map(|frame| frame.map_err(Into::into)));
        }
        if this.timer.is_none() {
            this.timer.set(Some((this.timeout)()));
        }
        if let Some(timer) = this.timer.as_mut().as_pin_mut() {
            ready!(timer.poll(cx));
        }
        this.timer.set(None);
        Poll::Ready(Some(Err(Box::new(TimeoutError))))
    }

    fn is_end_stream(&self) -> bool {
        self.body.is_end_stream()
    }

    fn size_hint(&self) -> SizeHint {
        self.body.size_hint()
    }
}
