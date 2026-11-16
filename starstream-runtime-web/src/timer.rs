//! Timers backed by the embedder.
//!
//! [`sleep`] arms a timer through the `host_set_timeout(timer, ms)` import,
//! which the embedder implements with e.g. `setTimeout`, calling the
//! `starstream_timer_fire(timer)` export once `ms` milliseconds have elapsed,
//! unless [`Sleep`] is dropped first and cancels it through the
//! `host_clear_timeout(timer)` import. That export does not suspend, so the
//! embedder may call it while an activation is parked in
//! [`block_on`](crate::block_on).

use core::pin::Pin;
use core::task::{Context, Poll};
use core::time::Duration;

use futures::channel::oneshot;

#[cfg(target_family = "wasm")]
#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Calls `starstream_timer_fire(timer)` once `ms` milliseconds have
    /// elapsed.
    fn host_set_timeout(timer: *mut u8, ms: i32);
    /// Cancels the timer armed by `host_set_timeout(timer, ..)`, which must
    /// not have fired yet.
    fn host_clear_timeout(timer: *mut u8);
}

#[cfg(not(target_family = "wasm"))]
unsafe fn host_set_timeout(_timer: *mut u8, _ms: i32) {
    unreachable!("timers require the JS glue")
}

#[cfg(not(target_family = "wasm"))]
unsafe fn host_clear_timeout(_timer: *mut u8) {
    unreachable!("timers require the JS glue")
}

/// Fire a timer armed by `host_set_timeout`.
///
/// # Safety
///
/// `timer` must be passed to `host_set_timeout`, not cleared and fired exactly
/// once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn starstream_timer_fire(timer: *mut u8) {
    let timer = unsafe { Box::from_raw(timer.cast::<oneshot::Sender<()>>()) };
    _ = timer.send(());
}

/// Future returned by [`sleep`].
pub struct Sleep {
    duration: Duration,
    timer: Option<(*mut u8, oneshot::Receiver<()>)>,
}

/// Wait until `duration` has elapsed, counted from the first poll.
pub fn sleep(duration: Duration) -> Sleep {
    Sleep {
        duration,
        timer: None,
    }
}

impl Future for Sleep {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
        let duration = self.duration;
        let (.., fired) = self.timer.get_or_insert_with(|| {
            let (tx, rx) = oneshot::channel();
            let timer = Box::into_raw(Box::new(tx)).cast();
            let ms = i32::try_from(duration.as_millis()).unwrap_or(i32::MAX);
            unsafe { host_set_timeout(timer, ms) };
            (timer, rx)
        });
        Pin::new(fired).poll(cx).map(drop)
    }
}

impl Drop for Sleep {
    fn drop(&mut self) {
        if let Some((timer, fired)) = &mut self.timer
            && let Ok(None) = fired.try_recv()
        {
            unsafe {
                host_clear_timeout(*timer);
                drop(Box::from_raw(timer.cast::<oneshot::Sender<()>>()));
            }
        }
    }
}
