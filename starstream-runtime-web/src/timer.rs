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
use core::sync::atomic::{AtomicBool, Ordering};
use core::task::{Context, Poll};
use core::time::Duration;

use std::sync::Arc;

use futures::task::AtomicWaker;

#[cfg(target_family = "wasm")]
#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Calls `starstream_timer_fire(timer)` once `ms` milliseconds have
    /// elapsed.
    fn host_set_timeout(timer: *const u8, ms: i32);
    /// Cancels the timer armed by `host_set_timeout(timer, ..)`, which must
    /// not have fired yet.
    fn host_clear_timeout(timer: *const u8);
}

#[cfg(not(target_family = "wasm"))]
unsafe fn host_set_timeout(_timer: *const u8, _ms: i32) {
    unreachable!("timers require the JS glue")
}

#[cfg(not(target_family = "wasm"))]
unsafe fn host_clear_timeout(_timer: *const u8) {
    unreachable!("timers require the JS glue")
}

#[derive(Default)]
struct Timer {
    fired: AtomicBool,
    waker: AtomicWaker,
}

/// Fire a timer armed by `host_set_timeout`.
///
/// # Safety
///
/// `timer` must be passed to `host_set_timeout`, not cleared and fired exactly
/// once.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn starstream_timer_fire(timer: *const u8) {
    let timer = unsafe { Arc::from_raw(timer.cast::<Timer>()) };
    timer.fired.store(true, Ordering::Release);
    timer.waker.wake();
}

/// Future returned by [`sleep`].
pub struct Sleep {
    duration: Duration,
    timer: Option<Arc<Timer>>,
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
        let timer = self.timer.get_or_insert_with(|| {
            let timer = Arc::<Timer>::default();
            let ms = i32::try_from(duration.as_millis()).unwrap_or(i32::MAX);
            unsafe { host_set_timeout(Arc::into_raw(Arc::clone(&timer)).cast(), ms) };
            timer
        });
        timer.waker.register(cx.waker());
        if timer.fired.load(Ordering::Acquire) {
            Poll::Ready(())
        } else {
            Poll::Pending
        }
    }
}

impl Drop for Sleep {
    fn drop(&mut self) {
        if let Some(timer) = &self.timer
            && !timer.fired.load(Ordering::Acquire)
        {
            let timer = Arc::as_ptr(timer);
            unsafe {
                host_clear_timeout(timer.cast());
                Arc::decrement_strong_count(timer);
            }
        }
    }
}
