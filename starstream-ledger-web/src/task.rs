use core::cell::{Cell, RefCell};
use core::task::{Context, Poll, Waker};

use std::collections::HashMap;
use std::sync::Arc;
use std::task::Wake;

use futures::StreamExt as _;
use futures::future::{self, AbortHandle, LocalBoxFuture};
use futures::stream::FuturesUnordered;

use crate::report;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Calls the `ledger_poll` export once the export calling this import has
    /// returned and no `ledger_import`, `ledger_start` or `ledger_poll` call is
    /// in progress.
    fn ledger_wake();
}

thread_local! {
    static WAKER: Waker = Waker::from(Arc::new(LedgerWake));
    static SPAWNED: RefCell<Vec<LocalBoxFuture<'static, ()>>> = RefCell::default();
    static TASKS: RefCell<FuturesUnordered<LocalBoxFuture<'static, ()>>> = RefCell::default();
    static ABORTS: RefCell<HashMap<u32, AbortHandle>> = RefCell::default();
    static POLLING: Cell<bool> = Cell::default();
    static WOKEN: Cell<bool> = Cell::default();
}

struct LedgerWake;

impl Wake for LedgerWake {
    fn wake(self: Arc<Self>) {
        if POLLING.get() {
            WOKEN.set(true);
        } else {
            unsafe { ledger_wake() }
        }
    }
}

/// Spawn request `id`, which `ledger_abort(id)` aborts.
pub fn spawn(id: u32, fut: impl Future<Output = ()> + 'static) {
    let (fut, abort) = future::abortable(fut);
    ABORTS.with_borrow_mut(|aborts| aborts.insert(id, abort));
    SPAWNED.with_borrow_mut(|spawned| {
        spawned.push(Box::pin(async move {
            _ = fut.await;
            ABORTS.with_borrow_mut(|aborts| aborts.remove(&id));
        }));
    });
    WAKER.with(Waker::wake_by_ref);
}

/// Abort request `id`, which then gets no response.
#[unsafe(no_mangle)]
pub extern "C" fn ledger_abort(id: u32) {
    if let Some(abort) = ABORTS.with_borrow_mut(|aborts| aborts.remove(&id)) {
        abort.abort();
    }
}

/// Poll the requests being handled until none can make progress.
#[unsafe(no_mangle)]
pub extern "C" fn ledger_poll() -> i32 {
    report(|| {
        WAKER.with(|waker| {
            let mut cx = Context::from_waker(waker);
            TASKS.with_borrow_mut(|tasks| {
                POLLING.set(true);
                loop {
                    WOKEN.set(false);
                    tasks.extend(SPAWNED.take());
                    if let Poll::Ready(Some(())) = tasks.poll_next_unpin(&mut cx) {
                        continue;
                    }
                    if !WOKEN.get() {
                        break;
                    }
                }
                POLLING.set(false);
                Ok(())
            })
        })
    })
}
