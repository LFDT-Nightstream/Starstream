use core::cell::RefCell;
use core::task::{Context, Poll, Waker};

use std::sync::Arc;
use std::task::Wake;

use futures::StreamExt as _;
use futures::future::LocalBoxFuture;
use futures::stream::FuturesUnordered;

use crate::report;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Calls the `ledger_poll` export once the export calling this import has
    /// returned and no `ledger_poll` call is in progress.
    fn ledger_wake();
}

thread_local! {
    static SPAWNED: RefCell<Vec<LocalBoxFuture<'static, ()>>> = RefCell::default();
    static TASKS: RefCell<FuturesUnordered<LocalBoxFuture<'static, ()>>> = RefCell::default();
}

struct LedgerWake;

impl Wake for LedgerWake {
    fn wake(self: Arc<Self>) {
        unsafe { ledger_wake() }
    }
}

pub fn spawn(fut: impl Future<Output = ()> + 'static) {
    SPAWNED.with_borrow_mut(|spawned| spawned.push(Box::pin(fut)));
    unsafe { ledger_wake() }
}

/// Poll the requests being handled until none can make progress.
#[unsafe(no_mangle)]
pub extern "C" fn ledger_poll() -> i32 {
    report(|| {
        let waker = Waker::from(Arc::new(LedgerWake));
        let mut cx = Context::from_waker(&waker);
        TASKS.with_borrow_mut(|tasks| {
            loop {
                tasks.extend(SPAWNED.take());
                if let Poll::Ready(Some(())) = tasks.poll_next_unpin(&mut cx) {
                    continue;
                }
                if SPAWNED.with_borrow(Vec::is_empty) {
                    return Ok(());
                }
            }
        })
    })
}
