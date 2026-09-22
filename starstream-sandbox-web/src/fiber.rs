//! The guest side of wasmtime's `custom-fiber` C ABI.
//!
//! Wasmtime's async support switches native stacks, which a wasm32 program
//! cannot do itself. With the `custom-fiber` feature wasmtime instead calls two
//! hooks, `wasmtime_fiber_init` and `wasmtime_fiber_switch`, which the run
//! worker (`website/src/run.worker.ts`) implements with JSPI
//! (`WebAssembly.Suspending` / `WebAssembly.promising`): the browser switches
//! the wasm stack, and the worker swaps this module's shadow-stack pointer
//! (`__stack_pointer`, exported via `build.rs`). This module provides the
//! trampoline JS re-enters the module through when a fiber first runs, and a
//! [`block_on`] for the `*_async` runtime calls.
//!
//! JSPI can only suspend an activation entered through a `promising`-wrapped
//! export, so every export that may run guest code is wrapped that way by the
//! worker, which also serializes them: root activations share the main shadow
//! stack, which is only safe LIFO.

use std::future::Future;
use std::pin::pin;
use std::ptr;
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

/// A fiber entry function, as passed to `wasmtime_fiber_init`:
/// `entry(entry_arg0, top_of_stack)`.
pub type FiberEntry = extern "C" fn(*mut u8, *mut u8) -> *mut u8;

/// First-activation trampoline: JS calls this, wrapped in
/// `WebAssembly.promising`, when a fiber is first switched to. `entry` returns
/// once the fiber's closure has completed; the worker then switches back to
/// whoever last resumed the fiber.
#[unsafe(no_mangle)]
pub extern "C" fn wasmtime_fiber_enter(entry: FiberEntry, arg0: *mut u8, top_of_stack: *mut u8) {
    entry(arg0, top_of_stack);
}

#[cfg(target_family = "wasm")]
#[link(wasm_import_module = "env")]
unsafe extern "C" {
    /// Suspends the current activation until `host_unpark` is called.
    fn host_park();
    /// Resumes the activation parked on `host_park`, or marks the wake pending
    /// if none is parked yet.
    fn host_unpark();
}

/// Native builds of this crate compile but never run; the JSPI glue only
/// exists on wasm targets.
#[cfg(not(target_family = "wasm"))]
unsafe fn host_park() {
    unreachable!("fiber parking requires the JSPI glue")
}

#[cfg(not(target_family = "wasm"))]
unsafe fn host_unpark() {}

/// Drive a wasmtime future to completion inside a `promising` activation.
///
/// Guest calls suspend through `wasmtime_fiber_switch`, which JSPI turns into a
/// suspension of the whole activation, so `poll` normally returns `Ready` at
/// once. Should it return `Pending`, the activation parks until the future's
/// waker unparks it.
pub fn block_on<F: Future>(fut: F) -> F::Output {
    let mut fut = pin!(fut);
    let waker = unpark_waker();
    let mut cx = Context::from_waker(&waker);
    loop {
        match fut.as_mut().poll(&mut cx) {
            Poll::Ready(v) => return v,
            Poll::Pending => unsafe { host_park() },
        }
    }
}

fn unpark_waker() -> Waker {
    fn clone(_: *const ()) -> RawWaker {
        RawWaker::new(ptr::null(), &VTABLE)
    }
    fn wake(_: *const ()) {
        unsafe { host_unpark() }
    }
    fn noop(_: *const ()) {}
    const VTABLE: RawWakerVTable = RawWakerVTable::new(clone, wake, wake, noop);
    unsafe { Waker::from_raw(RawWaker::new(ptr::null(), &VTABLE)) }
}
