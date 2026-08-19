//! Wasmtime "custom platform" implementation for a browser-like wasm32 host.
//!
//! Wasmtime doesn't recognize wasm32 as a supported OS, so with the
//! `custom-virtual-memory` feature it calls out to a small C-ABI for its
//! platform needs instead of using mmap/signals directly. We satisfy that ABI
//! here, in pure Rust, backed by the global allocator.
//!
//! There is no real virtual memory in the sandbox, so:
//!   * "mmap" is a page-aligned, zeroed heap allocation,
//!   * "mprotect" is a no-op (Pulley bytecode is interpreted *data*, never
//!     executed as native code, so W^X is irrelevant), and
//!   * copy-on-write memory images are disabled (we return "no image").

use core::ffi::c_void;
use core::ptr;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicUsize, Ordering};

use std::alloc;

/// Wasmtime requires two pointers' space of TLS, addressed by slot: slot 0 is
/// the runtime TLS pointer, slot 1 the `component-model-async` state. The
/// browser runs this module single-threaded, so plain atomic globals are
/// sufficient. Indexing panics (loudly, via the panic hook) if wasmtime ever
/// passes a slot we don't know about.
static WASMTIME_TLS: [AtomicUsize; 2] = [AtomicUsize::new(0), AtomicUsize::new(0)];

#[unsafe(no_mangle)]
pub extern "C" fn wasmtime_tls_get(slot: usize) -> *mut u8 {
    WASMTIME_TLS[slot].load(Ordering::Relaxed) as *mut u8
}

#[unsafe(no_mangle)]
pub extern "C" fn wasmtime_tls_set(slot: usize, ptr: *mut u8) {
    WASMTIME_TLS[slot].store(ptr as usize, Ordering::Relaxed);
}

/// Page size we report and align every allocation to. 64 KiB matches the Wasm
/// page size and keeps Wasmtime's host-page-count bookkeeping consistent.
const PAGE_SIZE: usize = 1 << 16;

#[unsafe(no_mangle)]
pub extern "C" fn wasmtime_page_size() -> usize {
    PAGE_SIZE
}

fn layout(size: usize) -> alloc::Layout {
    // Sizes Wasmtime passes are already page-multiples; align to a page too.
    alloc::Layout::from_size_align(size, PAGE_SIZE).expect("valid layout")
}

/// `mmap(NULL, size, ...)`: hand back a fresh, zeroed, page-aligned region.
/// Returns 0 on success and writes the base pointer to `*ret`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_mmap_new(
    size: usize,
    _prot_flags: u32,
    ret: *mut *mut u8,
) -> i32 {
    if size == 0 {
        // Never paired with a munmap; a dangling aligned pointer is fine.
        unsafe { *ret = PAGE_SIZE as *mut u8 };
        return 0;
    }
    let ptr = unsafe { alloc::alloc_zeroed(layout(size)) };
    if ptr.is_null() {
        return 1;
    }
    unsafe { *ret = ptr };
    0
}

/// `mmap(addr, size, ... MAP_FIXED)`: replace a sub-range with blank (zeroed)
/// memory in place. We don't reallocate, just clear the bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_mmap_remap(addr: *mut u8, size: usize, _prot_flags: u32) -> i32 {
    unsafe { ptr::write_bytes(addr, 0, size) };
    0
}

/// `munmap(ptr, size)`: free a region previously returned by `wasmtime_mmap_new`.
/// Wasmtime frees with the same `size` it mapped, so the layout matches.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_munmap(ptr: *mut u8, size: usize) -> i32 {
    if size != 0 {
        unsafe { alloc::dealloc(ptr, layout(size)) };
    }
    0
}

/// `mprotect`: no-op — there is no protection to change in the sandbox.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_mprotect(_ptr: *mut u8, _size: usize, _prot_flags: u32) -> i32 {
    0
}

// --- Copy-on-write memory images: disabled. The symbols must still exist to
// link; returning a NULL image tells Wasmtime "no image available" and it
// falls back to plain allocation + copy.

#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_memory_image_new(
    _ptr: *const u8,
    _len: usize,
    ret: *mut *mut c_void,
) -> i32 {
    unsafe { *ret = null_mut() };
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_memory_image_map_at(
    _image: *mut c_void,
    _addr: *mut u8,
    _len: usize,
) -> i32 {
    1
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn wasmtime_memory_image_free(_image: *mut c_void) {}
