#![no_std]

// a "ledger variable"
// we need atomics because resource methods are bound to &self and not &mut self, this is a Rust limitation, not a wasm limitation
static COUNT: AtomicU64 = AtomicU64::new(0);

impl GuestUtxo for Utxo {
    fn new() -> Self {
        Self
    }

    fn increment(&self) {
        let _ = COUNT.fetch_add(1, Ordering::Relaxed);
    }
}

// tmp: workaround for the lack of wasmtime component api to get a snapshof of the state
impl bindings::Guest for Component {
    fn snapshot_state() -> u64 {
        COUNT.load(Ordering::Relaxed)
    }

    fn snapshot_restore(state: u64) {
        COUNT.store(state, Ordering::Relaxed);
    }
}

// cargo component scaffolding

#[cfg(target_arch = "wasm32")]
#[global_allocator]
static ALLOC: dlmalloc::GlobalDlmalloc = dlmalloc::GlobalDlmalloc;

#[cfg(target_arch = "wasm32")]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo<'_>) -> ! {
    loop {}
}

#[allow(warnings)]
mod bindings;

use bindings::exports::starstream_demo::utxo_b_rs::utxo_b_api::{Guest as UtxoGuest, GuestUtxo};
use core::sync::atomic::{AtomicU64, Ordering};

struct Component;
struct Utxo;

impl UtxoGuest for Component {
    type Utxo = Utxo;
}

bindings::export!(Component with_types_in bindings);
