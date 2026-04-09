#![no_std]

impl Guest for Component {
    fn main(utxo_a: &bindings::UtxoA, utxo_b: &bindings::UtxoB) {
        let iters = utxo_a.get_data();

        for _ in 0..iters {
            utxo_b.increment();
        }
    }
}

// just cargo component scaffolding

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

use bindings::Guest;

struct Component;

bindings::export!(Component with_types_in bindings);
