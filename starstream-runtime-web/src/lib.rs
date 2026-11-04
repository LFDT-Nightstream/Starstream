//! Wasmtime's `custom-fiber` and `custom-virtual-memory` C ABIs for wasm32
//! embedders, which also export `__stack_pointer` and use JSPI for fibers.

mod fiber;
mod platform;
mod timer;

pub use fiber::block_on;
pub use timer::{Sleep, sleep};

use core::any::Any;

use cranelift_codegen::timing::{self, Pass, Profiler};

struct CraneliftProfiler;

impl Profiler for CraneliftProfiler {
    fn start_pass(&self, _pass: Pass) -> Box<dyn Any> {
        Box::new(())
    }
}

/// Disable the default Cranelift profiler, which calls `Instant::now()`, which
/// panics on `wasm32-unknown-unknown`.
pub fn disable_cranelift_profiler() {
    _ = timing::set_thread_profiler(Box::new(CraneliftProfiler));
}

/// Configure Wasmtime for Pulley bytecode in the malloc-backed memory of this
/// platform, without signals, guard pages or copy-on-write images.
pub fn configure_wasmtime(config: &mut wasmtime::Config) -> wasmtime::Result<()> {
    config.target("pulley32")?;
    config.signals_based_traps(false);
    config.memory_guard_size(0);
    config.memory_reservation(0);
    config.memory_reservation_for_growth(1 << 20);
    config.memory_init_cow(false);
    Ok(())
}
