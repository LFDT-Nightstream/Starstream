#![no_std]
#![no_main]
#![allow(dead_code)]
#![feature(allocator_api)]

use core::alloc::{GlobalAlloc, Layout};
use rand::SeedableRng as _;
use starstream::Utxo;
use talc::{ClaimOnOom, Span, Talc, Talck};
use toy_vm::Op;

extern crate alloc;

starstream::panic_handler!();

// don't set a global allocator
// this is needed because we are using the alloc crate, but we can use the
// allocator api instead.
struct DummyAllocator;

unsafe impl GlobalAlloc for DummyAllocator {
    unsafe fn alloc(&self, _layout: Layout) -> *mut u8 {
        core::ptr::null_mut()
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}
}

#[global_allocator]
static DUMMY_ALLOCATOR: DummyAllocator = DummyAllocator;

// half a page of wasm memory for the vm
const MEMORY_SIZE: usize = 65536 / 2;

// fixed size arena for allocator, if the program requires more memory than this
// then the program will panic
static mut ARENA: [u8; MEMORY_SIZE] = [0; MEMORY_SIZE];

static ALLOCATOR: Talck<spin::Mutex<()>, ClaimOnOom> =
    Talc::new(unsafe { ClaimOnOom::new(Span::from_array(core::ptr::addr_of!(ARENA).cast_mut())) })
        .lock();

// Vm utxo
pub struct VmUtxo {}

impl VmUtxo {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(sleep: fn(&VmUtxo)) {
        let this = VmUtxo {};

        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(42);

        // we set the stack initially to have a 0, since we need a value first
        // to do the addition.
        let ops = [Op::Push(toy_vm::Value::from_slice(&[0], &ALLOCATOR))];

        let stack = toy_vm::Stack::new(&ALLOCATOR);

        let mut stack = toy_vm::run(&ops, &ALLOCATOR, stack, &mut rng).unwrap();

        // each time the utxo is resumed, it generates 500 random values, adds
        // that to the current state, and then sorts that array
        //
        // this is not supposed to be useful, it's just an example of how the VM
        // can be used in a stateful way.
        let ops = [Op::Rand { len: 500 }, Op::Add, Op::Sort];

        loop {
            // because we are keeping the stack in memory, we are also keeping
            // the state for the next call.
            stack = toy_vm::run(&ops, &ALLOCATOR, stack, &mut rng).unwrap();

            sleep(&this);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_VmUtxo_new() {
    VmUtxo::new(starstream::sleep::<(), VmUtxo>)
}

// ----------------------------------------------------------------------------
// Coordination script

#[unsafe(no_mangle)]
pub extern "C" fn new_utxo() -> example_toy_vm::VmUtxo {
    example_toy_vm::VmUtxo::new()
}

#[unsafe(no_mangle)]
pub extern "C" fn state_transition(utxo: example_toy_vm::VmUtxo) {
    utxo.resume(());
}

// #[unsafe(no_mangle)]
// pub extern "C" fn test() {
//     let mut rng = rand_chacha::ChaCha12Rng::seed_from_u64(42);

//     let ops = [Op::Push(toy_vm::Value::from_slice(&[0], &ALLOCATOR))];

//     let stack = toy_vm::Stack::new(&ALLOCATOR);

//     let mut stack = toy_vm::run(&ops, &ALLOCATOR, stack, &mut rng).unwrap();

//     let ops = [Op::Rand { len: 500 }, Op::Add, Op::Sort];

//     stack = toy_vm::run(&ops, &ALLOCATOR, stack, &mut rng).unwrap();
// }
