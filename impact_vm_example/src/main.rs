#![no_main]

mod defs;

use defs::{deserialize_state, free_buffer, free_state, run_program, serialize_state};
use starstream::eprintln;

// TODO: don't really know how to setup the panic handler without no_std
pub fn hook(info: &std::panic::PanicHookInfo) {
    #[link(wasm_import_module = "env")]
    unsafe extern "C" {
        unsafe fn abort();
    }

    unsafe {
        eprintln!("{info}");

        abort();
        #[allow(clippy::empty_loop)]
        loop {}
    }
}

#[inline]
pub fn set_once() {
    use std::sync::Once;
    static SET_HOOK: Once = Once::new();
    SET_HOOK.call_once(|| {
        std::panic::set_hook(Box::new(hook));
    });
}

pub struct ImpactVM {
    // this is a pointer to the heap allocated state.
    //
    // since the library is statically linked it shares memory with the utxo.
    // otherwise this could hold a serialized versioned of the current state.
    current_state: *const (),
}

impl ImpactVM {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(sleep: fn(&mut Self), current_state: *const ()) {
        let mut this = ImpactVM { current_state };
        sleep(&mut this);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_ImpactVM_new() {
    // an array with a cell with a number set to 1.
    // this in most cases be an input to the coordination script.
    let initial_state = [
        0, 2, 2, 225, 1, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 1, 65, 1, 0, 0, 0,
        32, 0, 0, 0, 201, 170, 41, 9, 144, 82, 87, 49, 160, 49, 144, 30, 239, 178, 231, 84, 119,
        202, 205, 66, 153, 73, 239, 83, 224, 62, 167, 161, 12, 99, 232, 187, 1, 0, 0, 0, 1, 1, 0,
        0, 0, 32, 0, 0, 0, 235, 20, 33, 218, 247, 211, 225, 169, 176, 105, 251, 68, 191, 56, 15,
        145, 40, 40, 175, 74, 10, 210, 163, 206, 65, 152, 68, 89, 161, 106, 141, 101, 7, 0, 0, 0,
        3, 1, 1, 0, 0, 0, 0,
    ];

    let mut current_state = std::ptr::null::<()>();

    unsafe {
        assert_eq!(
            deserialize_state(
                initial_state.as_ptr(),
                initial_state.len(),
                &mut current_state as *mut _,
            ),
            0
        )
    };

    ImpactVM::new(starstream::sleep_mut::<(), ImpactVM>, current_state)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_mutate_ImpactVM_increment(this: &mut ImpactVM) {
    // serialized version of:
    //
    // dup 0
    // idx [<[-]: f>]
    // addi 1
    // pushs <[-]: f>
    // swap 0
    // ins 1
    let program = [
        0u8, 6, 0, 0, 0, 2, 2, 48, 2, 2, 80, 64, 65, 2, 2, 14, 1, 2, 2, 17, 64, 65, 2, 2, 64, 2, 2,
        145,
    ];

    unsafe {
        let mut out = std::ptr::null::<()>();
        assert!(
            run_program(
                program.as_ptr(),
                program.len(),
                this.current_state,
                &mut out as *mut _
            ) == 0
        );

        assert!(!out.is_null());

        free_state(this.current_state);

        this.current_state = out;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_ImpactVM_get_counter(this: &mut ImpactVM) -> u32 {
    unsafe {
        let mut serialized_state_ptr = std::ptr::null::<u8>();
        let mut serialized_state_len = 0usize;

        assert_eq!(
            serialize_state(
                this.current_state,
                &mut serialized_state_ptr as *mut _,
                &mut serialized_state_len as *mut _,
            ),
            0
        );

        let buffer = std::slice::from_raw_parts(serialized_state_ptr, serialized_state_len);

        // TODO: this only works as long as the value fits in a byte. but this
        // is mostly here for debugging purposes, since it's probably not worth
        // writing bindings for deserialization as that would be done in the
        // frontend.
        let counter = buffer[23] as u32;

        free_buffer(serialized_state_ptr, serialized_state_len);

        counter
    }
}

// ----------------------------------------------------------------------------
// Coordination script
#[unsafe(no_mangle)]
pub extern "C" fn new_counter() -> defs::ImpactVM {
    defs::ImpactVM::new()
}

#[unsafe(no_mangle)]
pub extern "C" fn increase_counter(utxo: defs::ImpactVM) {
    utxo.increment();
}

#[unsafe(no_mangle)]
pub extern "C" fn get_counter(utxo: defs::ImpactVM) -> u32 {
    utxo.get_counter()
}
