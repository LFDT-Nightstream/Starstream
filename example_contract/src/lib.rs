#![feature(extern_types)]
#![no_std]

use starstream::{Utxo, UtxoCoroutine};

// "starstream:example_contract" should probably be something content-addressed
#[link(wasm_import_module = "starstream:example_contract")]
unsafe extern "C" {
    pub type MyMain;
    safe fn starstream_status_MyMain(utxo: Utxo<MyMain>) -> bool;
    safe fn starstream_resume_MyMain(utxo: Utxo<MyMain>, arg: <MyMain as UtxoCoroutine>::Resume);
    safe fn starstream_new_MyMain_new() -> Utxo<MyMain>;
    safe fn starstream_effect_MyMain_get_supply(utxo: Utxo<MyMain>) -> u32;
}

impl UtxoCoroutine for MyMain {
    type Resume = ();

    #[inline]
    unsafe fn ffi_status(utxo: Utxo<Self>) -> bool {
        starstream_status_MyMain(utxo)
    }

    #[inline]
    unsafe fn ffi_resume(utxo: Utxo<Self>, arg: ()) {
        starstream_resume_MyMain(utxo, arg)
    }
}

impl MyMain {
    #[inline]
    pub fn new() -> Utxo<MyMain> {
        starstream_new_MyMain_new()
    }
}

pub trait MyMainExt {
    fn get_supply(self) -> u32;
}

impl MyMainExt for Utxo<MyMain> {
    #[inline]
    fn get_supply(self) -> u32 {
        starstream_effect_MyMain_get_supply(self)
    }
}
