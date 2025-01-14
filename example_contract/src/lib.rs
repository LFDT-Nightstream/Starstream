#![feature(coroutines, coroutine_trait, extern_types, never_type)]
#![no_std]

use starstream::{Utxo, UtxoCoroutine};

// "starstream:example_contract" should probably be something content-addressed
#[link(wasm_import_module = "starstream:example_contract")]
unsafe extern "C" {
    pub type MyMain;
    safe fn MyMain_status(utxo: &Utxo<MyMain>) -> bool;
    safe fn MyMain_resume(utxo: &mut Utxo<MyMain>, arg: <MyMain as UtxoCoroutine>::Resume);
    safe fn MyMain_main_new() -> Utxo<MyMain>;
    safe fn MyMain_effect_get_supply(utxo: &Utxo<MyMain>) -> u32;
}

impl UtxoCoroutine for MyMain {
    type Resume = ();

    #[inline]
    unsafe fn ffi_status(utxo: &Utxo<Self>) -> bool {
        MyMain_status(utxo)
    }

    #[inline]
    unsafe fn ffi_resume(utxo: &mut Utxo<Self>, arg: ()) {
        MyMain_resume(utxo, arg)
    }
}

impl MyMain {
    #[inline]
    pub fn new() -> Utxo<MyMain> {
        MyMain_main_new()
    }
}

pub trait MyMainExt {
    fn get_supply(&self) -> u32;
}

impl MyMainExt for Utxo<MyMain> {
    #[inline]
    fn get_supply(&self) -> u32 {
        MyMain_effect_get_supply(self)
    }
}
