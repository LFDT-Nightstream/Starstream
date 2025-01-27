#![feature(extern_types)]
#![no_std]

use starstream::{PublicKey, Utxo, UtxoCoroutine};

// "starstream:example_contract" should probably be something content-addressed
#[link(wasm_import_module = "starstream:example_contract")]
unsafe extern "C" {
    pub type MyMain;
    safe fn starstream_status_MyMain(utxo: Utxo<MyMain>) -> bool;
    unsafe fn starstream_resume_MyMain(
        utxo: Utxo<MyMain>,
        resume_arg: *const (),
        resume_arg_size: usize,
    );
    safe fn starstream_new_MyMain_new() -> Utxo<MyMain>;
    safe fn starstream_query_MyMain_get_supply(utxo: Utxo<MyMain>) -> u32;
    safe fn starstream_handle_MyMain_my_effect(handler: on_my_effect) -> on_my_effect;

    pub type StarToken;
    safe fn starstream_status_StarToken(utxo: Utxo<StarToken>) -> bool;
    unsafe fn starstream_resume_StarToken(
        utxo: Utxo<StarToken>,
        resume_arg: *const (),
        resume_arg_size: usize,
    );
    safe fn starstream_new_StarToken_new(owner: PublicKey, amount: u64) -> Utxo<StarToken>;
    safe fn starstream_query_StarToken_get_owner(utxo: Utxo<StarToken>) -> PublicKey;
    safe fn starstream_query_StarToken_get_amount(utxo: Utxo<StarToken>) -> u64;
}

impl UtxoCoroutine for MyMain {
    type Resume = ();

    #[inline]
    fn ffi_status(utxo: Utxo<Self>) -> bool {
        starstream_status_MyMain(utxo)
    }

    #[inline]
    fn ffi_resume(utxo: Utxo<Self>, arg: ()) {
        unsafe {
            starstream_resume_MyMain(
                utxo,
                &raw const arg as *const (),
                core::mem::size_of::<Self::Resume>(),
            )
        }
    }
}

impl MyMain {
    #[inline]
    pub fn new() -> Utxo<Self> {
        starstream_new_MyMain_new()
    }

    pub fn handle_my_effect<R, F: FnOnce() -> R>(
        scope: F,
        handler: on_my_effect,
    ) -> R {
        let old = starstream_handle_MyMain_my_effect(handler);
        let r = scope();
        starstream_handle_MyMain_my_effect(old);
        r
    }
}

pub trait MyMainExt {
    fn get_supply(self) -> u32;
}

impl MyMainExt for Utxo<MyMain> {
    #[inline]
    fn get_supply(self) -> u32 {
        starstream_query_MyMain_get_supply(self)
    }
}

#[allow(non_camel_case_types)]
pub type on_my_effect = extern "C" fn(supply: u32);

impl UtxoCoroutine for StarToken {
    type Resume = u64;

    #[inline]
    fn ffi_status(utxo: Utxo<Self>) -> bool {
        starstream_status_StarToken(utxo)
    }

    #[inline]
    fn ffi_resume(utxo: Utxo<Self>, arg: u64) {
        unsafe {
            starstream_resume_StarToken(
                utxo,
                &raw const arg as *const (),
                core::mem::size_of::<Self::Resume>(),
            )
        }
    }
}

pub trait StarTokenExt {
    fn get_owner(self) -> PublicKey;
    fn get_amount(self) -> u64;
}

impl StarToken {
    #[inline]
    pub fn new(owner: PublicKey, amount: u64) -> Utxo<Self> {
        starstream_new_StarToken_new(owner, amount)
    }
}

impl StarTokenExt for Utxo<StarToken> {
    #[inline]
    fn get_owner(self) -> PublicKey {
        starstream_query_StarToken_get_owner(self)
    }

    #[inline]
    fn get_amount(self) -> u64 {
        starstream_query_StarToken_get_amount(self)
    }
}
