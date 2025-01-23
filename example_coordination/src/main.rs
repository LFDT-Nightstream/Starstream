#![no_std]
#![no_main]

use example_contract::{MyMain, MyMainExt};
use starstream::Utxo;

extern "C" fn my_effect_handler(supply: u32) {
    starstream::log(100 + supply);
}

#[no_mangle]
pub fn produce() {
    // All UTXOs that aren't exhausted are implicitly part of the output.
    MyMain::handle_my_effect(|| {
        _ = MyMain::new();
    }, my_effect_handler);
    // ^ not pretty but it illustrates the implementation
}

#[no_mangle]
pub fn consume(mut utxo: Utxo<MyMain>) {
    utxo.get_supply();
    utxo.next();
}
