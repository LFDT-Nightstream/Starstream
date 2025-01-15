#![no_std]
#![no_main]

use example_contract::{MyMain, MyMainExt};
use starstream::Utxo;

#[no_mangle]
pub fn produce() {
    // All UTXOs that aren't exhausted are implicitly part of the output.
    _ = MyMain::new();
}

#[no_mangle]
pub fn consume(mut utxo: Utxo<MyMain>) {
    utxo.get_supply();
    utxo.next();
}
