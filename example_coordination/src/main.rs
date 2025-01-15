#![no_std]
#![no_main]

use example_contract::{MyMain, MyMainExt};
use starstream::Utxo;

#[no_mangle]
pub fn produce() {
    // All UTXOs that aren't exhausted are implicitly part of the output.
    let mut foo = MyMain::new();
    foo.get_supply();
    while foo.can_resume() {
        foo.next();
        foo.get_supply();
    }
}

#[no_mangle]
pub fn consume(utxo: &mut Utxo<MyMain>) {
    utxo.get_supply();

    utxo.next();
}
