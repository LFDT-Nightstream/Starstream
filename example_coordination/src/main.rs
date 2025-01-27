#![no_std]
#![no_main]

use example_contract::{MyMain, MyMainExt};
use starstream::{PublicKey, Utxo};

extern "C" fn my_effect_handler(supply: u32) {
    starstream::log(100 + supply);
}

// This is the tap that makes this freely mintable.
#[no_mangle]
pub fn mint_star(owner: PublicKey, amount: u64) {
    //MyMain::new(amount)

}

// Split and combine functions are always relevant.
pub fn star_combine(first: StarToken, second: StarToken) {
    // TODO: assert that this TX has a signature from first.get_owner()
    assert!(first.get_owner() == second.get_owner());
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
