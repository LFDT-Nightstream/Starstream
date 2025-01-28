#![no_std]
#![no_main]

use example_contract::{MyMain, MyMainExt, StarToken, StarTokenExt};
use starstream::{PublicKey, Utxo};

extern "C" fn my_effect_handler(supply: u32) {
    starstream::log(100 + supply);
}

// This is the tap that makes this freely mintable.
#[no_mangle]
pub extern "C" fn star_mint(owner: PublicKey, amount: u64) -> Utxo<StarToken> {
    StarToken::new(owner, amount)
}

// Split and combine functions are always relevant.
#[no_mangle]
pub extern "C" fn star_combine(first: Utxo<StarToken>, second: Utxo<StarToken>) -> Utxo<StarToken> {
    // TODO: assert that this TX has a signature from first.get_owner()
    let owner = first.get_owner();
    let first_amount = first.get_amount();
    let second_amount = second.get_amount();
    assert!(owner == second.get_owner());
    // ^ or maybe it's also OK for them to be different if the TX has a signature from second.get_owner() ???
    let total = first_amount.checked_add(second_amount).unwrap();
    first.resume(first_amount);
    second.resume(second_amount);
    StarToken::new(owner, total)
}

#[no_mangle]
pub extern "C" fn star_split(from: Utxo<StarToken>, amount: u64) -> Utxo<StarToken> {
    let owner = from.get_owner();
    from.resume(amount);
    // if amount was the max, from is dead now, so we can't call get_owner after
    StarToken::new(owner, amount)
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
pub fn consume(utxo: Utxo<MyMain>) {
    utxo.get_supply();
    utxo.next();
}
