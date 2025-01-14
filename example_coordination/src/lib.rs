#![no_std]
#![no_main]

use example_contract::InfiniteToken;
use starstream::Utxo;

#[no_mangle]
pub fn main(mut token_utxo: Utxo<InfiniteToken>) {
    let value = starstream::resume(&mut token_utxo, ());
    token_utxo.get_supply();
}
