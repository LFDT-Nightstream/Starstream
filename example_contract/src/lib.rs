#![feature(coroutines, coroutine_trait)]
#![no_std]
#![no_main]

use stack_dst::{buffers::ConstArrayBuf, stack, Value};
use starstream::{Utxo};
use core::{ops::Coroutine, pin::{pin, Pin}};

static mut SUPPLY: u32 = 0;

#[no_mangle]
pub fn create_infinite_mint() -> ! {
    //let mut supply = 0;
    loop {
        unsafe { SUPPLY += 1; }
        // The UTXO is of type Token.
        starstream::yield_::<(), _>(());
    }
    // spent!
}

pub struct InfiniteToken {
    supply: u32,
}

impl InfiniteToken {
    pub fn get_supply(&self) -> u32 {
        self.supply
    }
}

impl InfiniteToken {
    pub fn next(this: Utxo<Self>) -> InfiniteToken {
        //starstream::resume(utxo, ()).unwrap
        unimplemented!()
    }
}
