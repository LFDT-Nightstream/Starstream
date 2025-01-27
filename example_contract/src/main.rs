#![no_std]
#![no_main]

use starstream::PublicKey;

// fn foo(_: A, _: B, sleep: fn(Yield) -> (E, F)) -> Yield
// entry point name: "foo"
// init args: A, B
// yield: C, D
// ^ the resume result determines the available effect handlers
// resume: E, F
// terminate result: G, H
// ^ can still call stuff on these maybe

// init args are unconstrained
// resume result should be a single struct w/ methods on it
// yield = terminate

//#[starstream::event]
//fn my_event(supply: u32);
// expands to:
unsafe extern "C" {
    #[link_name = "starstream_event_my_event"]
    safe fn my_event(supply: u32);
}

//#[starstream::effect]
//fn my_effect(supply: u32);
unsafe extern "C" {
    #[link_name = "starstream_effect_my_effect"]
    safe fn my_effect(supply: u32);
}

//#[starstream::error]
//fn my_error(supply: u32);
// expands to:
unsafe extern "C" {
    #[link_name = "starstream_error_my_error"]
    safe fn my_error(supply: u32);
}

// This is kind of a cheap UTXO that is meant to function like a "Star" token.
// This isn't how tokens are planned to be represented in the final design.
pub struct StarToken {
    owner: PublicKey,
    amount: u64,
}

impl StarToken {
    pub fn new(owner: PublicKey, mut amount: u64, sleep: fn(&StarToken) -> u64) {
        // resume arg is the amount to subtract from this token
        while amount > 0 {
            let subtract = sleep(&StarToken { owner, amount });
            amount = amount.checked_sub(subtract).unwrap();
        }
    }

    pub fn get_owner(&self) -> PublicKey {
        self.owner
    }

    pub fn get_amount(&self) -> u64 {
        self.amount
    }
}

pub struct MyMain {
    supply: u32,
}

impl MyMain {
    //#[starstream::new]
    pub fn new(sleep: fn(&MyMain)) {
        let mut supply = 0;
        loop {
            supply += 1;
            my_event(supply);
            starstream::log(10 + supply);
            my_effect(supply);
            starstream::log(20 + supply);
            //my_error(supply);
            sleep(&MyMain { supply });
        }
    }

    //#[starstream::query]
    pub fn get_supply(&self) -> u32 {
        self.supply
    }
}

// ----------------------------------------------------------------------------
// Generated

#[no_mangle]
pub extern "C" fn starstream_new_MyMain_new() {
    MyMain::new(starstream::sleep::<(), MyMain>)
}

#[no_mangle]
pub extern "C" fn starstream_query_MyMain_get_supply(this: &MyMain) -> u32 {
    this.get_supply()
}

#[no_mangle]
pub extern "C" fn starstream_new_StarToken_new(owner: PublicKey, amount: u64) {
    StarToken::new(owner, amount, starstream::sleep::<u64, StarToken>)
}

#[no_mangle]
pub extern "C" fn starstream_query_StarToken_get_owner(this: &StarToken) -> PublicKey {
    this.get_owner()
}

#[no_mangle]
pub extern "C" fn starstream_query_StarToken_get_amount(this: &StarToken) -> u64 {
    this.get_amount()
}
