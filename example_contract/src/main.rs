#![no_std]
#![no_main]

use example_contract::StarTokenExt;
use starstream::{PublicKey, Utxo};

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

#[repr(C)]
struct StarTokenIntermediate {
    pub amount: u64,
}

impl StarTokenIntermediate {
    pub fn mint(self, owner: PublicKey) -> Utxo<example_contract::StarToken> {
        // assert that current coordination script is the right one
        example_contract::StarToken::new(owner, self.amount)
    }
}

impl Drop for StarTokenIntermediate {
    fn drop(&mut self) {
        // simulation of linear type
        if self.amount > 0 {
            panic!("not allowed to drop this");
        }
    }
}

// This is kind of a cheap UTXO that is meant to function like a "Star" token.
// This isn't how tokens are planned to be represented in the final design.
pub struct StarToken {
    owner: PublicKey,
    amount: u64,
}

impl StarToken {
    /* (owner, amount) is like the intermediate */
    pub fn new(owner: PublicKey, mut amount: u64, sleep: fn(&mut StarToken)) {
        // This is the "mint" section where we can make permissioned assertions.
        // Non-builtin effects we can call here are part of our function
        // signature and must be handled by code that attempts to mint us.
        assert!(starstream::coordination_code() == starstream::this_code());

        let mut this = StarToken { owner, amount };

        // Token must always end with while(true) yield;
        // so that tokens don't have to actually have their stacks suspended.
        loop {
            sleep(&mut this);
        }
    }

    pub fn get_owner(&self) -> PublicKey {
        self.owner
    }

    pub fn get_amount(&self) -> u64 {
        self.amount
    }

    pub fn burn(self) -> u64 {
        // This is the "burn" section where we can make permissioned assertions.
        // Non-builtin effects we can call here are part of our function
        // signature and must be handled by code that attempts to burn us.
        assert!(starstream::coordination_code() == starstream::this_code());

        /*StarTokenIntermediate {
            amount:*/ self.amount
        //}
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

pub struct StarNft {
    supply: u64,
}

impl StarNft {
    pub fn new(
        sleep: fn(&StarNft),
    ) {
        let mut this = StarNft { supply: 0 };
        loop {
            // "true" is a stand-in for an actual NFT token, representation TBD
            sleep(&this);
            this.supply += 1;
        }
    }

    pub fn get_supply(&self) -> u64 {
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
    StarToken::new(owner, amount, starstream::sleep_mut::<(), StarToken>)
}

#[no_mangle]
pub extern "C" fn starstream_query_StarToken_get_owner(this: &StarToken) -> PublicKey {
    this.get_owner()
}

#[no_mangle]
pub extern "C" fn starstream_query_StarToken_get_amount(this: &StarToken) -> u64 {
    this.get_amount()
}

#[no_mangle]
pub unsafe extern "C" fn starstream_consume_StarToken_burn(this: *mut StarToken) -> u64 {
    core::ptr::read(this).burn()
}

#[no_mangle]
pub extern "C" fn starstream_new_StarNft_new() {
    StarNft::new(starstream::sleep::<(), StarNft>)
}

#[no_mangle]
pub extern "C" fn starstream_query_StarNft_get_supply(this: &StarNft) -> u64 {
    this.get_supply()
}

// ----------------------------------------------------------------------------
// Coordination script

#[no_mangle]
pub extern "C" fn star_mint(owner: PublicKey, amount: u64) -> Utxo<example_contract::StarToken> {
    // This fulfills StarToken's mint requirement that its `new` is called from
    // THIS FILE, so this function becomes a freely callable tap.
    example_contract::StarToken::new(owner, amount)
}

#[no_mangle]
pub extern "C" fn star_combine(first: Utxo<example_contract::StarToken>, second: Utxo<example_contract::StarToken>) -> Utxo<example_contract::StarToken> {
    let owner = first.get_owner();
    starstream::assert_call_signature(owner);
    assert!(owner == second.get_owner());
    // ^ or maybe it's also OK for them to be different if the TX has a signature from second.get_owner() ???
    let first_amount = first.burn();
    let second_amount = second.burn();
    let total = first_amount.checked_add(second_amount).unwrap();
    // ^ panicking here will roll back the burns
    // v fulfills StarToken's mint requirement
    example_contract::StarToken::new(owner, total)
}

#[no_mangle]
pub extern "C" fn star_split(from: Utxo<example_contract::StarToken>, amount: u64) -> Utxo<example_contract::StarToken> {
    let owner = from.get_owner();
    starstream::assert_call_signature(owner);
    // TODO: sucking out of
    let mut from_amount = from.burn();
    from_amount = from_amount.checked_sub(amount).unwrap();
    // TODO: shouldn't this line
    example_contract::StarToken::new(owner, from_amount);
    example_contract::StarToken::new(owner, amount)
}
