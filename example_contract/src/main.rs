#![no_std]
#![no_main]
#![allow(dead_code)]
#![feature(coroutines, coroutine_trait)]

use core::ops::Coroutine;

use example_contract::{StarNft, StarNftIntermediate};
use starstream::{
    PublicKey, Token, TokenStorage, Utxo, assert_tx_signed_by, eprintln, token_export,
};

starstream::panic_handler!();

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

/*
//#[starstream::event]
//fn my_event(supply: u32);
// expands to:
unsafe extern "C" {
    #[link_name = "starstream_event_my_event"]
    safe fn my_event(supply: u32);
}

//#[starstream::error]
//fn my_error(supply: u32);
// expands to:
unsafe extern "C" {
    #[link_name = "starstream_error_my_error"]
    safe fn my_error(supply: u32);
}
*/

pub struct PayToPublicKeyHash {
    owner: PublicKey,
}

impl PayToPublicKeyHash {
    pub fn new(owner: PublicKey, sleep: fn(&mut Self)) {
        // It's currently the TX where the UTXO is created.
        let mut this = PayToPublicKeyHash { owner };
        sleep(&mut this);
        // Now it's the TX where someone has requested to consume the UTXO.
        // They are allowed to do that if that TX is signed by the owner we
        // started with.
        assert_tx_signed_by(owner);
        // When the UTXO's lifetime ends, all its tokens are freed up, and then
        // the calling coordination script must either put them directly into
        // another UTXO or burn them according to that token's code, or else
        // the TX will fail.
    }

    pub fn get_owner(&self) -> PublicKey {
        self.owner
    }

    // Any token can be attached to PayToPublicKeyHash.
    pub fn attach<T: Token>(&mut self, i: T::Intermediate) {
        T::bind(i);
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
    pub fn new(owner: PublicKey, amount: u64, sleep: fn(&mut StarToken)) {
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
        amount:*/
        self.amount
        //}
    }
}

pub struct MyMain {
    supply: u32,
}

impl MyMain {
    //#[starstream::new]
    pub fn new(sleep: fn(&MyMain)) {
        let mut supply = 0u32;
        loop {
            supply += 1;
            //my_event(supply);
            eprintln!("MyMain says supply = {supply}");
            //example_contract::MyEffect::raise(supply);
            //my_error(supply);
            sleep(&MyMain { supply });
        }
    }

    //#[starstream::query]
    pub fn get_supply(&self) -> u32 {
        self.supply
    }
}

pub struct StarNftMint {
    supply: u64,
}

impl StarNftMint {
    // `sleep` is supplied by the scheduler and suspends execution.
    pub fn new(max_supply: u64, sleep: fn(&mut StarNftMint)) {
        let mut this = StarNftMint { supply: 0 };
        while this.supply < max_supply {
            // While sleeping, get_supply and mint can be called.
            sleep(&mut this);
            // Code here can control the .resume() function of the UTXO, which
            // can mutate it, equivalent to consuming the UTXO and producing
            // the "next" UTXO state.
        }
        // When we hit here, the lifetime of the StarNftMint UTXO ends, but the
        // StarNft tokens attached to PayToPublicKeyHash UTXOs remain valid.
    }

    pub fn get_supply(&self) -> u64 {
        self.supply
    }

    pub fn mint(&mut self) -> example_contract::StarNftIntermediate {
        // Note: TX is meant to fail if StarNftIntermediate is dropped without
        // being bind()ed, so supply += 1 is kept accurate.
        self.supply += 1;
        example_contract::StarNftIntermediate { id: self.supply }
    }
}

token_export! {
    for StarNftIntermediate;
    bind fn starstream_bind_StarNft(this: Self) -> TokenStorage {
        // Example of common assertion: only sanctioned coordination code
        // can bind this NFT. This indirectly enforces that only intermediates
        // produced by calls to `StarNftMint::mint` are minted.
        assert!(starstream::coordination_code() == starstream::this_code());
        TokenStorage { id: this.id, amount: 1 }
    }
    unbind fn starstream_unbind_StarNft(storage: TokenStorage) -> Self {
        assert!(starstream::coordination_code() == starstream::this_code());
        assert!(storage.amount == 1);
        StarNftIntermediate { id: storage.id }
    }
}

// ----------------------------------------------------------------------------
// Generated

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_MyMain_new() {
    MyMain::new(starstream::sleep::<(), MyMain>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_MyMain_get_supply(this: &MyMain) -> u32 {
    this.get_supply()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_StarToken_new(owner: PublicKey, amount: u64) {
    StarToken::new(owner, amount, starstream::sleep_mut::<(), StarToken>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_StarToken_get_owner(this: &StarToken) -> PublicKey {
    this.get_owner()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_StarToken_get_amount(this: &StarToken) -> u64 {
    this.get_amount()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn starstream_consume_StarToken_burn(this: *mut StarToken) -> u64 {
    unsafe { core::ptr::read(this) }.burn()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_StarNftMint_new(max_supply: u64) {
    StarNftMint::new(max_supply, starstream::sleep_mut::<(), StarNftMint>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_StarNftMint_get_supply(this: &StarNftMint) -> u64 {
    this.get_supply()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_mutate_StarNftMint_mint(
    this: &mut StarNftMint,
) -> StarNftIntermediate {
    this.mint()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_PayToPublicKeyHash_new(owner: PublicKey) {
    PayToPublicKeyHash::new(owner, starstream::sleep_mut::<(), PayToPublicKeyHash>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_PayToPublicKeyHash_get_owner(
    this: &PayToPublicKeyHash,
) -> PublicKey {
    this.get_owner()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_mutate_PayToPublicKeyHash_attach(
    this: &mut PayToPublicKeyHash,
    i: StarNftIntermediate,
) {
    this.attach::<StarNft>(i)
}

// ----------------------------------------------------------------------------
// Coordination script

#[unsafe(no_mangle)]
pub extern "C" fn star_mint(owner: PublicKey, amount: u64) -> example_contract::StarToken {
    // This fulfills StarToken's mint requirement that its `new` is called from
    // THIS FILE, so this function becomes a freely callable tap.
    example_contract::StarToken::new(owner, amount)
}

#[unsafe(no_mangle)]
pub extern "C" fn star_combine(
    first: example_contract::StarToken,
    second: example_contract::StarToken,
) -> example_contract::StarToken {
    let owner = first.get_owner();
    starstream::assert_tx_signed_by(owner);
    assert!(owner == second.get_owner());
    // ^ or maybe it's also OK for them to be different if the TX has a signature from second.get_owner() ???
    let first_amount = first.burn();
    let second_amount = second.burn();
    let total = first_amount.checked_add(second_amount).unwrap();
    // ^ panicking here will roll back the burns
    // v fulfills StarToken's mint requirement
    example_contract::StarToken::new(owner, total)
}

#[unsafe(no_mangle)]
pub extern "C" fn star_split(
    from: example_contract::StarToken,
    amount: u64,
) -> example_contract::StarToken {
    let owner = from.get_owner();
    starstream::assert_tx_signed_by(owner);
    // TODO: sucking out of
    let mut from_amount = from.burn();
    eprintln!("from_amount={from_amount} subtract={amount}");
    from_amount = from_amount.checked_sub(amount).expect("checked_sub");
    // TODO: shouldn't this line
    example_contract::StarToken::new(owner, from_amount);
    example_contract::StarToken::new(owner, amount)
}

#[unsafe(no_mangle)]
pub extern "C" fn new_nft() -> example_contract::StarNftMint {
    example_contract::StarNftMint::new(u64::MAX)
}

#[unsafe(no_mangle)]
pub extern "C" fn star_nft_mint_to(nft_contract: example_contract::StarNftMint, owner: PublicKey) {
    let out = example_contract::PayToPublicKeyHash::new(owner);
    out.attach(nft_contract.mint());
}

#[unsafe(no_mangle)]
pub extern "C" fn star_nft_mint_count(
    nft_contract: example_contract::StarNftMint,
    owner: PublicKey,
    count: u64,
) {
    for _ in 0..count {
        let out = example_contract::PayToPublicKeyHash::new(owner);
        out.attach(nft_contract.mint());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn star_nft_mint_up_to(
    // This parameter's type is a handle to a UTXO object.
    // The scheduler routes calls to the correct sleeping UTXO code instance.
    nft_contract: example_contract::StarNftMint,
    desired_supply: u64,
    owner: PublicKey,
) {
    while nft_contract.get_supply() < desired_supply {
        // In this example, we create many UTXOs with one NFT each. We could
        // just as easily create one UTXO containing all NFTs minted by this
        // call.
        example_contract::PayToPublicKeyHash::new(owner).attach(nft_contract.mint());
    }
}

/*
// Split and combine functions are always relevant.
#[unsafe(no_mangle)]
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

#[unsafe(no_mangle)]
pub extern "C" fn star_split(from: Utxo<StarToken>, amount: u64) -> Utxo<StarToken> {
    let owner = from.get_owner();
    from.resume(amount);
    // if amount was the max, from is dead now, so we can't call get_owner after
    StarToken::new(owner, amount)
}
     */

#[unsafe(no_mangle)]
pub extern "C" fn produce_and_consume() {
    let utxo = example_contract::PayToPublicKeyHash::new(PublicKey::zero());
    utxo.resume(());
}

#[unsafe(no_mangle)]
pub extern "C" fn produce_effect() {
    // All UTXOs that aren't exhausted are implicitly part of the output.
    example_contract::MyEffect::handle(
        || {
            _ = example_contract::MyMain::new();
        },
        |supply| {
            eprintln!("effect handler says supply={supply}");
            Ok(())
        },
    );
}

#[unsafe(no_mangle)]
pub extern "C" fn consume(utxo: example_contract::MyMain) {
    utxo.get_supply();
    utxo.next();
}

starstream::static_coroutine! {
    #[static_coroutine(init = stackless_init, resume = stackless_resume)]
    fn stackless(start_arg: u32) -> impl Coroutine<u16> {
        let start_arg = start_arg * 2;
        #[coroutine]
        move |resume_arg: u16| return yield resume_arg as u32 * start_arg
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn stackless_main() {
    let _stackless = example_contract::Stackless::new(17);
}
