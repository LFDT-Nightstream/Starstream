#![no_std]
#![no_main]
#![allow(dead_code)]

use example_contract_permissioned::{PermissionedToken, TokenIntermediate};
use starstream::{
    Token, TokenStorage, Utxo, eprintln, get_raised_effect_data, register_effect_handler,
    resume_throwing_program, token_export,
};

starstream::panic_handler!();

const IS_BLACKLISTED_EFFECT_ID: &str = "IsBlacklisted";
const TX_CALLER: &str = "GetTxCaller";

pub struct PayToPublicKeyHash {
    owner: i32,
}

impl PayToPublicKeyHash {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(owner: i32, sleep: fn(&mut Self)) {
        // It's currently the TX where the UTXO is created.
        let mut this = PayToPublicKeyHash { owner };

        sleep(&mut this);

        // TODO: assert signature so that only the owner can consume this
    }

    pub fn get_owner(&self) -> i32 {
        self.owner
    }

    // Any token can be attached to PayToPublicKeyHash.
    pub fn attach<T: Token>(&mut self, i: T::Intermediate) {
        T::bind(i);
    }

    pub fn burn(self) {}
}

pub struct LinkedListNode {
    key: i32,
    next: i32,
}

impl LinkedListNode {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(key: i32, next: i32, sleep: fn(&mut Self)) {
        let mut this = LinkedListNode { key, next };
        sleep(&mut this);
    }

    pub fn get_key(&self) -> i32 {
        self.key
    }

    pub fn get_next(&self) -> i32 {
        self.next
    }

    pub fn burn(self) {}

    // pub fn attach<T: Token>(&mut self, i: T::Intermediate) {
    //     T::bind(i);
    // }
}

// ----------------------------------------------------------------------------
// Generated

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_PayToPublicKeyHash_new(owner: i32) {
    PayToPublicKeyHash::new(owner, starstream::sleep_mut::<(), PayToPublicKeyHash>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_PayToPublicKeyHash_get_owner(this: &PayToPublicKeyHash) -> i32 {
    this.get_owner()
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_mutate_PayToPublicKeyHash_attach(
    this: &mut PayToPublicKeyHash,
    i: TokenIntermediate,
) {
    this.attach::<PermissionedToken>(i)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_LinkedListNode_new(key: i32, next: i32) {
    eprintln!("does this get called before?");
    LinkedListNode::new(key, next, starstream::sleep_mut::<(), LinkedListNode>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_LinkedListNode_get_next(this: &LinkedListNode) -> i32 {
    this.next
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_LinkedListNode_get_key(this: &LinkedListNode) -> i32 {
    this.key
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn starstream_consume_LinkedListNode_burn(this: *mut LinkedListNode) {
    unsafe { core::ptr::read(this) }.burn()
}

#[unsafe(no_mangle)]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn starstream_consume_PayToPublicKeyHash_burn(this: *mut PayToPublicKeyHash) {
    unsafe { core::ptr::read(this) }.burn()
}

// ----------------------------------------------------------------------------
// Coordination script

#[unsafe(no_mangle)]
pub extern "C" fn transfer_usdc(
    source: example_contract_permissioned::PayToPublicKeyHash,
    proof_from: example_contract_permissioned::LinkedListNode,
    proof_to: example_contract_permissioned::LinkedListNode,
    to: i32,
) {
    let _blacklisted_effect_guard = register_effect_handler(IS_BLACKLISTED_EFFECT_ID);
    let _tx_caller_effect_guard = register_effect_handler(TX_CALLER);

    let from = source.get_owner();

    // TODO: this should unbind the token, which currently doesn't happen.
    // but it's also not clear how you'd get that back?
    source.next();

    // source.burn();

    let out = example_contract_permissioned::PayToPublicKeyHash::new(to);

    // TODO: how to check this is balanced? It probably requires getting the
    // token bound to the source utxo after yielding/burning?
    // And then just asserting?

    let amount = 100;

    let intermediate = example_contract_permissioned::TokenIntermediate { amount };

    out.attach(intermediate);

    loop {
        if let Some(address) = get_raised_effect_data::<i32>(IS_BLACKLISTED_EFFECT_ID) {
            // currently unbind is not really called, so only the proof_to proof
            // matters
            let res1 = is_in_range(proof_from, address);
            let res2 = is_in_range(proof_to, address);

            resume_throwing_program::<bool>(IS_BLACKLISTED_EFFECT_ID, &(res1 || res2));
        } else if let Some(()) = get_raised_effect_data(TX_CALLER) {
            resume_throwing_program::<i32>(TX_CALLER, &from);
        } else {
            break;
        }
    }
}

fn is_in_range(proof: example_contract_permissioned::LinkedListNode, addr: i32) -> bool {
    eprintln!(
        "checking range: {} < {} < {}",
        proof.get_key(),
        addr,
        proof.get_next()
    );

    proof.get_key() < addr && addr < proof.get_next()
}

#[unsafe(no_mangle)]
pub extern "C" fn blacklist_empty() -> example_contract_permissioned::LinkedListNode {
    let key = 0;
    let next = i32::MAX;

    example_contract_permissioned::LinkedListNode::new(key, next)
}

#[unsafe(no_mangle)]
pub extern "C" fn blacklist_insert(
    prev: example_contract_permissioned::LinkedListNode,
    new: i32,
) -> example_contract_permissioned::LinkedListNode {
    let prev_next = prev.get_next();
    let prev_key = prev.get_key();

    prev.burn();

    assert!(prev_key < new);
    assert!(new < prev_next);

    example_contract_permissioned::LinkedListNode::new(prev_key, new);
    example_contract_permissioned::LinkedListNode::new(new, prev_next)
}

#[unsafe(no_mangle)]
pub extern "C" fn blacklist_node_get_key(
    prev: example_contract_permissioned::LinkedListNode,
) -> i32 {
    prev.get_key()
}

#[unsafe(no_mangle)]
pub extern "C" fn token_mint_new() -> example_contract_permissioned::TokenMint {
    example_contract_permissioned::TokenMint::new()
}

#[unsafe(no_mangle)]
pub extern "C" fn token_mint_to(
    minter: example_contract_permissioned::TokenMint,
    owner: i32,
    amount: i32,
    proof: example_contract_permissioned::LinkedListNode,
) -> example_contract_permissioned::PayToPublicKeyHash {
    let _blacklisted_effect_guard = register_effect_handler(IS_BLACKLISTED_EFFECT_ID);
    let _tx_caller_effect_guard = register_effect_handler(TX_CALLER);

    let out = example_contract_permissioned::PayToPublicKeyHash::new(owner);
    let intermediate = minter.mint(amount);
    out.attach(intermediate);

    // NOTE: this assumes that effects are eventually raised, otherwise this
    // will loop forever.
    loop {
        if let Some(to_address) = get_raised_effect_data::<i32>(IS_BLACKLISTED_EFFECT_ID) {
            let res = is_in_range(proof, to_address);
            resume_throwing_program::<bool>(IS_BLACKLISTED_EFFECT_ID, &res);
        } else if let Some(()) = get_raised_effect_data(TX_CALLER) {
            resume_throwing_program::<i32>(TX_CALLER, &owner);
        } else {
            break;
        }
    }

    out
}

// Token

pub struct TokenMint {}

impl TokenMint {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(sleep: fn(&mut TokenMint)) {
        let mut this = TokenMint {};
        loop {
            sleep(&mut this);
        }
    }

    pub fn mint(&mut self, amount: i32) -> example_contract_permissioned::TokenIntermediate {
        example_contract_permissioned::TokenIntermediate { amount }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_TokenMint_new() {
    TokenMint::new(starstream::sleep_mut::<(), TokenMint>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_mutate_TokenMint_mint(
    this: &mut TokenMint,
    amount: i32,
) -> example_contract_permissioned::TokenIntermediate {
    this.mint(amount)
}

fn starstream_bind_token_inner(this: TokenIntermediate) -> TokenStorage {
    assert!(starstream::coordination_code() == starstream::this_code());

    // TODO: what should this value be?
    let owner = starstream::raise::<(), i32>(TX_CALLER, &());

    let is_blacklisted_output = starstream::raise::<i32, bool>(IS_BLACKLISTED_EFFECT_ID, &owner);

    assert!(is_blacklisted_output);

    TokenStorage {
        id: 1003,
        amount: this.amount.try_into().unwrap(),
    }
}
token_export! {
    for TokenIntermediate;
    bind fn starstream_bind_Token(this: Self) -> TokenStorage {
        starstream_bind_token_inner(this)
    }
    unbind fn starstream_unbind_Token(storage: TokenStorage) -> Self {
        assert!(starstream::coordination_code() == starstream::this_code());
        TokenIntermediate { amount: storage.amount.try_into().unwrap() }
    }
}
