#![no_std]
#![no_main]
#![allow(dead_code)]

use example_contract_permissioned::{PermissionedToken, TokenIntermediate};
use starstream::{
    Effect, EffectHandler, Token, TokenStorage, Utxo, eprintln, get_tokens_in_current_utxo,
    run_effectful_computation, token_export,
};

starstream::panic_handler!();

const PERMISSIONED_TOKEN_ID: u64 = 1003;

// this should be an array, but passing those to the coordination script is not
// possible now so let's use plain numbers for now.
type PublicKey = i32;

pub struct PayToPublicKeyHash {
    owner: PublicKey,
}

#[link(wasm_import_module = "starstream_utxo_env")]
unsafe extern "C" {
    unsafe fn starstream_get_tokens(data: *mut (), data_len: u32, skip: u32) -> u32;
}

impl PayToPublicKeyHash {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(owner: PublicKey, sleep: fn(&mut Self)) {
        let mut this = PayToPublicKeyHash { owner };

        sleep(&mut this);

        // TODO: assert signature so that only the owner can consume this

        let tokens_iter = get_tokens_in_current_utxo();

        for (handle, _) in tokens_iter {
            // TODO: this should be checked, but currently it's not really
            // possible to figure out what type of token this is.
            //
            // We could look at the id in the token storage, but for ntfs
            // the id should be different even if they have the same type of
            // intermediate?
            let intermediate =
                run_effectful_computation(EffectHandler::<CallerOwner>::with(&|_| owner), || {
                    PermissionedToken::from(handle.unsafe_coerce::<PermissionedToken>()).unbind()
                });

            // TODO: maybe the unbind should do this by default?
            // it doesn't really give you linearity since the handler could just
            // not do anything with the value, but at least it should guarantee
            // you have a handler.
            TokenUnbound::raise(&intermediate);
        }
    }

    pub fn get_owner(&self) -> PublicKey {
        self.owner
    }

    // TODO: generalize
    pub fn attach(&mut self, i: TokenIntermediate) {
        run_effectful_computation(EffectHandler::<CallerOwner>::with(&|_| self.owner), || {
            PermissionedToken::bind(i);
        });
    }

    pub fn burn(self) {}
}

pub struct LinkedListNode {
    key: PublicKey,
    next: PublicKey,
}

impl LinkedListNode {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(key: PublicKey, next: PublicKey, sleep: fn(&mut Self)) {
        let mut this = LinkedListNode { key, next };
        sleep(&mut this);
    }

    pub fn get_key(&self) -> PublicKey {
        self.key
    }

    pub fn get_next(&self) -> PublicKey {
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
    i: TokenIntermediate,
) {
    this.attach(i)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_new_LinkedListNode_new(key: PublicKey, next: PublicKey) {
    LinkedListNode::new(key, next, starstream::sleep_mut::<(), LinkedListNode>)
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_LinkedListNode_get_next(this: &LinkedListNode) -> PublicKey {
    this.next
}

#[unsafe(no_mangle)]
pub extern "C" fn starstream_query_LinkedListNode_get_key(this: &LinkedListNode) -> PublicKey {
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
pub extern "C" fn transfer_permissioned_token(
    source: example_contract_permissioned::PayToPublicKeyHash,
    proof_from: example_contract_permissioned::LinkedListNode,
    proof_to: example_contract_permissioned::LinkedListNode,
    to: PublicKey,
    to_amount: i32,
) -> example_contract_permissioned::PayToPublicKeyHash {
    let is_blacklisted_handler = |address| {
        let res1 = is_in_range(proof_from, address);
        let res2 = is_in_range(proof_to, address);

        res1 || res2
    };

    let from = source.get_owner();

    // Note: right now there can only be a single token type in here.
    //
    // Otherwise we'd need to also know the value of the other tokens.
    let input_amount = core::cell::RefCell::new(0);

    run_effectful_computation(
        (
            EffectHandler::<TokenUnbound>::with(&|token| {
                *input_amount.borrow_mut() += token.amount
            }),
            EffectHandler::<IsBlacklisted>::with(&is_blacklisted_handler),
        ),
        || {
            // TODO: this should probably yield the tokens, but currently it's not easy
            // to yield something that it's not the utxo handler, so we use an effect
            // instead.
            //
            // although maybe we just need a different function call to get the tokens
            // of a dead utxo? Or maybe unbind should always raise an effect?
            source.next();
        },
    );

    let input_amount = *input_amount.borrow();

    let output_utxo = example_contract_permissioned::PayToPublicKeyHash::new(to);
    let output_amount = to_amount;

    let output_intermediate = example_contract_permissioned::TokenIntermediate {
        amount: output_amount,
    };

    run_effectful_computation(
        EffectHandler::<IsBlacklisted>::with(&is_blacklisted_handler),
        || {
            output_utxo.attach(output_intermediate);
        },
    );

    let change_utxo = example_contract_permissioned::PayToPublicKeyHash::new(from);
    let change_intermediate = example_contract_permissioned::TokenIntermediate {
        amount: input_amount
            .checked_sub(output_amount)
            .expect("not enough inputs"),
    };

    run_effectful_computation(
        EffectHandler::<IsBlacklisted>::with(&is_blacklisted_handler),
        || {
            change_utxo.attach(change_intermediate);
        },
    );

    output_utxo
}

fn is_in_range(proof: example_contract_permissioned::LinkedListNode, addr: PublicKey) -> bool {
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
    // TODO: assert that the transaction is signed by someone with permissions to create nodes
    let key = 0;
    let next = PublicKey::MAX;

    example_contract_permissioned::LinkedListNode::new(key, next)
}

#[unsafe(no_mangle)]
pub extern "C" fn blacklist_insert(
    prev: example_contract_permissioned::LinkedListNode,
    new: PublicKey,
) -> example_contract_permissioned::LinkedListNode {
    // TODO: assert that the transaction is signed by someone with permissions to create nodes
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
) -> PublicKey {
    prev.get_key()
}

#[unsafe(no_mangle)]
pub extern "C" fn token_mint_new() -> example_contract_permissioned::TokenMint {
    example_contract_permissioned::TokenMint::new()
}

#[unsafe(no_mangle)]
pub extern "C" fn token_mint_to(
    minter: example_contract_permissioned::TokenMint,
    owner: PublicKey,
    amount: i32,
    proof: example_contract_permissioned::LinkedListNode,
) -> example_contract_permissioned::PayToPublicKeyHash {
    run_effectful_computation::<_, _>(
        EffectHandler::<IsBlacklisted>::with(&|to_address| is_in_range(proof, to_address)),
        || {
            let out = example_contract_permissioned::PayToPublicKeyHash::new(owner);
            let intermediate = minter.mint(amount);
            out.attach(intermediate);

            out
        },
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn pay_to_public_key_hash_owner(
    utxo: example_contract_permissioned::PayToPublicKeyHash,
) -> PublicKey {
    utxo.get_owner()
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
    let owner = CallerOwner::raise(&());

    let is_blacklisted_output = IsBlacklisted::raise(&owner);

    assert!(
        is_blacklisted_output,
        "tried to bind token to blacklisted utxo (owner)"
    );

    TokenStorage {
        id: PERMISSIONED_TOKEN_ID,
        amount: this.amount.try_into().unwrap(),
    }
}

fn starstream_unbind_token_inner(storage: TokenStorage) -> TokenIntermediate {
    let owner = CallerOwner::raise(&());

    let is_blacklisted_output = IsBlacklisted::raise(&owner);

    assert!(
        is_blacklisted_output,
        "tried to unbind token from blacklisted utxo (owner)"
    );

    TokenIntermediate {
        amount: storage.amount.try_into().unwrap(),
    }
}

token_export! {
    for TokenIntermediate;
    bind fn starstream_bind_Token(this: Self) -> TokenStorage {
        assert!(starstream::coordination_code() == starstream::this_code());
        starstream_bind_token_inner(this)
    }
    unbind fn starstream_unbind_Token(storage: TokenStorage) -> Self {
        starstream_unbind_token_inner(storage)
    }
}

// Effects
//
pub enum IsBlacklisted {}

impl Effect for IsBlacklisted {
    const NAME: &'static str = "IsBlacklisted";

    type Input = PublicKey;
    type Output = bool;
}

#[unsafe(no_mangle)]
pub extern "C" fn IsBlacklisted_handle(this: &EffectHandler<'_, IsBlacklisted>) {
    this.handle();
}

pub enum TokenUnbound {}

impl Effect for TokenUnbound {
    const NAME: &'static str = "TokenUnbound";

    type Input = TokenIntermediate;
    type Output = ();
}

#[unsafe(no_mangle)]
pub extern "C" fn TokenUnbound_handle(this: &EffectHandler<'_, TokenUnbound>) {
    this.handle();
}

pub enum CallerOwner {}

impl Effect for CallerOwner {
    const NAME: &'static str = "CallerOwner";

    type Input = ();
    type Output = PublicKey;
}

#[unsafe(no_mangle)]
pub extern "C" fn CallerOwner_handle(this: &EffectHandler<'_, CallerOwner>) {
    this.handle();
}
