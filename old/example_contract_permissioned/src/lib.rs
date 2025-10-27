#![no_std]

use starstream::{token_import, utxo_import};

#[link(wasm_import_module = "starstream_utxo:example_contract_permissioned")]
unsafe extern "C" {
    safe fn starstream_new_PayToPublicKeyHash_new(owner: i32) -> PayToPublicKeyHash;

    safe fn starstream_mutate_PayToPublicKeyHash_attach(
        utxo: PayToPublicKeyHash,
        i: TokenIntermediate,
    );

    // # LinkedListNode
    // ## constructors
    safe fn starstream_new_LinkedListNode_new(key: i32, next: i32) -> LinkedListNode;

    // ## views
    safe fn starstream_query_LinkedListNode_get_next(utxo: LinkedListNode) -> i32;
    safe fn starstream_query_LinkedListNode_get_key(utxo: LinkedListNode) -> i32;

    safe fn starstream_query_PayToPublicKeyHash_get_owner(utxo: PayToPublicKeyHash) -> i32;

    // ## mutations
    safe fn starstream_consume_LinkedListNode_burn(utxo: LinkedListNode);

    safe fn starstream_new_TokenMint_new() -> TokenMint;
    safe fn starstream_mutate_TokenMint_mint(utxo: TokenMint, amount: i32) -> TokenIntermediate;

    safe fn starstream_consume_PayToPublicKeyHash_burn(utxo: PayToPublicKeyHash);
}

utxo_import! {
    "starstream_utxo:example_contract_permissioned";
    PayToPublicKeyHash;
    starstream_status_PayToPublicKeyHash;
    starstream_resume_PayToPublicKeyHash;
    ();
}

impl PayToPublicKeyHash {
    #[inline]
    pub fn new(owner: i32) -> Self {
        starstream_new_PayToPublicKeyHash_new(owner)
    }

    #[inline]
    // TODO: generics over the FFI boundary have to be erased somehow
    // pub fn attach<T: Token>(self, i: T::Intermediate) {
    pub fn attach(self, i: TokenIntermediate) {
        starstream_mutate_PayToPublicKeyHash_attach(self, i)
    }

    #[inline]
    pub fn get_owner(self) -> i32 {
        starstream_query_PayToPublicKeyHash_get_owner(self)
    }

    #[inline]
    pub fn burn(self) {
        starstream_consume_PayToPublicKeyHash_burn(self)
    }
}

utxo_import! {
    "starstream_utxo:example_contract_permissioned";
    LinkedListNode;
    starstream_status_LinkedListNode;
    starstream_resume_LinkedListNode;
    ();
}

impl LinkedListNode {
    #[inline]
    pub fn new(key: i32, next: i32) -> Self {
        starstream_new_LinkedListNode_new(key, next)
    }

    #[inline]
    pub fn get_key(self) -> i32 {
        starstream_query_LinkedListNode_get_key(self)
    }

    #[inline]
    pub fn get_next(self) -> i32 {
        starstream_query_LinkedListNode_get_next(self)
    }

    #[inline]
    pub fn burn(self) {
        starstream_consume_LinkedListNode_burn(self)
    }

    // #[inline]
    // // TODO: generics over the FFI boundary have to be erased somehow
    // // pub fn attach<T: Token>(self, i: T::Intermediate) {
    // pub fn attach(self, i: PermissionedTokenIntermediate) {
    //     todo!()
    // }
}

token_import! {
    from "starstream_token:example_contract_permissioned";
    type PermissionedToken;
    intermediate struct TokenIntermediate {
        pub amount: i32,
    }
    bind fn starstream_bind_Token;
    unbind fn starstream_unbind_Token;
}

utxo_import! {
    "starstream_utxo:example_contract_permissioned";
    TokenMint;
    starstream_status_TokenMint;
    starstream_resume_TokenMint;
    ();
}

impl TokenMint {
    #[inline]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        starstream_new_TokenMint_new()
    }

    #[inline]
    pub fn mint(self, amount: i32) -> TokenIntermediate {
        starstream_mutate_TokenMint_mint(self, amount)
    }
}
