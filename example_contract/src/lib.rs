#![no_std]

use starstream::{token_import, utxo_import, PublicKey, UtxoHandle};

// "starstream:example_contract" should probably be something content-addressed
#[link(wasm_import_module = "starstream:example_contract")]
unsafe extern "C" {
    safe fn starstream_new_PayToPublicKeyHash_new(
        owner: PublicKey,
    ) -> UtxoHandle<PayToPublicKeyHash>;

    safe fn starstream_new_MyMain_new() -> UtxoHandle<MyMain>;
    safe fn starstream_query_MyMain_get_supply(utxo: UtxoHandle<MyMain>) -> u32;
    safe fn starstream_handle_MyMain_my_effect(handler: on_my_effect) -> on_my_effect;

    safe fn starstream_new_StarToken_new(owner: PublicKey, amount: u64) -> UtxoHandle<StarToken>;
    safe fn starstream_query_StarToken_get_owner(utxo: UtxoHandle<StarToken>) -> PublicKey;
    safe fn starstream_query_StarToken_get_amount(utxo: UtxoHandle<StarToken>) -> u64;
    safe fn starstream_consume_StarToken_burn(utxo: UtxoHandle<StarToken>) -> u64;

    safe fn starstream_new_StarNft_new() -> UtxoHandle<StarNftMint>;
    safe fn starstream_query_StarNft_get_supply(utxo: UtxoHandle<StarNftMint>) -> u64;
}

utxo_import! {
    "starstream:example_contract";
    PayToPublicKeyHash;
    starstream_status_PayToPublicKeyHash;
    starstream_resume_PayToPublicKeyHash;
    ();
}

impl PayToPublicKeyHash {
    #[inline]
    pub fn new(owner: PublicKey) -> Self {
        PayToPublicKeyHash(starstream_new_PayToPublicKeyHash_new(owner))
    }
}

utxo_import! {
    "starstream:example_contract";
    MyMain;
    starstream_status_MyMain;
    starstream_resume_MyMain;
    ();
}

impl MyMain {
    #[inline]
    pub fn new() -> UtxoHandle<Self> {
        starstream_new_MyMain_new()
    }

    pub fn handle_my_effect<R, F: FnOnce() -> R>(scope: F, handler: on_my_effect) -> R {
        let old = starstream_handle_MyMain_my_effect(handler);
        let r = scope();
        starstream_handle_MyMain_my_effect(old);
        r
    }

    #[inline]
    pub fn get_supply(self) -> u32 {
        starstream_query_MyMain_get_supply(self.0)
    }
}

#[allow(non_camel_case_types)]
pub type on_my_effect = extern "C" fn(supply: u32);

utxo_import! {
    "starstream:example_contract";
    StarToken;
    starstream_status_StarToken;
    starstream_resume_StarToken;
    ();
}

impl StarToken {
    #[inline]
    pub fn new(owner: PublicKey, amount: u64) -> Self {
        Self(starstream_new_StarToken_new(owner, amount))
    }

    #[inline]
    pub fn get_owner(self) -> PublicKey {
        starstream_query_StarToken_get_owner(self.0)
    }

    #[inline]
    pub fn get_amount(self) -> u64 {
        starstream_query_StarToken_get_amount(self.0)
    }

    #[inline]
    pub fn burn(self) -> u64 {
        starstream_consume_StarToken_burn(self.0)
    }
}

utxo_import! {
    "starstream:example_contract";
    StarNftMint;
    starstream_status_StarNftMint;
    starstream_resume_StarNftMint;
    ();
}

impl StarNftMint {
    #[inline]
    pub fn new() -> Self {
        Self(starstream_new_StarNft_new())
    }

    pub fn get_supply(self) -> u64 {
        starstream_query_StarNft_get_supply(self.0)
    }
}

token_import! {
    from "starstream:example_contract";
    type StarNft;
    intermediate struct StarNftIntermediate {
        pub id: u64,
    }
    mint fn starstream_mint_StarNft;
    burn fn starstream_burn_StarNft;
}
