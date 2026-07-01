token MyToken {
    storage {
        indexed let mut supply: u32;
    }
    mint fn mint() {
        supply = 1;
    }
    impl Token {
        fn attach(to: Utxo) { }
        fn detach(source: Utxo) { }
    }
}
