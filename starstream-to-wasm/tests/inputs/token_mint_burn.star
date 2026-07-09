token MyToken {
    storage {
        let mut total: u64;
    }
    mint fn mint() {
        total = total + 1;
    }
    burn fn burn() {
        total = total - 1;
    }
    impl Token {
        fn attach(to: Utxo) { }
        fn detach(source: Utxo) { }
    }
}
