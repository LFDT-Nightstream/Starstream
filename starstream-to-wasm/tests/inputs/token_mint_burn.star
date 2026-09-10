abi MyTokenEvents {
    event Test(total: u64);
}

token MyToken {
    storage {
        let mut total: u64;
    }

    mint fn my_mint() {
        total = 1;
    }

    burn fn my_burn() -> u64 {
        emit Test(total);
        total
    }

    impl Token {
        fn attach(to: Utxo) { }
        fn detach(source: Utxo) { }
    }
}

script fn call_token_mint() {
    let t: MyToken = MyToken::my_mint();
    emit Test(10);
    let ret = t.my_burn();
    emit Test(20);
    emit Test(ret);
}
