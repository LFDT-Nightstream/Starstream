abi MyTokenEvents {
    event Attached(count: u64);
    event Detached(count: u64);
}

token MyToken {
    storage {
        let mut attached: u64;
    }

    mint fn my_mint() {
        attached = 0;
    }

    impl Token {
        fn attach(to: Utxo) {
            attached = attached + 1;
            emit Attached(attached);
        }
        fn detach(source: Utxo) {
            attached = attached - 1;
            emit Detached(attached);
        }
    }
}

utxo Holder {
    main fn new() {
        yield();
    }
}

script fn move_token(source: Utxo, dest: Utxo) {
    let t = MyToken::my_mint();
    t.attach(source);
    t.detach(source);
    t.attach(dest);
}

test "Attach a token to concrete utxos" {
    move_token(Holder::new(), Holder::new());
}
