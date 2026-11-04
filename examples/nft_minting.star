contract;

token Nft {
    storage {
        let mut id: u16;
    }

    mint fn create<T>(pub identifier: u16) where T: CoroutineId<Collection> {
        assert(raise GetUtxoCaller() == T::Id());

        id = identifier;
    }

    impl Token {
        fn attach(to: Utxo) { }
        fn detach(source: Utxo) { }
    }
}

// coroutine_id is an instance of Collection
utxo Collection {
    storage {
        let mut total: u16;
        let mut count: u16;
    }

    main fn create(pub t: u16) {
        total = t;

        yield();
    }

    pub fn mint_nft() -> Nft {
        if (count >= 10000) {
            resume;
        }

        count = count + 1;

        Nft::create::<Self::CoroutineId>(count)
    }
}

script fn initialize(total: u16) -> Collection {
    Collection::create(total)
}

script fn mint(destination: Attach<Nft>, collection: Collection, amount: u16) {
    let mut start = 0;

    // TODO: add collection types
    while (start < amount) {
        collection.mint_nft().attach(destination);

        start = start + 1;
    }
}
