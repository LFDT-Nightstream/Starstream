contract;

fn sha256_u64(input: u64) -> (u64, u64, u64, u64) {
    (input, 0, 0, 0)
}

abi IRequireHashPreimage {
    // `u64` used for illustration purposes because compiler doesn't implement `list<u8>` yet.
    fn consume(preimage: u64);
}

utxo RequireHashPreimage {
    storage {
        let mut _hash: (u64, u64, u64, u64);
    }

    main fn create(pub hash: (u64, u64, u64, u64)) {
        _hash = hash;
        yield(IRequireHashPreimage);
    }

    impl IRequireHashPreimage {
        fn consume(preimage: u64) {
            let hash = sha256_u64(preimage);
            // Destructure and compare elements directly because compiler doesn't implement tuple != tuple yet.
            let (a, b, c, d) = hash;
            let (e, f, g, h) = _hash;
            if (a != e || b != f || c != g || d != h) {
                error;
            }
            resume;
        }
    }
}

script fn create_hash(input: u64) -> RequireHashPreimage {
    RequireHashPreimage::create(sha256_u64(input))
}

script fn consume_hash(utxo: RequireHashPreimage, input: u64) {
    utxo.consume(input)
}

test "Succeeds when matching" {
    let utxo = create_hash(1);
    consume_hash(utxo, 1);
}

test "Fails when not matching" {
    let utxo = create_hash(1);
    consume_hash(utxo, 2);
}

test "Alive UTXO" {
    create_hash(1);
}
