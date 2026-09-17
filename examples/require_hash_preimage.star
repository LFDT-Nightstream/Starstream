abi IRequireHashPreimage {
    // `u64` used for illustration purposes because Starstream doesn't implement `list<u8>` yet.
    fn consume(preimage: u64);

    effect HashMismatch();
}

utxo RequireHashPreimage {
    storage {
        // Tuple of 4 u64s totals 256 bits of SHA-256 digest.
        let mut _hash: tuple<u64, u64, u64, u64>;
    }

    main fn create(hash: tuple<u64, u64, u64, u64>) {
        _hash = hash;
        yield(IRequireHashPreimage);
        // This point is reached once `resume;` is reached which requires the
        // preimage passed to `fn consume` to be correct.
    }

    impl IRequireHashPreimage {
        fn consume(preimage: u64) {
            let hash = sha256(preimage);
            if hash != _hash {
                raise HashMismatch();
            }
            resume;
        }
    }
}

script fn create_hash(input: u64) -> RequireHashPreimage {
    RequireHashPreimage::create(sha256(input))
}

script fn consume_hash(utxo: RequireHashPreimage, input: u64) {
    utxo.consume(input)
}
