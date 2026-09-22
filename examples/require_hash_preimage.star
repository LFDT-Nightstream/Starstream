contract;

import { sha256_u64 } from "../target/wasm32-unknown-unknown/release/sha256lib.wasm";

abi IRequireHashPreimage {
    // `u64` used for illustration purposes because compiler doesn't implement `list<u8>` yet.
    fn consume(preimage: u64);
}

utxo RequireHashPreimage {
    storage {
        let mut _hash: u64;
    }

    main fn create(pub hash: u64) {
        _hash = hash;
        yield(IRequireHashPreimage);
    }

    impl IRequireHashPreimage {
        fn consume(preimage: u64) {
            let hash = sha256_u64(preimage);
            if (hash != _hash) {
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
