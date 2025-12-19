utxo MyUtxo {
    storage {
        let mut counter: i64;
        let mut multiplier: i64;
    }
}

script fn increment() -> i64 {
    counter = counter + 1;
    counter * multiplier
}

script fn increment_mult() -> i64 {
    multiplier = multiplier + 1;
    counter * multiplier
}
