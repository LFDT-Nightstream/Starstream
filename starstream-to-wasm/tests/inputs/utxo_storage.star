utxo MyUtxo {
    storage {
        let mut counter: i64;
    }
}

script fn increment() -> i64 {
    counter = counter + 1;
    counter
}
