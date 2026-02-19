utxo MyUtxo {
    storage {
        let mut counter: i64;
    }
}

script fn get() -> i64 {
    counter
}
