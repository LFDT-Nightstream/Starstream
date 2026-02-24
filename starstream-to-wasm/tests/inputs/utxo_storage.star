utxo MyUtxo {
    storage {
        let mut counter: i64;
        let mut multiplier: i64;
    }
    main fn increment() {
        counter = counter + 1;
    }
    main fn increment_mult() {
        multiplier = multiplier + 1;
    }
}
