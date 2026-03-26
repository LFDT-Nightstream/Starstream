abi FooAbi {
    event HelloEvent();
}

utxo MyUtxo {
    fn private_fn() {
        emit HelloEvent();
    }
    main fn hello_utxo() {
        private_fn();
    }
}
