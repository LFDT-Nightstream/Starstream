abi FooAbi {
    event HelloEvent();
}

utxo MyUtxo {
    main fn hello_utxo() {
        emit HelloEvent();
    }
}
