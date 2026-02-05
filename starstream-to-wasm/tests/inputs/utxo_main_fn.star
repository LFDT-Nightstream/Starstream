abi Foo {
    event Hello();
}

utxo MyUtxo {
    main fn hello() {
        emit Hello();
    }
}
