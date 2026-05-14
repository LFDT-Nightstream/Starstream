abi Foo {
    fn foo();
}

utxo MyUtxo {
    impl Foo {
        fn foo() { }
    }
    main fn hello_utxo() {
        yield(Foo);
    }
}
