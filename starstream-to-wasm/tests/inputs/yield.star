abi Foo {
    fn foo();
}

utxo MyUtxo {
    main fn hello_utxo() {
        yield(Foo);
    }
    impl Foo {
        fn foo() {
            resume;
        }
    }
}
