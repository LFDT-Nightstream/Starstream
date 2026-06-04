abi Foo {
    fn foo();
}

utxo MyUtxo {
    main fn hello_utxo(x: i32) {
        yield(Foo);
    }
    impl Foo {
        fn foo() {
            resume;
        }
    }
}
