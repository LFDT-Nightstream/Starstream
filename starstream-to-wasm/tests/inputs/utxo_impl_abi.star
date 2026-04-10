abi MyAbi {
    fn hello() -> i32;
}

utxo MyUtxo {
    impl MyAbi {
        fn hello() -> i32 {
            17
        }
    }
}
