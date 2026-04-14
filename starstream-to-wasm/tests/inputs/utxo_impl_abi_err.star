abi MyAbi {
    fn hello();
}

utxo MyUtxo {
    impl MyAbi {
        fn goodbye() { }
    }
}

utxo MyUtxo2 {
    impl MyAbi { }
}

utxo MyUtxo3 {
    impl MyAbi {
        fn hello(x: i32) { }
    }
}

utxo MyUtxo4 {
    impl MyAbi {
        fn hello() -> i32 {
            17
        }
    }
}
