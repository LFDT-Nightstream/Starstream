abi MyAbi {
    fn hello() -> i32;
}

script fn maybe_hello(u: Utxo) -> i32 {
    if u is MyAbi {
        u.hello()
    } else {
        0
    }
}
