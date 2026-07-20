utxo Alpha {
    storage {
        let mut a: u64;
    }
    main fn new(pub start: u64) {
        a = start;
        yield();
    }
}

utxo Beta {
    storage {
        let mut b: u64;
    }
    main fn new(pub start: u64) {
        b = start;
        yield();
    }
}

script fn use_one(x: Alpha) -> u64 {
    0
}

script fn same_type(x: Alpha, y: Alpha) -> u64 {
    0
}

script fn diff_types(p: Alpha, q: Beta) -> u64 {
    0
}
