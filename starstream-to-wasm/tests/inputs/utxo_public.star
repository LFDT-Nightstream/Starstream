abi Extra {
    fn extra();
}

abi Observations {
    event Value(value: i64);
}

utxo Counter {
    storage {
        let mut count: i64;
    }

    pub fn advance() {
        resume;
    }

    main fn new() {
        count = 10;
        yield();
        count = count + 100;
        yield(Extra);
        count = count + 1000;
        yield();
    }

    main fn other() {
        count = 40;
        if (true) {
            yield();
        } else {
            yield(Extra);
        }
    }

    pub fn add(x: i64) -> i64 {
        count = disclose(count + x);
        count
    }

    impl Extra {
        fn extra() { }
    }
}

test "Public methods at an empty yield" {
    let counter = Counter::new();
    emit Value(counter.add(5));
}

test "Public methods alongside an explicit ABI" {
    let counter = Counter::new();
    counter.advance();
    emit Value(counter.add(7));
}

test "Storage updates and public resume across three yields" {
    let counter = Counter::new();
    emit Value(counter.add(5));
    counter.advance();
    emit Value(counter.add(7));
    counter.advance();
    emit Value(counter.add(8));
}

test "Public methods in another constructor's nested yield" {
    let counter = Counter::other();
    emit Value(counter.add(2));
}

test "Constructor call chaining" {
    emit Value(Counter::new().add(5));
}
