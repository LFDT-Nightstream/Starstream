abi Foo {
    effect Basic();
    effect Complex(in: u32) -> u32;
    event Log(point: u32);
}

fn inner() {
    raise Basic();
    emit Log(raise Complex(5));
}

script fn main() {
    try {
        inner();
    } with Basic() {
        emit Log(1);
    } with Complex(in) {
        in * 13
    }
}

test "Test" {
    main()
}

test "Unhandled" {
    raise Basic();
}
