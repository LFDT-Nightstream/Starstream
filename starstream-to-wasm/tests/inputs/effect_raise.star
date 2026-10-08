abi Foo {
    effect Hello();
    event Log(point: u32);
}

script fn main() {
    emit Log(1);
    try {
        emit Log(2);
        raise Hello();
        emit Log(4);
    } with Hello() {
        emit Log(3);
        resume;
    }
    emit Log(5);
}

test "Test" {
    main()
}
