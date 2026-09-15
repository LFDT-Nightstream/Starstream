abi Foo {
    event Hello(one: i32, two_two: i32);
}

script fn main() {
    emit Hello(1, 2);
}

test "Simple Emit" {
    emit Hello(13, 17);
}

test "Call Main" {
    main();
}
