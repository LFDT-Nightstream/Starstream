abi Foo {
    event Hello(one: i32, two: i32);
}

script fn main() {
    emit Hello(1, 2);
}
