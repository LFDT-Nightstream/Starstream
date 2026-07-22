abi Foo {
    effect Hello();
    event Goodbye();
}

script fn main() {
    try {
        raise Hello();
    } with Hello() {
        emit Goodbye();
    }
}
