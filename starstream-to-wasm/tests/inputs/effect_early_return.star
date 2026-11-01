abi Foo {
    effect Hello();
}

test "Test" {
    try {
        return;
        raise Hello();
    } with Hello() { }
}
