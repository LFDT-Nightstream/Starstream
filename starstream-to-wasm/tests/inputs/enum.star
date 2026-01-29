enum Foo {
    Bar(bool),
    Baz(i64),
}

script fn get_bar() -> Foo {
    Foo::Bar(true)
}

script fn get_baz() -> Foo {
    Foo::Baz(2)
}
