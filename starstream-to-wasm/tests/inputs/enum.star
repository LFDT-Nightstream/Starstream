enum Foo {
    Bar(bool),
    Baz(i64),
}

fn accepts_foo(foo: Foo) { }

script fn get_bar() {
    accepts_foo(Foo::Bar(true));
}

script fn get_baz() {
    accepts_foo(Foo::Baz(2));
}
