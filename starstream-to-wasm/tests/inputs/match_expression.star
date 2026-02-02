struct Ctx {
    x: i64,
}

enum Thing {
    Wow(Ctx),
    Who,
}

enum Foo {
    Bar(bool),
    Baz(i64),
    Thing(Thing),
}

fn match_foo(foo: Foo) -> i64 {
    match foo {
        Foo::Thing(Thing::Wow(ctx)) => {
            ctx.x
        },
        Foo::Bar(b) => {
            0
        },
        Foo::Baz(n) => {
            n
        },
        _ => {
            2
        },
    }
}

fn match_with_wildcard(foo: Foo) -> i64 {
    match foo {
        Foo::Bar(b) => {
            1
        },
        _ => {
            0
        },
    }
}

fn match_literal(x: i64) -> i64 {
    match x {
        42 => {
            1
        },
        _ => {
            0
        },
    }
}

script fn get_bar() {
    match_foo(Foo::Bar(true));
}
