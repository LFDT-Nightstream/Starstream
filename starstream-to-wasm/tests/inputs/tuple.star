fn swap(pair: (i64, bool)) -> (bool, i64) {
    match pair {
        (a, b) => {
            (b, a)
        },
    }
}

fn nested(value: ((i64, i64), bool)) -> i64 {
    match value {
        ((x, y), true) => {
            x + y
        },
        ((x, _), false) => {
            x
        },
    }
}

fn sum_pair() -> i64 {
    match swap((1, true)) {
        (true, n) => {
            n
        },
        (false, _) => {
            0
        },
    }
}

script fn run_tuples() {
    sum_pair();
    nested(((1, 2), true));
}
