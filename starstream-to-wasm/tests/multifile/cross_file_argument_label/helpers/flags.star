fn always_true() -> bool {
    true
}

fn always_false() -> bool {
    false
}

fn negate(flag: bool) -> bool {
    !flag
}

fn takes_bool(flag: bool) -> i64 {
    if (flag) {
        1
    } else {
        0
    }
}
