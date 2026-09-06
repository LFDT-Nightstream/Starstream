script fn unwrap_or_zero(value: Option<i64>) -> i64 {
    let Option::Some(inner) = value else {
        return 0;
    };
    inner
}
