fn make_some() -> Option<i64> {
    Option::Some(42)
}

fn make_none() -> Option<i64> {
    Option::None
}

fn unwrap_option(opt: Option<i64>) -> i64 {
    match opt {
        Option::Some(v) => {
            v
        },
        Option::None => {
            0
        },
    }
}

fn make_ok() -> Result<i64, bool> {
    Result::Ok(42)
}

fn make_err() -> Result<i64, bool> {
    Result::Err(false)
}

fn unwrap_result(res: Result<i64, bool>) -> i64 {
    match res {
        Result::Ok(v) => {
            v
        },
        Result::Err(e) => {
            0
        },
    }
}

struct Thing {
    x: Option<i64>,
}

script fn run(x: Thing, y: Result<i64, bool>) {
    unwrap_option(make_some());
    unwrap_result(make_ok());
}
