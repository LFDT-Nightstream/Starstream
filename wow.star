import { blockHeight } from starstream:std/cardano;

struct Point {
    x: i64,
    y: i64,
}

enum Message {
    Ping,
    Pong { x: i64 },
    P(Point),
}

fn add(a: Point, b: Message) -> i64 {
    match b {
        Message::Ping => {
            a.y
        },
        Message::Pong { x } => {
            x + a.x
        },
        Message::P(o) => {
            o.x + o.y + a.x + a.y
        },
    }
}

fn who(a: i64, b: i64) -> i64 {
    a + b
}

fn thing() -> i64 {
    let flag = who(1, 2);
    let x = flag + 1;
    let height = runtime blockHeight();
    let y = x + flag + 0;
    if (x > 1) {
        x
    } else {
        y
    }
}
