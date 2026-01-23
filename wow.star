import { blockHeight } from starstream:std/cardano;

/// Yes more
struct Point {
    // wow
    x: i64,
    y: i64,
}

/// Yes
enum Message {
    // wow
    /// yes yes yes
    Ping,
    Pong { x: i64 },
    P(Point),
} // wow

/// This function adds Point and Message
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

/// This function adds two numbers
fn who(a: i64, b: i64) -> i64 {
    a + b
}

fn thing() -> i64 {
    // thing
    let flag = who(1, 2); // more
    let x = flag + 1;
    let height = runtime blockHeight();
    // wow
    let y = x + flag + 0;
    if (x > 1) {
        x
    } else {
        y
    }
}
