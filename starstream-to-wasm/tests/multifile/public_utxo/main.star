import { Counter } from "./counter.star";

script fn run() -> i64 {
    Counter::new().value()
}
