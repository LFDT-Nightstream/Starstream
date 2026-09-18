contract;

import { Counter } from "./counter.star";

abi Observations {
    event Value(value: i64);
}

script fn run() -> i64 {
    Counter::new().value()
}

test "Call a public method on an imported UTXO" {
    emit Value(run());
}
