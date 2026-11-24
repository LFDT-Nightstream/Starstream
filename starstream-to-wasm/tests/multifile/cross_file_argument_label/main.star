contract;

import { takes_bool } from "./helpers/flags.star";

script fn run() -> i64 {
    takes_bool(5)
}
