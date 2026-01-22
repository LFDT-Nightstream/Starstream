import { blockHeight } from starstream:std/cardano;

script fn reexport() -> i64 {
    runtime blockHeight();
    runtime blockHeight()
}

script fn second() -> i64 {
    reexport()
}
