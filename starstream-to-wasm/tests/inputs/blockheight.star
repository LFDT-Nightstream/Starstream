import { blockHeight } from starstream:std/cardano;

script fn reexport() -> i64 {
    runtime blockHeight();
    runtime blockHeight()
}
