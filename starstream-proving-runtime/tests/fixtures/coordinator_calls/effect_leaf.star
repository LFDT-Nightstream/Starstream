contract;

abi Effects {
    effect Ping();
}

script fn effect_leaf(value: i64) -> i64 {
    raise Ping();
    value
}
