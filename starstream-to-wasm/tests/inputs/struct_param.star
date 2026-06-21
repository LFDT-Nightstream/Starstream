struct Item {
    amount: i64,
    price: i64,
}

script fn total_value(item: Item) -> i64 {
    item.amount * item.price
}
