struct Token {
    amount: i64,
    price: i64,
}

script fn total_value(token: Token) -> i64 {
    token.amount * token.price
}
