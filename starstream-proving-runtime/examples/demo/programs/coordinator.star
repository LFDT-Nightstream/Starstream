script fn example() {
  let oracle = OracleUtxo::new(3);
  let consumer = ConsumerUtxo::new(10);
  let n = oracle.read();
  let mut i: u64 = 0;
  while (i < n) {
    consumer.increment();
    i = i + 1;
  }
}
