abi Counter {
  fn increment();
}

utxo CounterUtxo {
  storage {
    let mut count: u64;
  }

  main fn new(pub initial: u64) {
    count = initial;
    yield(Counter);
  }

  impl Counter {
    fn increment() {
      count = count + 1;
    }
  }
}
