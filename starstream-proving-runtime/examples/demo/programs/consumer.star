abi Consumer {
  fn increment();
}

utxo ConsumerUtxo {
  storage {
    let mut count: u64;
  }

  main fn new(pub initial: u64) {
    count = initial;
    yield(Consumer);
  }

  impl Consumer {
    fn increment() {
      count = count + 1;
    }
  }
}
