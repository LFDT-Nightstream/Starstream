abi Oracle {
  fn read() -> u64;
}

utxo OracleUtxo {
  storage {
    let mut reading: u64;
  }

  main fn new(pub initial: u64) {
    reading = initial;
    yield(Oracle);
  }

  impl Oracle {
    fn read() -> u64 {
      reading
    }
  }
}
