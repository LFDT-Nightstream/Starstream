typedef Data = string

const ORACLE_FEE = 10;
const PAYMENT_ADDRESS = 10;

utxo PayToPublicKeyHash {
  main(owner: PublicKey) {
    yield;
    assert(raise StarstreamEnv::IsTxSignedBy(owner));
  }
}

abi Oracle {
  error Error(string);

  fn get_data(): Data;
}

utxo OracleContract {
  storage {
    data: Data;
  }

  main(data: Data) {
    storage.data = data;
    loop { yield; }
  }

  impl Oracle {
    fn get_data(): Data / { StarstreamEnv } {
      let caller = raise StarstreamEnv::Caller();
      let this_contract = raise StarstreamEnv::ThisCode();

      if (caller != this_contract) {
        // oracle data can only be called from a coordination script in
        // this contract, that ensures data is paid for
        raise Oracle::Error("InvalidContext");
      }

      return storage.data; // note: this non-mutable, so it's just a reference input
    }
  }
}

token FeeToken {}

script {
  fn get_oracle_data(input: PayToPublicKeyHash, oracle: OracleContract): Data / { StarstreamEnv, Oracle } {
    let change_utxo = PayToPublicKeyHash::new(raise StarstreamEnv::Caller());

    let fee_utxo = PayToPublicKeyHash::new(PAYMENT_ADDRESS);

    try {
      input.resume();
    }
    with StarstreamToken::TokenUnbound(intermediate: Intermediate<any, any>) {
      if(intermediate.type() == FeeToken::id()) {
        let fee = intermediate.spend(10);
        change_utxo.attach(intermediate);
        fee_utxo.attach(fee);
      }
      else {
        change_utxo.attach(intermediate);
      }
    }

    oracle.get_data()
  }
}
