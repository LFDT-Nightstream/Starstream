const FeeToken = 3;

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
    loop { yield; }
  }

  impl Oracle {
    fn get_data(self): Data / { StarstreamEnv } {
      let caller = raise StarstreamEnv::Caller();
      let this_contract = raise StarstreamEnv::ThisCode();

      if (caller != this_contract) {
        // oracle data can only be called from a coordination script in
        // this contract, that ensures data is paid for
        raise Oracle::Error("InvalidContext");
      }

      return self.data; // note: this non-mutable, so it's just a reference input
    }
  }
}

script {
  fn get_oracle_data(input: PayToPublicKeyHash, oracle: OracleContract): Data / { StarstreamEnv, Oracle } {
    let change_utxo = PayToPublicKeyHash::new(raise StarstreamEnv::Caller());

    let fee_utxo = PayToPublicKeyHash::new(PAYMENT_ADDRESS);

    try {
      input.resume();
    }
    with Starstream::TokenUnbound(intermediate: Intermediate<any, any>, type: u32) {
      if(type == FeeToken) {
        let intermediates = intermediate.change_for(ORACLE_FEE);
        change_utxo.attach(intermediates.fst);
        fee_utxo.attach(intermediates.snd);
      }
      else {
        change_utxo.attach(intermediate);
      }
    }

    oracle.get_data()
  }
}
