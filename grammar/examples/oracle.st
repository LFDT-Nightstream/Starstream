utxo OracleContract {
  abi {
    error Error(string);
  }

  storage {
    data: Data;
  }

  main(data: Data) {
    loop { yield; }
  }

  impl OracleContract {
    fn get_data(self): Data {
      let caller = raise Caller();
      let this_contract = raise ThisCode();

      if (caller != this_contract) {
        // oracle data can only be called from a coordination script in
        // this contract, that ensures data is paid for
        raise Error("InvalidContext");
      }

      return self.data; // note: this non-mutable, so it's just a reference input
    }
  }
}

script {
  fn get_oracle_data(input: PayToPublicKeyHash, oracle: OracleContract): Data {
    let change_utxo = PayToPublicKeyHash::new(fee.get_owner());

    let fee_utxo = PayToPublicKeyHash::new(PAYMENT_ADDRESS);

    try {
      resume input;
    }
    with TokenUnbound(intermediate: Intermediate<any, any>) {
      if(intermediate.type == FeeToken) {
        let change = intermediate.change_for(ORACLE_FEE);
        change_utxo.attach(change);
        fee_utxo.attach(intermediate);
      }
      else {
        change_utxo.attach(intermediate);
      }
    }

    oracle.get_data()
  }
}
