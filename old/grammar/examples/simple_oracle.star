typedef Data = u32

abi HasOwner {
  fn get_owner(): PublicKey;
}

abi SimpleOracleAbi {
  fn get_data(Intermediate<any, any>): Data;
}

abi HasTokens {
  fn attach_token(Intermediate<any, any>);
}

utxo PayToPublicKeyHash {
  main(owner: PublicKey) {
    yield;
    assert(IsTxSignedBy(owner));
  }

  impl HasTokens {
    fn attach_token(intermediate: Intermediate<any, any>) / { StarstreamEnv } {
      // PayToPublicKeyHash accepts any token
      intermediate.bind();
    }
  }
}

utxo SimpleOracle {
  storage {
    owner: u32;
    data: Data;
  }

  main(owner: PublicKey) {
    storage.owner = owner;
    storage.data = 42;

    yield;
    // only the owner of the oracle can resume it
    assert(IsTxSignedBy(owner));
  }

  impl SimpleOracleAbi {
    fn get_data(intermediate: Intermediate<any, any>): Data / { StarstreamEnv } {
      assert(intermediate.type() == OracleToken::id());
      assert(intermediate.amount() == 1);

      // take the token as payment
      intermediate.bind();

      storage.data
    }
  }

  impl HasOwner {
    fn get_owner(): PublicKey {
      storage.owner
    }
  }
}

script {
  fn main(): PayToPublicKeyHash / { StarstreamEnv } {
    // the PublicKey for the change
    let caller = 45000;

    // these would be inputs to the tx
    let mint_amount = 10;
    let input = OracleToken::mint(mint_amount);

    let oracle = SimpleOracle::new(113);

    // leaves a value of 9 in the input token
    // and this new token has a value of 1
    let fee_payment = input.spend(1);

    let data = oracle.get_data(fee_payment);

    let change = PayToPublicKeyHash::new(caller);

    // balance the transaction

    // change has to be either attached or burned
    // otherwise we get an error (tokens are linear types)
    change.attach_token(input);

    change
  }

}

token OracleToken {
  mint {
    assert(IsTxSignedBy(110));
  }
}
