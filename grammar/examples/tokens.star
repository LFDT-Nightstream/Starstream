abi HasTokens {
  fn attach_token(Intermediate<any, any>);
}

utxo PayToPublicKeyHash {
  main(owner: u32) {
    yield;
    assert(IsTxSignedBy(owner));

    unbind_utxo_tokens();
  }

  impl StarstreamToken {}

  impl HasTokens {
    fn attach_token(intermediate: Intermediate<any, any>) / { StarstreamEnv } {
      intermediate.bind();
    }
  }
}

token Token1 {
  mint {
    assert(IsTxSignedBy(0));
  }
}

token Token2 {
  mint {
    assert(amount == 15);
    assert(IsTxSignedBy(0));
  }
}

script {
  fn main() / { StarstreamEnv, HasTokens } {
    try {
      let output = PayToPublicKeyHash::new(0);

      let utxo1 = PayToPublicKeyHash::new(0);
      let utxo2 = PayToPublicKeyHash::new(0);

      let utxo_1_token_1 = Token1::mint(10);
      let utxo_1_token_2 = Token2::mint(15);

      let utxo_2_token_1 = Token1::mint(10);

      utxo1.attach_token(utxo_1_token_1);
      utxo1.attach_token(utxo_1_token_2);

      utxo2.attach_token(utxo_2_token_1);

      try {
        utxo1.resume(());
      }
      with StarstreamToken::TokenUnbound(i: Intermediate<any, any>) {
        assert(i.type() == Token1::id() || i.type() == Token2::id());

        let other = i.spend(5);

        output.attach_token(i);
        output.attach_token(other);
      }

      try {
        utxo2.resume(());
      }
      with StarstreamToken::TokenUnbound(i: Intermediate<any, any>) {
        assert(i.type() == Token1::id() && i.amount() == 10);

        i.burn();
      }
    }
    with StarstreamToken::TokenUnbound(i: Intermediate<any, any>) {
	  // there shouldn't be any unbound tokens before the first yield
      assert(false);
      i.burn();
    }
  }
}
