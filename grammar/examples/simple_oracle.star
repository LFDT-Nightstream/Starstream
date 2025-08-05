typedef Data = u32

abi HasOwner {
  fn get_owner(): PublicKey;
}

abi SimpleOracleAbi {
  fn get_data(Intermediate<any, any>): Data;
}

utxo SimpleOracle {
  storage {
    owner: u32;
    data: Data;
  }

  main(owner: PublicKey) {
    storage.owner = owner;
    storage.data = 111;

    yield;
    assert(IsTxSignedBy(owner));
  }

  impl SimpleOracleAbi {
    fn get_data(intermediate: Intermediate<any, any>): Data / { StarstreamEnv } {
      Token::bind(intermediate);

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
  fn main() / { StarstreamEnv, Starstream } {
    let oracle = SimpleOracle::new(1);

    let token_intermediate = Token::mint(1);
    let data = oracle.get_data(token_intermediate);
  }
}

token Token {
  mint {
    assert(amount == 1);
    // some public key or permission for the admin
    // although the actual permissions should be in bind
    assert(IsTxSignedBy(110));
  }
}
