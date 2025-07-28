abi HasOwner {
  fn get_owner(): PublicKey;
}

utxo PayToPublicKeyHash {
  storage {
    owner: PublicKey;
  }

  main(owner: PublicKey) {
    storage.owner = owner;

    yield;
    assert(IsTxSignedBy(owner));
  }

  impl HasOwner {
    fn get_owner(): PublicKey {
      storage.owner
    }
  }
}

script {
  fn main() / { StarstreamEnv, Starstream } {
    let input = PayToPublicKeyHash::new(10);

    let owner = input.get_owner();

    input.resume(());
  }
}
