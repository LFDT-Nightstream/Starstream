utxo PayToPublicKeyHash {
  main(owner: PublicKey) {
    yield;
    assert(IsTxSignedBy(owner));
  }
}

script {
  fn main() / { StarstreamEnv, Starstream } {
    let input = PayToPublicKeyHash::new(1);
    input.resume(());
  }
}
