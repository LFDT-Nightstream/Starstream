abi A {
  effect Foo(u32): u32;
  effect Bar(bool): bool;
}

abi UtxoAbi {
  fn abi_call1();
  fn abi_call2(): u32;
  fn abi_call3(bool): bool;
}

utxo Utxo {
  main {
    let r = raise A::Foo(2);
    yield;
  }

  impl A {}

  impl UtxoAbi {
    fn abi_call1() {
      let r = raise A::Foo(33);
    }

    fn abi_call2(): u32 {
      1
    }

    fn abi_call3(b: bool): bool {
      raise A::Bar(b)
    }
  }
}

script {
  fn main() / { StarstreamEnv } {
    let x = 5;
    try {
      let utxo = Utxo::new();

      utxo.abi_call1();

      assert(utxo.abi_call2() == 1);

      let r = utxo.abi_call3(false);

      assert(r);

      try {
        let r = utxo.abi_call3(true);

        assert(r);
      }
      with A::Foo(i: u32) {
        resume i;
      }
      with A::Bar(b: bool) {
        resume b;
      }
    }
    with A::Foo(i: u32) {
      assert(i == 33 || i == 2);
      resume x * i;
    }
    with A::Bar(b: bool) {
      resume !b;
    }
  }
}
