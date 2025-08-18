const PERMISSIONED_TOKEN_ID = 42;
const ADMIN = 42;

abi LinkedListNodeAbi {
  fn get_key(): PublicKey;
  fn get_next(): PublicKey;
}

utxo LinkedListNode {
  storage {
    key: Option<PublicKey>;
    next: Option<PublicKey>;
  }

  main(key: Option<PublicKey>, next: Option<PublicKey>) {
    assert(raise StarstreamEnv::CoordinationCode() == raise StarstreamEnv::ThisCode());
    loop { yield; }
  }

  impl LinkedListNodeAbi {
    fn get_key(): PublicKey {
      storage.key
    }

    fn get_next(): PublicKey {
      storage.next
    }
  }
}

script {
  fn is_in_range(proof: LinkedListNode, address: PublicKey) {
    if (proof.get_key().is_none()) {
      // empty list
      false
    }
    else {
      let next = proof.get_next();
      proof.get_key() < address && ( next.is_none() || next < next.unwrap() )
    }
  }

  fn transfer_permissioned_token(
    source: PayToPublicKeyHash,
    proof_from: LinkedListNode,
    proof_to: LinkedListNode,
    to: PublicKey,
    output_amount: Value,
  ): PayToPublicKeyHash {
    let from = source.get_owner();

    let input_amount = 0;
    let change_tokens = List::new();

    try {
      source.next();
    }
    with Starstream::TokenUnbound(token: Intermediate<any, any>) {
      if(token.type == PermissionedUSDC::type()) {
        input_amount = input_amount + token.amount;
      }
      else {
        change_tokens.push(token);
      }
    }
    with PermissionedToken::IsBlacklisted(address: PublicKey) {
      let res1 = is_in_range(proof_from, address);
      let res2 = is_in_range(proof_to, address);

      resume res1 || res2;
    }

    let output_utxo = PayToPublicKeyHash::new(to);
    let output_intermediate = PermissionedUSDC::mint(output_amount);

    let change_utxo = PayToPublicKeyHash::new(from);
    let change_intermediate = PermissionedUSDC::mint(input_amount - output_amount);

    while(!change_tokens.is_empty()) {
      let non_usdc_token_intermediate = change_tokens.pop();

      // NOTE: if any of these require some effects, the current script would have to
      // be wrapped with the handlers somehow.
      change_utxo.attach(non_usdc_token_intermediate);
    }

    try {
      output_utxo.attach(output_intermediate);
      change_utxo.attach(change_intermediate);
    }
    with PermissionedToken::IsBlacklisted(address: PublicKey) {
      let res1 = is_in_range(proof_from, address);
      let res2 = is_in_range(proof_to, address);

      resume res1 || res2;
    }

    output_utxo
  }

  fn blacklist_empty(): LinkedListNode {
    assert(raise StarstreamEnv::IsTxSignedBy(ADMIN));
    LinkedListNode::new(None(), None());
  }

  fn blacklist_insert(
    prev: LinkedListNode,
    new: PublicKey,
  ): LinkedListNode {
    assert(context.tx.is_signed_by(ADMIN));

    let prev_next = prev.get_next();
    let prev_key = prev.get_key();

    prev.burn();

    assert(prev_key == None() || prev_key < new);
    assert(prev_next == None() || new < prev_next);

    if (prev_key != None()) {
      LinkedListNode::new(prev_key, new);
    }

    LinkedListNode::new(new, prev_next)
  }

  fn token_mint_to(
    owner: PublicKey,
    amount: Value,
    proof: LinkedListNode,
  ): PayToPublicKeyHash {
    try {
      let out = PayToPublicKeyHash::new(owner);
      let intermediate = PermissionedUSDC::mint(amount);

      out.attach(intermediate);

      out
    }
    with PermissionedToken::IsBlacklisted(address) {
      is_in_range(proof, address);
    }
  }
}

abi PermissionedToken {
  effect IsBlacklisted(PublicKey): Bool;
  effect CallerOwner(): PublicKey;
}

token PermissionedUSDC {
  mint {
    assert(context.tx.is_signed_by(ADMIN));
    assert(context.tx.is_signed_by(ADMIN));
  }

  bind {
    assert(raise StarstreamEnv::CoordinationCode() == raise StarstreamEnv::ThisCode());

    let owner = raise PermissionedToken::CallerOwner();

    let is_blacklisted = raise PermissionedToken::IsBlacklisted(owner);

    assert(!is_blacklisted);

    TokenStorage {
      id: PERMISSIONED_TOKEN_ID,
      amount: storage.amount,
    }
  }

  unbind {
    assert(raise StarstreamEnv::CoordinationCode() == raise StarstreamEnv::ThisCode());

    let owner = raise PermissionedToken::CallerOwner();

    let is_blacklisted = raise PermissionedToken::IsBlacklisted(owner);

    assert(!is_blacklisted);
  }
}
