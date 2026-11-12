contract;

// 1. some other coordination script can call PayToPublicKey::new(someone_to_send_to)
// 2. that same coordination script would then attach tokens to the PayToPublicKeyutxo created above
// 3. coordination script runs off chain producing a proof which includes the PayToPublicKey utxo in it's output set
// 4. a transaction is submitted to the ledger with this proof attached to it
// 5. ledger verifies proof and transitions it's ledger state and `someone_to_send_to` now has the tokens
import { PublicKey, assert_transaction_signed_by } from starstream:std/ledger;
import { TokenReleased } from starstream:effects/std;

abi IPayToPublicKey {
    // TODO: / [TokenReleased]
    fn consume();
}

utxo PayToPublicKey {
    storage {
        let mut _recipient: PublicKey;
    }

    main fn start(pub recipient: PublicKey) {
        _recipient = recipient;
        yield(IPayToPublicKey);
    }

    impl IPayToPublicKey {
        fn consume() {
            // ^TODO: / [TokenReleased]
            // recipient is the owner at this point
            runtime assert_transaction_signed_by(_recipient);
            resume;
        }
    }
}

// A coordination script for interaction with the PayToPublicKey utxo.
// The `t: Token` type enforces that the token is either detached prior or brand new.
// PayToPublicKey does not enforce that its callers must be defined in this
// contract, but this serves as a demonstration and standard entry point.
script fn send_single_token(pub recipient: PublicKey, t: Token) {
    let send_to = PayToPublicKey::start(recipient);
    t.attach(send_to);
}

script fn send_with_change(input: PayToPublicKey, to: PublicKey, amount: u64, change_recipient: PublicKey) {
    try {
        input.consume();
    } with TokenReleased(t) {
        // NOTE: only works sensibly if there is exactly one token on `input`.
        let s = t.subtract(amount);
        send_single_token(to, s.amount);
        send_single_token(change_recipient, s.change);
    }
}
