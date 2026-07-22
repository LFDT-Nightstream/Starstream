// 1. some other coordination script can call PayToPublicKey::new(someone_to_send_to)
// 2. that same coordination script would then attach tokens to the PayToPublicKeyutxo created above
// 3. coordination script runs off chain producing a proof which includes the PayToPublicKey utxo in it's output set
// 4. a transaction is submitted to the ledger with this proof attached to it
// 5. ledger verifies proof and transitions it's ledger state and `someone_to_send_to` now has the tokens
import { PublicKey } from "starstream:std/types";
// package namespace to be discussed later
import { assert_transaction_signed_by } from "starstream:std/host"

abi IPayToPublicKey {
    fn consume();
}

utxo PayToPublicKey {
    storage {
        let mut _recipient: PublicKey;
    }

    main fn start(recipient: PublicKey) {
        _recipient = recipient;
        yield(IPayToPublicKey);
    }

    impl IPayToPublicKey {
        fn consume() {
            runtime assert_transaction_signed_by(_recipient);
            resume;
        }
    }
}

// A coordination script for interaction with the PayToPublicKey utxo.
// This coordination script assumes that the `t: Token` is either detached prior or brand new.
// This coordination script does not need to leave in the same module, it's here for
// readability and to demonstrate how to use the utxo definition above.
script fn send_single_token(recipient: PublicKey, t: Token) {
    let send_to = PayToPublicKey::start(recipient);

    t.attach(send_to);
}

