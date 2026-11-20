contract;

import { PublicKey } from starstream:std/ledger;
import { PayToPublicKey, send_single_token } from "./pay_to_public_key.star";

token TestToken {
    storage {
        let mut dummy: u32;
    }

    mint fn new() {}

    burn fn destroy() {}

    impl Token {
        fn attach(u: Utxo) {}
        fn detach(u: Utxo) {}
    }
}

test "Send test token to dummy recipient" {
    let key = PublicKey { dummy: 17 };
    let token = TestToken::new();
    // NOTE: this line also tests TestToken -> Token upcast.
    send_single_token(key, token);
}
