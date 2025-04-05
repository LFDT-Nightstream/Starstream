We define the USDC contract itself. It's a token contract where the token can only be bound/unbound from a UTXO if the address isn't on a blacklist

```js
token USDC {
    abi {
        // represents querying the MerkleTree contract to know if we can send USDC to a specific address
        effect IsBlacklisted(addr: Address) -> UsdcTransferPermission::Intermediate
    }

    // can only bind (send to) this token to a UTXO owned by an address not on the blacklist
    bind() {
        const permission = raise IsBlacklisted(tx.context.caller);
        const caller = raise Caller();
        if (caller != permission.targetAddress) {
            fail "Wrong address for transfer permission"
        }
        permission.burn();
    }
    // can only unbind (send from) this token to a UTXO owned by an address not on the blacklist
    unbind() {
        const permission = raise IsBlacklisted(tx.context.caller);
        const caller = raise Caller();
        if (caller != permission.targetAddress) {
            fail "Wrong address for transfer permission"
        }
        permission.burn();
    }
}
```

```js
token UsdcTransferPermission {
    storage {
        targetAddr: Address // keep track of who we're giving permission to send USDC to
    }

    mint(targetAddr: Address) {
        const caller = raise Caller();
        // only the USDC permission MerkleTree contract can mint permission tokens
        if (caller !== UsdcPermission::address) {
            fail "Transfer permission can only be minted by UsdcPermission contract"
        }
        return Intermediate { targetAddr }
    }
    burn() {}
    bind() {
        fail "Cannot bind UsdcTransferPermission token"
    }
}
```

```js

utxo UsdcPermission {
    abi {
        // assume there exists some "MerkleTree" type implementation in Starstream
        fn add(tree: MerkleTree, addr: Address) -> void,
        fn includes(tree: MerkleTree, addr: Address) -> null | UsdcTransferPermission::Intermediate,
        fn remove(tree: MerkleTree, addr: Address) -> void,
    }

    storage {
        admin: Address,
        merkleRoot: uin256,
    }
    
    main {
        while (true) yield;
    }
    impl UsdcPermission {
        fn add(self, tree: MerkleTree, addr: Address) {
            if (tx.context.caller !== self.admin) {
                fail "Only admin can add new entries"
            }
            const newRoot = tree.add(addr);
            self.merkleRoot = newRoot;
        }
        fn remove(self, tree: MerkleTree, addr: Address) {
            // omitted for simplicity
        }
        // note: &self means this is a readonly input
        fn includes(&self, tree: MerkleTree, addr: Address) -> null | UsdcTransferPermission::Intermediate {
            if (!tree.includes(addr) {
                return null;
            }
            return UsdcTransferPermission::mint(addr);
        }
    }
}
```

## Coordination script

```js
script {
    fn transferUsdc(
        source: PublicKeyHashUtxo, // instance of a pay-to-public-key-hash contract
        target: string, // key hash
        permissionState: UsdcPermission,
        merkleTree: MerkleTree,
        amount: uint256
    ) {
        const fromAddr = address(source.publicKeyHash);
        const targetAddr = address(target);
        const sendFromPermission = permissionState.includes(fromAddr , merkleTree)
        const sendToPermission = permissionState.includes(targetAddr, merkleTree)
        if (sendFromPermission == null || sendToPermission == null) fail "No permission for target"
        try {
            const tokens = yield source; // consumes the source UTXO
            const usdc = tokens.filter(token => token.token_id == USDC::token_id);
            const nonUsdc = tokens.filter(token => token.token_id != USDC::token_id);
            
            // omit: you would need to handle "amount" here to send the right amount
            // I leave it out to simplify
            
            // create the change utxo
            PublicKeyHash::main(source.publicKeyHash, nonUsdc)
            // create new UTXO that contains the USDC transfer

            // this calls bind on the tokens, which also calls into the effect handler
            PublicKeyHash::main(target, usdc)
        } with IsBlacklisted(addr) => {
            // recall: this is called by the `bind`
            if (addr === targetAddr) {
                return sendToPermission;
            }
            // recall: this is called by the `unbind`
            if (addr === fromAddr) {
                return sendFromPermission ;
            }
            return null;
        }
    }
}
```
