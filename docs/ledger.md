---
sidebar_position: 6
---

# Ledger

`starstream node` runs a development Starstream ledger, and `starstream
ledger` is its client. Both are also available as the standalone
`starstream-ledger-node` and `starstream-ledger-cli` binaries, which take
the same arguments.

Coordination scripts run on the client: it executes the script locally,
snapshots every UTXO it constructs into a transaction output, and submits
the signed transaction. The ledger checks the inputs and records the
outputs.

:::warning

This is a development ledger, not a secure one. Anyone can spend any
unspent output except genesis outputs, which require the admin's
signature, and proofs are not checked. State is held in memory and is lost
when the node stops.

:::

## Quick start

From a checkout of the repository, compile the
[`require_hash_preimage` example](https://github.com/LFDT-Nightstream/Starstream/blob/main/examples/require_hash_preimage.star)
and the Rust library it imports:

```sh
cargo build --release --target wasm32-unknown-unknown -p sha256lib
starstream wasm -c examples/require_hash_preimage.star --output-component hash.wasm
DIGEST=$(starstream ledger digest hash.wasm)
```

Start a node whose genesis output is created by `create-hash` called with
`42`, which stores `sha256_u64(42)`:

```sh
cat > genesis.toml <<EOF
[[scripts]]
contract = "$DIGEST"
script = "create-hash"
args = ["42"]
EOF
starstream node --genesis genesis.toml --import hash.wasm
```

In another terminal, with `DIGEST` set as above, inspect the genesis
output and write the default admin key:

```sh
starstream ledger genesis
printf admin | sha256sum | cut -d' ' -f1 > admin.key
```

Spend the output with `consume-hash`, signed by the admin as genesis inputs
require. The contract is not published, so `--import` supplies it. A wrong
preimage traps, and nothing is submitted:

```sh
starstream ledger contract script call --key admin.key --import hash.wasm $DIGEST consume-hash :0 41
```

The right one is submitted, and also written to `tx.cbor`:

```sh
starstream ledger contract script call --key admin.key --import hash.wasm --output-transaction tx.cbor $DIGEST consume-hash :0 42
starstream ledger transaction show tx.cbor
```

## Running a node

```
starstream node [--network NETWORK] [--admin PUBKEY] [--addr ADDR]
                [--max-requests N] [--io-timeout SECONDS] [--fuel FUEL]
                [--shutdown-timeout SECONDS]
                [--cardano-block-height N] [--cardano-current-slot N]
                [--genesis PATH [--import PATH]...]
```

- **`--network`** (default `dev`): network identifier every envelope must
  be bound to.
- **`--admin`**: hex-encoded Ed25519 public key of the admin, who funds
  accounts and owns the genesis outputs. Defaults to the key whose private
  key is the SHA-256 digest of `admin`.
- **`--addr`** (default `[::]:9000`): address to serve the HTTP API on.
- **`--max-requests`** (default `65535`): maximum number of open
  connections and of concurrent requests. Further connections wait to be
  accepted, and excess requests get `503 Service Unavailable`.
- **`--io-timeout`** (default `30`): seconds a connection may stall before
  it is closed: waiting for a request head, between chunks of a request
  body (answered with `408 Request Timeout`), or writing a response. Only
  HTTP/1.1 is served.
- **`--fuel`** (default `1000000000`): fuel for each UTXO method call
  made over [`POST /rpc`](#post-rpc).
- **`--shutdown-timeout`** (default `10`): seconds to wait for in-flight
  requests on `SIGINT` or `SIGTERM`.
- **`--cardano-block-height`**, **`--cardano-current-slot`** (default `0`):
  values contracts observe through `starstream:std/cardano`.
- **`--genesis`**: TOML file listing coordination scripts whose outputs
  form the genesis UTXO set, in order. The scripts run once at startup and
  cannot take UTXO inputs. They are resolved from the `--import`ed
  contracts, which are not published:

  ```toml
  [[scripts]]
  contract = "bciq..." # digest of an `--import`ed contract
  script = "example"
  args = ["42"]        # WAVE-encoded, optional
  ```

`STARSTREAM_LOG` (`STARSTREAM_LEDGER_LOG` for the standalone binaries)
sets the log filter, `info` by default.

## Client

`starstream ledger` takes the global `--url` (default
`http://[::1]:9000/`) and `--pool-idle-timeout` (default `15`, seconds an
idle connection is kept for reuse, shorter than the node's `--io-timeout`)
options. `--key PATH` is a file containing a hex-encoded Ed25519 private
key. `--network` binds a signed envelope to a network and defaults to the
one reported by the ledger. Digests are accepted in any [multibase] base or
as `sha256:HEX`.

- **`key generate`**: print a new private key; the public key is logged.
- **`digest <path>`**: print the digest of a file.
- **`account fund --key PATH [--network NETWORK] [--nonce N] <account>
  <amount>`**: credit an account (a hex-encoded public key), signed by the
  admin. `--nonce` defaults to the admin's last fund nonce plus one.
- **`account balance <account>`**, **`account nonce <account>`**: print the
  balance or the last publish nonce of an account.
- **`admin nonce`**: print the admin's last fund nonce.
- **`contract publish --key PATH [--network NETWORK] [--nonce N] <wasm>`**:
  publish a contract and print its digest. `--nonce` defaults to the
  account's last publish nonce plus one.
- **`contract script call [--key PATH [--network NETWORK] | --simulate]
  [--import PATH]... [--output-transaction PATH]
  [--cardano-block-height N] [--cardano-current-slot N] <digest> <script>
  [<arg>]...`**: run a coordination script locally and submit the
  resulting transaction, logging its digest. UTXO arguments are
  `[TRANSACTION]:INDEX` references, with an empty transaction for genesis;
  other arguments are WAVE. Prints the results as a WAVE tuple, with
  returned UTXOs replaced by their output index. `--import` resolves
  contracts from local files instead of the ledger, `--simulate` skips
  submission.
- **`utxo call [--transaction DIGEST] <index> <method> [<arg>]...`**: call
  a method of an unspent output, discarding the resulting state, and print
  its results as WAVE.
- **`utxo wit [--transaction DIGEST] <index>`**: print the WIT of an
  unspent output's methods.
- **`block height`**: print the block height.
- **`block get <height>`**: print the envelopes of a block, hex-encoded,
  one per line.
- **`network name`**: print the network the ledger accepts envelopes for.
- **`genesis`**, **`transaction get <digest>`**, **`transaction show
  <path>`**: print the genesis outputs, a transaction on the ledger, or one
  written by `--output-transaction`, as TOML.

## Model

- **Digests.** Contracts are identified by the SHA-256 digest of their
  Wasm, transactions by that of the CBOR-encoded transaction message,
  independent of who signed it. Digests are written as a [multibase]
  sha2-256 [multihash], canonically in base32-lower (`b` followed by 55
  lowercase alphanumeric characters).
- **Accounts.** An Ed25519 public key with a balance and a nonce, created
  and credited only by the admin. Publishing costs one unit per envelope
  byte.
- **Nonces.** Each fund and publish carries a nonce, which must be greater
  than the last one its signer used, so the first is at least `1`. The
  admin's fund nonces and its account's publish nonces are counted
  separately. A publish rejected for an insufficient balance still uses up
  its nonce.
- **UTXOs.** Transaction outputs, referenced as `(transaction, index)`; an
  empty transaction refers to the genesis outputs. Spent outputs are
  removed.
- **Blocks.** Every accepted envelope is a new block, so the block height
  is the number of accepted envelopes. Block `1` holds the first one;
  height `0` is genesis, whose outputs are queried separately.

## HTTP API

### `POST /rpc`

Dispatched on `Content-Type`: `application/cose` submits an
[envelope](#envelopes), `application/x.wrpc` is a wRPC invocation. Any
other type is `415 Unsupported Media Type`, a malformed one
`400 Bad Request`.

A wRPC invocation body is the wRPC invocation header naming the instance
and function, followed by the parameters in the wRPC value encoding. The
response is a single wRPC frame with the results. Unknown functions are
`404 Not Found`. The ledger queries are described by
[`starstream-ledger/wit/ledger.wit`](https://github.com/LFDT-Nightstream/Starstream/blob/main/starstream-ledger/wit/ledger.wit):

| Instance                        | Function          | Returns                                         |
| ------------------------------- | ----------------- | ----------------------------------------------- |
| `starstream:ledger/account`     | `get-balance`     | the balance of an account                       |
| `starstream:ledger/account`     | `get-nonce`       | the last publish nonce of an account            |
| `starstream:ledger/admin`       | `get-nonce`       | the admin's last fund nonce                     |
| `starstream:ledger/block`       | `get-height`      | the block height                                |
| `starstream:ledger/block`       | `get-block`       | the envelopes of a block, `none` for height `0` |
| `starstream:ledger/contract`    | `get-envelope`    | the publish envelope of a contract              |
| `starstream:ledger/contract`    | `get-wasm`        | the Wasm of a contract                          |
| `starstream:ledger/transaction` | `get-envelope`    | the envelope of a transaction                   |
| `starstream:ledger/transaction` | `get-transaction` | the decoded transaction                         |
| `starstream:ledger/genesis`     | `get-outputs`     | the genesis outputs, including spent ones       |
| `starstream:ledger/network`     | `get-name`        | the network envelopes must be bound to          |

Accounts are identified by their raw 32-byte public key. Lookups return a
`get-error` of `not-found`, or `malformed` for an invalid digest or key.

Instance `starstream:ledger/utxo`, function `<method>` calls `<method>` on
an unspent output, discarding the resulting state. Its parameters are an
`option<string>` transaction digest (`none` for genesis), a `u32` output
index, and the method's own parameters. A missing output, contract or
method is `404 Not Found`; a trap, including running out of fuel, is
`422 Unprocessable Entity`.

### `GET /rpc`

Serves the WIT of `POST /rpc` invocations, for generating wRPC bindings,
e.g. with `wit-bindgen-wrpc`:

- without a query, the ledger queries;
- with `?index=INDEX[&transaction=DIGEST]`, the `utxo` interface of that
  unspent output, with method parameters prefixed by `arg-`. For the
  [score example](https://github.com/LFDT-Nightstream/Starstream/blob/main/examples/score.star):

  ```wit
  package starstream:ledger;

  interface utxo {
    get-chips: func(transaction: option<string>, index: u32) -> u64;
    plus-chips: func(transaction: option<string>, index: u32, arg-chips2: u64);
    // ...
  }
  ```

Responds with WIT text by default, or Wasm for
`Accept: application/wasm`.

### `GET /contracts/<digest>`, `GET /transactions/<digest>`

The publish or transaction envelope as `application/cose`, byte for byte
as accepted, so it can be re-verified offline. Contracts also serve their
Wasm for `Accept: application/wasm`.

### Envelopes

Envelopes are canonically encoded COSE (RFC 9052) `COSE_Sign1` or
`COSE_Sign` messages of at most 16 MiB. Each signature has a protected
EdDSA `alg` and a protected `kid` holding the signer's raw 32-byte public
key, no unprotected or critical headers, and empty external AAD. The
payload is a CBOR array `[network, context, message]`:

| `context` | Message     | Encoding                                  | Success               |
| --------- | ----------- | ----------------------------------------- | --------------------- |
| `0`       | fund        | `[nonce, account: bytes(32), amount]`     | `200 OK`              |
| `1`       | publish     | `[nonce, wasm: bytes]`                    | `201 Created`, digest |
| `2`       | transaction | `[inputs, outputs, events, proof: bytes]` | `201 Created`, digest |

- **Fund** is signed by the admin alone and creates the account if needed.
- **Publish** has one signer with an account. The Wasm must be a component
  whose UTXO methods are synchronous and take and return only booleans,
  integers, and options, results and tuples of them.
- **Transaction** has at least one unspent input and no duplicates.
  Spending a genesis output requires the admin's signature.

Errors are plain text: `400` for a malformed request or another network,
`401` for a bad signature, `402` for a missing account or insufficient
balance, `403` for a missing admin signature, `404` for a missing or spent
input, `408` for a stalled body, `409` for a stale nonce or an existing
contract or transaction, and `413` for an oversized envelope.

[multibase]: https://github.com/multiformats/multibase
[multihash]: https://github.com/multiformats/multihash
