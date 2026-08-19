---
sidebar_position: 5
---

# Ledger

`starstream-ledger` is a standalone ledger server built on the
`starstream-runtime-next` host. It stores published contracts as
content-addressed Wasm components, runs their coordination scripts,
persists the UTXOs those scripts construct, and invokes methods on the
persisted UTXOs — all over a single HTTP API.

Run `starstream-ledger --help` for the auto-generated flag overview.

## Running

```
starstream-ledger [--addr ADDR] [--network NETWORK]
                  [--admin-key PUBKEY] [--admin-balance BALANCE]
                  [--cardano-block-height N] [--cardano-current-slot N]
                  [--max-requests N]
```

- **`--addr`** (default `[::]:9000`) — address the HTTP API is served on.
- **`--network`** (default `dev`) — the network identifier publish
  transactions must be bound to; a publish signed for a different network
  is rejected.
- **`--admin-key PUBKEY`** — the pre-funded admin account (genesis
  allocation). `PUBKEY` is a hex-encoded raw 32-byte Ed25519 public key.
  If not specified, a well-known pre-seeded key is used: the Ed25519 key
  whose private key is the SHA-256 digest of `admin`.
- **`--admin-balance BALANCE`** (default `u64::MAX`) — the initial
  balance of the admin account.
- **`--cardano-block-height`**, **`--cardano-current-slot`** (default
  `0`) — the Cardano context reported to running contracts via the
  `starstream:std/cardano` host interface (`block-height`,
  `current-slot`).
- **`--max-requests`** (default `65535`) — the maximum number of
  requests served concurrently; excess requests are rejected with
  `503 Service Unavailable`.

## Client CLI

`starstream-ledger-cli` drives the API from the command line; it is
gated behind the `cli` cargo feature
(`cargo build -p starstream-ledger --features cli`). Every subcommand
takes `--url` (default `http://[::1]:9000`) selecting the server.

Invocation parameters are given in
[WAVE](https://github.com/bytecodealliance/wasm-wave) (WebAssembly Value
Encoding), one argument per parameter, and results are printed back as
WAVE, one line per result. The parameter and result types come from the
WIT the ledger serves for the invocation target
(`GET /contracts/<digest>/rpc` and `GET /utxos/<digest>/rpc`); the
invocation itself is `POST`ed to `/rpc`.

- **`publish --key HEX --network NETWORK --nonce N <wasm>`** — sign the
  publish transaction with the hex-encoded 32-byte Ed25519 key, `PUT`
  the `COSE_Sign1` envelope, and print the contract digest.
- **`fund-account --key HEX --network NETWORK --nonce N <account> <amount>`** —
  sign the fund-account transaction with the hex-encoded 32-byte
  Ed25519 admin key and `POST` the `COSE_Sign1` envelope, transferring
  `<amount>` from the admin balance to the account `<account>` (a
  hex-encoded raw 32-byte Ed25519 public key).
- **`script [--utxo INSTANCE=DIGEST]... <digest> [<name> [<arg>]...]`** —
  invoke coordination script `<name>` of contract `<digest>`; each
  `--utxo` maps a UTXO import instance of the contract to the digest of
  the contract providing it. The digests of the persisted UTXOs and the
  recorded transaction index and block height are reported on stderr.
  With no `<name>`, print the contract's script ABI as WIT instead.
- **`method <utxo-digest> [<name> [<arg>]...]`** — invoke method
  `<name>` on the persisted UTXO addressed by `<utxo-digest>`, as
  reported by `script`. With no `<name>`, print the UTXO's ABI as WIT
  instead.

The full contract flow — publish, construct a UTXO through a
coordination script (its digest is reported on stderr), inspect its ABI,
invoke its methods:

```sh
starstream-ledger-cli publish --key $KEY --network dev --nonce 1 score.wasm
starstream-ledger-cli script --utxo score-progress=$DIGEST $DIGEST example
starstream-ledger-cli method $UTXO              # print the ABI as WIT
starstream-ledger-cli method $UTXO plus-chips 7
```

## Model

- **Contracts are content-addressed.** A contract is a Wasm component;
  its identifier is the SHA-256 digest of the component bytes, encoded
  as a [multibase]-encoded [multihash] — canonically the base32-lower
  encoding of the sha2-256 multihash: `b` followed by 55 lowercase
  alphanumeric characters. Every contract URL embeds that digest; any
  multibase base is accepted on input and normalized to the canonical
  form, which the server uses in storage, responses, and errors. The
  canonical form is also a valid component-model label, so it can be
  embedded verbatim in Wasm import/export and wRPC instance names.
- **Accounts pay for publishing.** An account is identified by the
  lowercase-hex encoding of its Ed25519 public key. A publish charges the
  account one balance unit per byte of Wasm and consumes a nonce: every
  signed transaction (publish or fund-account) must carry a nonce
  strictly greater than the signing account's last accepted one (replay
  protection). The admin account configured via `--admin-key` is the
  genesis allocation; further accounts are created and credited by
  admin-signed fund-account transfers (`POST /accounts/<account>`).
- **UTXOs are content-addressed too.** Each successful
  coordination-script invocation is recorded as a transaction in a new
  block. Every UTXO the script constructs becomes an output of that
  transaction, in construction order: its instance state is snapshotted
  with Wizer and its storage extracted, and the snapshot is persisted
  under the SHA-256 digest of its Wasm, encoded the same way as contract
  digests. That digest — reported by the invocation response — is the
  UTXO's address. The response also reports the recorded transaction
  index and block height, though transactions and blocks are not (yet)
  addressable over HTTP.
- **Invocation uses wRPC framing.** All invocations `POST` to the single
  `/rpc` endpoint. The request body is a wRPC frame stream: an
  invocation header naming the target instance and function, followed by
  the parameters encoded with the wRPC value codec. The instance selects
  the target — `starstream:contract/<digest>` a coordination script,
  `starstream:utxo/<digest>` a persisted-UTXO method — and is exactly the
  ID of the WIT interface the ledger serves for that target. Responses
  are a single frame carrying the encoded results, served as
  `application/octet-stream`. Async values (streams, futures) are not
  supported.

## Publish a contract: `PUT /contracts/<digest>`

The body is a `COSE_Sign1` envelope (RFC 9052; tagged preferred, untagged
accepted). The `Content-Type` header, if present, must be
`application/cose`. The envelope must carry:

- protected `alg` header: EdDSA;
- protected `kid` header: the signer's raw 32-byte Ed25519 public key
  (its lowercase hex is the paying account's identifier);
- payload: the CBOR publish transaction, a four-element array

  ```
  ["starstream:publish", <network: text>, <nonce: uint>, <wasm: bytes>]
  ```

  where `starstream:publish` is the domain-separation context binding the
  signature to this protocol, `network` must equal the server's
  `--network`, `nonce` must fit in a `u64` and be strictly greater than
  the account's last accepted nonce, and `wasm` is the contract component
  whose SHA-256 digest must equal `<digest>`;
- signature: over the standard `Signature1` structure with empty external
  AAD.

On success the contract is stored under its digest, the account is
charged `len(wasm)` and its nonce advanced, and the server responds
`200 OK`. Re-publishing an already-stored digest (with a valid envelope
and fresh nonce) is a no-op `200`: nothing is charged and the nonce is
not consumed.

Failure responses:

- `400 Bad Request` — the digest is not a multibase-encoded sha2-256
  multihash; the body is
  not a `COSE_Sign1`; `alg` or `kid` is malformed; the payload is not the
  four-element publish array; wrong context or network; the nonce
  overflows `u64`; the digest doesn't match the Wasm; or the Wasm is not
  a loadable Starstream component.
- `401 Unauthorized` — the signature does not verify against `kid`.
- `402 Payment Required` — the account balance is smaller than the Wasm
  size.
- `403 Forbidden` — no account exists for `kid`.
- `409 Conflict` — the nonce is not strictly greater than the last
  accepted one (e.g. a replayed envelope).
- `415 Unsupported Media Type` — a `Content-Type` other than
  `application/cose`.

## Fund an account: `POST /accounts/<account>`

`<account>` is a hex-encoded raw 32-byte Ed25519 public key; its
lowercase form is the funded account's identifier. The body is a
`COSE_Sign1` envelope under the same rules as a publish (protected
EdDSA `alg`, protected raw 32-byte `kid`, signature over `Signature1`
with empty external AAD), except that the signer must be the admin
account: funding is a transfer from the admin balance. The payload is
the CBOR fund-account transaction, a five-element array

```
["starstream:fund-account", <network: text>, <nonce: uint>, <account: bytes>, <amount: uint>]
```

where `starstream:fund-account` is the domain-separation context,
`network` must equal the server's `--network`, `nonce` must fit in a
`u64` and be strictly greater than the admin account's last accepted
nonce, `account` is the raw 32-byte public key that must match
`<account>`, and `amount` must fit in a `u64`.

On success `amount` is transferred from the admin balance to the
account — created first if it does not exist yet — the admin nonce is
advanced, and the server responds `200 OK`.

Failure responses:

- `400 Bad Request` — `<account>` is not a hex-encoded 32-byte Ed25519
  public key; the body is not a `COSE_Sign1`; `alg` or `kid` is
  malformed; the payload is not the five-element fund-account array;
  wrong context or network; the nonce or amount overflows `u64`; or
  the payload account does not match `<account>`.
- `401 Unauthorized` — the signature does not verify against `kid`.
- `402 Payment Required` — the admin balance is smaller than `amount`.
- `403 Forbidden` — the signer is not the admin account.
- `409 Conflict` — the nonce is not strictly greater than the admin's
  last accepted one (e.g. a replayed envelope), or the credit would
  overflow the account balance.
- `415 Unsupported Media Type` — a `Content-Type` other than
  `application/cose`.

## Fetch a contract: `GET /contracts/<digest>`

Content-negotiated via `Accept`, server preference first:

- **`application/cose`** (default) — the stored `COSE_Sign1` publish
  envelope, byte-for-byte as accepted. This is the durable record:
  another party can re-verify the signature and re-derive the digest
  offline.
- **`application/wasm`** — the raw contract component.

`404 Not Found` for an unknown digest, `406 Not Acceptable` when the
`Accept` header matches neither representation.

## Fetch a contract's script ABI: `GET /contracts/<digest>/rpc`

Serves the coordination-script ABI of a published contract as a WIT
interface in the `starstream:contract` package, named after the
contract's canonical digest. Every coordination-script export of the
contract appears as a function of that interface:

```wit
package starstream:contract;
interface bciqfbwcy4cmf5td7mbayvlymywvvq72cyjlqvccaswu6rtfm2d3fixa {
  example: func();
}
```

The interface's fully-qualified ID —
`starstream:contract/<digest>` — is exactly the wRPC instance the
invocation is addressed to; a world importing the interface can be fed
straight to `wit-bindgen-wrpc` to generate invocation bindings.

Content-negotiated via `Accept`, server preference first:

- **`text/plain;charset=utf-8`** (default) — the WIT text.
- **`application/wasm`** — the same package encoded as Wasm via
  `wit-component`.

`404 Not Found` for an unknown digest, `406 Not Acceptable` when the
`Accept` header matches neither representation.

## Invoke: `POST /rpc`

The single invocation endpoint. The wRPC invocation header opening the
request body names the target instance and function; the instance is
dispatched on:

- `starstream:contract/<digest>` — invoke coordination script
  `<function>` of the published contract `<digest>`;
- `starstream:utxo/<digest>` — invoke method `<function>` on the
  persisted UTXO `<digest>`;
- anything else responds `501 Not Implemented` (reserved for ledger
  RPC). A malformed invocation header is `400 Bad Request`.

### Coordination scripts (`starstream:contract/<digest>`)

Each UTXO type the script uses resolves through a UTXO import of the
contract; the request must map every such import instance to the digest
of the (published) contract providing it, one `X-Starstream-Utxo` header
per import:

```
X-Starstream-Utxo: <instance>=<contract-digest>
```

On success the UTXOs the script constructed are persisted as a new
transaction (see [Model](#model)), reported by the response headers:

- `X-Starstream-Utxo: <utxo-digest>` — one per persisted UTXO, in
  output order: the digest now addressing it;
- `X-Starstream-Transaction: <index>` — the zero-based index of the
  recorded transaction;
- `X-Starstream-Block: <height>` — the height of the block recording
  it.

The body is the wRPC-framed script results.

`404 Not Found` for an unknown contract digest, script name, or UTXO
import digest; `400 Bad Request` for malformed headers, framing, or
parameters; `500 Internal Server Error` for a failed script or
snapshot.

### UTXO methods (`starstream:utxo/<digest>`)

The function is named as served in the WIT (kebab-case, e.g.
`plus-chips`). The parameters are the WIT-declared ones — the implicit
`self` receiver is supplied by the server, which restores the UTXO from
its persisted snapshot and storage before the call. The response body is
the wRPC-framed results.

Invocations run against the persisted snapshot: state the method mutates
is not written back as a new transaction (yet), so every invocation
observes the state captured when the UTXO was persisted.

`404 Not Found` for an unknown UTXO digest or a method the UTXO does not
implement; `400 Bad Request` for malformed framing or parameters;
`500 Internal Server Error` for a failed method.

## Fetch a UTXO's ABI: `GET /utxos/<digest>/rpc`

Serves the ABI of a persisted UTXO as a WIT interface in the
`starstream:utxo` package, named after the UTXO's canonical digest. Only
the methods the UTXO declared it implements (via `implements-method`
during construction) appear, as functions of that interface, without the
implicit `self` receiver:

```wit
package starstream:utxo;
interface bciqelzhvansl7qzql6bbqpkluky3ryapr2sf2aoewquyusjra2dfz4q {
  plus-chips: func(chips2: u64);
  plus-mult: func(mult2: u64);
  mult-mult: func(mult-pct: u64);
  finish: func();
}
```

As with contracts, the interface's fully-qualified ID —
`starstream:utxo/<digest>` — is exactly the wRPC instance method
invocations are addressed to.

Content-negotiated via `Accept`, server preference first:

- **`text/plain;charset=utf-8`** (default) — the WIT text.
- **`application/wasm`** — the same package encoded as Wasm via
  `wit-component`.

`404 Not Found` for an unknown UTXO digest, `406 Not Acceptable` when
the `Accept` header matches neither representation.

## Everything else

Any other path responds `404 Not Found`; a known path with an unsupported
method responds `405 Method Not Allowed`. When `--max-requests` requests
are already in flight the server responds `503 Service Unavailable`.

[multibase]: https://github.com/multiformats/multibase
[multihash]: https://github.com/multiformats/multihash
