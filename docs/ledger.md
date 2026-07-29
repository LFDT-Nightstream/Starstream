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
                  [--account PUBKEY=BALANCE]...
                  [--cardano-block-height N] [--cardano-current-slot N]
                  [--max-requests N]
```

- **`--addr`** (default `[::]:9000`) — address the HTTP API is served on.
- **`--network`** (default `dev`) — the network identifier publish
  transactions must be bound to; a publish signed for a different network
  is rejected.
- **`--account PUBKEY=BALANCE`** (repeatable) — a pre-funded account
  (genesis allocation). `PUBKEY` is a hex-encoded raw 32-byte Ed25519
  public key, `BALANCE` a `u64`.
- **`--cardano-block-height`**, **`--cardano-current-slot`** (default
  `0`) — the Cardano context reported to running contracts via the
  `starstream:std/cardano` host interface (`block-height`,
  `current-slot`).
- **`--max-requests`** (default `65535`) — the maximum number of
  requests served concurrently; excess requests are rejected with
  `503 Service Unavailable`.

## Model

- **Contracts are content-addressed.** A contract is a Wasm component;
  its identifier is the lowercase-hex SHA-256 digest of the component
  bytes, and every contract URL embeds that digest.
- **Accounts pay for publishing.** An account is identified by the
  lowercase-hex encoding of its Ed25519 public key. A publish charges the
  account one balance unit per byte of Wasm and consumes a nonce: every
  publish must carry a nonce strictly greater than the account's last
  accepted one (replay protection). Accounts only come into existence via
  `--account`.
- **Transactions persist UTXOs.** Each successful coordination-script
  invocation is recorded as a transaction. Every UTXO the script
  constructs becomes an output of that transaction, in construction
  order: its instance state is snapshotted with Wizer and its storage
  extracted. Transactions and outputs are addressed by zero-based index —
  the first invocation is transaction `0`, its first UTXO
  `/transactions/0/utxos/0`.
- **Invocation uses wRPC framing.** RPC request bodies are wRPC frame
  streams: an invocation header naming the function on the root (empty)
  instance, followed by the parameters encoded with the wRPC value codec.
  Responses are a single frame carrying the encoded results, served as
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

- `400 Bad Request` — the digest is not hex-encoded SHA-256; the body is
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

## Fetch a contract: `GET /contracts/<digest>`

Content-negotiated via `Accept`, server preference first:

- **`application/cose`** (default) — the stored `COSE_Sign1` publish
  envelope, byte-for-byte as accepted. This is the durable record:
  another party can re-verify the signature and re-derive the digest
  offline.
- **`application/wasm`** — the raw contract component.

`404 Not Found` for an unknown digest, `406 Not Acceptable` when the
`Accept` header matches neither representation.

## Invoke a coordination script: `POST /contracts/<digest>/rpc`

Invokes a coordination-script export of a published contract. The wRPC
invocation header names the script; the instance must be empty.

Each UTXO type the script uses resolves through a UTXO import of the
contract; the request must map every such import instance to the digest
of the (published) contract providing it, one `X-Starstream-Utxo` header
per import:

```
X-Starstream-Utxo: <instance>=<contract-digest>
```

On success the UTXOs the script constructed are persisted as a new
transaction (see [Model](#model)). The response carries one
`X-Starstream-Utxo: <instance>` header per persisted UTXO — in output
order, so the *n*-th header describes `/transactions/<tx>/utxos/<n>` —
and the body is the wRPC-framed script results.

`404 Not Found` for an unknown contract digest, script name, or UTXO
import digest; `400 Bad Request` for malformed headers, framing, or
parameters, and for a failed script or snapshot.

## Fetch a UTXO's ABI: `GET /transactions/<tx>/utxos/<utxo>/rpc`

Serves the ABI of a persisted UTXO as a WIT world in the
`starstream:utxo` package, named after the UTXO's exported instance. Only
the methods the UTXO declared it implements (via `implements-method`
during construction) appear, as root-level function imports — the client
view, suitable for feeding straight to `wit-bindgen-wrpc` to generate
invocation bindings:

```wit
package starstream:utxo;

world score-progress {
    import plus-chips: func(chips2: u64);
    import plus-mult: func(mult2: u64);
    import mult-mult: func(mult-pct: u64);
    import finish: func();
}
```

Content-negotiated via `Accept`, server preference first:

- **`text/plain;charset=utf-8`** (default) — the WIT text.
- **`application/wasm`** — the same package encoded as Wasm via
  `wit-component`.

`404 Not Found` for an unknown transaction or output index,
`406 Not Acceptable` when the `Accept` header matches neither
representation.

## Invoke a UTXO method: `POST /transactions/<tx>/utxos/<utxo>/rpc`

Invokes a method on a persisted UTXO. The wRPC invocation header names
the function as served in the WIT (kebab-case, e.g. `plus-chips`); the
instance must be empty. The parameters are the WIT-declared ones — the
implicit `self` receiver is supplied by the server, which restores the
UTXO from its persisted snapshot and storage before the call. The
response body is the wRPC-framed results.

Invocations run against the persisted snapshot: state the method mutates
is not written back as a new transaction (yet), so every invocation
observes the state captured when the transaction persisted the UTXO.

`404 Not Found` for an unknown transaction or output index;
`400 Bad Request` for an unknown method or malformed framing or
parameters.

## Everything else

Any other path responds `404 Not Found`; a known path with an unsupported
method responds `405 Method Not Allowed`. When `--max-requests` requests
are already in flight the server responds `503 Service Unavailable`.
