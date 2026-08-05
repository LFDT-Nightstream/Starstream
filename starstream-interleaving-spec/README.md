# starstream-interleaving-spec

Executable reference semantics for the control-relevant part of Starstream
component execution.

This crate intentionally contains no proof circuit. It has four jobs:

1. Define a canonical semantic `ExecutionTrace`.
2. Record/project Wasmtime execution into process-local traces.
3. Merge those traces in cooperative control-flow order.
4. Replay the resulting execution against `spec/starstream.qnt`.

## Trace boundary

The Quint model currently covers:

- starting and returning from the entrypoint coordination script;
- constructing a UTXO;
- allocating a model-local UTXO identity in constructor order and binding a
  returned, caller-local resource handle to it;
- starting a fresh ABI epoch through `abis-clear`;
- advertising the ABI at a yield point;
- calling an advertised UTXO method;
- carrying constructor and method argument tuples as canonical 32-bit-limb
  lists;
- retaining the arguments observed by an imported method call and matching
  them against the method export's receiver-side `EnterMethod` boundary;
- retaining the result observed atomically by an imported method call and
  matching it against the result published by the callee's export return;
- a UTXO export returning control, with `abis-clear` distinguishing a fresh
  yield from a normal method return;
- requiring every constructor path to reach an initial yield before returning
  its live resource;
- the main continuation yielding again or making an explicit terminal
  coroutine `return`/`burn` call.

Application events emitted through ABI event interfaces are deliberately
excluded. They are observational and have no control or ledger semantics for
the interleaving prover. Ledger/environment reads will be modeled separately.

## Verification

`QuintVerifier` generates a deterministic Quint `run` from a concrete
`ExecutionTrace` and invokes:

```sh
quint test ... --main=replay_trace --match=execution_satisfies_spec
```

Every event must be enabled in sequence, and the entrypoint coordination script
must have returned with an empty call stack and no pending call frame. Quint
returns the first failed action as a rejected test trace.

Before running the replay, the verifier passes the same generated module
through `quint typecheck`. Static-analysis failures and model rejections are
reported separately.

The verifier passes the generated, self-contained Quint test module through
the child process's stdin and invokes `quint test /dev/stdin`. It does not
create temporary trace files. This transport is currently Unix-specific.

Install the repository-pinned Quint CLI and run the specification checks:

```sh
cd starstream-interleaving-spec
npm ci
npm run check
```

For model exploration, print one reproducible nondeterministic trace or open a
REPL with the simulation module preloaded:

```sh
npm run trace
npm run repl
```

Inside the REPL, call `init`, concrete semantic actions, or the nondeterministic
`step`, and evaluate `state` to inspect the current model state.

Run the Quint-backed Rust tests through npm so its local
`node_modules/.bin/quint` is available on `PATH`:

```sh
npm test
```

With a globally installed `quint`, the equivalent workspace command is:

```sh
cargo test -p starstream-interleaving-spec
```

To verify a JSON trace directly:

```sh
cargo run -p starstream-interleaving-spec \
  --bin starstream-verify-trace -- \
  starstream-interleaving-spec/examples/score-trace.json
```

Pass `-` instead of a path to read the trace from standard input. The JSON
format is the serde representation of `ExecutionTrace`; the example is also a
starting point for the Wasmtime projection layer.

The npm package pins Quint 0.32.0. A different executable may be supplied
directly with `QuintVerifier::new`.

## Wasmtime integration status

`TraceRecorder` and `TraceSink` define the semantic integration boundary.
`starstream-runtime-next` exposes backend-neutral instance hooks, while the
separate `starstream-proving-runtime` crate owns Neo-Wasm tracing and builds
paired host-event grammar and decoder templates.

The current vertical slice projects atomic linked UTXO constructor imports,
compiler-emitted `abis-clear` and `implements-method` calls, plus
interface-derived imported method calls and export-boundary return events.

`interleave_traces` merges already-decoded process-local traces. The first
trace is the entrypoint coordination script, and subsequent traces are
assigned to `NewUtxo` events in constructor order. `NewUtxo` contains the
arguments and returned caller-local resource handle observed atomically by
Neo-Wasm. The merger keeps that handle pending while it follows the
constructed trace, binds it to the new process when an initial yield reaches
`ReturnControl`, and resolves later `CallMethod` events through that binding.
Constructors are required to reach that initial yield: `CoroutineReturn` is
only valid on a later method/main-continuation turn. The merger also adds the
transaction-level `Init` event.

The runtime E2E test normalizes a constructor turn followed by a method turn
on one UTXO core instance. The scheduler takes the user arguments decoded from
`CallMethod`, supplies them as the method export's entry-bootstrap claims, and
then decodes the matching `EnterMethod`. The callee-local resource receiver is
a separate bootstrap claim: it is constrained against the executed Wasm local
but is not equated with the caller-local resource-table handle.

The control trace deliberately does not contain stable transaction process IDs
or program hashes. A scheduler allocates model-local process IDs while merging
the per-instance traces. The transaction/proof context is responsible for
binding its process slots to concrete input/output identities and program
hashes; those values constrain the Wasm/ledger proof, not coroutine
interleaving.

The intended runtime projection can provide:

- coordination-script start/return;
- imported UTXO constructor and method calls;
- `abis-clear` calls once compiler/template support is finalized;
- `implements-method` calls;
- the provisional terminal coroutine `return`/`burn` call.

The intended protocol emits `abis-clear` exactly once immediately before the
`implements-method` calls at every yield, including an empty-ABI yield.
At a `ReturnControl` export exit:

- a preceding `abis-clear` means the UTXO publishes a refreshed ABI at a fresh
  yield point;
- without `abis-clear`, a normally returning method preserves the previous
  ABI; and
- a preceding coroutine `return`/`burn` call leaves the coroutine terminated.

`Dead` in the persistent UTXO lifecycle is the interleaving model's logical
retirement boundary. Physical destruction of the guest resource happens in
the runtime after the export has returned and any Component Model borrow has
ended; it does not require a separate interleaving event. Transient states such
as ABI publication and a pending coroutine return live in the single
`active_turn` field: only `curr` can execute, so they are not duplicated in the
state of every UTXO.

No internal function reference, yield-global, or yield-site PC needs to enter
the semantic transcript.

The `neo_wasm::WasmtimeTraceHandler` integration in
`starstream-proving-runtime` is the current implementation reference.
`neo-wasm` supports the guest-to-guest Wasm `return_call` used by the current
Starstream `resume;` lowering. The interleaving projection does not need to
interpret that internal transfer.

## Cross-transaction persistence

The compiler's stackless-coroutine storage contains the yield selector and
saved locals. Restoring it resumes after the previous `yield`, so the
preceding `abis-clear` and `implements-method` host calls do not execute
again. A ledger model must therefore persist the yielded UTXO's enabled-method
set (or an authenticated commitment with a membership witness) and use it to
initialize the next transaction's execution state. Deriving the set from a
yield selector is only sound when that selector is checked against
program-hash-bound compiler metadata.

## Prototype absorbed-block decoding

`nightstream` begins the transport adapter between semantic `ExecutionEvent`s
and the eight-word blocks returned by
`neo_wasm::comm_chain::absorbed_event_blocks`. It does not model Nightstream's
IVC state or permutation schedule.

The encoding is not self-describing. The injected grammar template and static
component function type determine the block count and the meaning of every
slot. In the initial `CallMethod` assignment the resource handle is a named
slot, the method identity comes from the statically selected import, and the
argument and result widths come from its core function type:

```text
first block: [CallMethod, resource, value_0, ..., value_5]
continuation: [value_6, ..., value_13]
result block: [result_lo, result_hi, padding...]
```

A method export publishes the same flat result at its control-return boundary:

```text
[ReturnControl, result_lo, result_hi, padding...]
```

Before the method's first instruction, its export-entry template absorbs the
flattened user arguments and writes all entry locals through Neo-Wasm's
`ClaimLocal` bootstrap mechanism:

```text
[EnterMethod, value_0, ..., value_n, internal_receiver, local_0, ...]
```

The static template separates the user-argument prefix from internal
bootstrap words. `EnterMethod` exposes only that prefix to Quint. The internal
receiver and zero-initialized declared locals remain committed execution
claims, but they do not acquire caller-visible resource semantics.

The current prototype supports no result or one flat core `i32`/`i64` result.
Other result shapes are rejected until Neo-Wasm templates can commit an opaque
value digest.

Neo-Wasm observes a constructor import's arguments and result atomically.
`NewUtxo` therefore places the statically sized argument value followed by the
low/high limbs of the returned caller-local `i32` resource handle:

```text
[NewUtxo, value_0, ..., value_n, resource_lo, resource_hi, padding...]
```

The value width locates the resource statically, so continuation blocks need
no length or tag. A future genuinely dynamic value type can place a length in
its own canonical value encoding.

`TemplateRegistry` is the initial component-local registry for this
projection. A first block is dispatched by:

```text
(attributed_fref, first_block_discriminant)
```

The selected `EventTemplate` supplies the static block count and decoding
rule. This lets one function reference emit different entry/exit opcodes, and
prevents a continuation word that happens to equal an opcode from being
mistaken for a new event. Every continuation block must retain the first
block's `attributed_fref` and `turn_export_fref`.

The registry currently supports atomic `NewUtxo`, static result-bearing
`CallMethod`, method-entry `EnterMethod`, `ReturnControl`, and fixed control
templates. It is populated explicitly; deriving these entries from compiler
component metadata is the next integration boundary.
The metadata fields are circuit-constrained in a valid Nightstream trace but
are not part of the event-chain commitment, so the projector must only consume
them after the Wasm trace has been validated.

`ClearAbi`, UTXO `EnterMethod`/`ReturnControl`, and coordination-script
`CoordReturn` have Nightstream grammar templates. The current flat method
entry template supports `i32`/`i64` parameters and bootstraps every declared
local required by Neo-Wasm's multi-turn relation.
