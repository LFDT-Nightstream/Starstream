# starstream-proving-runtime

Wasm instruction-tracing integration for Starstream execution traces.

This crate is the dependency boundary between:

- `neo-wasm`, which captures and normalizes Wasm instructions and expands
  verifier-authored host-event grammar templates; and
- `starstream-runtime-next`, which executes Starstream components and provides
  their host-facing runtime integration; and
- `starstream-interleaving-spec`, which decodes committed blocks into semantic
  events and replays complete traces through Quint.

`TracedContract` pairs a runtime contract with its Wasm tracing artifacts. Its
`instantiate` method registers each new core instance before returning the
runtime's `ContractInstance`; the caller can then invoke a constructor, storage
setter, or coordination script. Consequently, `starstream-runtime-next` does
not depend on `neo-wasm` or enable Wasmtime's debug feature; those details live
here.

`build_component_templates` parses the first core Wasm module and constructs
the emitter and decoder sides together. Its coordination-export allowlist is
explicit: compiler-shaped UTXO exports are classified automatically, but
every plain function export must be named as a coordination script. An
unclassified plain export such as `cabi_realloc` is rejected instead of
silently producing `CoordReturn`; a missing allowlist entry is also an error.

The current supported semantic events are:

- `NewUtxo`, atomically carrying the statically typed constructor arguments
  and its returned caller-local resource handle;
- `ClearAbi`, derived from the fixed `abis-clear: func()` interface;
- `AdvertiseMethod`, derived from the four-`u64` `implements-method`
  interface; and
- `CallMethod`, whose argument and result widths and continuation-block count
  are derived from the flattened imported method function type;
- `EnterMethod`, emitted before the first instruction of a UTXO method export.
  Its user-argument prefix is compared with `CallMethod`, while its internal
  receiver and declared-local words only bootstrap the executed entry frame;
- `ReturnControl`, emitted by the exit template of every UTXO constructor and
  method export. Method exits publish the same flat result committed by their
  caller's atomic import, while constructor exits have no semantic result; and
- `CoordReturn`, emitted by coordination-script exit templates.

For now, method parameters must flatten to `i32`/`i64`, and a result must be
absent or be one flat core `i32` or `i64`. Other shapes fail template
construction explicitly. This keeps the caller/callee equality rules in place
without prematurely choosing the encoding for a future opaque
Starstream-value digest.

Other unsupported imports receive advice-only host-event templates. Empty export
templates allow their turns to normalize without claiming semantic entry/exit
events.

The runtime E2E tests cover:

1. Starstream source compilation through a linked constructor and
   compiler-emitted `abis-clear`/`implements-method` calls, instruction capture
   and normalization, commitment sanity checking, and per-instance semantic
   decoding.
2. A coordination component calling a runtime-linked UTXO method, including
   guest-resource resolution, interface-derived `CallMethod` blocks,
   scheduler-derived entry claims, two-turn UTXO normalization, and matching
   `EnterMethod`/`ReturnControl` semantic decoding.

The second component is written directly in component WAT because
`if resource is Abi` currently typechecks but is not implemented by
`starstream-to-wasm`. Once that lowering exists, the test can use compiled
Starstream source on both sides.

Wasmtime runs all component instances in one `Store`, but the tracing adapter
keeps one trace buffer per core instance because function references, PCs, and
lowering tables are module-local. The adapter preserves deterministic order
inside each instance. A control scheduler can flatten those lists without a
store-global instruction timestamp: it starts at the coordination script,
switches to the next allocated process on `NewUtxo`, resolves and switches on
`CallMethod`, checks its user arguments at `EnterMethod`, and pops its call
stack on `ReturnControl`. A `NewUtxo` handle
stays pending until that constructor return, when it becomes bound to the new
process. The atomic constructor import decodes in the coordinator trace, while
ABI publication decodes in the constructed UTXO trace.

The per-instance process list is verifier advice, but the scheduler checks its
contiguous allocation order; process IDs are not guest values or host-call
claims. The transaction/proof context separately binds each process slot to
its on-ledger identity and program hash.
