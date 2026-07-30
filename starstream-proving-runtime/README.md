# starstream-proving-runtime

Wasm instruction-tracing integration for Starstream execution traces.

This crate is the dependency boundary between:

- `neo-wasm`, which captures and normalizes Wasm instructions and expands
  verifier-authored host-event grammar templates; and
- `starstream-runtime-next`, which exposes backend-neutral callbacks after
  component instantiation but before an exported function is called; and
- `starstream-interleaving-spec`, which decodes committed blocks into semantic
  events and replays complete traces through Quint.

`TracedContract` pairs a runtime contract with its Wasm tracing artifacts.
It installs a post-instantiation hook on the runtime contract, which registers
each new core instance before the constructor, storage setter, or coordination
script begins executing. Consequently, `starstream-runtime-next` does not
depend on `neo-wasm` or enable Wasmtime's debug feature; those details live here.

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
- `CallMethod`, whose payload width and continuation-block count are derived
  from the flattened imported method function type;
- `ReturnControl`, emitted by the exit template of every UTXO constructor and
  method export; and
- `CoordReturn`, emitted by coordination-script exit templates.

Other unsupported imports receive advice-only host-event templates. Empty export
templates allow their turns to normalize without claiming semantic entry/exit
events.

The runtime E2E tests cover:

1. Starstream source compilation through a linked constructor and
   compiler-emitted `abis-clear`/`implements-method` calls, instruction capture
   and normalization, commitment sanity checking, and per-instance semantic
   decoding.
2. A coordination component calling a runtime-linked UTXO method, including
   guest-resource resolution, interface-derived `CallMethod` blocks, and
   semantic decoding.

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
`CallMethod`, and pops its call stack on `ReturnControl`. A `NewUtxo` handle
stays pending until that constructor return, when it becomes bound to the new
process. The atomic constructor import decodes in the coordinator trace, while
ABI publication decodes in the constructed UTXO trace.

The per-instance process list is verifier advice, but the scheduler checks its
contiguous allocation order; process IDs are not guest values or host-call
claims. The transaction/proof context separately binds each process slot to
its on-ledger identity and program hash.
