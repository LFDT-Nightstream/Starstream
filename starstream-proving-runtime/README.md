# starstream-proving-runtime

Neo-Wasm integration for Starstream execution traces.

This crate is the dependency boundary between:

- `neo-wasm`, which captures and normalizes Wasm instructions and expands
  verifier-authored host-event grammar templates; and
- `starstream-runtime-next`, which exposes backend-neutral callbacks after
  component instantiation but before an exported function is called; and
- `starstream-interleaving-spec`, which decodes committed blocks into semantic
  events and replays complete traces through Quint.

`TracedContract` pairs a runtime contract with its Neo-Wasm program artifacts.
Its constructor, load, and coordination-script methods use the runtime's
`*_with_hook` entry points to register each new core instance before guest
execution. Consequently, `starstream-runtime-next` does not depend on
Neo-Wasm or enable Wasmtime's debug feature; those details live here.

`build_component_templates` parses the first core Wasm module and constructs
the emitter and decoder sides together. The current supported semantic events
are:

- `BeginNewUtxo`, carrying only the statically typed constructor arguments;
- `NewUtxoReturn`, derived from the constructor's returned guest resource
  handle;
- `ClearAbi`, derived from the fixed `abis-clear: func()` interface;
- `AdvertiseMethod`, derived from the four-`u64` `implements-method`
  interface; and
- `CallMethod`, whose payload width and continuation-block count are derived
  from the flattened imported method function type;
- `ReturnControl`, emitted by the exit template of every UTXO constructor and
  method export; and
- `CoordReturn`, emitted by coordination-script exit templates.

Other unsupported imports receive advice-only Neo-Wasm templates. Empty export
templates allow their turns to normalize without claiming semantic entry/exit
events.

The runtime E2E tests cover:

1. Starstream source compilation through a linked constructor and
   compiler-emitted `abis-clear`/`implements-method` calls, Neo-Wasm capture
   and normalization, commitment sanity checking, and per-instance semantic
   decoding.
2. A coordination component calling a runtime-linked UTXO method, including
   guest-resource resolution, interface-derived `CallMethod` blocks, and
   semantic decoding.

The second component is written directly in component WAT because
`if resource is Abi` currently typechecks but is not implemented by
`starstream-to-wasm`. Once that lowering exists, the test can use compiled
Starstream source on both sides.

Wasmtime runs all component instances in one `Store`, but Neo-Wasm keeps one
trace buffer per core instance because function references, PCs, and lowering
tables are module-local. The adapter preserves deterministic order inside each
instance. A control scheduler can flatten those lists without a store-global
instruction timestamp: it starts at the coordination script, switches to the
next allocated process on `BeginNewUtxo`, resolves and switches on
`CallMethod`, and pops its call stack on `ReturnControl`.
Constructor entry/return currently decode in the coordinator trace, while ABI
publication decodes in the constructed UTXO trace.

The per-instance process list is verifier advice, but the scheduler checks its
contiguous allocation order; process IDs are not guest values or host-call
claims. The transaction/proof context separately binds each process slot to
its on-ledger identity and program hash.
