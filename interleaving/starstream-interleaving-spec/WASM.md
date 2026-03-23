# WASM Starstream hooks

This document describes the interface between:

1. a zkWASM / component-model execution engine, and
2. the Starstream interleaving semantics in
   [EFFECTS_REFERENCE.md](./EFFECTS_REFERENCE.md).

The goal is to make the interaction between programs proveable without forcing
guest code to manually emit interleaving effects in the source language.

This document is intentionally narrower than "all of Wasm components". It
describes the extra information and rewrite rules needed for Starstream-aware
calls between components/processes.

## 1. Inputs available to the zkVM adapter

To generate the Starstream trace, the zkVM adapter needs access to:

1. The component-level function signature being called.
2. A stable `function_id` for that function.
3. The canonical ABI lowering/lifting plan for the parameter and result types.
4. The stack and/or linear-memory reads and writes used by that lowering.

The important point is that the trace cannot be derived from the lowered core
Wasm signature alone. For example, two `i64` arguments could mean:

- two unrelated scalar arguments
- one tuple `(u64, u64)`
- one record `{ left: u64, right: u64 }`

The Starstream proof therefore works from the original component-level type and
the canonical ABI lowering for that type.

TBD:
- define the canonical `function_id` assignment rule
- define how guest-side resource/instance handles are resolved to global
  `ProcessId`s for interleaving traces

## 2. Starstream type universe and logical value model

The current Starstream type universe is defined in
[`starstream-types/src/types.rs`](../../starstream-types/src/types.rs) and
currently includes:

- integers: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- `bool`
- `()`
- tuples
- records
- enums
- `Utxo`
- named UTXO resource types
- function types with an effect annotation

For proving cross-process communication, the adapter first reconstructs a
logical value from the canonical ABI boundary.

This is not the witness format. It is an internal logical representation used
to connect:

- component-level function signatures
- canonical ABI stack/memory behavior
- Starstream ref encoding

A more complete logical value model is:

```text
V ::= Int(width, value)
    | Bool(bool)
    | Unit
    | Tuple(V*)
    | Record((FieldName, V)*)
    | Enum(VariantName, Payload)
    | Resource(ResourceKind, Handle)
```

where enum payloads can be:

```text
Payload ::= UnitPayload
          | TuplePayload(V*)
          | RecordPayload((FieldName, V)*)
```

Function types are not currently first-class transport values in Starstream's
inter-process ABI. They describe callable interfaces, not values that are
serialized into `Ref`s.

## 3. Mapping from DSL types

The Starstream backend must define a mapping from source-language Starstream
types into:

1. component/WIT types at the ABI level, and
2. logical values / Starstream ref encoding at the proof level.

The intended mapping is:

| Starstream type | Component/WIT view | Logical value / transport view |
| --- | --- | --- |
| `i8`, `i16`, `i32`, `i64` | signed integer of same width | `Int(width, value)` |
| `u8`, `u16`, `u32`, `u64` | unsigned integer of same width | `Int(width, value)` |
| `bool` | `bool` | `Bool(bool)` |
| `()` | unit / empty tuple | `Unit` |
| `(T0, ..., Tn)` | tuple | `Tuple(V0, ..., Vn)` |
| `record { ... }` | record | `Record(...)` |
| `enum E` | variant | `Enum(variant, payload)` |
| `Utxo` | resource handle | `Resource(Utxo, handle)` |
| named UTXO type | resource handle | `Resource(UtxoNamed(name), handle)` |
| function type | function signature | not serialized as a transport value (not first class) |

## 4. Starstream ref encoding

Starstream shared values are represented with explicit `Ref`s.

Each logical value `V` is encoded into a sequence of 4-lane words:

```text
encode_to_ref_words : V -> [Value; 4]*
```

This encoding is the source of truth for:

- the `size_words` argument of `NewRef`
- the number of `RefPush` operations to emit
- the number of `RefGet` operations needed to reconstruct a value

The exact field/word encoding is a Starstream choice, independent from the
canonical ABI. The canonical ABI tells us how to read/write the guest-side
value; the Starstream ref encoding tells us how to commit that value into the
interleaving proof.

A useful way to define it is in two stages:

```text
encode_to_field_elems : V -> FieldElem*
pack_ref_words        : FieldElem* -> [Value; 4]*

encode_to_ref_words(v) = pack_ref_words(encode_to_field_elems(v))
```

The choice of 4 lanes per ref word is not arbitrary source-language semantics;
it is a proving/trace batching detail.

The interleaving proof currently processes a constant number of values per
folding step, and the `RefPush` / `RefGet` / `RefWrite` width is chosen to fit
that budget efficiently. The exact maximum batch size depends on the trace
commitment construction used by the proof system.

At the moment, the implementation uses Poseidon2 with width 12 for the trace
commitment path, and some of those slots are already consumed by the previous
commitment and other arguments. So the current choice of 4 data lanes
is mainly an optimization point for the existing proof layout, not a deep
language-level constant.

This means the batching width may change in the future if the trace commitment
scheme or folding layout changes. The semantic requirement is only that both
the zkVM adapter and the interleaving proof agree on the same packing rule.

The intended initial mapping is:

```text
encode_to_field_elems(Int(width, x)) = encode_int(width, x)
encode_to_field_elems(Bool(b))       = [if b then 1 else 0]
encode_to_field_elems(Unit)          = []
encode_to_field_elems(Tuple(vs))     = concat(encode_to_field_elems(v) for v in vs)
encode_to_field_elems(Record(fields))
  = concat(encode_to_field_elems(value) for fields in declared field order)
encode_to_field_elems(Enum(tag, payload))
  = [tag] ++ encode_payload_to_field_elems(payload)
encode_to_field_elems(Resource(_, handle)) = [handle]
```

where:

```text
encode_payload_to_field_elems(UnitPayload) = []
encode_payload_to_field_elems(TuplePayload(vs))
  = concat(encode_to_field_elems(v) for v in vs)
encode_payload_to_field_elems(RecordPayload(fields))
  = concat(encode_to_field_elems(value) for fields in declared field order)
```

and packing into ref words is:

```text
pack_ref_words([]) = []
pack_ref_words(xs) = split xs into groups of 4 and zero-pad the final group
```

### Integers

Because the current proof system uses the Goldilocks field, the integer
encoding cannot blindly treat all Starstream integers as single field elements.
In particular, arbitrary 64-bit integers do not fit injectively into one
Goldilocks element.

So `encode_int(width, x)` must be width-aware. Current convention is:

```text
encode_int(i8/i16/i32/u8/u16/u32, x) = [canonical_u32_repr(x)]
encode_int(i64/u64, x)               = [lo32(x), hi32(x)]
```

where signed integers use their fixed-width two's-complement bit pattern before
splitting into 32-bit limbs.

More explicitly:

```text
canonical_u32_repr(x) =
  the unsigned 32-bit integer whose bit pattern is the fixed-width
  representation of x

lo32(x) =
  the low 32-bit limb of the 64-bit representation of x, interpreted as an
  unsigned integer in [0, 2^32)

hi32(x) =
  the high 32-bit limb of the 64-bit representation of x, interpreted as an
  unsigned integer in [0, 2^32)
```

For `i64`, this means:

1. take the 64-bit two's-complement bit pattern of `x`
2. split it into low and high 32-bit limbs
3. encode each limb as its unsigned integer value

For `u64`, this means:

1. take the ordinary 64-bit unsigned representation of `x`
2. split it into low and high 32-bit limbs
3. encode each limb as its unsigned integer value

The limb order is always:

```text
[low_32_bits, high_32_bits]
```

This ordering is part of the Starstream encoding convention and does not depend
on the host machine's native endianness.

If the proof system later switches field or adopts a different integer encoding,
this function is the place where that change must be specified.

### Function arguments

Multiple function arguments are encoded by first forming a logical tuple in
declared parameter order, then encoding that tuple:

```text
encode_call_args(arg0, ..., argn) = encode_to_ref_words(Tuple(arg0, ..., argn))
```

So zero-argument calls naturally encode to an empty tuple and therefore a
zero-sized ref:

```text
encode_call_args() = encode_to_ref_words(Tuple()) = []
```

### Properties

Note that this encoding is not self-describing. For example, an `i64` may occupy two
field elements while an `i32` occupies one, but that arity is not tagged inside
the ref payload itself.

Instead, the boundaries are derived from the known function signature and the
agreed Starstream type-to-encoding rules. In other words, the decoder must
already know the expected WIT/component type of the payload in order to split
the field-element stream back into values correctly.

## 5. Sender-side rewrite rule

Consider a call from process `P` to an imported function:

```text
call F(args)
```

where `F` is known, from component metadata, to correspond to a Starstream
operation such as `Resume(target, f_id, val_ref)` or
`CallEffectHandler(interface_id, f_id, val_ref)`.

The zkVM adapter should logically rewrite this as:

1. Decode the argument value(s) from the canonical ABI boundary into a logical
   value `V`.
   - For flat canonical ABI values, this means reading stack/immediate values.
   - For memory-backed canonical ABI values, this means reading the required
     linear-memory range according to the canonical ABI lowering.

2. Compute:

```text
words = encode_to_ref_words(V)
```

3. Emit:

```text
NewRef(len(words)) -> r
RefPush(words[0])
...
RefPush(words[n-1])
```

While a process is emitting this `RefPush` sequence for a newly allocated ref,
it must not emit any other Starstream effect in between. In the interleaving
spec this is the `ref_building_state` constraint: `NewRef` plus the following
`RefPush` rows form one decomposed logical allocation/initialization step.

4. Emit the corresponding Starstream control/effect opcode using `r`.

### Concrete sender example

Suppose the imported function is:

```text
foo(
  x: i32,
  y: u64,
  p: record { left: u64, right: bool },
  e: enum Choice { A, B(u64), C { z: u32 } }
)
```

and the caller invokes:

```text
foo(
  x = -7,
  y = 9,
  p = { left = 5, right = true },
  e = B(11),
)
```

The call arguments are treated as one logical tuple:

```text
Tuple(
  Int(i32, -7),
  Int(u64, 9),
  Record(("left", Int(u64, 5)), ("right", Bool(true))),
  Enum(1, TuplePayload(Int(u64, 11))),
)
```

Using the current sketch:

```text
encode_to_field_elems(Int(i32, -7)) = [4294967289]
encode_to_field_elems(Int(u64, 9))  = [9, 0]
encode_to_field_elems(Record(...))  = [5, 1]
encode_to_field_elems(Enum(1, TuplePayload(Int(u64, 11)))) = [1, 11, 0]
```

So the full call payload is:

```text
encode_to_field_elems(call_args)
  = [4294967289, 9, 0, 5, 1, 1, 11, 0]
```

and therefore:

```text
encode_to_ref_words(call_args)
  = [
      [4294967289, 9, 0, 5],
      [1, 1, 11, 0],
    ]
```

The corresponding Starstream trace fragment is:

```text
NewRef(2) -> r
RefPush([4294967289, 9, 0, 5])
RefPush([1, 1, 11, 0])
Resume(target, f_id = foo, val_ref = r) -> (...)
```

TBD: the spec still needs to define how the guest-visible `target` handle in a
component/resource call is resolved into the `ProcessId` used by the
interleaving trace.

For a zero-argument call:

```text
foo()
```

the expected trace fragment is:

```text
NewRef(0) -> r
Resume(target, f_id = foo, val_ref = r) -> (...)
```

For example:

```text
call imported method target.amount()
```

can rewrite to something like:

```text
Resume(target_pid, f_id = amount, val_ref = r)
```

where `r` encodes the method arguments (possibly an empty tuple).

## 6. Receiver-side rewrite rule

Starstream already has explicit sources of incoming refs:

- `Activation()` for resumed calls
- `Init()` for newly created processes

So the receiver-side rule is:

1. Read the incoming ref explicitly from `Activation()` or `Init()`.
2. Emit `RefGet` operations against that ref to recover the Starstream word
   sequence.
3. Decode those words back into the logical value `V`.
4. Re-lower `V` into the guest's expected canonical ABI representation:
   - write to stack/value lanes for flat cases
   - write to linear memory for out-pointer / memory-backed cases

For memory-backed canonical ABI values, these linear-memory reads and writes are
not "free". They must be witnessed by the zkVM's own memory proof system. The
Starstream trace only specifies how those reads/writes are connected to the
inter-process `Ref` transport; the actual memory accesses still need to be
proved by the underlying zkVM.

So a call receiver is proved by:

```text
Activation() -> (arg_ref, caller)
RefGet(arg_ref, 0) -> ...
...
canonical writeback into guest stack/memory
```

This is the symmetric counterpart to the sender-side `NewRef` / `RefPush`
encoding.

### Concrete receiver example

Continuing the sender example above, suppose the callee was resumed with the
payload ref `r`.

The callee-side trace begins from the explicit activation:

```text
Activation() -> (r, caller)
```

The adapter then reads enough words to reconstruct the logical tuple:

```text
RefGet(r, 0) -> [4294967289, 9, 0, 5]
RefGet(r, 1) -> [1, 1, 11, 0]
```

Flattening those words and decoding them using the function signature yields:

```text
Tuple(
  Int(i32, -7),
  Int(u64, 9),
  Record(("left", Int(u64, 5)), ("right", Bool(true))),
  Enum(1, TuplePayload(Int(u64, 11))),
)
```

The zkVM adapter must then re-lower those values according to the callee's
canonical ABI expectations:

- flat canonical ABI cases write stack/value lanes
- memory-backed canonical ABI cases write linear memory according to the
  lowering plan

For a zero-argument function, the receiver-side flow is simply:

```text
Activation() -> (r, caller)
```

with no payload `RefGet`s, because the parameter tuple is empty and therefore
encodes to zero field elements.

## 7. Result values

Results follow the same pattern.

There are two useful cases:

1. Direct local results inside one VM.
   These do not need Starstream communication semantics.

2. Cross-process results.
   These should be represented as explicit refs and traced the same way as
   arguments.

For the current Starstream call model, the important cross-process return paths
are:

- `Yield(result_ref)` from a UTXO
- future handler/method return conventions, which should also resolve to
  explicit refs

On the receiving side, those refs are decoded through the same `RefGet` +
canonical ABI lifting rule described above.

## 8. Function boundaries

To prove that the intended function actually ran, the zkVM adapter must also
emit function-frame events:

- `Enter(function_id)`
- `Exit`

These function ids come from component metadata plus a stable function-id
assignment rule.

The minimum requirement is:

1. When Starstream transfers control to a process with
   `Resume(..., f_id, ...)` or `CallEffectHandler(..., f_id, ...)`, the target
   process must next emit `Enter(f_id)`.
2. Function exit must be witnessed explicitly with `Exit`, or implicitly by a
   tail-resumptive operation if the interleaving semantics say so.

## 9. Starstream backend obligations

A VM / prover backend qualifies as a Starstream backend only if it:

- reconstructs calls using the component/WIT function signature, not just the
  lowered core Wasm types
- uses the Starstream-defined `function_id` convention
- emits `Enter` / `Exit` in the places required by the Starstream call
  semantics
- uses the Starstream-defined ref encoding for cross-process values
- emits the Starstream interleaving effects implied by those calls

These are the proof-critical obligations. Without them, a backend may still
produce some internally consistent proof, but it would not be proving
conformance to the Starstream call ABI and interleaving semantics.

## 10. Implementation freedom

A backend may still differ in:

- whether it instruments guest code, host adapters, or both
- whether it is Wasm-based or uses some other VM internally
- how it internally represents memory, stacks, and component adapters

As long as the backend satisfies the obligations above, these are engineering
choices rather than semantic differences.

Because this is a spec document, the mapping above should be understood as the
target Starstream ABI/semantics, even if concrete implementations lag behind for
some cases. In particular, memory-backed canonical ABI types should still be
handled by the same sender/receiver rules above; they just require the adapter
to witness the relevant linear-memory reads and writes.
