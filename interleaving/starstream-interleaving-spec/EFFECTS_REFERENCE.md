# Verification

This document describes operational semantics for the
interleaving/transaction/communication circuit in a somewhat formal way (but
abstract).

Each opcode corresponds to an operation that a wasm program can do in a
transaction and involves communication with another program (utxo -> coord,
coord -> utxo, coord -> coord).

The rules are intented to be fairly low-level, and provide the contract with:

1. Attestation. Being able to enforce a caller coordination script, which can
also be used to attest the other utxos (the coordination script can also check
if a utxo
is instance of a contract).
2. Control flow. Resuming a process (coroutine) needs to enforce that only that
program can do an operation next. Yield is a resume with an implicit target (the
caller, or previous program).
3. Sent data matches received data. And viceversa. Which is enforced through the
memory argument. Note that for this machine data can be just treated as opaque
blobs (Value).

# 1. State Configuration

The global state of the interleaving machine σ is defined as:


```text
Configuration (σ)
=================
σ = (id_curr, id_prev, M, activation, init, ref_store, process_table, host_calls, counters, on_yield, yield_to, finalized, is_utxo, initialized, handler_stack, ownership, did_burn)

Where:
  id_curr          : ID of the currently executing VM. In the range [0..#coord + #utxo]
  id_prev          : ID of the VM that executed immediately before the current one (trace-local).
  expected_input   : A map {ProcessID -> Ref}
  expected_resumer : A map {ProcessID -> ProcessID}
  on_yield         : A map {ProcessID -> Bool} (true if the process most recently yielded)
  yield_to         : A map {ProcessID -> Option<ProcessID>} (who to return to on yield)
  activation       : A map {ProcessID -> Option<(Ref, ProcessID)>}
  init             : A map {ProcessID -> Option<(Value, ProcessID)>}
  ref_store        : A map {Ref -> Value}
  process_table    : Read-only map {ID -> ProgramHash} for attestation.
  host_calls       : A map {ProcessID -> Host-calls lookup table}
  counters         : A map {ProcessID -> Counter}
  finalized        : A map {ProcessID -> Bool} (true if the process ends the transaction with a final yield)
  is_utxo          : Read-only map {ProcessID -> Bool}
  initialized      : A map {ProcessID -> Bool}
  handler_stack    : A map {InterfaceID -> Stack<ProcessID>}
  ownership        : A map {ProcessID -> Option<ProcessID>} (token -> owner)
  did_burn         : A map {ProcessID -> Bool}
```

Note that the maps are used here for convenience of notation. In practice they
are memory arguments enforced through an auxiliary protocol, like Twist And
Shout or Nebula. I'm also going to notate memory reads as preconditions, even if
in practice it's closer to emitting a constraint. But using a memory is easier
to reason about.

Depending on the memory implementation it may be simpler to just flatten all the
maps into a multi-valued memory, or it may be better to have a flat memory and
use offsets (easy since all are fixed length).

The rules are expressed in a notation inspired by denotational semantics, but
it's not functional. Only the fields that change are specified in the
post-condition, the rest can be assumed to remain equal.
A tick (') is used to indicate _next state_.

The notation should be understood as

```text
requirements (pattern match + conditions)
------------------------------------------
new_state (assignments)
```

---

# 2. Shared Operations

## Resume (Call)

The primary control flow operation. Transfers control to `target`. It records a
"claim" that this process can only be resumed by passing `ret` as an argument
(since it is what actually happpened).

Since we are also resuming a currently suspended process, we can only do it if
our value matches its claim.

When a process yields, it sets `on_yield = true`. The next resumer records
`yield_to[target] = id_curr` and clears `on_yield`. Subsequent resumes do not
change `yield_to` unless the process yields again.

```text
Rule: Resume
============
    op = Resume(target, val_ref) -> (ret_ref, caller)

    1. id_curr ≠ target

    (No self resume)

    2. let val = ref_store[val_ref] in
       if expected_input[target] is set, it must equal val

    (Check val matches target's previous claim)

    3. if expected_resumer[target] is set, it must equal id_curr

    (Check that the current process matches the expected resumer for the target)

    4. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Resume, target, val_ref, ret_ref, caller>

    (The opcode matches the host call lookup table used in the wasm proof at the current index)

    5. is_utxo(id_curr) => !is_utxo(target)

    (Utxo's can't call into utxos)

    6. initialized[target]

    (Can't jump to an unitialized process)
--------------------------------------------------------------------------------------------
    1. expected_input[id_curr]   <- ret_ref   (Claim, needs to be checked later by future resumer)
    2. expected_resumer[id_curr] <- caller   (Claim, needs to be checked later by future resumer)
    3. counters'[id_curr]        += 1         (Keep track of host call index per process)
    4. id_prev'                  <- id_curr   (Trace-local previous id)
    5. id_curr'                  <- target    (Switch)
    6. if on_yield[target] then
           yield_to'[target]     <- Some(id_curr)
           on_yield'[target]     <- False
    7. activation'[target]       <- Some(val_ref, id_curr)
```

## Activation

Rule: Activation
===========
    op = Activation() -> (val, caller)

    1. activation[id_curr] == Some(val, caller)

    2. let t = CC[id_curr] in
       let c = counters[id_curr] in
           t[c] == <Activation, val, caller>

-----------------------------------------------------------------------
    1. counters'[id_curr] += 1

## Init

Rule: Init
===========
    op = Init() -> (val, caller)

    1. init[id_curr] == Some(val, caller)

    2. let t = CC[id_curr] in
       let c = counters[id_curr] in
           t[c] == <Init, val, caller>

-----------------------------------------------------------------------
    1. counters'[id_curr] += 1


## Yield

Suspend the current continuation and optionally transfer control to the previous
coordination script (since utxos can't call utxos, that's the only possible
parent).

This also marks the utxo as **safe** to persist in the ledger.

If the utxo is not iterated again in the transaction, the return value is null
for this execution (next transaction will have to execute `yield` again, but
with an actual result).

```text
Rule: Yield (resumed)
============
    op = Yield(val_ref) -> (ret_ref, caller)

    1. yield_to[id_curr] is set

    2. let val = ref_store[val_ref] in
       if expected_input[yield_to[id_curr]] is set, it must equal val

    (Check val matches target's previous claim)

    3. if expected_resumer[yield_to[id_curr]] is set, it must equal id_curr

    (Check that the current process matches the expected resumer for the parent)

    4. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Yield, val_ref, ret_ref, caller>

    (The opcode matches the host call lookup table used in the wasm proof at the current index)
--------------------------------------------------------------------------------------------

    1. expected_input[id_curr]   <- ret_ref       (Claim, needs to be checked later by future resumer)
    2. expected_resumer[id_curr] <- caller      (Claim, needs to be checked later by future resumer)
    3. counters'[id_curr]        += 1         (Keep track of host call index per process)
    4. on_yield'[id_curr]        <- True      (The next resumer sets yield_to)
    5. id_curr'                  <- yield_to[id_curr]   (Switch to parent)
    6. id_prev'                  <- id_curr   (Trace-local previous id)
    7. finalized'[id_curr]       <- False
    8. activation'[id_curr]      <- None
```

```text
Rule: Yield (end transaction)
=============================
    op = Yield(val_ref)

    1. yield_to[id_curr] is set

    2. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Yield, val_ref, null, caller>

    (Remember, there is no ret value since that won't be known until the next transaction)

    (The opcode matches the host call lookup table used in the wasm proof at the current index)
--------------------------------------------------------------------------------------------
    1. expected_resumer[id_curr] <- caller (Claim, needs to be checked later by future resumer)
    2. counters'[id_curr]        += 1        (Keep track of host call index per process)
    3. on_yield'[id_curr]        <- True     (The next resumer sets yield_to)
    4. id_curr'                  <- yield_to[id_curr]  (Switch to parent)
    5. id_prev'                  <- id_curr  (Trace-local previous id)
    6. finalized'[id_curr]       <- True     (This utxo creates a transaction output)
    7. activation'[id_curr]      <- None
```

## Program Hash

Allows introspection of a Continuation's code identity without changing control flow.

```text
Rule: Program Hash
==================
    (Lookup the static hash of a program ID. State remains unchanged.)

    op = ProgramHash(target_id)
    hash = process_table[target_id]

    let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <ProgramHash, program_hash>

    (Host call lookup condition)

-----------------------------------------------------------------------
    1. counters'[id_curr] += 1
```

---

# 3. Coordination Script Operations

## New UTXO

```text
Rule: New UTXO
==============
Assigns a new (transaction-local) ID for a UTXO program.

    op = NewUtxo(program_hash, val) -> id

    1. process_table[id] == program_hash

    (The hash matches the one in the process table)
    (Remember that this is just verifying, so we already have the full table)

    2. counters[id] == 0

    (The trace for this utxo starts fresh)

    3. is_utxo[id]

    (We check that it is indeed a utxo)

    4. is_utxo[id_curr] == False

    (A utxo can't crate utxos)

    5. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <NewUtxo, program_hash, val, id>

    (Host call lookup condition)

-----------------------------------------------------------------------
    1. initialized[id]     <- True
    2. init'[id]           <- Some(val, id_curr)
    3. counters'[id_curr]  += 1
```

## New Coordination Script (Spawn)

Allocates a new transient VM ID. The start is "Cold" (it does not execute immediately).

```text
Rule: New coord (Spawn)
=======================
Assigns a new (transaction-local) ID for a coordination script (an effect
handler) instance.

    op = NewCoord(program_hash, val) -> id

    1. process_table[id] == program_hash

    (The hash matches the one in the process table)
    (Remember that this is just verifying, so we already have the full table)

    2. counters[id] == 0

    (The trace for this handler starts fresh)

    3. is_utxo[id] == False

    (We check that it is a coordination script)

    4. is_utxo[id_curr] == False

    (A utxo can't spawn coordination scripts)

    5. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <NewCoord, program_hash, val, id>

    (Host call lookup condition)

-----------------------------------------------------------------------
    1. initialized[id]     <- True
    2. init'[id]           <- Some(val, id_curr)
    3. counters'[id_curr]  += 1
```

---

# 4. Effect Handler Operations

## Install Handler

Pushes the current program ID onto the handler stack for a given interface. This operation is restricted to coordination scripts.

```text
Rule: Install Handler
=====================
    op = InstallHandler(interface_id)

    1. is_utxo[id_curr] == False

    (Only coordination scripts can install handlers)

    2. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <InstallHandler, interface_id>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. handler_stack'[interface_id].push(id_curr)
    2. counters'[id_curr] += 1
```

## Uninstall Handler

Pops a program ID from the handler stack for a given interface. This operation is restricted to coordination scripts.

```text
Rule: Uninstall Handler
=======================
    op = UninstallHandler(interface_id)

    1. is_utxo[id_curr] == False

    (Only coordination scripts can uninstall handlers)

    2. handler_stack[interface_id].top() == id_curr

    (Only the installer can uninstall)

    3. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <UninstallHandler, interface_id>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. handler_stack'[interface_id].pop()
    2. counters[id_curr] += 1
```

## Get Handler For

Retrieves the handler for a given interface without altering the handler stack.

```text
Rule: Get Handler For
=====================
    op = GetHandlerFor(interface_id) -> handler_id

    1. handler_id == handler_stack[interface_id].top()

    (The returned handler_id must match the one on top of the stack)

    2. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <GetHandlerFor, interface_id, handler_id>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. counters'[id_curr] += 1
```

---

# 5. UTXO Operations

## Burn

Terminates the UTXO. No matching output is created in the ledger.

```text
Rule: Burn
==========
Destroys the UTXO state.

    op = Burn(ret)

    1. is_utxo[id_curr]
    2. is_initialized[id_curr]
    3. did_burn[id_curr]

    4. if expected_input[id_prev] is set, it must equal ret

    (Resume receives ret)

    5. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Burn, ret>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. finalized'[id_curr]      <- True
    2. id_curr'                 <- id_prev

    (Control flow goes to caller)

    3. initialized'[id_curr]    <- False

    (It's not possible to return to this, maybe it should be a different flag though)

    4. counters'[id_curr]       += 1

    5. activation'[id_curr]     <- None
    6. did_burn'[id_curr]       <- True
```

# 6. Tokens

## Bind

```text
Rule: Bind (token calls)
========================
    op = Bind(owner_id)

    token_id = id_curr

    1. is_utxo[token_id] == True
       (Only UTXOs can be tokens)

    2. is_utxo[owner_id] == True
       (Owners are UTXOs)

    3. initialized[token_id] == True
       initialized[owner_id] == True
       (Both exist in this transaction's process set)

    4. ownership[token_id] == ⊥
       (Token must currently be unbound)

    5. let t = CC[token_id] in
       let c = counters[token_id] in
           t[c] == <Bind, owner_id>
       (Host call lookup condition)
-----------------------------------------------------------------------
    1. ownership'[token_id] <- owner_id
    2. counters'[token_id]  += 1
```

## Unbind

```text
Rule: Unbind (owner calls)
==========================
    op = Unbind(token_id)

    owner_id = id_curr

    1. is_utxo[owner_id] == True

    2. is_utxo[token_id] == True
       initialized[token_id] == True
       (Token exists in this transaction's process set)

    3. ownership[token_id] == owner_id
       (Authorization: only current owner may unbind)

    4. let t = CC[owner_id] in
       let c = counters[owner_id] in
           t[c] == <Unbind, token_id>
-----------------------------------------------------------------------
    1. ownership'[token_id] <- ⊥
    2. counters'[owner_id]  += 1
```

# 7. Data Operations

## NewRef

Allocates a new reference with a specific size (in 4-value words).

```text
Rule: NewRef
==============
    op = NewRef(size_words) -> ref

    1. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <NewRef, size, ref>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. size fits in 16 bits
    2. ref_store'[ref] <- [uninitialized; size_words * 4] (conceptually)
    3. ref_sizes'[ref] <- size_words
    4. counters'[id_curr] += 1
    5. ref_state[id_curr] <- (ref, 0, size_words) // storing the ref being built, current word offset, and total size
```

## RefPush

Appends data to the currently building reference.

```text
Rule: RefPush
==============
    op = RefPush(vals[4])

    1. let (ref, offset_words, size_words) = ref_state[id_curr]
    2. offset_words < size_words

    3. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <RefPush, vals[4]>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. for i in 0..3:
            ref_store'[ref][(offset_words * 4) + i] <- vals[i]
    2. ref_state[id_curr] <- (ref, offset_words + 1, size_words)
    3. counters'[id_curr] += 1
```

## RefGet

```text
Rule: RefGet
==============
    op = RefGet(ref, offset_words) -> vals[4]

    1. let size_words = ref_sizes[ref]
    2. offset_words < size_words
    3. for i in 0..3:
            vals[i] == ref_store[ref][(offset_words * 4) + i]

    2. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <RefGet, ref, offset_words, vals[4]>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. counters'[id_curr] += 1
```

## RefWrite

```text
Rule: RefWrite
==============
    op = RefWrite(ref, offset_words, vals[4])

    1. let size_words = ref_sizes[ref]
    2. offset_words < size_words

    3. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <RefWrite, ref, offset_words, vals[4]>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. for i in 0..3:
            ref_store'[ref][(offset_words * 4) + i] <- vals[i]
    2. counters'[id_curr] += 1
```

# Verification

To verify the transaction, the following additional conditions need to be met:

```text
for (process, proof, host_calls) in transaction.proofs:

    // we verified all the host calls for each process

    assert(counters[process] == host_calls[process].length)

    // every object had a constructor of some sort
    assert(is_initialized[process])

    // all the utxos either did `yield` at the end, or called `burn`
    if is_utxo[process] {
        assert(finalized[process] || did_burn[process])
    }

assert_not(is_utxo[id_curr])

// we finish in a coordination script
```
