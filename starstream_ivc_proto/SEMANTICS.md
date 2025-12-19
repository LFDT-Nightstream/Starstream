This document describes operational semantics for the
interleaving/transaction/communication circuit in a somewhat formal way (but
abstract). The [README](README.md) has a high-level description of the general architecture and how this things are used (and the motivation).

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
σ = (id_curr, id_prev, M, process_table, host_calls, counters, safe_to_ledger, is_utxo, initialized, handler_stack)

Where:
  id_curr        : ID of the currently executing VM. In the range [0..#coord + #utxo]
  id_prev        : ID of the VM that called the current one (return address).
  M              : A map {ProcessID -> Value}
  process_table  : Read-only map {ID -> ProgramHash} for attestation.
  host_calls     : A map {ProcessID -> Host-calls lookup table}
  counters       : A map {ProcessID -> Counter}
  safe_to_ledger : A map {ProcessID -> Bool}
  is_utxo        : Read-only map {ProcessID -> Bool}
  initialized    : A map {ProcessID -> Bool}
  handler_stack  : A map {InterfaceID -> Stack<ProcessID>}
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

```text
Rule: Resume
============
    op = Resume(target, val) -> ret

    1. id_curr ≠ target

    (No self resume)

    2. M[target] == val

    (Check val matches target's previous claim)

    3. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Resume, target, val, ret, id_prev>

    (The opcode matches the host call lookup table used in the wasm proof at the current index)

    4. is_utxo(id_curr) => !is_utxo(target)

    (Utxo's can't call into utxos)

    5. initialized[target]

    (Can't jump to an unitialized process)
--------------------------------------------------------------------------------------------
    1. M[id_curr]               <- ret       (Claim, needs to be checked later by future resumer)
    2. counters'[id_curr]       += 1         (Keep track of host call index per process)
    3. id_prev'                 <- id_curr   (Save "caller" for yield)
    4. id_curr'                 <- target    (Switch)
    5. safe_to_ledger'[target]  <- False     (This is not the final yield for this utxo in this transaction)

```

## Yield

Suspend the current continuation and optionally transfer control to the previous
coordination script (since utxos can't call utxos, that's the only possible
parent).

This also marks the utxo as **safe** to persist in the ledger.

If the utxo is not iterated again in the transaction, the return value is null
for this execution (next transaction will have to execute `yield` again, but
with an actual result). In that case, id_prev would be null (or some sentinel).

```text
Rule: Yield (resumed)
============
    op = Yield(val) -> ret

    1. M[id_prev] == val

    (Check val matches target's previous claim)

    2. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Yield, val, ret, id_prev>

    (The opcode matches the host call lookup table used in the wasm proof at the current index)
--------------------------------------------------------------------------------------------

    1. M[id_curr]               <- ret       (Claim, needs to be checked later by future resumer)
    2. counters'[id_curr]       += 1         (Keep track of host call index per process)
    3. id_curr'                 <- id_prev   (Switch to parent)
    4. id_prev'                 <- id_curr   (Save "caller")
    5. safe_to_ledger'[id_curr] <- False     (This is not the final yield for this utxo in this transaction)
```

```text
Rule: Yield (end transaction)
=============================
    op = Yield(val)

    3. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Yield, val, null, id_prev>

    (Remember, there is no ret value since that won't be known until the next transaction)

    (The opcode matches the host call lookup table used in the wasm proof at the current index)
--------------------------------------------------------------------------------------------
    1. counters'[id_curr]       += 1        (Keep track of host call index per process)
    2. id_curr'                 <- id_prev  (Switch to parent)
    3. id_prev'                 <- id_curr  (Save "caller")
    4. safe_to_ledger'[id_curr] <- True     (This utxo creates a transacition output)
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
            t[c] == <NewUtxo, program_hash, val, id>

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
    1. initialized[id] <- True
    2. counters'[id_curr] += 1
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
    1. initialized[id] <- True
    2. counters'[id_curr] += 1
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

    op = Burn()

    1. is_utxo[id_curr]
    2. is_initialized[id_curr]

    3. let
        t = CC[id_curr] in
        c = counters[id_curr] in
            t[c] == <Burn>

    (Host call lookup condition)
-----------------------------------------------------------------------
    1. is_initialized'[id_curr] <- False
    2. safe_to_ledger'[id_curr] <- True
    3. counters'[id_curr] += 1

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
        assert(safe_to_ledger[process])
    }

assert_not(is_utxo[id_curr])

// we finish in a coordination script
```