// Copy-pasta from `zkEngine_dev/third-party/wasmi/crates/wasmi/src/engine/regmach/executor/instrs/memory.rs`.
/*
Copyright (c) 2018-2023 Parity Technologies

Permission is hereby granted, free of charge, to any
person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the
Software without restriction, including without
limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software
is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice
shall be included in all copies or substantial portions
of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
*/
#![allow(non_snake_case)]

/// Read operation between an untrusted memory and a checker
pub fn read_op(
    addr: usize,
    global_ts: &mut u64,
    FS: &mut [(usize, u64, u64)],
    RS: &mut Vec<(usize, u64, u64)>,
    WS: &mut Vec<(usize, u64, u64)>,
) {
    // 1. ts ← ts + 1
    *global_ts += 1;

    // untrusted memory responds with a value-timestamp pair (v, t)
    let (_, r_val, r_ts) = FS[addr];

    // 2. assert t < ts
    debug_assert!(r_ts < *global_ts);

    // 3. RS ← RS ∪ {(a,v,t)};
    RS.push((addr, r_val, r_ts));

    // 4. store (v, ts) at address a in the untrusted memory; and
    FS[addr] = (addr, r_val, *global_ts);

    // 5. WS ← WS ∪ {(a,v,ts)}.
    WS.push((addr, r_val, *global_ts));
}

/// Write operation between an untrusted memory and a checker
pub fn write_op(
    addr: usize,
    val: u64,
    global_ts: &mut u64,
    FS: &mut [(usize, u64, u64)],
    RS: &mut Vec<(usize, u64, u64)>,
    WS: &mut Vec<(usize, u64, u64)>,
) {
    // 1. ts ← ts + 1
    *global_ts += 1;

    // untrusted memory responds with a value-timestamp pair (v, t)
    let (_, r_val, r_ts) = FS[addr];

    // 2. assert t < ts
    debug_assert!(r_ts < *global_ts);

    // 3. RS ← RS ∪ {(a,v,t)};
    RS.push((addr, r_val, r_ts));

    // 4. store (v', ts) at address a in the untrusted memory; and
    FS[addr] = (addr, val, *global_ts);

    // 5. WS ← WS ∪ {(a,v',ts)}.
    WS.push((addr, val, *global_ts));
}
