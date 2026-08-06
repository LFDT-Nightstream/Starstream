//! Cross-repo parity fixtures for the width-12 compression used by the
//! ledger-effects commitment chain (`LedgerEffectsCommitment`).
//!
//! The same vectors are pinned on the wasm zkVM side (plonky3-based,
//! `neo-wasm/tests/wasm_comm_chain.rs`). If either side changes the
//! permutation instantiation or the compression layout, both tests must be
//! updated together.

use ark_poseidon2::{F, compress_12_trace};

fn f(x: u64) -> F {
    F::from(x)
}

/// Layout per chain update: `[prev_4 | discriminant | args_7]`.
fn chain_input(prev: [F; 4], discriminant: F, args: [F; 7]) -> [F; 12] {
    let mut input = [f(0); 12];
    input[..4].copy_from_slice(&prev);
    input[4] = discriminant;
    input[5..].copy_from_slice(&args);
    input
}

#[test]
fn compress_12_matches_wasm_zkvm_chain() {
    // Vector 1: genesis state, discriminant 1, args 1..=7.
    let args: [F; 7] = std::array::from_fn(|i| f(i as u64 + 1));
    let state1 = compress_12_trace(&chain_input([f(0); 4], f(1), args)).unwrap();
    assert_eq!(
        state1,
        [
            f(16060384774117980274),
            f(6217562501851223455),
            f(9809238410420041413),
            f(4191298748431046296),
        ]
    );

    // Vector 2: chained on vector 1, discriminant 16, distinctive args.
    let args2: [F; 7] = [
        f(0xffff_ffff),
        f(0xffff_ffff_0000_0000),
        f(0),
        f(42),
        f(7),
        f(0),
        f(1),
    ];
    let state2 = compress_12_trace(&chain_input(state1, f(16), args2)).unwrap();
    assert_eq!(
        state2,
        [
            f(2581777910110991851),
            f(4248944502313846729),
            f(3337412769805346927),
            f(12455009736376722043),
        ]
    );
}
