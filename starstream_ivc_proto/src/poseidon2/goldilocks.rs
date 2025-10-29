use crate::goldilocks::FpGoldilocks;
use std::sync::OnceLock;

/// Degree of the chosen permutation polynomial for Goldilocks, used as the Poseidon2 S-Box.
///
/// As p - 1 = 2^32 * 3 * 5 * 17 * ... the smallest choice for a degree D satisfying gcd(p - 1, D) = 1 is 7.
const GOLDILOCKS_S_BOX_DEGREE: u64 = 7;

pub static MATRIX_DIAG_8_GOLDILOCKS: OnceLock<[FpGoldilocks; 8]> = OnceLock::new();

pub(crate) fn matrix_diag_8_goldilocks() -> &'static [FpGoldilocks; 8] {
    MATRIX_DIAG_8_GOLDILOCKS.get_or_init(|| {
        [
            FpGoldilocks::from(0xa98811a1fed4e3a5_u64),
            FpGoldilocks::from(0x1cc48b54f377e2a0_u64),
            FpGoldilocks::from(0xe40cd4f6c5609a26_u64),
            FpGoldilocks::from(0x11de79ebca97a4a3_u64),
            FpGoldilocks::from(0x9177c73d8b7e929c_u64),
            FpGoldilocks::from(0x2a6fe8085797e791_u64),
            FpGoldilocks::from(0x3de6e93329f8d5ad_u64),
            FpGoldilocks::from(0x3f7af9125da962fe_u64),
        ]
    })
}

pub static MATRIX_DIAG_12_GOLDILOCKS: OnceLock<[FpGoldilocks; 12]> = OnceLock::new();

pub(crate) fn matrix_diag_12_goldilocks() -> &'static [FpGoldilocks; 12] {
    MATRIX_DIAG_12_GOLDILOCKS.get_or_init(|| {
        [
            FpGoldilocks::from(0xc3b6c08e23ba9300_u64),
            FpGoldilocks::from(0xd84b5de94a324fb6_u64),
            FpGoldilocks::from(0x0d0c371c5b35b84f_u64),
            FpGoldilocks::from(0x7964f570e7188037_u64),
            FpGoldilocks::from(0x5daf18bbd996604b_u64),
            FpGoldilocks::from(0x6743bc47b9595257_u64),
            FpGoldilocks::from(0x5528b9362c59bb70_u64),
            FpGoldilocks::from(0xac45e25b7127b68b_u64),
            FpGoldilocks::from(0xa2077d7dfbb606b5_u64),
            FpGoldilocks::from(0xf3faac6faee378ae_u64),
            FpGoldilocks::from(0x0c6388b51545e883_u64),
            FpGoldilocks::from(0xd27dbb6944917b60_u64),
        ]
    })
}

pub static MATRIX_DIAG_16_GOLDILOCKS: OnceLock<[FpGoldilocks; 16]> = OnceLock::new();

pub(crate) fn matrix_diag_16_goldilocks() -> &'static [FpGoldilocks; 16] {
    MATRIX_DIAG_16_GOLDILOCKS.get_or_init(|| {
        [
            FpGoldilocks::from(0xde9b91a467d6afc0_u64),
            FpGoldilocks::from(0xc5f16b9c76a9be17_u64),
            FpGoldilocks::from(0x0ab0fef2d540ac55_u64),
            FpGoldilocks::from(0x3001d27009d05773_u64),
            FpGoldilocks::from(0xed23b1f906d3d9eb_u64),
            FpGoldilocks::from(0x5ce73743cba97054_u64),
            FpGoldilocks::from(0x1c3bab944af4ba24_u64),
            FpGoldilocks::from(0x2faa105854dbafae_u64),
            FpGoldilocks::from(0x53ffb3ae6d421a10_u64),
            FpGoldilocks::from(0xbcda9df8884ba396_u64),
            FpGoldilocks::from(0xfc1273e4a31807bb_u64),
            FpGoldilocks::from(0xc77952573d5142c0_u64),
            FpGoldilocks::from(0x56683339a819b85e_u64),
            FpGoldilocks::from(0x328fcbd8f0ddc8eb_u64),
            FpGoldilocks::from(0xb5101e303fce9cb7_u64),
            FpGoldilocks::from(0x774487b8c40089bb_u64),
        ]
    })
}

pub static MATRIX_DIAG_20_GOLDILOCKS: OnceLock<[FpGoldilocks; 20]> = OnceLock::new();

pub(crate) fn matrix_diag_20_goldilocks() -> &'static [FpGoldilocks; 20] {
    MATRIX_DIAG_20_GOLDILOCKS.get_or_init(|| {
        [
            FpGoldilocks::from(0x95c381fda3b1fa57_u64),
            FpGoldilocks::from(0xf36fe9eb1288f42c_u64),
            FpGoldilocks::from(0x89f5dcdfef277944_u64),
            FpGoldilocks::from(0x106f22eadeb3e2d2_u64),
            FpGoldilocks::from(0x684e31a2530e5111_u64),
            FpGoldilocks::from(0x27435c5d89fd148e_u64),
            FpGoldilocks::from(0x3ebed31c414dbf17_u64),
            FpGoldilocks::from(0xfd45b0b2d294e3cc_u64),
            FpGoldilocks::from(0x48c904473a7f6dbf_u64),
            FpGoldilocks::from(0xe0d1b67809295b4d_u64),
            FpGoldilocks::from(0xddd1941e9d199dcb_u64),
            FpGoldilocks::from(0x8cfe534eeb742219_u64),
            FpGoldilocks::from(0xa6e5261d9e3b8524_u64),
            FpGoldilocks::from(0x6897ee5ed0f82c1b_u64),
            FpGoldilocks::from(0x0e7dcd0739ee5f78_u64),
            FpGoldilocks::from(0x493253f3d0d32363_u64),
            FpGoldilocks::from(0xbb2737f5845f05c0_u64),
            FpGoldilocks::from(0xa187e810b06ad903_u64),
            FpGoldilocks::from(0xb635b995936c4918_u64),
            FpGoldilocks::from(0x0b3694a940bd2394_u64),
        ]
    })
}
