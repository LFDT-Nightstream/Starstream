use crate::F;
use ark_ff::PrimeField;

/// Degree of the chosen permutation polynomial for Goldilocks, used as the Poseidon2 S-Box.
///
/// As p - 1 = 2^32 * 3 * 5 * 17 * ... the smallest choice for a degree D satisfying gcd(p - 1, D) = 1 is 7.
pub const GOLDILOCKS_S_BOX_DEGREE: u64 = 7;
pub const HALF_FULL_ROUNDS: usize = 4;
pub const PARTIAL_ROUNDS: usize = 22;

pub const HL_GOLDILOCKS_8_EXTERNAL_ROUND_CONSTANTS: [[[u64; 8]; 4]; 2] = [
    [
        [
            0xdd5743e7f2a5a5d9,
            0xcb3a864e58ada44b,
            0xffa2449ed32f8cdc,
            0x42025f65d6bd13ee,
            0x7889175e25506323,
            0x34b98bb03d24b737,
            0xbdcc535ecc4faa2a,
            0x5b20ad869fc0d033,
        ],
        [
            0xf1dda5b9259dfcb4,
            0x27515210be112d59,
            0x4227d1718c766c3f,
            0x26d333161a5bd794,
            0x49b938957bf4b026,
            0x4a56b5938b213669,
            0x1120426b48c8353d,
            0x6b323c3f10a56cad,
        ],
        [
            0xce57d6245ddca6b2,
            0xb1fc8d402bba1eb1,
            0xb5c5096ca959bd04,
            0x6db55cd306d31f7f,
            0xc49d293a81cb9641,
            0x1ce55a4fe979719f,
            0xa92e60a9d178a4d1,
            0x002cc64973bcfd8c,
        ],
        [
            0xcea721cce82fb11b,
            0xe5b55eb8098ece81,
            0x4e30525c6f1ddd66,
            0x43c6702827070987,
            0xaca68430a7b5762a,
            0x3674238634df9c93,
            0x88cee1c825e33433,
            0xde99ae8d74b57176,
        ],
    ],
    [
        [
            0x014ef1197d341346,
            0x9725e20825d07394,
            0xfdb25aef2c5bae3b,
            0xbe5402dc598c971e,
            0x93a5711f04cdca3d,
            0xc45a9a5b2f8fb97b,
            0xfe8946a924933545,
            0x2af997a27369091c,
        ],
        [
            0xaa62c88e0b294011,
            0x058eb9d810ce9f74,
            0xb3cb23eced349ae4,
            0xa3648177a77b4a84,
            0x43153d905992d95d,
            0xf4e2a97cda44aa4b,
            0x5baa2702b908682f,
            0x082923bdf4f750d1,
        ],
        [
            0x98ae09a325893803,
            0xf8a6475077968838,
            0xceb0735bf00b2c5f,
            0x0a1a5d953888e072,
            0x2fcb190489f94475,
            0xb5be06270dec69fc,
            0x739cb934b09acf8b,
            0x537750b75ec7f25b,
        ],
        [
            0xe9dd318bae1f3961,
            0xf7462137299efe1a,
            0xb1f6b8eee9adb940,
            0xbdebcc8a809dfe6b,
            0x40fc1f791b178113,
            0x3ac1c3362d014864,
            0x9a016184bdb8aeba,
            0x95f2394459fbc25e,
        ],
    ],
];

pub const HL_GOLDILOCKS_8_INTERNAL_ROUND_CONSTANTS: [u64; 22] = [
    0x488897d85ff51f56,
    0x1140737ccb162218,
    0xa7eeb9215866ed35,
    0x9bd2976fee49fcc9,
    0xc0c8f0de580a3fcc,
    0x4fb2dae6ee8fc793,
    0x343a89f35f37395b,
    0x223b525a77ca72c8,
    0x56ccb62574aaa918,
    0xc4d507d8027af9ed,
    0xa080673cf0b7e95c,
    0xf0184884eb70dcf8,
    0x044f10b0cb3d5c69,
    0xe9e3f7993938f186,
    0x1b761c80e772f459,
    0x606cec607a1b5fac,
    0x14a0c2e1d45f03cd,
    0x4eace8855398574f,
    0xf905ca7103eff3e6,
    0xf8c8f8d20862c059,
    0xb524fe8bdd678e5a,
    0xfbb7865901a1ec41,
];

/// Round constants for Poseidon2, in a format that's convenient for R1CS.
#[derive(Debug, Clone)]
pub struct RoundConstants<
    F: PrimeField,
    const WIDTH: usize,
    const HALF_FULL_ROUNDS: usize,
    const PARTIAL_ROUNDS: usize,
> {
    pub beginning_full_round_constants: [[F; WIDTH]; HALF_FULL_ROUNDS],
    pub partial_round_constants: [F; PARTIAL_ROUNDS],
    pub ending_full_round_constants: [[F; WIDTH]; HALF_FULL_ROUNDS],
}

impl RoundConstants<F, 8, 4, 22> {
    // TODO: cache/lazyfy this
    pub fn new_goldilocks_8_constants() -> Self {
        let [beginning_full_round_constants, ending_full_round_constants] =
            HL_GOLDILOCKS_8_EXTERNAL_ROUND_CONSTANTS;

        Self {
            beginning_full_round_constants: constants_to_ark_arrays(beginning_full_round_constants),
            partial_round_constants: HL_GOLDILOCKS_8_INTERNAL_ROUND_CONSTANTS
                .into_iter()
                .map(F::from)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
            ending_full_round_constants: constants_to_ark_arrays(ending_full_round_constants),
        }
    }
}

fn constants_to_ark_arrays(beginning_full_round_constants: [[u64; 8]; 4]) -> [[F; 8]; 4] {
    beginning_full_round_constants
        .into_iter()
        .map(|inner| {
            inner
                .into_iter()
                .map(F::from)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap()
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}
