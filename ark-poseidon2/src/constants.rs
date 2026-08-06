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

/// Round constants for width-12 Poseidon2 on Goldilocks, matching plonky3's
/// `GOLDILOCKS_POSEIDON2_RC_12_EXTERNAL_INITIAL` / `_FINAL` (p3-goldilocks
/// 0.5.3; Grain LFSR: field_type=1, alpha=7, n=64, t=12, R_F=8, R_P=22).
///
/// Protocol constant of the `compress_12` instantiation shared with the wasm
/// zkVM's host-event chain — change only in lockstep with that side.
pub const GOLDILOCKS_12_EXTERNAL_ROUND_CONSTANTS: [[[u64; 12]; 4]; 2] = [
    [
        [
            0x13dcf33aba214f46,
            0x30b3b654a1da6d83,
            0x1fc634ada6159b56,
            0x937459964dc03466,
            0xedd2ef2ca7949924,
            0xede9affde0e22f68,
            0x8515b9d6bac9282d,
            0x6b5c07b4e9e900d8,
            0x1ec66368838c8a08,
            0x9042367d80d1fbab,
            0x400283564a3c3799,
            0x4a00be0466bca75e,
        ],
        [
            0x7913beee58e3817f,
            0xf545e88532237d90,
            0x22f8cb8736042005,
            0x6f04990e247a2623,
            0xfe22e87ba37c38cd,
            0xd20e32c85ffe2815,
            0x117227674048fe73,
            0x4e9fb7ea98a6b145,
            0xe0866c232b8af08b,
            0x00bbc77916884964,
            0x7031c0fb990d7116,
            0x240a9e87cf35108f,
        ],
        [
            0x2e6363a5a12244b3,
            0x5e1c3787d1b5011c,
            0x4132660e2a196e8b,
            0x3a013b648d3d4327,
            0xf79839f49888ea43,
            0xfe85658ebafe1439,
            0xb6889825a14240bd,
            0x578453605541382b,
            0x4508cda8f6b63ce9,
            0x9c3ef35848684c91,
            0x0812bde23c87178c,
            0xfe49638f7f722c14,
        ],
        [
            0x8e3f688ce885cbf5,
            0xb8e110acf746a87d,
            0xb4b2e8973a6dabef,
            0x9e714c5da3d462ec,
            0x6438f9033d3d0c15,
            0x24312f7cf1a27199,
            0x23f843bb47acbf71,
            0x9183f11a34be9f01,
            0x839062fbb9d45dbf,
            0x24b56e7e6c2e43fa,
            0xe1683da61c962a72,
            0xa95c63971a19bfa7,
        ],
    ],
    [
        [
            0xc68be7c94882a24d,
            0xaf996d5d5cdaedd9,
            0x9717f025e7daf6a5,
            0x6436679e6e7216f4,
            0x8a223d99047af267,
            0xbb512e35a133ba9a,
            0xfbbf44097671aa03,
            0xf04058ebf6811e61,
            0x5cca84703fac7ffb,
            0x9b55c7945de6469f,
            0x8e05bf09808e934f,
            0x2ea900de876307d7,
        ],
        [
            0x7748fff2b38dfb89,
            0x6b99a676dd3b5d81,
            0xac4bb7c627cf7c13,
            0xadb6ebe5e9e2f5ba,
            0x2d33378cafa24ae3,
            0x1e5b73807543f8c2,
            0x09208814bfebb10f,
            0x782e64b6bb5b93dd,
            0xadd5a48eac90b50f,
            0xadd4c54c736ea4b1,
            0xd58dbb86ed817fd8,
            0x6d5ed1a533f34ddd,
        ],
        [
            0x28686aa3e36b7cb9,
            0x591abd3476689f36,
            0x047d766678f13875,
            0xa2a11112625f5b49,
            0x21fd10a3f8304958,
            0xf9b40711443b0280,
            0xd2697eb8b2bde88e,
            0x3493790b51731b3f,
            0x11caf9dd73764023,
            0x7acfb8f72878164e,
            0x744ec4db23cefc26,
            0x1e00e58f422c6340,
        ],
        [
            0x21dd28d906a62dda,
            0xf32a46ab5f465b5f,
            0xbfce13201f3f7e6b,
            0xf30d2e7adb5304e2,
            0xecdf4ee4abad48e9,
            0xf94e82182d395019,
            0x4ee52e3744d887c5,
            0xa1341c7cac0083b2,
            0x2302fb26c30c834a,
            0xaea3c587273bf7d3,
            0xf798e24961823ec7,
            0x962deba3e9a2cd94,
        ],
    ],
];

/// Width-12 internal round constants, matching plonky3's
/// `GOLDILOCKS_POSEIDON2_RC_12_INTERNAL` (same provenance as above).
pub const GOLDILOCKS_12_INTERNAL_ROUND_CONSTANTS: [u64; 22] = [
    0x4adf842aa75d4316,
    0xf8fbb871aa4ab4eb,
    0x68e85b6eb2dd6aeb,
    0x07a0b06b2d270380,
    0xd94e0228bd282de4,
    0x8bdd91d3250c5278,
    0x209c68b88bba778f,
    0xb5e18cdab77f3877,
    0xb296a3e808da93fa,
    0x8370ecbda11a327e,
    0x3f9075283775dad8,
    0xb78095bb23c6aa84,
    0x3f36b9fe72ad4e5f,
    0x69bc96780b10b553,
    0x3f1d341f2eb7b881,
    0x4e939e9815838818,
    0xda366b3ae2a31604,
    0xbc89db1e7287d509,
    0x6102f411f9ef5659,
    0x58725c5e7ac1f0ab,
    0x0df5856c798883e7,
    0xf7bb62a8da4c961b,
];

impl RoundConstants<F, 12, 4, 22> {
    #[must_use]
    pub fn new_goldilocks_12_constants() -> Self {
        let [beginning_full_round_constants, ending_full_round_constants] =
            GOLDILOCKS_12_EXTERNAL_ROUND_CONSTANTS;

        Self {
            beginning_full_round_constants: constants_to_ark_arrays(beginning_full_round_constants),
            partial_round_constants: GOLDILOCKS_12_INTERNAL_ROUND_CONSTANTS
                .into_iter()
                .map(F::from)
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
            ending_full_round_constants: constants_to_ark_arrays(ending_full_round_constants),
        }
    }
}

fn constants_to_ark_arrays<const W: usize>(
    beginning_full_round_constants: [[u64; W]; 4],
) -> [[F; W]; 4] {
    beginning_full_round_constants.map(|inner| inner.map(F::from))
}
