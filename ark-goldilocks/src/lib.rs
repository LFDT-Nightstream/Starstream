use ark_ff::{Fp64, MontBackend, MontConfig};

#[derive(MontConfig)]
#[modulus = "18446744069414584321"] // 2^64 - 2^32 + 1
#[generator = "7"] // a small primitive root (7 works here)
pub struct FpGoldilocksConfig;
pub type FpGoldilocks = Fp64<MontBackend<FpGoldilocksConfig, 1>>;
