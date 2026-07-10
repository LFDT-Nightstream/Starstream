use crate::F;
use ark_r1cs_std::alloc::AllocVar;
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};
use starstream_interleaving_spec::{RamMemoryTag, RomMemoryTag};

pub trait MemoryTag {
    fn memory_tag(self) -> u64;

    fn allocate(self, cs: ConstraintSystemRef<F>) -> Result<FpVar<F>, SynthesisError>
    where
        Self: Sized,
    {
        FpVar::new_constant(cs, F::from(self.memory_tag()))
    }
}

// Memory tags share one namespace across ROM and RAM. Tag 0 is reserved for
// Nebula's scan sentinel and padding rows.
const ROM_NAMESPACE: u64 = 1;
const RAM_NAMESPACE: u64 = 1 << 16;

impl MemoryTag for RomMemoryTag {
    fn memory_tag(self) -> u64 {
        ROM_NAMESPACE + self as u64
    }
}

impl MemoryTag for RamMemoryTag {
    fn memory_tag(self) -> u64 {
        RAM_NAMESPACE + self as u64
    }
}
