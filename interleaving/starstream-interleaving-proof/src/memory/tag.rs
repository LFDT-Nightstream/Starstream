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

// the tags need to be unique right now
//
//
// this is not a twist and shout limitation, but a limitation in the current
// middleware, where we just have a single namespace for both ROM and RAM
// tracing
//
// Tag 0 is reserved and must never be a real segment: the Nebula memory scan
// uses it as the pre-scan sentinel for the running address and as its padding
// filler (see `memory::nebula::gadget::enforce_monotonic_commitment`). ROM tags
// therefore start at 1.
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
