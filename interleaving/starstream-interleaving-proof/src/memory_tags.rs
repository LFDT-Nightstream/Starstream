use crate::F;
use ark_r1cs_std::alloc::AllocVar;
use ark_r1cs_std::fields::fp::FpVar;
use ark_relations::gr1cs::{ConstraintSystemRef, SynthesisError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryTag {
    // ROM tags
    ProcessTable = 1,
    MustBurn = 2,
    IsUtxo = 3,
    Interfaces = 4,

    // RAM tags
    ExpectedInput = 5,
    Activation = 6,
    Counters = 7,
    Initialized = 8,
    Finalized = 9,
    DidBurn = 10,
    Ownership = 11,
    Init = 12,
    RefArena = 13,
    RefSizes = 14,
    HandlerStackArenaProcess = 15,
    HandlerStackArenaNextPtr = 16,
    HandlerStackHeads = 17,
    TraceCommitments = 18,
    ExpectedResumer = 19,
    OnYield = 20,
    YieldTo = 21,
}

impl From<MemoryTag> for u64 {
    fn from(tag: MemoryTag) -> u64 {
        tag as u64
    }
}

impl From<MemoryTag> for F {
    fn from(tag: MemoryTag) -> F {
        F::from(tag as u64)
    }
}

impl MemoryTag {
    pub fn allocate(&self, cs: ConstraintSystemRef<F>) -> Result<FpVar<F>, SynthesisError> {
        FpVar::new_constant(cs, F::from(*self))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramStateTag {
    ExpectedInput,
    ExpectedResumer,
    OnYield,
    YieldTo,
    Activation,
    Init,
    Counters,
    Initialized,
    Finalized,
    DidBurn,
    Ownership,
}

impl From<ProgramStateTag> for MemoryTag {
    fn from(tag: ProgramStateTag) -> MemoryTag {
        match tag {
            ProgramStateTag::ExpectedInput => MemoryTag::ExpectedInput,
            ProgramStateTag::ExpectedResumer => MemoryTag::ExpectedResumer,
            ProgramStateTag::OnYield => MemoryTag::OnYield,
            ProgramStateTag::YieldTo => MemoryTag::YieldTo,
            ProgramStateTag::Activation => MemoryTag::Activation,
            ProgramStateTag::Init => MemoryTag::Init,
            ProgramStateTag::Counters => MemoryTag::Counters,
            ProgramStateTag::Initialized => MemoryTag::Initialized,
            ProgramStateTag::Finalized => MemoryTag::Finalized,
            ProgramStateTag::DidBurn => MemoryTag::DidBurn,
            ProgramStateTag::Ownership => MemoryTag::Ownership,
        }
    }
}

impl From<ProgramStateTag> for u64 {
    fn from(tag: ProgramStateTag) -> u64 {
        let memory_tag: MemoryTag = tag.into();
        memory_tag.into()
    }
}
