use crate::Hash;
use neo_math::F;
use p3_field::{PrimeCharacteristicRing, PrimeField64};

pub mod abi;
pub mod instance;
pub mod witness;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Blob(Vec<u8>);

pub type InterfaceId = Hash<Blob>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProcessId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FunctionId(pub F);

impl std::fmt::Display for ProcessId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FunctionId {
    pub fn as_u64(self) -> u64 {
        self.0.as_canonical_u64()
    }
}

impl std::fmt::Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_u64())
    }
}

impl From<u64> for FunctionId {
    fn from(value: u64) -> Self {
        Self(F::from_u64(value))
    }
}

impl From<usize> for FunctionId {
    fn from(value: usize) -> Self {
        Self(F::from_u64(value as u64))
    }
}

impl From<usize> for ProcessId {
    fn from(value: usize) -> Self {
        ProcessId(value)
    }
}

impl From<ProcessId> for usize {
    fn from(value: ProcessId) -> Self {
        value.0
    }
}
