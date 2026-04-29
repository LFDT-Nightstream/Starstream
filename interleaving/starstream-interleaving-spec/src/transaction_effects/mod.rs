use crate::Hash;

pub mod abi;
pub mod instance;
pub mod witness;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Blob(Vec<u8>);

pub type InterfaceId = Hash<Blob>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProcessId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FunctionId(pub [u64; 4]);

impl std::fmt::Display for ProcessId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FunctionId {
    pub const fn from_hash(hash: [u64; 4]) -> Self {
        Self(hash)
    }

    pub const fn from_legacy_scalar(value: u64) -> Self {
        Self([value, 0, 0, 0])
    }

    pub const fn as_hash(self) -> [u64; 4] {
        self.0
    }
}

impl std::fmt::Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}, {}, {}, {}]",
            self.0[0], self.0[1], self.0[2], self.0[3]
        )
    }
}

impl From<u64> for FunctionId {
    fn from(value: u64) -> Self {
        Self::from_legacy_scalar(value)
    }
}

impl From<usize> for FunctionId {
    fn from(value: usize) -> Self {
        Self::from_legacy_scalar(value as u64)
    }
}

impl From<[u64; 4]> for FunctionId {
    fn from(value: [u64; 4]) -> Self {
        Self::from_hash(value)
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
