use crate::Hash;

pub mod abi;
pub mod instance;
pub mod witness;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Blob(Vec<u8>);

pub type InterfaceId = Hash<Blob>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProcessId(pub usize);

impl std::fmt::Display for ProcessId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
