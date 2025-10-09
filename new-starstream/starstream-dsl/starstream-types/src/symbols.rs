use serde::Serialize;


#[derive(Clone, Copy, Debug, Hash, PartialEq, PartialOrd, Eq, Ord, Serialize)]
pub struct SymbolId {
    pub id: u64,
}
