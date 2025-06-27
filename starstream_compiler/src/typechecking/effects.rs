use crate::scope_resolution::SymbolId;
use std::collections::HashSet;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[must_use]
pub struct EffectSet {
    effects: HashSet<SymbolId>,
}

impl EffectSet {
    pub fn empty() -> Self {
        Self {
            effects: HashSet::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.effects.is_empty()
    }

    pub fn combine(mut self, other: Self) -> EffectSet {
        self.effects.extend(other.effects);

        self
    }

    pub fn add(&mut self, symbol_id: SymbolId) {
        self.effects.insert(symbol_id);
    }

    pub fn remove(&mut self, symbol_id: SymbolId) {
        self.effects.remove(&symbol_id);
    }

    pub fn is_subset(&self, other: &EffectSet) -> bool {
        self.effects.is_subset(&other.effects)
    }
}
