use std::collections::HashSet;

use crate::symbols::{SymbolId, Symbols};

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

    pub fn singleton(effect: SymbolId) -> Self {
        let mut res = Self::empty();

        res.add(effect);

        res
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

    pub fn to_readable_names(&self, symbols: &Symbols) -> HashSet<String> {
        self.effects
            .iter()
            .map(|symbol_id| symbols.interfaces[symbol_id].source.clone())
            .collect()
    }
}
