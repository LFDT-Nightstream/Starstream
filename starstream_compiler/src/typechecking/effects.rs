use crate::symbols::{SymbolId, Symbols};
use std::collections::{BTreeSet, HashSet};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[must_use]
pub struct EffectSet {
    // using a BTreeSet instead of a HashSet so that the order is deterministic
    //
    // this order is used for the effect handlers when passed through the stack
    // as arguments.
    effects: BTreeSet<SymbolId>,
}

impl EffectSet {
    pub fn empty() -> Self {
        Self {
            effects: BTreeSet::new(),
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

    pub fn iter(&self) -> impl Iterator<Item = &SymbolId> {
        self.effects.iter()
    }

    pub(crate) fn filter(&self, f: impl Fn(&SymbolId) -> bool) -> Self {
        Self {
            effects: self.effects.iter().copied().filter(f).collect(),
        }
    }
}
