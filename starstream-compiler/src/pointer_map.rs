use std::collections::{HashMap, hash_map::Entry};

/// Helper for concisely mapping AST nodes to cached data by their address.
pub struct PointerMap<V> {
    inner: HashMap<usize, V>,
}

impl<V> Default for PointerMap<V> {
    #[inline]
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<V> PointerMap<V> {
    #[inline]
    pub fn insert<T>(&mut self, k: *const T, v: V) -> Option<V> {
        self.inner.insert(k as usize, v)
    }

    #[inline]
    pub fn get<T>(&self, k: *const T) -> Option<&V> {
        self.inner.get(&(k as usize))
    }

    #[inline]
    pub fn entry<T>(&mut self, k: *const T) -> Entry<'_, usize, V> {
        self.inner.entry(k as usize)
    }

    #[inline]
    pub fn remove<T>(&mut self, k: *const T) -> Option<V> {
        self.inner.remove(&(k as usize))
    }
}
