//! Collection support for `#[jomini(collect)]`.

use std::collections::{BTreeMap, HashMap};

/// Collect items produced by `#[jomini(collect)]`.
pub trait Collect {
    /// Item type produced by collection.
    type Item;

    /// Collect the given item into the container.
    fn collect(&mut self, item: Self::Item);
}

impl<T> Collect for Vec<T> {
    type Item = T;

    fn collect(&mut self, item: T) {
        self.push(item);
    }
}

impl<K, V, S> Collect for HashMap<K, V, S>
where
    K: Eq + std::hash::Hash,
    S: std::hash::BuildHasher,
{
    type Item = (K, V);

    fn collect(&mut self, item: (K, V)) {
        self.insert(item.0, item.1);
    }
}

impl<K, V> Collect for BTreeMap<K, V>
where
    K: Ord,
{
    type Item = (K, V);

    fn collect(&mut self, item: (K, V)) {
        self.insert(item.0, item.1);
    }
}
