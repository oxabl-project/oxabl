//! Dense side-table keyed by [`NodeId`](oxabl_ast::NodeId).
//!
//! NodeIds are monotonic u32s allocated by the parser, so a side table keyed
//! on them is a dense `Vec<Option<T>>` with zero hashing and cache-friendly
//! lookup. Matches Oxc's `IndexVec` and Ruff's `ruff_python_semantic` arenas.
//! See plan §Proposed Solution ("Side tables are stored as `IndexVec<NodeId,
//! Option<T>>`") and CLAUDE.md's no-heap-alloc guidance.

use oxabl_ast::NodeId;

/// A dense side table keyed by [`NodeId`]. Expands the backing `Vec` as
/// needed on insert; `None` means "no entry at this NodeId". Does not grow
/// automatically on read — `get` returns `None` for out-of-range ids.
#[derive(Debug, Clone, Default)]
pub struct NodeIndexVec<T> {
    inner: Vec<Option<T>>,
}

impl<T> NodeIndexVec<T> {
    /// Construct an empty side table.
    pub fn new() -> Self {
        NodeIndexVec { inner: Vec::new() }
    }

    /// Pre-allocate capacity for at least `n` entries. Useful when the parser
    /// has already counted its allocations.
    pub fn with_capacity(n: usize) -> Self {
        NodeIndexVec {
            inner: Vec::with_capacity(n),
        }
    }

    /// Number of slots (both populated and empty) in the backing vec.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Whether the backing vec has zero slots.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Read the entry at `id`, if any. Out-of-range reads return `None`.
    pub fn get(&self, id: NodeId) -> Option<&T> {
        self.inner
            .get(id.as_u32() as usize)
            .and_then(|e| e.as_ref())
    }

    /// Insert `value` at `id`, extending the backing vec with `None` as
    /// needed. Overwrites any existing entry and returns the prior value.
    pub fn insert(&mut self, id: NodeId, value: T) -> Option<T> {
        let idx = id.as_u32() as usize;
        if idx >= self.inner.len() {
            self.inner.resize_with(idx + 1, || None);
        }
        self.inner[idx].replace(value)
    }

    /// Iterate over `(NodeId, &T)` pairs. Empty slots are skipped.
    pub fn iter(&self) -> impl Iterator<Item = (NodeId, &T)> {
        self.inner
            .iter()
            .enumerate()
            .filter_map(|(i, slot)| slot.as_ref().map(|t| (NodeId::from_u32(i as u32), t)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_and_get() {
        let mut v: NodeIndexVec<&'static str> = NodeIndexVec::new();
        assert!(v.get(NodeId::from_u32(5)).is_none());
        v.insert(NodeId::from_u32(5), "hello");
        assert_eq!(v.get(NodeId::from_u32(5)), Some(&"hello"));
        assert_eq!(v.len(), 6);
    }

    #[test]
    fn insert_overwrites() {
        let mut v: NodeIndexVec<u32> = NodeIndexVec::new();
        assert_eq!(v.insert(NodeId::from_u32(3), 1), None);
        assert_eq!(v.insert(NodeId::from_u32(3), 2), Some(1));
        assert_eq!(v.get(NodeId::from_u32(3)), Some(&2));
    }

    #[test]
    fn iter_skips_empty_slots() {
        let mut v: NodeIndexVec<u32> = NodeIndexVec::new();
        v.insert(NodeId::from_u32(1), 10);
        v.insert(NodeId::from_u32(4), 40);
        let collected: Vec<_> = v.iter().map(|(id, t)| (id.as_u32(), *t)).collect();
        assert_eq!(collected, vec![(1, 10), (4, 40)]);
    }
}
