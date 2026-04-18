//! Stable node identity for AST nodes.
//!
//! Every [`Statement`] and [`Expression`] produced by the parser carries a
//! [`NodeId`], allocated from a monotonic [`NodeIdAllocator`] on the parser.
//! NodeIds are the key for semantic side tables — see
//! `docs/design/ast-invariants.md` §NodeId invariants.
//!
//! [`Statement`]: crate::Statement
//! [`Expression`]: crate::Expression

/// Stable, parser-assigned identity for an AST node.
///
/// NodeIds are dense, unique, and monotonic within a single parse. They are
/// assigned once by the parser and never change. `PartialEq` on wrapper AST
/// types (`Statement`, `Expression`) ignores the `id` field so structural
/// value-equality in tests continues to work without a compare-ignoring helper.
///
/// `NodeId::DUMMY` is the sentinel returned by `Statement::new(kind)` /
/// `Expression::new(kind)` — useful for hand-constructed AST in tests. It must
/// never appear in a parser-produced tree.
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct NodeId(u32);

impl NodeId {
    /// Reserved id for hand-constructed AST nodes outside of parsing.
    ///
    /// Never appears in a parser-produced tree. See
    /// `docs/design/ast-invariants.md` §NodeId invariants.
    pub const DUMMY: NodeId = NodeId(u32::MAX);

    /// Reserved id for the `Program` root node.
    pub const PROGRAM: NodeId = NodeId(0);

    /// Access the raw u32 for side-table indexing.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    /// Construct a `NodeId` from a raw u32. Intended for internal use and
    /// deserialization; prefer [`NodeIdAllocator::alloc`] in production code.
    #[inline]
    pub const fn from_u32(n: u32) -> Self {
        NodeId(n)
    }
}

/// Monotonic NodeId source owned by the parser.
///
/// Starts allocation at `1`; `NodeId(0)` is reserved for the `Program` root.
#[derive(Debug)]
pub struct NodeIdAllocator {
    next: u32,
}

impl NodeIdAllocator {
    /// Start a fresh allocator; the next allocation yields `NodeId(1)`.
    pub const fn new() -> Self {
        NodeIdAllocator { next: 1 }
    }

    /// Allocate the next monotonic NodeId.
    #[inline]
    pub fn alloc(&mut self) -> NodeId {
        let id = NodeId(self.next);
        debug_assert!(self.next < u32::MAX, "NodeId space exhausted");
        self.next += 1;
        id
    }

    /// Number of NodeIds allocated so far.
    #[inline]
    pub fn count(&self) -> u32 {
        self.next - 1
    }
}

impl Default for NodeIdAllocator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocator_starts_at_one() {
        let mut a = NodeIdAllocator::new();
        assert_eq!(a.alloc(), NodeId(1));
        assert_eq!(a.alloc(), NodeId(2));
        assert_eq!(a.alloc(), NodeId(3));
    }

    #[test]
    fn dummy_is_distinct_from_allocated() {
        let mut a = NodeIdAllocator::new();
        let id = a.alloc();
        assert_ne!(id, NodeId::DUMMY);
        assert_ne!(id, NodeId::PROGRAM);
    }

    #[test]
    fn round_trip_u32() {
        let id = NodeId::from_u32(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn count_tracks_allocations() {
        let mut a = NodeIdAllocator::new();
        assert_eq!(a.count(), 0);
        a.alloc();
        a.alloc();
        assert_eq!(a.count(), 2);
    }
}
