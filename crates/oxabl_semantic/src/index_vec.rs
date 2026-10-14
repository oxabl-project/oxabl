//! Dense side-table keyed by [`NodeId`](oxabl_ast::NodeId).
//!
//! NodeIds are monotonic u32s allocated by the parser, so a side table keyed
//! on them is a dense `Vec<Option<T>>` with zero hashing and cache-friendly
//! lookup. Matches Oxc's `IndexVec` and Ruff's `ruff_python_semantic` arenas.
//! See plan §Proposed Solution ("Side tables are stored as `IndexVec<NodeId,
//! Option<T>>`") and CLAUDE.md's no-heap-alloc guidance.

use oxabl_ast::{NodeId, Statement};

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

    /// Pre-allocate one slot for every NodeId in a parsed `program`.
    ///
    /// The parser gives a node its id after its children, so the last
    /// top-level statement holds the highest id. A hand-built tree breaks
    /// this at worst into a growing table, never a wrong one.
    pub fn for_program(program: &[Statement]) -> Self {
        let slots = match program.last() {
            Some(s) if s.id != NodeId::DUMMY => s.id.as_u32() as usize + 1,
            _ => 0,
        };
        Self::with_capacity(slots)
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
    ///
    /// [`NodeId::DUMMY`] is silently ignored — it represents an unassigned
    /// slot on hand-constructed AST (tests, error recovery) and keying a
    /// side table on `u32::MAX` would resize the backing vec to 2^32 slots.
    pub fn insert(&mut self, id: NodeId, value: T) -> Option<T> {
        if id == NodeId::DUMMY {
            return None;
        }
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

    /// Iterate mutably over populated `(NodeId, value)` pairs.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (NodeId, &mut T)> {
        self.inner
            .iter_mut()
            .enumerate()
            .filter_map(|(i, slot)| slot.as_mut().map(|t| (NodeId::from_u32(i as u32), t)))
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
    fn for_program_covers_every_parsed_node_id() {
        let source = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
                      PROCEDURE p:\n  DO WHILE x < 3:\n    x = x + 1.\n  END.\nEND PROCEDURE.\n\
                      FUNCTION f RETURNS INTEGER (): RETURN x * 2. END FUNCTION.\n";
        let tokens = oxabl_lexer::tokenize(source);
        let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
        let schema = oxabl_schema::Schema::empty();
        let ctx = crate::AnalysisContext::new(oxabl_common::FileId::new(1), source, &schema);
        let sem = crate::analyze_file(&program.statements, &ctx);

        let slots = NodeIndexVec::<u32>::for_program(&program.statements)
            .inner
            .capacity();
        assert!(sem.types.len() > 1 && sem.types.len() <= slots);
        assert!(sem.references.len() > 1 && sem.references.len() <= slots);
    }

    #[test]
    fn for_program_ignores_hand_built_ids() {
        let program = [Statement::new(oxabl_ast::StatementKind::Empty)];
        let v = NodeIndexVec::<u32>::for_program(&program);
        assert_eq!(v.inner.capacity(), 0);
    }

    #[test]
    fn iter_skips_empty_slots() {
        let mut v: NodeIndexVec<u32> = NodeIndexVec::new();
        v.insert(NodeId::from_u32(1), 10);
        v.insert(NodeId::from_u32(4), 40);
        let collected: Vec<_> = v.iter().map(|(id, t)| (id.as_u32(), *t)).collect();
        assert_eq!(collected, vec![(1, 10), (4, 40)]);
    }

    #[test]
    fn iter_mut_updates_only_populated_slots() {
        let mut v = NodeIndexVec::new();
        v.insert(NodeId::from_u32(1), 10);
        v.insert(NodeId::from_u32(4), 40);
        for (_, value) in v.iter_mut() {
            *value += 1;
        }
        assert_eq!(v.get(NodeId::from_u32(1)), Some(&11));
        assert_eq!(v.get(NodeId::from_u32(4)), Some(&41));
        assert!(v.get(NodeId::from_u32(2)).is_none());
    }
}
