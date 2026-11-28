//! Per-parse string and identifier interner (`NameId` + `NamePool`).
//!
//! Every [`Identifier`](crate::Identifier) and literal string in an AST stores a
//! 4-byte [`NameId`] handle into a contiguous [`NamePool`] owned by `Program`,
//! eliminating per-identifier heap `String` allocations while preserving exact
//! authored casing (`docs/design/ast-invariants.md` §3).

use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;
use std::ops::Index;

use hashbrown::HashTable;
use rustc_hash::FxHasher;

/// Compact 4-byte handle into a [`NamePool`].
///
/// Backed by [`NonZeroU32`] so `Option<NameId>` is also 4 bytes.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NameId(NonZeroU32);

impl NameId {
    /// Reserved sentinel for hand-constructed placeholder nodes.
    pub const DUMMY: NameId = NameId(NonZeroU32::MAX);

    #[inline]
    pub(crate) fn from_index(index: usize) -> Self {
        let raw = u32::try_from(index)
            .ok()
            .and_then(|n| n.checked_add(1))
            .and_then(NonZeroU32::new);
        match raw {
            Some(nz) if nz != NonZeroU32::MAX => NameId(nz),
            _ => panic!("NamePool capacity exceeded (index {index})"),
        }
    }

    /// Zero-based index inside the owning [`NamePool`].
    #[inline]
    pub const fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }

    /// Raw non-zero representation for diagnostics and debugging.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0.get()
    }
}

impl fmt::Debug for NameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == Self::DUMMY {
            f.write_str("NameId(DUMMY)")
        } else {
            write!(f, "NameId({})", self.index())
        }
    }
}

#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<NameId>() == 4);
#[cfg(target_pointer_width = "64")]
const _: () = assert!(std::mem::size_of::<Option<NameId>>() == 4);

/// Read-only contiguous string pool owned by a parsed AST.
///
/// Stores every unique identifier/literal string of one parse in a single UTF-8
/// buffer (`bytes`) plus one `u32` offset table (`starts`), with zero hash-map
/// overhead once parsing finishes.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct NamePool {
    bytes: String,
    /// Byte start offsets in `bytes`; empty when no names have been stored,
    /// otherwise `len == count + 1` with `starts[0] == 0` and
    /// `starts[count] == bytes.len() as u32`.
    starts: Vec<u32>,
}

impl NamePool {
    /// Construct an empty pool with zero allocations.
    pub const fn new() -> Self {
        Self {
            bytes: String::new(),
            starts: Vec::new(),
        }
    }

    /// Pre-allocate backing buffers for a parse with roughly `name_count`
    /// distinct names totaling `byte_len` UTF-8 bytes.
    pub fn with_capacity(name_count: usize, byte_len: usize) -> Self {
        let starts = if name_count == 0 {
            Vec::new()
        } else {
            Vec::with_capacity(name_count + 1)
        };
        Self {
            bytes: String::with_capacity(byte_len),
            starts,
        }
    }

    /// Number of stored strings.
    #[inline]
    pub fn len(&self) -> usize {
        self.starts.len().saturating_sub(1)
    }

    /// Whether the pool contains no strings.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Total UTF-8 length of the stored strings, so a pass that copies them
    /// can size its buffer once.
    #[inline]
    pub fn byte_len(&self) -> usize {
        self.bytes.len()
    }

    /// Append `text` without deduplication and return its [`NameId`].
    ///
    /// Prefer [`NameInterner::intern`] during parsing; this method is convenient
    /// for hand-constructed test ASTs.
    pub fn push(&mut self, text: &str) -> NameId {
        if self.starts.is_empty() {
            self.starts.push(0);
        }
        let id = NameId::from_index(self.starts.len() - 1);
        self.bytes.push_str(text);
        let end = u32::try_from(self.bytes.len())
            .unwrap_or_else(|_| panic!("NamePool byte buffer exceeds u32::MAX"));
        self.starts.push(end);
        id
    }

    /// Look up `id`, returning `None` if out of bounds or [`NameId::DUMMY`].
    #[inline]
    pub fn try_get(&self, id: NameId) -> Option<&str> {
        let idx = id.index();
        let start = *self.starts.get(idx)? as usize;
        let end = *self.starts.get(idx + 1)? as usize;
        self.bytes.get(start..end)
    }

    /// Look up `id`. Panics if `id` does not belong to this pool.
    #[inline]
    pub fn get(&self, id: NameId) -> &str {
        match self.try_get(id) {
            Some(s) => s,
            None => panic!(
                "invalid {id:?} for NamePool of length {} \
                 (did you pass the owning Program's AstPools?)",
                self.len()
            ),
        }
    }
}

impl Index<NameId> for NamePool {
    type Output = str;

    #[inline]
    fn index(&self, id: NameId) -> &Self::Output {
        self.get(id)
    }
}

/// Deduplicating builder for [`NamePool`], used while parsing a file and
/// discarded via [`NameInterner::finish`] so the retained AST carries no hash
/// table.
///
/// The table stores only the [`NameId`]; a probe hashes the text once and
/// compares against the pool, so a collision needs no side list.
#[derive(Debug, Default)]
pub struct NameInterner {
    pool: NamePool,
    table: HashTable<NameId>,
}

impl NameInterner {
    /// Construct an empty interner.
    pub fn new() -> Self {
        Self::default()
    }

    /// Pre-allocate buffers for roughly `name_count` distinct strings.
    pub fn with_capacity(name_count: usize, byte_len: usize) -> Self {
        Self {
            pool: NamePool::with_capacity(name_count, byte_len),
            table: HashTable::with_capacity(name_count),
        }
    }

    /// Intern `text` case-sensitively, returning an existing [`NameId`] if
    /// already present or storing it once.
    pub fn intern(&mut self, text: &str) -> NameId {
        let hash = hash_str(text);
        let pool = &self.pool;
        if let Some(&existing) = self.table.find(hash, |&id| pool.get(id) == text) {
            return existing;
        }
        let id = self.pool.push(text);
        let pool = &self.pool;
        self.table
            .insert_unique(hash, id, |&id| hash_str(pool.get(id)));
        id
    }

    /// Read back an already-interned string while parsing.
    #[inline]
    pub fn get(&self, id: NameId) -> &str {
        self.pool.get(id)
    }

    /// Access the underlying pool while parsing.
    #[inline]
    pub fn pool(&self) -> &NamePool {
        &self.pool
    }

    /// Drop the deduplication table and return the compact [`NamePool`].
    pub fn finish(self) -> NamePool {
        self.pool
    }
}

#[inline]
fn hash_str(text: &str) -> u64 {
    let mut hasher = FxHasher::default();
    text.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interner_deduplicates_case_sensitively() {
        let mut interner = NameInterner::new();
        let a = interner.intern("Customer");
        let b = interner.intern("Customer");
        let c = interner.intern("customer");
        let empty = interner.intern("");
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(a, empty);

        let pool = interner.finish();
        assert_eq!(pool.len(), 3);
        assert_eq!(pool.get(a), "Customer");
        assert_eq!(pool.get(c), "customer");
        assert_eq!(pool.get(empty), "");
        assert_eq!(&pool[a], "Customer");
    }

    #[test]
    fn interner_keeps_every_name_through_table_growth() {
        let mut interner = NameInterner::new();
        let names: Vec<String> = (0..5000).map(|n| format!("name-{n}")).collect();
        let ids: Vec<NameId> = names.iter().map(|n| interner.intern(n)).collect();
        for (name, id) in names.iter().zip(&ids) {
            assert_eq!(interner.intern(name), *id);
            assert_eq!(interner.get(*id), name);
        }
        assert_eq!(interner.finish().len(), names.len());
    }

    fn pool_of(names: &[&str]) -> (NamePool, Vec<NameId>) {
        let mut pool = NamePool::new();
        let ids = names.iter().map(|n| pool.push(n)).collect();
        (pool, ids)
    }

    #[test]
    fn try_get_gives_the_text_of_a_valid_handle() {
        let (pool, ids) = pool_of(&["a", "", "bc"]);
        assert_eq!(pool.try_get(ids[0]), Some("a"));
        assert_eq!(pool.try_get(ids[1]), Some(""));
        assert_eq!(pool.try_get(ids[2]), Some("bc"));
    }

    #[test]
    fn try_get_gives_none_for_dummy() {
        let (pool, _) = pool_of(&["a"]);
        assert_eq!(pool.try_get(NameId::DUMMY), None);
        assert_eq!(NamePool::new().try_get(NameId::DUMMY), None);
    }

    #[test]
    fn try_get_gives_none_for_an_out_of_range_handle() {
        let (pool, _) = pool_of(&["a"]);
        let (_, other) = pool_of(&["x", "y", "z"]);
        assert_eq!(pool.try_get(other[1]), None);
        assert_eq!(pool.try_get(other[2]), None);
        assert_eq!(NamePool::new().try_get(other[0]), None);
    }

    #[test]
    #[should_panic(expected = "invalid NameId(DUMMY) for NamePool of length 1")]
    fn get_panics_on_dummy() {
        let (pool, _) = pool_of(&["a"]);
        let _ = pool.get(NameId::DUMMY);
    }

    #[test]
    #[should_panic(expected = "invalid NameId(2) for NamePool of length 1")]
    fn get_panics_on_an_out_of_range_handle() {
        let (pool, _) = pool_of(&["a"]);
        let (_, other) = pool_of(&["x", "y", "z"]);
        let _ = pool.get(other[2]);
    }

    #[test]
    #[should_panic(expected = "invalid NameId(DUMMY)")]
    fn index_panics_on_dummy() {
        let (pool, _) = pool_of(&["a"]);
        let _ = &pool[NameId::DUMMY];
    }
}
