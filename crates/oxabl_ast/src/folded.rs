//! Per-program case-folded names (`FoldedNameId` + `FoldedNames`).
//!
//! ABL names are case-insensitive, so every pass that looks a name up folds it
//! first. `FoldedNames` folds each name of one program **once**: it maps every
//! [`NameId`] of the program's [`NamePool`] to a 4-byte [`FoldedNameId`] in a
//! flat vector, and two names that differ only in ASCII case get the same id.
//! A lookup by `NameId` is then an array index, and a comparison of two names
//! is a `u32` comparison. The table keeps no global state and takes no lock.
//!
//! A name that is not in the program's pool (a built-in, a schema name, a
//! piece of a dotted path) is looked up by its text ([`FoldedNames::lookup`])
//! or added by it ([`FoldedNames::intern`]).
//!
//! The fold is ASCII only, as the ABL keywords and the schema's atoms are:
//! `Ä` and `ä` are two names.

use std::fmt;

use crate::name::{NameId, NamePool};

/// Handle of a case-folded name inside one [`FoldedNames`] table.
///
/// An id means something only for the table that issued it: two tables number
/// their names on their own.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldedNameId(u32);

impl FoldedNameId {
    /// Zero-based position of the name in its table.
    #[inline]
    pub const fn index(self) -> usize {
        self.0 as usize
    }

    /// The raw value, for diagnostics and debugging.
    #[inline]
    pub const fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Debug for FoldedNameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FoldedNameId({})", self.0)
    }
}

const _: () = assert!(std::mem::size_of::<FoldedNameId>() == 4);

/// The dense table of the case-folded names of one program.
///
/// `by_name[n.index()]` is the id of the pool name `n`. The folded texts sit
/// in one buffer, and `slots` is an open-addressing hash set of the ids, so a
/// lookup by text allocates nothing and stores no second copy of a name.
///
/// A table may sit on a fixed set of names that every program shares (the
/// built-in handles of the language): [`with_base`](Self::with_base). The ids
/// of the base come first, and the table makes no copy of it, so a program
/// pays for its own names only.
#[derive(Clone, Default)]
pub struct FoldedNames {
    /// The names every table of a kind starts with. Its ids are `0..base.len()`
    /// and the ids of this table follow them.
    base: Option<&'static FoldedNames>,
    /// The folded texts of this table, back to back.
    text: String,
    /// Where each folded text starts in `text`; `starts[count]` is its end.
    /// Empty when this table holds no name of its own.
    starts: Vec<u32>,
    /// The id of each [`NameId`] of the pool, by `NameId::index`.
    by_name: Vec<FoldedNameId>,
    /// `id - base + 1` of the name in each slot, or 0 for a free slot. The
    /// length is a power of two, and at most half the slots are in use.
    slots: Vec<u32>,
}

impl fmt::Debug for FoldedNames {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FoldedNames")
            .field("names", &self.len())
            .field("pool_names", &self.by_name.len())
            .finish()
    }
}

/// The hash of `bytes` read in ASCII lower case, so a name and its folded form
/// hash alike without a copy.
#[inline]
fn hash_folded(bytes: &[u8]) -> u32 {
    const K: u32 = 0x9e37_79b9;
    let mut hash = 0u32;
    for &byte in bytes {
        hash = (hash.rotate_left(5) ^ u32::from(byte.to_ascii_lowercase())).wrapping_mul(K);
    }
    // The low bits of a product depend only on the low bits of its factors,
    // and the slot is taken from the low bits.
    hash ^ (hash >> 15)
}

/// Whether `query` read in ASCII lower case is the folded text `stored`.
#[inline]
fn eq_folded(stored: &[u8], query: &[u8]) -> bool {
    stored.eq_ignore_ascii_case(query)
}

impl FoldedNames {
    /// An empty table. It allocates nothing.
    pub const fn new() -> Self {
        Self {
            base: None,
            text: String::new(),
            starts: Vec::new(),
            by_name: Vec::new(),
            slots: Vec::new(),
        }
    }

    /// An empty table on top of `base`: a name of `base` keeps its id here.
    pub const fn with_base(base: &'static FoldedNames) -> Self {
        Self {
            base: Some(base),
            text: String::new(),
            starts: Vec::new(),
            by_name: Vec::new(),
            slots: Vec::new(),
        }
    }

    /// How many names the base holds.
    #[inline]
    fn base_len(&self) -> usize {
        match self.base {
            Some(base) => base.len(),
            None => 0,
        }
    }

    /// The table of every name of `pool`.
    pub fn from_pool(pool: &NamePool) -> Self {
        let mut names = Self::new();
        names.extend_from_pool(pool);
        names
    }

    /// Number of distinct folded names, those of the base included.
    #[inline]
    pub fn len(&self) -> usize {
        self.base_len() + self.own_len()
    }

    /// Number of names of this table beyond its base.
    #[inline]
    fn own_len(&self) -> usize {
        self.starts.len().saturating_sub(1)
    }

    /// Whether the table holds no name.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Fold every name of `pool` that the table has not seen, so that
    /// [`id_of`](Self::id_of) answers for each of them. Names already in the
    /// table keep their ids, which lets a table start from a fixed set of
    /// names (a built-in list) and then take the program's.
    pub fn extend_from_pool(&mut self, pool: &NamePool) {
        let from = self.by_name.len();
        if pool.len() <= from {
            return;
        }
        self.by_name.reserve(pool.len() - from);
        // The folded names are at most the pool's text, and there are at most
        // as many of them as pool names: no buffer grows on the way.
        self.text.reserve(pool.byte_len());
        self.starts.reserve(pool.len() + 1);
        self.grow_for(pool.len() - from);
        for index in from..pool.len() {
            let id = self.intern_str(pool_text(pool, index));
            self.by_name.push(id);
        }
    }

    /// The id of the pool name `name`, or `None` when `name` is the dummy
    /// placeholder or is not in the pool this table was built from.
    #[inline]
    pub fn try_id_of(&self, name: NameId) -> Option<FoldedNameId> {
        if name == NameId::DUMMY {
            return None;
        }
        self.by_name.get(name.index()).copied()
    }

    /// The id of the pool name `name`. Panics when `name` is not in the pool
    /// this table was built from, as [`NamePool::get`] does.
    #[inline]
    pub fn id_of(&self, name: NameId) -> FoldedNameId {
        match self.try_id_of(name) {
            Some(id) => id,
            None => panic!(
                "invalid {name:?} for a FoldedNames of {} pool names \
                 (did you build it from the owning Program's pool?)",
                self.by_name.len()
            ),
        }
    }

    /// The id of `text` read in ASCII lower case, or `None` when no name of
    /// the table folds to it. Allocates nothing.
    pub fn lookup(&self, text: &str) -> Option<FoldedNameId> {
        let hash = hash_folded(text.as_bytes());
        if let Some(base) = self.base
            && let Some(id) = base.find(hash, text)
        {
            return Some(id);
        }
        self.find(hash, text).map(|id| self.shifted(id))
    }

    /// The id of `text` read in ASCII lower case, adding the folded name when
    /// the table has none like it.
    pub fn intern(&mut self, text: &str) -> FoldedNameId {
        self.grow_for(1);
        self.intern_str(text)
    }

    /// The folded (ASCII lower case) text of `id`. Panics when `id` is not
    /// from this table.
    #[inline]
    pub fn text(&self, id: FoldedNameId) -> &str {
        let base_len = self.base_len();
        if id.index() < base_len
            && let Some(base) = self.base
        {
            return base.own_text(id.index());
        }
        self.own_text(id.index() - base_len)
    }

    /// The text of the name at `index` among the names of this table, the
    /// base apart.
    #[inline]
    fn own_text(&self, index: usize) -> &str {
        match (self.starts.get(index), self.starts.get(index + 1)) {
            (Some(&start), Some(&end)) => &self.text[start as usize..end as usize],
            _ => panic!(
                "invalid FoldedNameId for a FoldedNames of {} names",
                self.len()
            ),
        }
    }

    /// The id, in the numbering of this table alone, of the name that folds
    /// to `text`, which hashes to `hash`.
    fn find(&self, hash: u32, text: &str) -> Option<FoldedNameId> {
        if self.slots.is_empty() {
            return None;
        }
        let mask = self.slots.len() - 1;
        let mut at = hash as usize & mask;
        loop {
            let slot = self.slots[at];
            if slot == 0 {
                return None;
            }
            let id = FoldedNameId(slot - 1);
            if eq_folded(self.own_text(id.index()).as_bytes(), text.as_bytes()) {
                return Some(id);
            }
            at = (at + 1) & mask;
        }
    }

    /// The id of a name of this table in the numbering of the whole table,
    /// which counts the base first.
    #[inline]
    fn shifted(&self, own: FoldedNameId) -> FoldedNameId {
        match self.base {
            Some(base) => FoldedNameId(own.0 + base.len_u32()),
            None => own,
        }
    }

    #[inline]
    fn len_u32(&self) -> u32 {
        u32::try_from(self.len()).unwrap_or(u32::MAX)
    }

    /// Make room for `extra` more names without a second rehash.
    fn grow_for(&mut self, extra: usize) {
        let need = (self.own_len() + extra).saturating_mul(2).max(16);
        if self.slots.len() >= need {
            return;
        }
        let size = need.next_power_of_two();
        self.slots = vec![0; size];
        let mask = size - 1;
        let count = u32::try_from(self.own_len()).unwrap_or(u32::MAX);
        for raw in 0..count {
            let hash = hash_folded(self.own_text(raw as usize).as_bytes());
            let mut at = hash as usize & mask;
            while self.slots[at] != 0 {
                at = (at + 1) & mask;
            }
            self.slots[at] = raw + 1;
        }
    }

    /// [`intern`](Self::intern) with the slots already sized.
    fn intern_str(&mut self, text: &str) -> FoldedNameId {
        debug_assert!(self.slots.len() > 2 * self.own_len());
        let hash = hash_folded(text.as_bytes());
        if let Some(base) = self.base
            && let Some(id) = base.find(hash, text)
        {
            return id;
        }
        let mask = self.slots.len() - 1;
        let mut at = hash as usize & mask;
        loop {
            let slot = self.slots[at];
            if slot == 0 {
                break;
            }
            let own = FoldedNameId(slot - 1);
            if eq_folded(self.own_text(own.index()).as_bytes(), text.as_bytes()) {
                return self.shifted(own);
            }
            at = (at + 1) & mask;
        }
        if self.starts.is_empty() {
            self.starts.push(0);
        }
        let own = FoldedNameId(
            u32::try_from(self.starts.len() - 1)
                .unwrap_or_else(|_| panic!("FoldedNames capacity exceeded")),
        );
        let start = self.text.len();
        self.text.push_str(text);
        self.text[start..].make_ascii_lowercase();
        let end = u32::try_from(self.text.len())
            .unwrap_or_else(|_| panic!("FoldedNames text exceeds u32::MAX bytes"));
        self.starts.push(end);
        self.slots[at] = own.0 + 1;
        self.shifted(own)
    }
}

/// The text of the pool name at `index`.
fn pool_text(pool: &NamePool, index: usize) -> &str {
    pool.get(NameId::from_index(index))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pool(names: &[&str]) -> NamePool {
        let mut pool = NamePool::new();
        for name in names {
            pool.push(name);
        }
        pool
    }

    #[test]
    fn names_that_differ_only_in_case_share_an_id() {
        let pool = pool(&["Customer", "customer", "CUSTOMER", "cUsToMeR", "order"]);
        let names = FoldedNames::from_pool(&pool);
        let ids: Vec<_> = (0..pool.len())
            .map(|i| names.id_of(NameId::from_index(i)))
            .collect();
        assert_eq!(ids[0], ids[1]);
        assert_eq!(ids[0], ids[2]);
        assert_eq!(ids[0], ids[3]);
        assert_ne!(ids[0], ids[4]);
        assert_eq!(names.len(), 2);
        assert_eq!(names.text(ids[0]), "customer");
    }

    #[test]
    fn a_hyphenated_name_folds_only_its_letters() {
        let pool = pool(&["Order-Line-NUM", "order-line-num", "order_line_num"]);
        let names = FoldedNames::from_pool(&pool);
        assert_eq!(
            names.id_of(NameId::from_index(0)),
            names.id_of(NameId::from_index(1))
        );
        assert_ne!(
            names.id_of(NameId::from_index(0)),
            names.id_of(NameId::from_index(2))
        );
        assert_eq!(
            names.lookup("ORDER-LINE-NUM").map(|id| names.text(id)),
            Some("order-line-num")
        );
        assert_eq!(names.lookup("order-line"), None);
    }

    #[test]
    fn a_name_longer_than_any_stack_buffer_folds_whole() {
        let long = "A".repeat(300) + "-Tail";
        let lower = long.to_ascii_lowercase();
        let pool = pool(&[&long, &lower]);
        let names = FoldedNames::from_pool(&pool);
        let id = names.id_of(NameId::from_index(0));
        assert_eq!(id, names.id_of(NameId::from_index(1)));
        assert_eq!(names.text(id), lower);
        assert_eq!(names.lookup(&long), Some(id));
        // A name that differs in its last byte is another name.
        assert_eq!(names.lookup(&(long.clone() + "x")), None);
    }

    #[test]
    fn a_non_ascii_letter_is_not_folded() {
        let pool = pool(&["Ärger", "ärger", "ÄRGER", "Ärger"]);
        let names = FoldedNames::from_pool(&pool);
        let id = |i| names.id_of(NameId::from_index(i));
        // ASCII folding leaves `Ä` alone, so `Ärger` and `ärger` differ;
        // `ÄRGER` folds to `Ärger`, and a second `Ärger` is the same name.
        assert_ne!(id(0), id(1));
        assert_eq!(id(0), id(2));
        assert_eq!(id(0), id(3));
        assert_eq!(names.text(id(0)), "Ärger");
        assert_eq!(names.text(id(1)), "ärger");
    }

    #[test]
    fn lookup_and_intern_agree_with_the_pool() {
        let pool = pool(&["Alpha", "Beta"]);
        let mut names = FoldedNames::from_pool(&pool);
        let beta = names.id_of(NameId::from_index(1));
        assert_eq!(names.lookup("BETA"), Some(beta));
        assert_eq!(names.lookup("gamma"), None);
        assert_eq!(names.intern("BeTa"), beta);
        let gamma = names.intern("Gamma");
        assert_eq!(names.lookup("gamma"), Some(gamma));
        assert_eq!(names.text(gamma), "gamma");
        assert_eq!(names.len(), 3);
    }

    #[test]
    fn extending_with_a_pool_keeps_the_ids_of_earlier_names() {
        let mut names = FoldedNames::new();
        let seeded = names.intern("Session");
        names.extend_from_pool(&pool(&["x", "SESSION"]));
        assert_eq!(names.id_of(NameId::from_index(1)), seeded);
        assert_eq!(names.len(), 2);
    }

    static BASE: std::sync::LazyLock<FoldedNames> = std::sync::LazyLock::new(|| {
        let mut base = FoldedNames::new();
        base.intern("Session");
        base.intern("this-procedure");
        base
    });

    #[test]
    fn a_table_on_a_base_keeps_the_ids_of_the_base_and_numbers_its_own_after_them() {
        let pool = pool(&["Order", "SESSION", "This-Procedure", "order", "Line"]);
        let mut names = FoldedNames::with_base(&BASE);
        names.extend_from_pool(&pool);
        let id = |i| names.id_of(NameId::from_index(i));
        assert_eq!(id(1).index(), 0, "SESSION is the first name of the base");
        assert_eq!(id(2).index(), 1);
        assert_eq!(
            id(0).index(),
            2,
            "the first name of its own follows the base"
        );
        assert_eq!(id(3), id(0));
        assert_eq!(id(4).index(), 3);
        assert_eq!(names.len(), 4);
        for i in 0..pool.len() {
            let folded = names.text(id(i));
            assert_eq!(folded, pool.get(NameId::from_index(i)).to_ascii_lowercase());
            assert_eq!(names.lookup(&folded.to_ascii_uppercase()), Some(id(i)));
        }
        assert_eq!(names.lookup("absent"), None);
        assert_eq!(BASE.len(), 2, "the base is not changed");
        let mut other = FoldedNames::with_base(&BASE);
        assert_eq!(other.intern("Line").index(), 2);
        assert_eq!(other.intern("session").index(), 0);
    }

    #[test]
    fn a_table_on_a_base_answers_before_anything_is_added() {
        let names = FoldedNames::with_base(&BASE);
        assert_eq!(names.len(), 2);
        assert_eq!(
            names.lookup("SESSION").map(|id| names.text(id)),
            Some("session")
        );
        assert_eq!(names.lookup("order"), None);
    }

    #[test]
    fn hash_and_comparison_ignore_ascii_case_and_nothing_else() {
        let texts = [
            "",
            "a",
            "A",
            "abcdefg",
            "ABCDEFGH",
            "abcdefgh",
            "AbCdEfGhI",
            "order-line-total",
            "ORDER-LINE-TOTAL",
            "Ärger",
            "ÄRGER",
            "ärger",
            "x_y",
            "X-Y",
            "a@[`{",
            "A@[`{",
        ];
        for a in texts {
            for b in texts {
                let same = a.eq_ignore_ascii_case(b);
                assert_eq!(
                    hash_folded(a.as_bytes()) == hash_folded(b.as_bytes()),
                    same,
                    "{a} {b}"
                );
                assert_eq!(
                    eq_folded(a.to_ascii_lowercase().as_bytes(), b.as_bytes()),
                    same,
                    "{a} {b}"
                );
            }
        }
    }

    #[test]
    fn many_names_survive_growth() {
        let texts: Vec<String> = (0..5_000).map(|i| format!("Name-{i}")).collect();
        let refs: Vec<&str> = texts.iter().map(String::as_str).collect();
        let pool = pool(&refs);
        let names = FoldedNames::from_pool(&pool);
        assert_eq!(names.len(), 5_000);
        for (i, text) in texts.iter().enumerate() {
            let id = names.id_of(NameId::from_index(i));
            assert_eq!(names.text(id), text.to_ascii_lowercase());
            assert_eq!(names.lookup(&text.to_ascii_uppercase()), Some(id));
        }
        assert_eq!(names.lookup("name-5000"), None);
    }

    #[test]
    fn the_dummy_name_and_a_foreign_name_have_no_id() {
        let names = FoldedNames::from_pool(&pool(&["a"]));
        assert_eq!(names.try_id_of(NameId::DUMMY), None);
        assert_eq!(names.try_id_of(NameId::from_index(5)), None);
        assert_eq!(FoldedNames::new().lookup("a"), None);
        assert!(FoldedNames::new().is_empty());
    }
}
