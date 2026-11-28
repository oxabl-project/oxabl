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

/// Sentinel stored in [`FoldedNames::by_name`] for a [`NameId`] not yet folded.
const UNSET_NAME: u32 = u32::MAX;

/// Own-name count up to which [`FoldedNames`] scans linearly without allocating
/// a hash table. Covers small and medium files (including single-class files)
/// without paying hash-table allocation or rehashing.
const LINEAR_CAP: usize = 16;
const _: () = assert!(LINEAR_CAP < 4096);

/// The dense table of the case-folded names of one program.
///
/// `by_name[n.index()]` caches the id of pool name `n` (`UNSET_NAME` until
/// folded). The folded texts sit in one buffer, and `slots` is an
/// open-addressing hash set of the ids once the table grows past
/// [`LINEAR_CAP`], so a lookup by text allocates nothing and stores no second
/// copy of a name.
///
/// A table may sit on a fixed set of names that every program shares (the
/// built-in handles of the language): [`with_base`](Self::with_base). The ids
/// of the base come first, and the table makes no copy of it, so a program
/// pays for its own names only.
#[derive(Clone)]
pub struct FoldedNames {
    /// The names every table of a kind starts with. Its ids are `0..base_len`
    /// and the ids of this table follow them.
    base: Option<&'static FoldedNames>,
    /// Cached `base.len()` so `shifted` and `text` need no pointer chase into
    /// `base` on every intern or lookup.
    base_len: u32,
    /// The folded texts of this table, back to back.
    text: String,
    /// Where each folded text starts in `text`; `starts[count]` is its end.
    /// Empty when this table holds no name of its own.
    starts: Vec<u32>,
    /// Sparse `NameId::index() -> FoldedNameId::as_u32()`, or `UNSET_NAME`.
    by_name: Vec<u32>,
    /// `id - base + 1` of the name in each slot, or 0 for a free slot. Empty
    /// while `own_len() <= LINEAR_CAP`; otherwise a power of two with at most
    /// half the slots in use.
    slots: Vec<u32>,
    /// Length bounds and first/second/last-byte letter masks over a root
    /// table's own names (populated only when `base: None`), so child tables
    /// skip hashing and probing `base` when a query cannot match.
    min_len: u8,
    max_len: u8,
    first_mask: u32,
    second_mask: u32,
    last_mask: u32,
}

impl Default for FoldedNames {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for FoldedNames {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FoldedNames")
            .field("names", &self.len())
            .field("pool_names", &self.by_name.len())
            .finish()
    }
}

/// Bit for `byte` read in ASCII lower case: bits `0..26` for `'a'..='z'`,
/// bit `26` for every other byte.
#[inline]
fn letter_bit(byte: u8) -> u32 {
    let idx = (byte | 0x20).wrapping_sub(b'a');
    if idx < 26 { 1u32 << idx } else { 1u32 << 26 }
}

/// Lowercase ASCII letters in 8 bytes at once without cross-byte carry.
///
/// Masking bit 7 before the adds keeps every byte in `0x00..=0x7F`, so neither
/// add can carry into the next byte. `& !word` clears the bit for any non-ASCII
/// byte (`>= 0x80`), leaving non-ASCII UTF-8 untouched.
#[inline]
fn swar_lower_u64(word: u64) -> (u64, u64) {
    let ascii = word & 0x7f7f_7f7f_7f7f_7f7f;
    let lo = ascii.wrapping_add(0x3f3f_3f3f_3f3f_3f3f);
    let hi = ascii.wrapping_add(0x2525_2525_2525_2525);
    let upper = (lo & !hi & !word) & 0x8080_8080_8080_8080;
    (word | (upper >> 2), upper)
}

/// Single-pass ASCII case-insensitive hash of `bytes` plus whether any byte is
/// an ASCII upper-case letter (`'A'..='Z'`).
#[inline]
fn hash_folded_with_case(bytes: &[u8]) -> (u32, bool) {
    const K64: u64 = 0x517c_c1b7_2722_0a95;
    let mut hash = bytes.len() as u64;
    let mut upper_any = 0u64;
    let mut rest = bytes;
    while rest.len() >= 8 {
        let (head, tail) = rest.split_at(8);
        let word = u64::from_ne_bytes([
            head[0], head[1], head[2], head[3], head[4], head[5], head[6], head[7],
        ]);
        let (lower, upper) = swar_lower_u64(word);
        upper_any |= upper;
        hash = (hash.rotate_left(5) ^ lower).wrapping_mul(K64);
        rest = tail;
    }
    if !rest.is_empty() {
        let mut tail_word = 0u64;
        if rest.len() >= 4 {
            let (head, rem) = rest.split_at(4);
            tail_word = u64::from(u32::from_ne_bytes([head[0], head[1], head[2], head[3]]));
            rest = rem;
        }
        if rest.len() >= 2 {
            let (head, rem) = rest.split_at(2);
            tail_word = (tail_word << 16) | u64::from(u16::from_ne_bytes([head[0], head[1]]));
            rest = rem;
        }
        if let Some(&last) = rest.first() {
            tail_word = (tail_word << 8) | u64::from(last);
        }
        // Zero bytes in unused lanes are unaffected by `swar_lower_u64`.
        let (lower, upper) = swar_lower_u64(tail_word);
        upper_any |= upper;
        hash = (hash.rotate_left(5) ^ lower).wrapping_mul(K64);
    }
    let mixed = ((hash >> 32) ^ hash) & u64::from(u32::MAX);
    let folded = u32::try_from(mixed).unwrap_or(0);
    (folded ^ (folded >> 15), upper_any != 0)
}

/// The hash of `bytes` read in ASCII lower case, so a name and its folded form
/// hash alike without a copy.
#[inline]
fn hash_folded(bytes: &[u8]) -> u32 {
    hash_folded_with_case(bytes).0
}

/// Whether `query` read in ASCII lower case is the folded text `stored`.
#[inline]
fn eq_folded(stored: &[u8], query: &[u8]) -> bool {
    stored.eq_ignore_ascii_case(query)
}

/// [`eq_folded`] when the caller already knows whether `query` contains any
/// ASCII upper-case byte: already-folded queries compare with plain equality.
#[inline]
fn eq_folded_with_case(stored: &[u8], query: &[u8], has_upper: bool) -> bool {
    if has_upper {
        stored.eq_ignore_ascii_case(query)
    } else {
        stored == query
    }
}

impl FoldedNames {
    /// An empty table. It allocates nothing.
    pub const fn new() -> Self {
        Self {
            base: None,
            base_len: 0,
            text: String::new(),
            starts: Vec::new(),
            by_name: Vec::new(),
            slots: Vec::new(),
            min_len: u8::MAX,
            max_len: 0,
            first_mask: 0,
            second_mask: 0,
            last_mask: 0,
        }
    }

    /// An empty table on top of a root `base`: a name of `base` keeps its id
    /// here. `base` must be a root table (itself having no base).
    pub fn with_base(base: &'static FoldedNames) -> Self {
        assert!(
            base.base.is_none(),
            "FoldedNames::with_base requires a root table"
        );
        let len = base.len();
        if len >= u32::MAX as usize {
            folded_capacity_exceeded();
        }
        Self {
            base: Some(base),
            base_len: u32::try_from(len).unwrap_or_else(|_| folded_capacity_exceeded()),
            text: String::new(),
            starts: Vec::new(),
            by_name: Vec::new(),
            slots: Vec::new(),
            min_len: u8::MAX,
            max_len: 0,
            first_mask: 0,
            second_mask: 0,
            last_mask: 0,
        }
    }

    /// Pre-size the hash table to at least `slots_min` slots (rounded up to a
    /// power of two) for a fixed static base (such as built-in handles) so
    /// probes from derived tables miss on an empty slot in L1 cache.
    pub fn reserve_base_slots(&mut self, slots_min: usize) {
        let min_for_load = self.own_len().saturating_mul(2).saturating_add(1);
        let size = slots_min.max(min_for_load).max(16).next_power_of_two();
        self.rehash_to(size);
    }

    /// How many names the base holds.
    #[inline(always)]
    pub fn base_len(&self) -> usize {
        self.base_len as usize
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

    /// Fast negative filter for a root table's own names: returns `false` only
    /// when `bytes` cannot match any own name of `self`.
    #[inline(always)]
    fn may_contain(&self, bytes: &[u8]) -> bool {
        if self.starts.is_empty() {
            return false;
        }
        let len = bytes.len();
        if len < usize::from(self.min_len)
            || (self.max_len != u8::MAX && len > usize::from(self.max_len))
        {
            return false;
        }
        if len == 0 {
            return true;
        }
        if (self.first_mask & letter_bit(bytes[0])) == 0
            || (self.last_mask & letter_bit(bytes[len - 1])) == 0
        {
            return false;
        }
        if len >= 2 && (self.second_mask & letter_bit(bytes[1])) == 0 {
            return false;
        }
        true
    }

    /// Direct first-slot probe of a static base table when `hash` is already
    /// computed: misses hit `slot == 0` in L1 cache without a function call.
    #[inline(always)]
    fn find_in_base(&self, hash: u32, bytes: &[u8], has_upper: bool) -> Option<FoldedNameId> {
        if self.slots.is_empty() {
            return self.find_linear(bytes);
        }
        let mask = self.slots.len() - 1;
        let at = hash as usize & mask;
        if self.slots[at] == 0 {
            return None;
        }
        self.find_from(at, bytes, has_upper)
    }

    /// Fold every name of `pool` that the table has not seen, so that
    /// [`id_of`](Self::id_of) answers for each of them. Names already in the
    /// table keep their ids, which lets a table start from a fixed set of
    /// names (a built-in list) and then take the program's.
    pub fn extend_from_pool(&mut self, pool: &NamePool) {
        let count = pool.len();
        if count == 0 {
            return;
        }
        if self.by_name.is_empty() {
            self.by_name.reserve(count);
            self.text.reserve(pool.byte_len());
            self.starts.reserve(count + 1);
            if self.own_len() + count > LINEAR_CAP {
                self.grow_for(count);
                for index in 0..count {
                    let (id, _) = self.intern_hashed(pool_text(pool, index));
                    self.by_name.push(id.0);
                }
            } else {
                for index in 0..count {
                    let (id, _) = self.intern_with_hint(pool_text(pool, index), 0);
                    self.by_name.push(id.0);
                }
            }
            return;
        }
        if self.by_name.len() < count {
            self.by_name.resize(count, UNSET_NAME);
        }
        for index in 0..count {
            if self.by_name[index] == UNSET_NAME {
                let (id, _) = self.intern_with_hint(pool_text(pool, index), count);
                self.by_name[index] = id.0;
            }
        }
    }

    /// The id of the pool name `name`, folding and recording it on first sight.
    ///
    /// Unlike [`extend_from_pool`](Self::extend_from_pool), names of `pool`
    /// that are never asked for are never hashed or copied.
    #[inline]
    pub fn intern_pool_name(&mut self, pool: &NamePool, name: NameId) -> FoldedNameId {
        let idx = name.index();
        if let Some(&raw) = self.by_name.get(idx)
            && raw != UNSET_NAME
        {
            return FoldedNameId(raw);
        }
        self.intern_pool_str_cold(pool, name, pool.get(name)).0
    }

    /// Like [`intern_pool_name`](Self::intern_pool_name) when the caller
    /// already holds `text = pool.get(name)`. Also reports whether `text` is
    /// known to contain no ASCII upper-case letters (always `false` on a
    /// `by_name` cache hit so the caller can fall back to [`text`](Self::text)).
    #[inline]
    pub fn intern_pool_str(
        &mut self,
        pool: &NamePool,
        name: NameId,
        text: &str,
    ) -> (FoldedNameId, bool) {
        let idx = name.index();
        if let Some(&raw) = self.by_name.get(idx)
            && raw != UNSET_NAME
        {
            return (FoldedNameId(raw), false);
        }
        self.intern_pool_str_cold(pool, name, text)
    }

    #[inline(never)]
    fn intern_pool_str_cold(
        &mut self,
        pool: &NamePool,
        name: NameId,
        text: &str,
    ) -> (FoldedNameId, bool) {
        let idx = name.index();
        let pool_len = pool.len();
        let pool_bytes = pool.byte_len();
        assert!(
            name != NameId::DUMMY && idx < pool_len,
            "FoldedNames::intern_pool_str requires a valid NameId from pool"
        );
        debug_assert_eq!(pool.get(name), text);
        if self.slots.is_empty() && self.starts.is_empty() && pool_len > 0 {
            if pool_len > LINEAR_CAP * 2 {
                // Large pool: start directly in hashed mode sized once so we
                // skip linear-scan and spill-rehash overhead.
                let target = pool_len.min(4096);
                self.starts.reserve(target + 1);
                let target_bytes = pool_bytes.min(target.saturating_mul(16));
                self.text.reserve(target_bytes);
                if self.by_name.len() < pool_len {
                    self.by_name.resize(pool_len, UNSET_NAME);
                }
                self.grow_for(target);
            } else {
                // Small pool: stay in linear mode with zero reallocations.
                self.starts.reserve(pool_len.min(LINEAR_CAP) + 1);
                self.text.reserve(pool_bytes.min(128));
                let init_by = pool_len.max(idx + 1);
                if self.by_name.len() < init_by {
                    self.by_name.resize(init_by, UNSET_NAME);
                }
            }
        }
        let (id, is_lower) = if !self.slots.is_empty() {
            if self.slots.len() <= self.own_len().saturating_mul(2) {
                self.grow_for(1);
            }
            self.intern_hashed(text)
        } else {
            let was_linear_full = self.own_len() == LINEAR_CAP;
            let res = self.intern_with_hint(text, pool_len);
            if was_linear_full && !self.slots.is_empty() {
                let target = pool_len.clamp(self.own_len(), 4096);
                let extra_names = target.saturating_sub(self.own_len());
                self.starts.reserve(extra_names + 1);
                let target_bytes = pool_bytes.min(target.saturating_mul(16));
                self.text
                    .reserve(target_bytes.saturating_sub(self.text.len()));
                if self.by_name.len() < pool_len {
                    self.by_name.resize(pool_len, UNSET_NAME);
                }
            }
            res
        };
        if idx >= self.by_name.len() {
            self.by_name.resize(idx + 1, UNSET_NAME);
        }
        self.by_name[idx] = id.0;
        (id, is_lower)
    }

    /// The id of the pool name `name`, or `None` when `name` is the dummy
    /// placeholder or has not been folded into this table.
    #[inline]
    pub fn try_id_of(&self, name: NameId) -> Option<FoldedNameId> {
        if name == NameId::DUMMY {
            return None;
        }
        let raw = *self.by_name.get(name.index())?;
        if raw == UNSET_NAME {
            return None;
        }
        Some(FoldedNameId(raw))
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
        let bytes = text.as_bytes();
        let check_base = self.base.filter(|base| base.may_contain(bytes));
        let has_own = !self.starts.is_empty();
        if check_base.is_none() && !has_own {
            return None;
        }
        if check_base.is_none() && self.slots.is_empty() {
            return self.find_linear(bytes).map(|id| self.shifted(id));
        }
        let (hash, has_upper) = hash_folded_with_case(bytes);
        if let Some(base) = check_base
            && let Some(id) = base.find_in_base(hash, bytes, has_upper)
        {
            return Some(id);
        }
        if !has_own {
            return None;
        }
        if self.slots.is_empty() {
            self.find_linear(bytes).map(|id| self.shifted(id))
        } else {
            self.find(hash, bytes, has_upper).map(|id| self.shifted(id))
        }
    }

    /// The id of `text` read in ASCII lower case, adding the folded name when
    /// the table has none like it.
    pub fn intern(&mut self, text: &str) -> FoldedNameId {
        self.intern_with_hint(text, 0).0
    }

    /// The folded (ASCII lower case) text of `id`. Panics when `id` is not
    /// from this table.
    #[inline(always)]
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
    #[inline(always)]
    fn own_text(&self, index: usize) -> &str {
        match (self.starts.get(index), self.starts.get(index + 1)) {
            (Some(&start), Some(&end)) => &self.text[start as usize..end as usize],
            _ => invalid_folded_id(self.len()),
        }
    }

    /// Raw UTF-8 bytes of the own name at `index`, skipping `str` char-boundary
    /// checks in internal equality probes.
    #[inline(always)]
    fn own_bytes(&self, index: usize) -> &[u8] {
        let start = self.starts[index] as usize;
        let end = self.starts[index + 1] as usize;
        &self.text.as_bytes()[start..end]
    }

    /// Linear scan of own names while `self.slots` is empty (`own_len <= LINEAR_CAP`).
    /// Checks byte length and first folded byte before comparing the tail.
    #[inline(always)]
    fn find_linear(&self, bytes: &[u8]) -> Option<FoldedNameId> {
        let target_len = bytes.len();
        let starts = self.starts.as_slice();
        let text = self.text.as_bytes();
        let own_len = self.own_len();
        if target_len == 0 {
            for index in 0..own_len {
                if starts[index] == starts[index + 1] {
                    let raw = u32::try_from(index).unwrap_or_else(|_| folded_capacity_exceeded());
                    return Some(FoldedNameId(raw));
                }
            }
            return None;
        }
        let first_lower = bytes[0].to_ascii_lowercase();
        for index in 0..own_len {
            let start = starts[index] as usize;
            let end = starts[index + 1] as usize;
            if end - start == target_len
                && text[start] == first_lower
                && (target_len == 1 || eq_folded(&text[start + 1..end], &bytes[1..]))
            {
                let raw = u32::try_from(index).unwrap_or_else(|_| folded_capacity_exceeded());
                return Some(FoldedNameId(raw));
            }
        }
        None
    }

    /// Continue a hash probe in `self.slots` starting at occupied slot `at`.
    fn find_from(&self, mut at: usize, bytes: &[u8], has_upper: bool) -> Option<FoldedNameId> {
        let mask = self.slots.len() - 1;
        for _ in 0..self.slots.len() {
            let slot = self.slots[at];
            if slot == 0 {
                return None;
            }
            let id = FoldedNameId(slot - 1);
            if eq_folded_with_case(self.own_bytes(id.index()), bytes, has_upper) {
                return Some(id);
            }
            at = (at + 1) & mask;
        }
        unreachable!("open-addressing table always has an empty slot")
    }

    /// The id, in the numbering of this table alone, of the name that folds
    /// to `bytes`, which hashes to `hash`.
    fn find(&self, hash: u32, bytes: &[u8], has_upper: bool) -> Option<FoldedNameId> {
        if self.slots.is_empty() {
            return self.find_linear(bytes);
        }
        let mask = self.slots.len() - 1;
        let at = hash as usize & mask;
        if self.slots[at] == 0 {
            return None;
        }
        self.find_from(at, bytes, has_upper)
    }

    /// The id of a name of this table in the numbering of the whole table,
    /// which counts the base first.
    #[inline(always)]
    fn shifted(&self, own: FoldedNameId) -> FoldedNameId {
        FoldedNameId(own.0 + self.base_len)
    }

    /// Record `bytes` in the root table's length bounds and byte masks.
    #[inline]
    fn note_own_summary(&mut self, bytes: &[u8]) {
        let len_clamped = u8::try_from(bytes.len()).unwrap_or(u8::MAX);
        if len_clamped < self.min_len {
            self.min_len = len_clamped;
        }
        if len_clamped > self.max_len {
            self.max_len = len_clamped;
        }
        if let Some(&b0) = bytes.first() {
            self.first_mask |= letter_bit(b0);
        }
        if let Some(&b1) = bytes.get(1) {
            self.second_mask |= letter_bit(b1);
        }
        if let Some(&bl) = bytes.last() {
            self.last_mask |= letter_bit(bl);
        }
    }

    /// Append a new own name to `self.text` and `self.starts` and return its
    /// unshifted id. Skips lowercasing when `has_upper` is false.
    #[inline(always)]
    fn append_own(&mut self, text: &str, has_upper: bool) -> FoldedNameId {
        if self.starts.is_empty() {
            self.starts.push(0);
        }
        let own_idx = self.starts.len() - 1;
        if self.base_len().saturating_add(own_idx) >= u32::MAX as usize {
            folded_capacity_exceeded();
        }
        let own =
            FoldedNameId(u32::try_from(own_idx).unwrap_or_else(|_| folded_capacity_exceeded()));
        let start = self.text.len();
        self.text.push_str(text);
        if has_upper {
            self.text[start..].make_ascii_lowercase();
        }
        let end = u32::try_from(self.text.len()).unwrap_or_else(|_| folded_text_exceeded());
        self.starts.push(end);
        // Only root tables (no parent `base`) can themselves be used as a base
        // pre-filter, so derived per-program tables skip maintaining the summary.
        if self.base.is_none() {
            self.note_own_summary(text.as_bytes());
        }
        own
    }

    /// Make room for `extra` more names without a second rehash.
    fn grow_for(&mut self, extra: usize) {
        let need = (self.own_len() + extra).saturating_mul(2).max(16);
        if self.slots.len() >= need {
            return;
        }
        self.rehash_to(need.next_power_of_two());
    }

    /// Allocate `size` (power-of-two) open-addressing slots and re-insert all
    /// own names.
    fn rehash_to(&mut self, size: usize) {
        debug_assert!(size.is_power_of_two() && size > self.own_len());
        if self.slots.len() >= size {
            return;
        }
        self.slots = vec![0; size];
        let mask = size - 1;
        let count = u32::try_from(self.own_len()).unwrap_or_else(|_| folded_capacity_exceeded());
        for raw in 0..count {
            let hash = hash_folded(self.own_bytes(raw as usize));
            let mut at = hash as usize & mask;
            for _ in 0..size {
                if self.slots[at] == 0 {
                    break;
                }
                at = (at + 1) & mask;
            }
            debug_assert_eq!(self.slots[at], 0);
            self.slots[at] = raw + 1;
        }
    }

    /// Intern `text`, staying in linear mode while `own_len <= LINEAR_CAP` and
    /// spilling to `slots` (sized with `pool_hint` when non-zero) on growth.
    /// Returns `(id, is_already_lowercase)`.
    fn intern_with_hint(&mut self, text: &str, pool_hint: usize) -> (FoldedNameId, bool) {
        if self.slots.is_empty() {
            let bytes = text.as_bytes();
            let mut case_known: Option<bool> = None;
            if let Some(base) = self.base
                && base.may_contain(bytes)
            {
                let (hash, has_upper) = hash_folded_with_case(bytes);
                case_known = Some(has_upper);
                if let Some(id) = base.find_in_base(hash, bytes, has_upper) {
                    return (id, !has_upper);
                }
            }
            if let Some(own) = self.find_linear(bytes) {
                let is_lower = case_known
                    .map_or_else(|| !bytes.iter().any(u8::is_ascii_uppercase), |upper| !upper);
                return (self.shifted(own), is_lower);
            }
            if self.own_len() < LINEAR_CAP {
                let upper = case_known.unwrap_or_else(|| bytes.iter().any(u8::is_ascii_uppercase));
                let own = self.append_own(text, upper);
                return (self.shifted(own), !upper);
            }
            // Spill to open-addressing once a file declares more than LINEAR_CAP
            // distinct names. Use `pool_hint` (capped) so large files do not rehash.
            let target = pool_hint.clamp(self.own_len() + 1, 4096);
            self.grow_for(target.saturating_sub(self.own_len()));
        } else if self.slots.len() <= self.own_len().saturating_mul(2) {
            self.grow_for(1);
        }
        self.intern_hashed(text)
    }

    /// Intern `text` into the active `slots` hash table.
    /// Returns `(id, is_already_lowercase)`.
    #[inline(always)]
    fn intern_hashed(&mut self, text: &str) -> (FoldedNameId, bool) {
        debug_assert!(self.slots.len() > 2 * self.own_len());
        let bytes = text.as_bytes();
        let (hash, upper) = hash_folded_with_case(bytes);
        // Hash is already computed, so probing `base.slots[hash & mask]` directly
        // misses in 4 instructions (cheaper than running `may_contain`).
        if let Some(base) = self.base
            && let Some(id) = base.find_in_base(hash, bytes, upper)
        {
            return (id, !upper);
        }
        let mask = self.slots.len() - 1;
        let mut at = hash as usize & mask;
        for _ in 0..self.slots.len() {
            let slot = self.slots[at];
            if slot == 0 {
                let own = self.append_own(text, upper);
                self.slots[at] = own.0 + 1;
                return (self.shifted(own), !upper);
            }
            let own = FoldedNameId(slot - 1);
            if eq_folded_with_case(self.own_bytes(own.index()), bytes, upper) {
                return (self.shifted(own), !upper);
            }
            at = (at + 1) & mask;
        }
        unreachable!("open-addressing table always has an empty slot")
    }
}

/// The text of the pool name at `index`.
fn pool_text(pool: &NamePool, index: usize) -> &str {
    pool.get(NameId::from_index(index))
}

#[cold]
#[inline(never)]
fn invalid_folded_id(len: usize) -> ! {
    panic!("invalid FoldedNameId for a FoldedNames of {len} names")
}

#[cold]
#[inline(never)]
fn folded_capacity_exceeded() -> ! {
    panic!("FoldedNames capacity exceeded")
}

#[cold]
#[inline(never)]
fn folded_text_exceeded() -> ! {
    panic!("FoldedNames text exceeds u32::MAX bytes")
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

    #[test]
    fn swar_lower_exact_for_all_bytes_and_tail_lengths() {
        for b in 0u8..=255 {
            let (lowered, upper) = swar_lower_u64(u64::from_ne_bytes([b; 8]));
            assert_eq!(
                lowered.to_ne_bytes(),
                [b.to_ascii_lowercase(); 8],
                "{b:#04x}"
            );
            let expected_upper = if b.is_ascii_uppercase() {
                0x8080_8080_8080_8080
            } else {
                0
            };
            assert_eq!(upper, expected_upper, "upper mask for {b:#04x}");
        }
        // Every length 0..=17 and every byte offset: verify hash matches
        // lowercased bytes and `has_upper` has no false positives/negatives.
        for len in 0..=17 {
            let lower = vec![b'a'; len];
            let (base_hash, base_upper) = hash_folded_with_case(&lower);
            assert!(!base_upper, "all-lower len={len}");
            for pos in 0..len {
                for &(probe, expect_upper) in &[
                    (b'A', true),
                    (b'Z', true),
                    (b'@', false),
                    (b'[', false),
                    (b'`', false),
                    (b'{', false),
                    (0x80, false),
                    (0xc1, false),
                    (0xff, false),
                ] {
                    let mut buf = lower.clone();
                    buf[pos] = probe;
                    let (h, has_upper) = hash_folded_with_case(&buf);
                    assert_eq!(
                        has_upper, expect_upper,
                        "len={len} pos={pos} probe={probe:#04x}"
                    );
                    if expect_upper {
                        let expected: Vec<u8> = buf.iter().map(u8::to_ascii_lowercase).collect();
                        assert_eq!(h, hash_folded(&expected), "len={len} pos={pos}");
                    } else if probe == b'a' {
                        assert_eq!(h, base_hash);
                    }
                }
            }
        }
    }

    #[test]
    fn prefilter_accepts_every_case_and_rejects_disjoint_root_tables() {
        for (name, reject) in [
            ("", "a"),
            ("a", "b"),
            ("ab", "ac"),
            ("session", "zz"),
            ("this-object", "other"),
            ("Ärger", "x"),
            (&"x".repeat(300), "y"),
        ] {
            let mut root = FoldedNames::new();
            root.intern(name);
            assert!(root.may_contain(name.as_bytes()), "{name:?}");
            assert!(
                root.may_contain(name.to_ascii_uppercase().as_bytes()),
                "{name:?} upper"
            );
            assert!(
                !root.may_contain(reject.as_bytes()),
                "{name:?} vs {reject:?}"
            );
        }
    }

    #[test]
    fn linear_mode_spills_to_hash_table_past_linear_cap() {
        // Pool size 24 sits in LINEAR_CAP+1..=LINEAR_CAP*2, so lazy interning
        // starts in linear mode and exercises the spill transition at LINEAR_CAP.
        let mut texts = vec!["unasked-literal".to_string(), "SESSION".to_string()];
        for i in 0..(LINEAR_CAP + 5) {
            texts.push(format!("Var-{i}"));
        }
        texts.push("VAR-0".to_string());
        let refs: Vec<&str> = texts.iter().map(String::as_str).collect();
        let pool = pool(&refs);
        assert!(pool.len() > LINEAR_CAP && pool.len() <= LINEAR_CAP * 2);

        let mut names = FoldedNames::with_base(&BASE);
        assert_eq!(names.try_id_of(NameId::from_index(0)), None);
        assert_eq!(names.lookup("unasked-literal"), None);
        let session = names.intern_pool_name(&pool, NameId::from_index(1));
        assert_eq!(session.index(), 0);
        assert!(names.slots.is_empty());

        let mut ids = Vec::new();
        for i in 0..(LINEAR_CAP + 5) {
            let id = names.intern_pool_name(&pool, NameId::from_index(2 + i));
            assert_eq!(names.text(id), format!("var-{i}"));
            if i < LINEAR_CAP {
                assert!(
                    names.slots.is_empty(),
                    "linear mode through own_len={}",
                    i + 1
                );
            } else {
                assert!(!names.slots.is_empty(), "spilled at own_len={}", i + 1);
            }
            ids.push(id);
        }
        let dup = names.intern_pool_name(&pool, NameId::from_index(2 + LINEAR_CAP + 5));
        assert_eq!(dup, ids[0]);
        assert_eq!(names.lookup("unasked-literal"), None);
        assert_eq!(names.lookup("VAR-0"), Some(ids[0]));

        // Also exercise direct `intern()` spilling across LINEAR_CAP.
        let mut direct = FoldedNames::new();
        for i in 0..(LINEAR_CAP + 4) {
            let id = direct.intern(&format!("Direct-{i}"));
            if i < LINEAR_CAP {
                assert!(direct.slots.is_empty());
            } else {
                assert!(!direct.slots.is_empty());
            }
            assert_eq!(direct.lookup(&format!("DIRECT-{i}")), Some(id));
        }
    }

    #[test]
    fn large_pool_starts_in_hashed_mode_and_folds_lazily() {
        let mut texts = vec!["unasked-literal".to_string(), "SESSION".to_string()];
        for i in 0..36 {
            texts.push(format!("Var-{i}"));
        }
        texts.push("VAR-0".to_string());
        let refs: Vec<&str> = texts.iter().map(String::as_str).collect();
        let pool = pool(&refs);
        assert!(pool.len() > LINEAR_CAP * 2);

        let mut names = FoldedNames::with_base(&BASE);
        let first = names.intern_pool_name(&pool, NameId::from_index(2));
        assert!(!names.slots.is_empty(), "large pool starts in hashed mode");
        assert_eq!(names.text(first), "var-0");
        assert_eq!(names.lookup("unasked-literal"), None);
        assert_eq!(names.intern_pool_name(&pool, NameId::from_index(38)), first);
    }

    #[test]
    fn reserve_base_slots_sizes_directly_to_requested_slot_count() {
        let mut base = FoldedNames::new();
        base.reserve_base_slots(512);
        assert_eq!(base.slots.len(), 512);
    }

    #[test]
    #[should_panic(expected = "FoldedNames::with_base requires a root table")]
    fn with_base_rejects_nested_base() {
        static CHILD: std::sync::LazyLock<FoldedNames> =
            std::sync::LazyLock::new(|| FoldedNames::with_base(&BASE));
        let _ = FoldedNames::with_base(&CHILD);
    }

    #[test]
    #[should_panic(expected = "FoldedNames::intern_pool_str requires a valid NameId from pool")]
    fn intern_pool_str_rejects_dummy_name_id() {
        let pool = pool(&["a"]);
        let mut names = FoldedNames::new();
        let _ = names.intern_pool_str(&pool, NameId::DUMMY, "a");
    }
}
