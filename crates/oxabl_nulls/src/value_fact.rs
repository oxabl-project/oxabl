//! Conditions that must hold on paths carrying a known `?` source.
//!
//! A condition is a bit. Bits below `limit` say "this tested local is `?`";
//! the bits above are the atoms of [`Fact::flag`] and [`Fact::buffer`].
use super::{KNOWN, QUESTION};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use std::ops::{Deref, DerefMut};

/// A set of conditions.
#[derive(Debug, Clone, Default)]
pub(super) struct Required(SmallVec<[u64; 4]>);

impl PartialEq for Required {
    fn eq(&self, other: &Self) -> bool {
        let (a, b) = (&self.0, &other.0);
        let n = a.len().max(b.len());
        (0..n).all(|w| a.get(w).copied().unwrap_or(0) == b.get(w).copied().unwrap_or(0))
    }
}
impl Eq for Required {}

impl Required {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn contains(&self, i: usize) -> bool {
        self.0.get(i / 64).is_some_and(|w| w & (1 << (i % 64)) != 0)
    }
    pub fn insert(&mut self, i: usize) {
        if self.0.len() <= i / 64 {
            self.0.resize(i / 64 + 1, 0);
        }
        self.0[i / 64] |= 1 << (i % 64);
    }
    pub fn remove(&mut self, i: usize) {
        if let Some(w) = self.0.get_mut(i / 64) {
            *w &= !(1 << (i % 64));
        }
    }
    /// Keep the bits from `from` on that `keep` selects.
    pub fn retain_from(&mut self, from: usize, keep: impl Fn(usize) -> bool) {
        for w in from / 64..self.0.len() {
            let mut bits = self.0[w];
            while bits != 0 {
                let b = bits.trailing_zeros() as usize;
                bits &= bits - 1;
                let i = w * 64 + b;
                if i >= from && !keep(i) {
                    self.0[w] &= !(1 << b);
                }
            }
        }
    }
    /// Clear every bit from `from` on.
    pub fn clear_from(&mut self, from: usize) {
        self.retain_from(from, |_| false);
    }
    pub fn intersect(&mut self, other: &Self) {
        self.0.truncate(other.0.len());
        for (a, b) in self.0.iter_mut().zip(&other.0) {
            *a &= b;
        }
    }
    pub fn union(&mut self, other: &Self) {
        if self.0.len() < other.0.len() {
            self.0.resize(other.0.len(), 0);
        }
        for (a, b) in self.0.iter_mut().zip(&other.0) {
            *a |= b;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Fact {
    pub values: Vec<u8>,
    /// Per local: the conditions on every path that carries its `?` source.
    pub requires: FxHashMap<usize, Required>,
    /// Conditions that hold on every path here.
    pub held: Required,
    /// The leading locals a condition may name: those a test reads.
    pub limit: usize,
}
impl Deref for Fact {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        &self.values
    }
}
impl DerefMut for Fact {
    fn deref_mut(&mut self) -> &mut [u8] {
        &mut self.values
    }
}
impl Fact {
    pub fn new(values: Vec<u8>, limit: usize) -> Self {
        let mut f = Self {
            requires: FxHashMap::default(),
            values,
            held: Required::new(),
            limit,
        };
        let certain = f.certain();
        for i in 0..f.values.len() {
            if f.values[i] & QUESTION != 0 {
                let mut r = certain.clone();
                f.own(&mut r, i);
                f.requires.insert(i, r);
            }
        }
        f
    }
    /// The atom "logical local `i` is true"; the next bit is "not true".
    pub fn flag(limit: usize, i: usize) -> Option<usize> {
        (i < limit).then_some(limit + 2 * i)
    }
    /// The atom "`AVAILABLE` of the `k`-th buffer is true".
    pub fn buffer(limit: usize, k: usize) -> usize {
        3 * limit + 2 * k
    }
    /// No path reaches this point. A zero value always zeroes every value.
    pub fn dead(&self) -> bool {
        self.values.first().is_some_and(|v| *v == 0)
    }
    fn own(&self, r: &mut Required, i: usize) {
        if i < self.limit {
            r.insert(i);
        }
    }
    fn sources(&mut self) -> impl Iterator<Item = &mut Required> {
        self.requires.values_mut()
    }
    pub fn fill(&mut self, value: u8) {
        *self = Self::new(vec![value; self.len()], self.limit);
    }
    pub fn certain(&self) -> Required {
        let mut out = self.held.clone();
        for (i, v) in self.values.iter().enumerate().take(self.limit) {
            if *v == QUESTION {
                out.insert(i);
            }
        }
        out
    }
    /// Record that atom `a` holds on every path here.
    pub fn assume(&mut self, a: usize) {
        let opposite = a ^ 1;
        self.held.remove(opposite);
        self.held.insert(a);
        for r in self.sources() {
            r.remove(opposite);
            r.insert(a);
        }
    }
    /// Forget the atoms from `from` on.
    pub fn drop_atoms_from(&mut self, from: usize) {
        self.held.clear_from(from);
        for r in self.sources() {
            r.clear_from(from);
        }
    }
    fn drop_flag(&mut self, i: usize) {
        if let Some(a) = Self::flag(self.limit, i) {
            self.held.remove(a);
            self.held.remove(a + 1);
            for r in self.sources() {
                r.remove(a);
                r.remove(a + 1);
            }
        }
    }
    pub fn write(&mut self, i: usize, value: u8, mut required: Required, preserves: bool) {
        if value == 0 {
            self.fill(0);
            return;
        }
        self.drop_flag(i);
        if let Some(a) = Self::flag(self.limit, i) {
            required.remove(a);
            required.remove(a + 1);
        }
        let tracked = i < self.limit;
        for r in self.sources() {
            if !preserves {
                r.remove(i);
            }
            if value == QUESTION && tracked {
                r.insert(i);
            }
        }
        self.requires.remove(&i);
        self.values[i] = value;
        if value & QUESTION != 0 {
            required.union(&self.certain());
            self.own(&mut required, i);
            self.requires.insert(i, required);
        }
    }
    pub fn forget(&mut self, i: usize, value: u8) {
        self.write(i, value, Required::new(), false);
    }
    pub fn drop_source(&mut self, i: usize) {
        self.values[i] &= !QUESTION;
        self.requires.remove(&i);
        if self.values[i] == 0 {
            self.fill(0);
        }
    }
    pub fn known(&mut self, i: usize) {
        if i < self.limit {
            let cut: Vec<usize> = self
                .requires
                .iter()
                .filter(|(_, r)| r.contains(i))
                .map(|(&v, _)| v)
                .collect();
            for v in cut {
                self.drop_source(v);
                if self.dead() {
                    return;
                }
            }
        }
        self.values[i] = KNOWN;
        self.requires.remove(&i);
    }
    pub fn question(&mut self, i: usize) {
        let mut required = match self.requires.get(&i) {
            Some(r) if self.values[i] & super::OPAQUE == 0 => r.clone(),
            _ => self.certain(),
        };
        self.own(&mut required, i);
        self.values[i] = QUESTION;
        self.requires.insert(i, required);
        if i < self.limit {
            for r in self.sources() {
                r.insert(i);
            }
        }
    }
    pub fn link(&mut self, a: usize, b: usize) {
        if a >= self.limit || b >= self.limit {
            return;
        }
        for r in self.sources() {
            if r.contains(a) || r.contains(b) {
                r.insert(a);
                r.insert(b);
            }
        }
    }
    pub fn join(&mut self, other: &Self) {
        if other.dead() {
            return;
        }
        if self.dead() {
            *self = other.clone();
            return;
        }
        self.held.intersect(&other.held);
        for (i, s) in &other.requires {
            match self.requires.get_mut(i) {
                Some(r) => r.intersect(s),
                None => {
                    self.requires.insert(*i, s.clone());
                }
            }
        }
        for (v, o) in self.values.iter_mut().zip(&other.values) {
            *v |= o;
        }
    }
}
