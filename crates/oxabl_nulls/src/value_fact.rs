//! Conditions that must hold on paths carrying a known `?` source.
//!
//! A condition is a bit. Bits below `limit` say "this tested local is `?`";
//! the bits above are the atoms of [`Fact::flag`] and [`Fact::buffer`].
use super::{KNOWN, QUESTION};
use smallvec::SmallVec;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

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
    pub fn words(&self) -> usize {
        self.0.len()
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
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.0
            .iter()
            .enumerate()
            .all(|(w, a)| a & !other.0.get(w).copied().unwrap_or(0) == 0)
    }
}

/// Sorted `(local_index, Rc<Required>)` pairs shared via `Rc` so `Fact::clone`
/// and `Fact::clone_from` are O(1) refcount bumps on every CFG edge.
#[derive(Debug, Default)]
pub(super) struct RequiresTable(Rc<Vec<(usize, Rc<Required>)>>);

impl Clone for RequiresTable {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
    fn clone_from(&mut self, source: &Self) {
        self.0 = Rc::clone(&source.0);
    }
}

impl PartialEq for RequiresTable {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}
impl Eq for RequiresTable {}

impl RequiresTable {
    pub fn get(&self, i: usize) -> Option<&Required> {
        self.0
            .binary_search_by_key(&i, |&(k, _)| k)
            .ok()
            .map(|at| self.0[at].1.as_ref())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&usize, &Required)> {
        self.0.iter().map(|(k, r)| (k, r.as_ref()))
    }

    pub fn values(&self) -> impl Iterator<Item = &Required> {
        self.0.iter().map(|(_, r)| r.as_ref())
    }

    pub fn insert(&mut self, i: usize, r: Required) {
        let entries = Rc::make_mut(&mut self.0);
        match entries.binary_search_by_key(&i, |&(k, _)| k) {
            Ok(at) => entries[at].1 = Rc::new(r),
            Err(at) => entries.insert(at, (i, Rc::new(r))),
        }
    }

    pub fn remove(&mut self, i: usize) {
        if let Ok(at) = self.0.binary_search_by_key(&i, |&(k, _)| k) {
            Rc::make_mut(&mut self.0).remove(at);
        }
    }

    pub fn clear(&mut self) {
        if !self.0.is_empty() {
            if let Some(entries) = Rc::get_mut(&mut self.0) {
                entries.clear();
            } else {
                self.0 = Rc::new(Vec::new());
            }
        }
    }

    /// Mutate every entry for which `needs_edit` returns true, skipping
    /// copy-on-write cloning whenever no entry changes.
    fn mutate_each(
        &mut self,
        needs_edit: impl Fn(&Required) -> bool,
        mut edit: impl FnMut(&mut Required),
    ) {
        if !self.0.iter().any(|(_, r)| needs_edit(r)) {
            return;
        }
        for (_, r) in Rc::make_mut(&mut self.0) {
            if needs_edit(r) {
                edit(Rc::make_mut(r));
            }
        }
    }

    /// Merge `other` into `self` by intersecting shared entries and sharing
    /// entries present only in `other`.
    fn join_with(&mut self, other: &Self) {
        if Rc::ptr_eq(&self.0, &other.0) || other.0.is_empty() {
            return;
        }
        if self.0.is_empty() {
            self.0 = Rc::clone(&other.0);
            return;
        }
        let a = self.0.as_slice();
        let b = other.0.as_slice();
        let mut merged: Vec<(usize, Rc<Required>)> = Vec::with_capacity(a.len().max(b.len()));
        let (mut ia, mut ib) = (0, 0);
        let mut changed = false;
        while ia < a.len() && ib < b.len() {
            let (ka, ra) = &a[ia];
            let (kb, rb) = &b[ib];
            match ka.cmp(kb) {
                std::cmp::Ordering::Less => {
                    merged.push((*ka, Rc::clone(ra)));
                    ia += 1;
                }
                std::cmp::Ordering::Greater => {
                    merged.push((*kb, Rc::clone(rb)));
                    changed = true;
                    ib += 1;
                }
                std::cmp::Ordering::Equal => {
                    if Rc::ptr_eq(ra, rb) || ra.is_subset_of(rb) {
                        merged.push((*ka, Rc::clone(ra)));
                    } else if rb.is_subset_of(ra) {
                        merged.push((*ka, Rc::clone(rb)));
                        changed = true;
                    } else {
                        let mut inter = (**ra).clone();
                        inter.intersect(rb);
                        merged.push((*ka, Rc::new(inter)));
                        changed = true;
                    }
                    ia += 1;
                    ib += 1;
                }
            }
        }
        while ia < a.len() {
            merged.push((a[ia].0, Rc::clone(&a[ia].1)));
            ia += 1;
        }
        while ib < b.len() {
            merged.push((b[ib].0, Rc::clone(&b[ib].1)));
            changed = true;
            ib += 1;
        }
        if changed {
            self.0 = Rc::new(merged);
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Fact {
    pub values: Vec<u8>,
    /// Per local: the conditions on every path that carries its `?` source.
    pub requires: RequiresTable,
    /// Conditions that hold on every path here.
    pub held: Required,
    /// The leading locals a condition may name: those a test reads.
    pub limit: usize,
}
impl Clone for Fact {
    fn clone(&self) -> Self {
        Self {
            values: self.values.clone(),
            requires: self.requires.clone(),
            held: self.held.clone(),
            limit: self.limit,
        }
    }
    /// Reuses the buffers of `self`: the solver clones a fact per edge.
    fn clone_from(&mut self, source: &Self) {
        self.values.clone_from(&source.values);
        self.requires.clone_from(&source.requires);
        self.held.clone_from(&source.held);
        self.limit = source.limit;
    }
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
            requires: RequiresTable::default(),
            values,
            held: Required::new(),
            limit,
        };
        f.add_sources();
        f
    }
    /// Make every `?` value a source under the conditions that hold here.
    fn add_sources(&mut self) {
        let certain = self.certain();
        let mut entries = Vec::new();
        for i in 0..self.values.len() {
            if self.values[i] & QUESTION != 0 {
                let mut r = certain.clone();
                self.own(&mut r, i);
                entries.push((i, Rc::new(r)));
            }
        }
        self.requires = RequiresTable(Rc::new(entries));
    }
    /// The atom "logical local `i` is true"; the next bit is "not true".
    /// Atoms are even, so `a ^ 1` is the opposite of `a`.
    pub fn flag(limit: usize, i: usize) -> Option<usize> {
        (i < limit).then_some(limit.next_multiple_of(2) + 2 * i)
    }
    /// The atom "the `j`-th counted loop has not finished an iteration since
    /// it was entered".
    pub fn fresh(limit: usize, j: usize) -> usize {
        limit.next_multiple_of(2) + 2 * limit + 2 * j
    }
    /// The atom "`AVAILABLE` of the `k`-th buffer is true", after the atoms
    /// of `loops` counted loops.
    pub fn buffer(limit: usize, loops: usize, k: usize) -> usize {
        Self::fresh(limit, loops) + 2 * k
    }
    /// Add atom `a` to the conditions of every source.
    pub fn mark_sources(&mut self, a: usize) {
        self.requires
            .mutate_each(|r| !r.contains(a), |r| r.insert(a));
    }
    /// Remove atom `a` from the conditions of every source.
    pub fn unmark_sources(&mut self, a: usize) {
        self.requires
            .mutate_each(|r| r.contains(a), |r| r.remove(a));
    }
    /// Drop the sources that carry atom `a`.
    pub fn drop_marked(&mut self, a: usize) {
        let cut: Vec<usize> = self
            .requires
            .iter()
            .filter(|(_, r)| r.contains(a))
            .map(|(&i, _)| i)
            .collect();
        for i in cut {
            self.drop_source(i);
            if self.dead() {
                return;
            }
        }
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
    /// The work to clone, join and compare the fact: one unit per local, and
    /// for each source one per word of its conditions.
    pub fn cost(&self) -> usize {
        self.values.len() / 16 + self.requires.values().map(|r| 1 + r.words()).sum::<usize>()
    }
    /// The same as `Fact::new(vec![value; self.len()], self.limit)`, in place.
    pub fn fill(&mut self, value: u8) {
        self.values.fill(value);
        self.requires.clear();
        self.held = Required::new();
        if value & QUESTION != 0 {
            self.add_sources();
        }
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
        self.requires.mutate_each(
            |r| r.contains(opposite) || !r.contains(a),
            |r| {
                r.remove(opposite);
                r.insert(a);
            },
        );
    }
    /// Forget the atoms from `from` on.
    pub fn drop_atoms_from(&mut self, from: usize) {
        self.held.clear_from(from);
        let has_from = |r: &Required| r.0.len() * 64 > from;
        self.requires.mutate_each(has_from, |r| r.clear_from(from));
    }
    fn drop_flag(&mut self, i: usize) {
        if let Some(a) = Self::flag(self.limit, i) {
            self.held.remove(a);
            self.held.remove(a + 1);
            self.requires.mutate_each(
                |r| r.contains(a) || r.contains(a + 1),
                |r| {
                    r.remove(a);
                    r.remove(a + 1);
                },
            );
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
        let remove_i = !preserves;
        let add_i = value == QUESTION && tracked;
        if remove_i || add_i {
            self.requires.mutate_each(
                |r| (remove_i && r.contains(i)) || (add_i && !r.contains(i)),
                |r| {
                    if remove_i {
                        r.remove(i);
                    }
                    if add_i {
                        r.insert(i);
                    }
                },
            );
        }
        self.requires.remove(i);
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
        self.requires.remove(i);
        if self.values[i] == 0 {
            self.fill(0);
        }
    }
    /// Local `i` is not `?`. A weak step keeps an Unknown value Unknown.
    pub fn known(&mut self, i: usize, strong: bool) {
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
        self.values[i] = if strong {
            KNOWN
        } else {
            self.values[i] & !QUESTION
        };
        self.requires.remove(i);
        if self.values[i] == 0 {
            self.fill(0);
        }
    }
    /// Local `i` is `?`. A strong step makes an Unknown value a source; a
    /// weak one keeps it Unknown.
    pub fn question(&mut self, i: usize, strong: bool) {
        if !strong {
            let value = self.values[i] & (QUESTION | super::OPAQUE);
            if value == 0 {
                self.fill(0);
                return;
            }
            self.values[i] = value;
            if value & QUESTION != 0 && i < self.limit {
                let mut required = match self.requires.get(i) {
                    Some(r) => r.clone(),
                    None => self.certain(),
                };
                required.insert(i);
                self.requires.insert(i, required);
            }
            if i < self.limit {
                self.requires
                    .mutate_each(|r| !r.contains(i), |r| r.insert(i));
            }
            return;
        }
        let mut required = match self.requires.get(i) {
            Some(r) if self.values[i] & super::OPAQUE == 0 => r.clone(),
            _ => self.certain(),
        };
        self.own(&mut required, i);
        self.values[i] = QUESTION;
        self.requires.insert(i, required);
        if i < self.limit {
            self.requires
                .mutate_each(|r| !r.contains(i), |r| r.insert(i));
        }
    }
    pub fn link(&mut self, a: usize, b: usize) {
        if a >= self.limit || b >= self.limit {
            return;
        }
        self.requires.mutate_each(
            |r| (r.contains(a) && !r.contains(b)) || (r.contains(b) && !r.contains(a)),
            |r| {
                r.insert(a);
                r.insert(b);
            },
        );
    }
    pub fn join(&mut self, other: &Self) {
        if other.dead() {
            return;
        }
        if self.dead() {
            self.clone_from(other);
            return;
        }
        self.held.intersect(&other.held);
        self.requires.join_with(&other.requires);
        for (v, o) in self.values.iter_mut().zip(&other.values) {
            *v |= o;
        }
    }
}

#[cfg(test)]
impl Required {
    pub fn subset(&self, other: &Self) -> bool {
        self.is_subset_of(other)
    }
}

#[cfg(test)]
impl Fact {
    /// `self` is below `other` in the order [`join`](Self::join) computes.
    pub fn leq(&self, other: &Self) -> bool {
        if self.dead() {
            return true;
        }
        if other.dead() {
            return false;
        }
        self.values
            .iter()
            .zip(&other.values)
            .all(|(a, b)| a & !b == 0)
            && other.held.subset(&self.held)
            && self.requires.iter().all(|(i, r)| {
                self.values[*i] & QUESTION == 0
                    || other.requires.get(*i).is_some_and(|o| o.subset(r))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::Fact;

    #[test]
    fn every_atom_pairs_with_its_opposite() {
        for limit in 0..6 {
            let mut atoms: Vec<usize> = (0..limit)
                .filter_map(|i| Fact::flag(limit, i))
                .chain((0..2).map(|j| Fact::fresh(limit, j)))
                .chain((0..3).map(|k| Fact::buffer(limit, 2, k)))
                .collect();
            assert!(atoms.iter().all(|a| a % 2 == 0 && *a >= limit), "{limit}");
            atoms.extend(atoms.clone().iter().map(|a| a ^ 1));
            let count = atoms.len();
            atoms.sort_unstable();
            atoms.dedup();
            assert_eq!(atoms.len(), count, "{limit}");
        }
    }
}
