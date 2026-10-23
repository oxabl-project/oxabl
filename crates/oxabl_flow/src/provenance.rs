//! A dataflow fact for "where may this value have come from".
//!
//! [`Provenance`] holds, for each of a fixed number of slots (variables, fields,
//! ...), the [`Labels`] that may reach it. A label is an *origin* (an index the
//! client gives to a source site) with a *mask* of properties the value still
//! has. A client clears properties as the value passes through code that
//! removes them, and a label with no property left is gone.
//!
//! Join is union: the origins of both sides, and for an origin on both sides
//! the union of its masks. The order is a finite lattice for a fixed set of
//! origins, so [`solve`](crate::dataflow::solve) reaches a fixed point.
//!
//! Taint tracking is the first client: the origins are untrusted sources and
//! the mask bits are the kinds of sink a value is still dangerous for.

use smallvec::SmallVec;

/// Origins with their property masks, sorted by origin, no mask zero.
pub type Labels = SmallVec<[(u32, u32); 2]>;

/// Merge `from` into `into`; `true` if `into` changed.
pub fn merge(into: &mut Labels, from: &[(u32, u32)]) -> bool {
    let mut changed = false;
    for &(origin, mask) in from {
        if mask == 0 {
            continue;
        }
        match into.binary_search_by_key(&origin, |l| l.0) {
            Ok(i) => {
                let new = into[i].1 | mask;
                changed |= new != into[i].1;
                into[i].1 = new;
            }
            Err(i) => {
                into.insert(i, (origin, mask));
                changed = true;
            }
        }
    }
    changed
}

/// Remove the bits of `mask` from every label, and drop the labels left with
/// none.
pub fn clear(labels: &mut Labels, mask: u32) {
    labels.retain(|l| {
        l.1 &= !mask;
        l.1 != 0
    });
}

/// The labels of each slot at one program point.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Provenance {
    slots: Vec<Labels>,
}

impl Provenance {
    /// No labels in any of `slots` slots.
    pub fn new(slots: usize) -> Self {
        Provenance {
            slots: vec![Labels::new(); slots],
        }
    }

    /// The number of slots.
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    /// Whether there are no slots.
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// The labels of `slot`. A slot out of range has none.
    pub fn get(&self, slot: usize) -> &[(u32, u32)] {
        self.slots.get(slot).map_or(&[], |l| l.as_slice())
    }

    /// Replace the labels of `slot` (a strong update).
    pub fn set(&mut self, slot: usize, labels: Labels) {
        if let Some(s) = self.slots.get_mut(slot) {
            *s = labels;
        }
    }

    /// Add labels to `slot` (a weak update); `true` if it changed.
    pub fn add(&mut self, slot: usize, labels: &[(u32, u32)]) -> bool {
        self.slots.get_mut(slot).is_some_and(|s| merge(s, labels))
    }

    /// Remove the bits of `mask` from the labels of `slot`.
    pub fn clear(&mut self, slot: usize, mask: u32) {
        if let Some(s) = self.slots.get_mut(slot) {
            clear(s, mask);
        }
    }

    /// Union `other` into `self`, slot by slot; `true` if `self` changed.
    pub fn join(&mut self, other: &Provenance) -> bool {
        if self.slots.len() < other.slots.len() {
            self.slots.resize(other.slots.len(), Labels::new());
        }
        let mut changed = false;
        for (a, b) in self.slots.iter_mut().zip(&other.slots) {
            changed |= merge(a, b);
        }
        changed
    }

    /// The size of the fact, for
    /// [`Analysis::fact_cost`](crate::dataflow::Analysis::fact_cost): one per
    /// slot and one per label.
    pub fn cost(&self) -> usize {
        self.slots.len() + self.slots.iter().map(|s| s.len()).sum::<usize>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn labels(items: &[(u32, u32)]) -> Labels {
        items.iter().copied().collect()
    }

    #[test]
    fn merge_unions_origins_and_masks_in_order() {
        let mut a = labels(&[(1, 0b01), (5, 0b10)]);
        assert!(merge(&mut a, &[(3, 0b1), (5, 0b01)]));
        assert_eq!(a.as_slice(), &[(1, 0b01), (3, 0b1), (5, 0b11)]);
        assert!(!merge(&mut a, &[(1, 0b01), (7, 0)]));
    }

    #[test]
    fn clearing_every_bit_drops_the_label() {
        let mut a = labels(&[(1, 0b011), (2, 0b100)]);
        clear(&mut a, 0b110);
        assert_eq!(a.as_slice(), &[(1, 0b001)]);
    }

    #[test]
    fn a_strong_update_replaces_and_a_weak_one_adds() {
        let mut p = Provenance::new(2);
        p.add(0, &[(4, 1)]);
        p.add(0, &[(2, 1)]);
        assert_eq!(p.get(0), &[(2, 1), (4, 1)]);
        p.set(0, labels(&[(9, 2)]));
        assert_eq!(p.get(0), &[(9, 2)]);
        p.clear(0, 2);
        assert!(p.get(0).is_empty());
        assert!(p.get(7).is_empty());
    }

    #[test]
    fn join_is_a_union_per_slot() {
        let mut a = Provenance::new(2);
        a.add(0, &[(1, 1)]);
        let mut b = Provenance::new(2);
        b.add(0, &[(1, 2)]);
        b.add(1, &[(3, 1)]);
        assert!(a.join(&b));
        assert_eq!(a.get(0), &[(1, 3)]);
        assert_eq!(a.get(1), &[(3, 1)]);
        assert!(!a.join(&b));
        assert_eq!(a.cost(), 4);
    }
}
