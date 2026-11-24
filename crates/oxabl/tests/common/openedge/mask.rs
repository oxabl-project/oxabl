//! The r-code oracle. r-code is not byte-deterministic, so two files are
//! equivalent when they have the same size and differ only at the offsets that
//! compiles of one unit in one root already differ at.
//!
//! The varying bytes are time stamps of 6 bytes, written at several places
//! of a file, whose first byte flips between two values and whose last byte
//! changes slowly. A handful of samples does not catch every flip, so the mask
//! also covers [`MARGIN`] bytes on each side of every byte that varies.

use std::collections::{BTreeMap, BTreeSet};
use std::time::Duration;

use super::compile::Compile;

/// Bytes around a varying byte that the mask covers.
pub const MARGIN: usize = 2;

/// The byte offsets at which compiles of one unit differ.
#[derive(Debug, Default, Clone)]
pub struct Mask {
    size: usize,
    offsets: BTreeSet<usize>,
}

impl Mask {
    /// The offsets at which any of `others` differs from `first`, and the
    /// bytes around them.
    pub fn from_samples(first: &[u8], others: &[&[u8]]) -> Mask {
        let mut offsets = BTreeSet::new();
        for other in others {
            assert_eq!(
                first.len(),
                other.len(),
                "two compiles of one unit have different sizes"
            );
            offsets.extend(differing(first, other));
        }
        let offsets = offsets
            .into_iter()
            .flat_map(|offset| {
                offset.saturating_sub(MARGIN)..=(offset + MARGIN).min(first.len() - 1)
            })
            .collect();
        Mask {
            size: first.len(),
            offsets,
        }
    }

    pub fn len(&self) -> usize {
        self.offsets.len()
    }

    pub fn size(&self) -> usize {
        self.size
    }

    /// An offset that the mask does not cover.
    pub fn unmasked_offset(&self) -> usize {
        (0..self.size)
            .rev()
            .find(|offset| !self.offsets.contains(offset))
            .expect("the mask does not cover the whole file")
    }

    /// The share of the file that the mask covers.
    pub fn share(&self) -> f64 {
        self.offsets.len() as f64 / self.size as f64
    }

    /// `Ok` when `a` and `b` have the mask's size and differ only inside it;
    /// otherwise what differs.
    pub fn equivalent(&self, a: &[u8], b: &[u8]) -> Result<(), String> {
        if a.len() != b.len() {
            return Err(format!("sizes differ: {} and {}", a.len(), b.len()));
        }
        let outside: Vec<usize> = differing(a, b)
            .filter(|offset| !self.offsets.contains(offset))
            .collect();
        match outside.is_empty() {
            true => Ok(()),
            false => Err(format!(
                "{} bytes differ outside the mask, the first at offsets {:?}",
                outside.len(),
                &outside[..outside.len().min(8)]
            )),
        }
    }
}

fn differing<'a>(a: &'a [u8], b: &'a [u8]) -> impl Iterator<Item = usize> + 'a {
    a.iter()
        .zip(b)
        .enumerate()
        .filter(|(_, (left, right))| left != right)
        .map(|(offset, _)| offset)
}

/// Units above this size are measured against the 1 % rule.
const LARGE: usize = 20_000;

/// The r-code of every unit of a workspace, with the mask of each.
pub struct Masks {
    pub first: BTreeMap<String, Vec<u8>>,
    pub masks: BTreeMap<String, Mask>,
}

impl Masks {
    /// Builds `compile` `samples` times in its root (every unit compiles each
    /// time) and records where the builds differ. The builds are more than a
    /// second apart, because the header of r-code holds a time stamp.
    pub fn record(compile: &Compile, samples: usize) -> Masks {
        assert!(samples >= 2, "a mask needs two compiles");
        compile.build(&["--json"], 0);
        let first = compile.rcode();
        let mut later = Vec::new();
        for _ in 1..samples {
            std::thread::sleep(Duration::from_millis(1200));
            compile.drop_rcode();
            compile.build(&["--json"], 0);
            later.push(compile.rcode());
        }
        let masks = first
            .iter()
            .map(|(name, bytes)| {
                let others: Vec<&[u8]> = later
                    .iter()
                    .map(|sample| {
                        sample
                            .get(name)
                            .unwrap_or_else(|| panic!("{name} was not compiled again"))
                            .as_slice()
                    })
                    .collect();
                (name.clone(), Mask::from_samples(bytes, &others))
            })
            .collect();
        Masks { first, masks }
    }

    /// The mask of every unit above 20,000 bytes covers under 1 % of its file,
    /// and at least one unit is above that.
    pub fn assert_small(&self) {
        for (name, mask) in &self.masks {
            eprintln!(
                "MASK {name}: {} of {} bytes ({:.2} %)",
                mask.len(),
                mask.size(),
                mask.share() * 100.0
            );
        }
        assert!(
            self.masks.values().any(|mask| mask.size() > LARGE),
            "no unit is larger than {LARGE} bytes"
        );
        let large: Vec<String> = self
            .masks
            .iter()
            .filter(|(_, mask)| mask.size() > LARGE && mask.share() >= 0.01)
            .map(|(name, mask)| format!("{name}: {} of {}", mask.len(), mask.size()))
            .collect();
        assert!(large.is_empty(), "masks over 1 %: {large:?}");
    }

    /// Every unit of `other` is equivalent to the first compile of the same
    /// unit here; every unit of the first compile is present.
    pub fn assert_equivalent(&self, other: &BTreeMap<String, Vec<u8>>, what: &str) {
        assert_eq!(
            self.first.keys().collect::<Vec<_>>(),
            other.keys().collect::<Vec<_>>(),
            "{what}: the compiled units differ"
        );
        let broken: Vec<String> = self
            .first
            .iter()
            .filter_map(|(name, bytes)| {
                self.masks[name]
                    .equivalent(bytes, &other[name])
                    .err()
                    .map(|why| format!("{name}: {why}"))
            })
            .collect();
        assert!(
            broken.is_empty(),
            "{what}: r-code is not equivalent:\n{}",
            broken.join("\n")
        );
    }
}
