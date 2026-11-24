//! The closure of each unit: the files whose bytes its compile can read.

use rustc_hash::FxHashSet;

use crate::graph::Scratch;
use crate::refresh::{Outcome, parallel};

/// The closure of each of `units`, sorted by path bytes, in the order of
/// `units`. A unit is never in its own closure.
pub fn of_units(outcome: &Outcome, units: &[u32]) -> Vec<Vec<u32>> {
    let nodes = outcome.names.len();
    parallel::map_with(
        units,
        || Scratch::new(nodes),
        |scratch, unit| {
            let mut closure = outcome.after.closure(*unit, scratch);
            closure.sort_unstable_by(|a, b| outcome.name(*a).cmp(outcome.name(*b)));
            closure
        },
    )
}

/// The nodes that are class sources: every target of a class edge, and every
/// class unit.
pub fn class_sources(outcome: &Outcome, class_units: impl Iterator<Item = u32>) -> FxHashSet<u32> {
    let mut sources: FxHashSet<u32> = class_units.collect();
    for (_, analysis) in outcome.analyses.iter() {
        sources.extend(
            analysis
                .edges
                .iter()
                .filter(|edge| edge.kind.is_class())
                .map(|edge| edge.target),
        );
    }
    sources
}
