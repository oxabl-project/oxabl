use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use super::files::{Current, Files};
use super::intern::Interner;
use super::listing::Listings;
use super::outcome::{Content, Delta, Observed, Outcome};
use super::probe::{directory_hash, unreadable_hash};
use super::settle::Analyses;
use crate::graph::Graph;
use crate::kinds::ObservationKind;
use crate::store::{BinaryRow, Changes, tracked};

/// What the refresh has when it is done analysing.
pub struct Parts {
    pub generation: u64,
    pub loaded_generation: u64,
    /// The new analysis identity, when it changed.
    pub identity: Option<[u8; 32]>,
    /// The hash of the running executable, when it was taken again.
    pub binary: Option<BinaryRow>,
    pub unit_ids: Vec<u32>,
    /// Stored file rows that nothing tracks any more.
    pub orphan_rows: Vec<u32>,
    pub interner: Interner,
    pub files: Files,
    pub listings: Listings,
    pub analyses: Analyses,
    pub delta: Delta,
    pub observed: FxHashMap<u32, Observed>,
    /// Units whose observed closure the store forgets.
    pub observed_deleted: Vec<u32>,
    /// The analyses were not loaded: nothing changed and no one asked.
    pub skipped: bool,
    /// Report the content of every tracked path.
    pub need_keys: bool,
    /// Report the generation in which each tracked path last changed.
    pub need_generations: bool,
}

/// The analyses reachable from the units through class edges: the units and
/// every class source a unit reaches.
fn reachable(unit_ids: &[u32], analyses: &Analyses) -> FxHashSet<u32> {
    let mut reach: FxHashSet<u32> = unit_ids.iter().copied().collect();
    let mut stack: Vec<u32> = unit_ids.to_vec();
    while let Some(node) = stack.pop() {
        let Some(analysis) = analyses.get(node) else {
            continue;
        };
        for edge in analysis.edges.iter().filter(|e| e.kind.is_class()) {
            if reach.insert(edge.target) {
                stack.push(edge.target);
            }
        }
    }
    reach
}

/// Turn a finished refresh into the changes to store and the outcome for the
/// planner.
pub fn assemble(parts: Parts) -> Outcome {
    let Parts {
        generation,
        loaded_generation,
        identity,
        binary,
        unit_ids,
        orphan_rows,
        interner,
        files,
        listings,
        analyses,
        mut delta,
        observed,
        observed_deleted,
        skipped,
        need_keys,
        need_generations,
    } = parts;

    let reach = if skipped {
        FxHashSet::default()
    } else {
        reachable(&unit_ids, &analyses)
    };
    let mut bits: FxHashMap<u32, u8> = FxHashMap::default();
    let mut listed: FxHashSet<u32> = FxHashSet::default();
    if skipped {
        for id in files.ids() {
            let old = files.get(id).and_then(|t| t.old.as_ref());
            bits.insert(id, old.map_or(0, |row| row.tracked_by));
        }
        listed.extend(listings.rows.keys().copied());
    } else {
        for id in &unit_ids {
            *bits.entry(*id).or_default() |= tracked::UNIT;
        }
        for (id, analysis) in analyses.iter().filter(|(id, _)| reach.contains(id)) {
            *bits.entry(id).or_default() |= tracked::READ;
            for observation in &analysis.observations {
                match observation.kind {
                    ObservationKind::Read => {
                        *bits.entry(observation.path).or_default() |= tracked::READ;
                    }
                    ObservationKind::Absent => {
                        *bits.entry(observation.path).or_default() |= tracked::ABSENT;
                    }
                    ObservationKind::Listed => {
                        listed.insert(observation.path);
                    }
                }
            }
        }
        for closure in observed.values() {
            for id in closure
                .files
                .iter()
                .chain(&closure.absent)
                .chain(&closure.classes)
            {
                *bits.entry(*id).or_default() |= tracked::OBSERVED;
            }
        }
    }

    let mut changes = Changes::default();
    for id in files.ids() {
        let wanted = bits.get(&id).copied().unwrap_or(0);
        let had_row = files.get(id).is_some_and(|t| t.old.is_some());
        if wanted == 0 {
            if had_row {
                changes.files_deleted.push(id);
            }
            continue;
        }
        if let Some((row, true)) = files.row(id, wanted) {
            changes.files.push((id, row));
        }
    }
    changes.files_deleted.extend(orphan_rows);
    changes.observed_deleted = observed_deleted;
    let (listing_rows, listings_deleted) = listings.rows_for(&listed);
    changes.listings = listing_rows;
    changes.listings_deleted = listings_deleted;

    let mut kept_deleted: Vec<u32> = Vec::new();
    if !skipped {
        for (id, analysis) in analyses.made.iter().filter(|(id, _)| reach.contains(id)) {
            changes.analyses.push((*id, analysis.encode()));
            delta.reanalysed.insert(*id);
        }
        kept_deleted = analyses
            .old
            .keys()
            .filter(|id| !reach.contains(id))
            .copied()
            .collect();
        changes.analyses_deleted = kept_deleted.clone();
    }

    delta.changed = files.changed.clone();
    let listings_changed = !listings.changed.is_empty();
    let bump = !delta.changed.is_empty()
        || listings_changed
        || identity.is_some()
        || !delta.removed_units.is_empty()
        || !changes.analyses.is_empty()
        || !kept_deleted.is_empty();
    let generation_after = if bump { generation } else { loaded_generation };
    if bump {
        changes.generation = Some(generation);
    }
    changes.identity = identity;
    changes.binary = binary;

    let (names, committed) = interner.into_names();
    let content = if need_keys {
        content_of(&files, names.len())
    } else {
        Vec::new()
    };
    let changed_gen = if need_keys || need_generations {
        changed_gens(&files, names.len())
    } else {
        Vec::new()
    };
    changes.new_paths = names
        .iter()
        .enumerate()
        .skip(committed as usize)
        .filter(|(_, name)| !name.is_empty())
        .map(|(id, name)| (id as u32, name.clone()))
        .collect();

    let (before, after) = if skipped {
        (Arc::new(Graph::default()), Arc::new(Graph::default()))
    } else {
        let before = Arc::new(Graph::build(
            names.len(),
            analyses.old.iter().map(|(id, a)| (*id, a)),
        ));
        let identical = changes.analyses.is_empty() && kept_deleted.is_empty();
        let after = if identical {
            before.clone()
        } else {
            Arc::new(Graph::build(
                names.len(),
                analyses.iter().filter(|(id, _)| reach.contains(id)),
            ))
        };
        (before, after)
    };

    Outcome {
        generation: generation_after,
        stored_generation: loaded_generation,
        names,
        units: unit_ids,
        delta,
        before,
        after,
        analyses,
        content,
        changed_gen,
        observed,
        warnings: Vec::new(),
        changes,
    }
}

/// The content of every tracked path, by id.
fn content_of(files: &Files, ids: usize) -> Vec<Content> {
    let unreadable = [unreadable_hash(), directory_hash()];
    let mut content = vec![Content::Untracked; ids];
    for id in files.ids() {
        let Some(tracked) = files.get(id) else {
            continue;
        };
        content[id as usize] = match tracked.current {
            Current::Absent => Content::Absent,
            Current::Present { hash, .. } if unreadable.contains(&hash) => Content::Unreadable,
            Current::Present { hash, .. } => Content::Hash(hash),
        };
    }
    content
}

/// The generation in which each tracked path last changed, by id.
fn changed_gens(files: &Files, ids: usize) -> Vec<u64> {
    let mut generations = vec![0; ids];
    for id in files.ids() {
        generations[id as usize] = files.changed_gen(id);
    }
    generations
}
