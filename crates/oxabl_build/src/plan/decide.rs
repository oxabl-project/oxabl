use std::collections::HashMap;

use rustc_hash::FxHashSet;

use super::reasons::{Named, Reason};
use super::stored::Stored;
use crate::graph::{Graph, Reverse, Scratch};
use crate::key::compute::{Keyed, UnitKey};
use crate::output::OutputLayout;
use crate::paths::PathMap;
use crate::refresh::Outcome;
use crate::store::{OutputRow, Results, StoredEnvironment};

/// One unit in the rebuild set, with every reason that applies, first reason
/// first.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    pub path: String,
    pub reasons: Vec<Reason>,
}

/// What `oxabl build --plan` says.
#[derive(Debug)]
pub struct Plan {
    /// The units to rebuild, sorted by path.
    pub rebuild: Vec<Entry>,
    /// The units not in the rebuild set, sorted by path.
    pub unchanged: Vec<String>,
    /// The units that cannot compile where they are, sorted by path.
    pub skipped: Vec<String>,
    /// The units of the store that are no longer in the working tree.
    pub removed: Vec<String>,
    /// The files `--files` named that are not in the graph.
    pub unknown: Vec<String>,
    /// The generation of the store: after the refresh when it was committed,
    /// the one the store holds otherwise.
    pub generation: u64,
    /// The compile keys, when the environment was probed.
    pub keys: Option<PlanKeys>,
    /// Outputs that stayed current although their stat changed: the stat to
    /// record, by unit id.
    pub restat: Vec<(u32, OutputRow)>,
    /// The environment of the last build, when this plan did not probe.
    pub stored_environment: Option<StoredEnvironment>,
}

/// What the store knows from earlier builds.
pub struct StoredInputs<'a> {
    pub results: &'a Results,
    pub layout: Option<&'a OutputLayout>,
    /// The record of the environment this plan probed.
    pub environment: Option<&'a str>,
    /// Compare every unit with its stored result even before a build has
    /// finished, so a unit with no result is `new`. A build sets it; a plan
    /// before the first build lists what changed since the last refresh.
    pub judge_all: bool,
}

/// The compile keys of a plan, by unit path.
#[derive(Debug)]
pub struct PlanKeys {
    pub keyed: Keyed,
    index: std::collections::HashMap<String, usize>,
}

impl PlanKeys {
    /// The key state of the unit at `path`.
    pub fn get(&self, path: &str) -> Option<&UnitKey> {
        self.index.get(path).map(|at| &self.keyed.units[*at])
    }
}

/// A file `--files` named, as a node of the graph.
struct Target {
    id: u32,
    /// For each node that reaches it, the next node on the way.
    via: HashMap<u32, u32>,
}

/// Whether the working tree or the store knows `id`: it is a unit, or some
/// node reads it.
fn in_graph(outcome: &Outcome, reverse: [&Reverse; 2], id: u32) -> bool {
    outcome
        .units
        .binary_search_by_key(&outcome.name(id), |u| outcome.name(*u))
        .is_ok()
        || reverse.iter().any(|r| !r.readers(id).is_empty())
}

fn names(outcome: &Outcome, ids: impl IntoIterator<Item = u32>) -> Vec<String> {
    let mut names: Vec<String> = ids
        .into_iter()
        .map(|id| outcome.name(id).to_string())
        .collect();
    names.sort();
    names
}

fn path_to(outcome: &Outcome, via: &HashMap<u32, u32>, unit: u32, target: u32) -> Vec<String> {
    let mut path = Vec::new();
    let mut node = unit;
    while node != target {
        path.push(outcome.name(node).to_string());
        node = via[&node];
    }
    path.push(outcome.name(target).to_string());
    path
}

/// Decide the rebuild set from what the refresh found different, from the
/// files `named` as changed, and from the keys: a unit without a key
/// compiles on every build, and a unit that cannot compile where it is is
/// skipped.
pub fn decide(
    outcome: &Outcome,
    paths: &PathMap,
    named: &[std::path::PathBuf],
    keyed: Option<Keyed>,
    stored: Option<StoredInputs<'_>>,
) -> Plan {
    let after = &outcome.after;
    let before = &outcome.before;
    let reverse_after = Reverse::of(after);
    let reverse_before = Reverse::of(before);

    let mut unknown = Vec::new();
    let mut targets: Vec<Target> = Vec::new();
    for path in named {
        let canonical = paths.canonical(path);
        match outcome
            .id_of(&canonical)
            .filter(|id| in_graph(outcome, [&reverse_after, &reverse_before], *id))
        {
            Some(id) => targets.push(Target {
                id,
                via: reverse_after.reaching([id]),
            }),
            None => unknown.push(canonical),
        }
    }

    let delta = &outcome.delta;
    let roots: Vec<u32> = delta
        .changed
        .iter()
        .chain(&delta.reanalysed)
        .copied()
        .chain(targets.iter().map(|t| t.id))
        .collect();
    let touched_after = reverse_after.reaching(roots.iter().copied());
    let touched_before = reverse_before.reaching(roots.iter().copied());

    let mut scratch = Scratch::new(outcome.names.len());
    let judge = stored
        .as_ref()
        .filter(|s| s.judge_all || s.results.built)
        .map(|s| Stored {
            outcome,
            results: s.results,
            layout: s.layout,
            root: paths.root(),
            environment: s.environment,
            after,
        });
    let mut restat = Vec::new();
    let mut rebuild = Vec::new();
    let mut unchanged = Vec::new();
    let mut skipped = Vec::new();
    for (position, &unit) in outcome.units.iter().enumerate() {
        let name = outcome.name(unit).to_string();
        let key = keyed.as_ref().map(|keyed| &keyed.units[position]);
        if key.is_some_and(|key| key.skipped) {
            skipped.push(name);
            continue;
        }
        let touched = touched_after.contains_key(&unit) || touched_before.contains_key(&unit);
        let mut reasons = match &judge {
            Some(judge) => {
                let in_refresh = match touched {
                    true => reasons_of(outcome, unit, &targets, &mut scratch, after, before),
                    false => Vec::new(),
                };
                let (named, closure): (Vec<Reason>, Vec<Reason>) = in_refresh
                    .into_iter()
                    .filter(|r| matches!(r, Reason::Named(_) | Reason::ClosureChanged { .. }))
                    .partition(|r| matches!(r, Reason::Named(_)));
                let verdict = judge.reasons(unit, key, closure.into_iter().next(), &mut scratch);
                restat.extend(verdict.restat);
                let mut reasons = verdict.reasons;
                let leading = usize::from(matches!(
                    reasons.first(),
                    Some(Reason::New | Reason::Failed)
                ));
                for (at, reason) in named.into_iter().enumerate() {
                    reasons.insert(leading + at, reason);
                }
                reasons
            }
            None => match touched {
                true => reasons_of(outcome, unit, &targets, &mut scratch, after, before),
                false => Vec::new(),
            },
        };
        if let Some(key) = key.filter(|key| key.key.is_none()) {
            let leading = usize::from(matches!(
                reasons.first(),
                Some(Reason::New | Reason::Failed)
            ));
            reasons.insert(leading, Reason::Unkeyable(key.unkeyable.clone()));
        }
        if reasons.is_empty() {
            unchanged.push(name);
        } else {
            rebuild.push(Entry {
                path: name,
                reasons,
            });
        }
    }

    let stored_environment = match (&keyed, &stored) {
        (None, Some(stored)) if stored.results.built => stored.results.environment.clone(),
        _ => None,
    };
    let keys = keyed.map(|keyed| {
        let index = outcome
            .units
            .iter()
            .enumerate()
            .map(|(position, unit)| (outcome.name(*unit).to_string(), position))
            .collect();
        PlanKeys { keyed, index }
    });
    Plan {
        rebuild,
        unchanged,
        skipped,
        keys,
        removed: names(outcome, delta.removed_units.iter().copied()),
        unknown,
        generation: outcome.generation,
        restat,
        stored_environment,
    }
}

fn reasons_of(
    outcome: &Outcome,
    unit: u32,
    targets: &[Target],
    scratch: &mut Scratch,
    after: &Graph,
    before: &Graph,
) -> Vec<Reason> {
    let delta = &outcome.delta;
    if delta.new_units.contains(&unit) {
        return vec![Reason::New];
    }
    let mut reasons = Vec::new();
    let named: Vec<Named> = targets
        .iter()
        .filter(|target| target.via.contains_key(&unit))
        .map(|target| Named {
            file: outcome.name(target.id).to_string(),
            via: path_to(outcome, &target.via, unit, target.id),
        })
        .collect();
    if !named.is_empty() {
        reasons.push(Reason::Named(named));
    }
    if delta.changed.contains(&unit) {
        reasons.push(Reason::SourceChanged);
    }
    let closure_before = before.closure(unit, scratch);
    let closure_after = after.closure(unit, scratch);
    let kept = closure_after
        .iter()
        .copied()
        .filter(|id| closure_before.binary_search(id).is_ok());
    let changed: Vec<u32> = kept.filter(|id| delta.changed.contains(id)).collect();
    if !changed.is_empty() {
        reasons.push(Reason::DependencyChanged(names(outcome, changed)));
    }
    let gained: FxHashSet<u32> = closure_after
        .iter()
        .copied()
        .filter(|id| closure_before.binary_search(id).is_err())
        .collect();
    let lost: FxHashSet<u32> = closure_before
        .iter()
        .copied()
        .filter(|id| closure_after.binary_search(id).is_err())
        .collect();
    if !gained.is_empty() || !lost.is_empty() {
        reasons.push(Reason::ClosureChanged {
            gained: names(outcome, gained),
            lost: names(outcome, lost),
        });
    }
    reasons
}
