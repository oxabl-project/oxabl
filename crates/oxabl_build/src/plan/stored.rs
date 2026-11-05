//! The reasons that come from the results of earlier builds.

use std::path::Path;

use super::reasons::Reason;
use crate::graph::{Graph, Scratch};
use crate::key::compute::UnitKey;
use crate::output::{OutputLayout, OutputState, check};
use crate::refresh::Outcome;
use crate::store::{OutputRow, ResultRow, Results, SUCCESS};

/// What the planner knows from the store.
pub struct Stored<'a> {
    pub outcome: &'a Outcome,
    pub results: &'a Results,
    pub layout: Option<&'a OutputLayout>,
    pub root: &'a Path,
    /// The record of the environment this build probed; `None` without a
    /// probe.
    pub environment: Option<&'a str>,
    pub after: &'a Graph,
}

/// The reasons for one unit, and the output stats to record again.
#[derive(Default)]
pub struct Verdict {
    pub reasons: Vec<Reason>,
    pub restat: Vec<(u32, OutputRow)>,
}

fn names(outcome: &Outcome, ids: impl IntoIterator<Item = u32>) -> Vec<String> {
    let mut names: Vec<String> = ids
        .into_iter()
        .map(|id| outcome.name(id).to_string())
        .collect();
    names.sort();
    names
}

impl Stored<'_> {
    /// Every unit that has a result is judged by it; one with none is `new`.
    /// `since_refresh` holds the closure change the refresh saw.
    pub fn reasons(
        &self,
        unit: u32,
        key: Option<&UnitKey>,
        since_refresh: Option<Reason>,
        scratch: &mut Scratch,
    ) -> Verdict {
        let mut verdict = Verdict::default();
        let Some(result) = self.results.by_unit.get(&unit) else {
            verdict.reasons.push(Reason::New);
            return verdict;
        };
        if result.status != SUCCESS {
            verdict.reasons.push(Reason::Failed);
            return verdict;
        }
        match key {
            Some(key) => self.compare_keys(unit, result, key, since_refresh, scratch, &mut verdict),
            None => self.compare_generations(unit, result, since_refresh, scratch, &mut verdict),
        }
        if verdict.reasons.is_empty() {
            self.check_outputs(unit, key, &mut verdict);
        }
        verdict
    }

    /// Whether the unit changed since `result`, and the files of its closure
    /// that did, the closure its compiler last reported included.
    fn changed_since(
        &self,
        unit: u32,
        result: &ResultRow,
        scratch: &mut Scratch,
    ) -> (bool, Vec<String>) {
        let outcome = self.outcome;
        let gen_of = |id: u32| outcome.changed_gen_of(id) > result.compiled_gen;
        let mut closure = self.after.closure(unit, scratch);
        if let Some(observed) = outcome.observed.get(&unit) {
            closure.extend(&observed.files);
            closure.sort_unstable();
            closure.dedup();
        }
        let files = closure.into_iter().filter(|id| gen_of(*id));
        (gen_of(unit), names(outcome, files))
    }

    fn compare_generations(
        &self,
        unit: u32,
        result: &ResultRow,
        since_refresh: Option<Reason>,
        scratch: &mut Scratch,
        verdict: &mut Verdict,
    ) {
        let (own, files) = self.changed_since(unit, result, scratch);
        if own {
            verdict.reasons.push(Reason::SourceChanged);
        }
        if !files.is_empty() {
            verdict.reasons.push(Reason::DependencyChanged(files));
        }
        verdict.reasons.extend(since_refresh);
        if result.key.is_none() {
            verdict.reasons.push(match &result.note {
                Some(note) => Reason::Unverified(note.clone()),
                None => Reason::Unkeyable(vec!["no key at the last build".to_string()]),
            });
        }
    }

    fn compare_keys(
        &self,
        unit: u32,
        result: &ResultRow,
        key: &UnitKey,
        since_refresh: Option<Reason>,
        scratch: &mut Scratch,
        verdict: &mut Verdict,
    ) {
        let (Some(now), Some(parts)) = (key.key, key.components) else {
            return;
        };
        let Some(before) = result.key else {
            verdict.reasons.push(Reason::Unverified(
                result
                    .note
                    .clone()
                    .unwrap_or_else(|| "the last compile had no key".to_string()),
            ));
            return;
        };
        if before == now {
            return;
        }
        let Some(old) = result.components else {
            verdict.reasons.push(Reason::SourceChanged);
            return;
        };
        if old[0] != parts[0] {
            verdict.reasons.push(Reason::SourceChanged);
        }
        if old[1] != parts[1] {
            let (_, files) = self.changed_since(unit, result, scratch);
            let named = !files.is_empty();
            if named {
                verdict.reasons.push(Reason::DependencyChanged(files));
            }
            match since_refresh {
                Some(change) => verdict.reasons.push(change),
                None if !named => verdict.reasons.push(Reason::ClosureChanged {
                    gained: Vec::new(),
                    lost: Vec::new(),
                }),
                None => {}
            }
        }
        if old[2] != parts[2] {
            verdict.reasons.push(Reason::PropathChanged);
        }
        if old[3] != parts[3] {
            let before = self.results.environments.get(&old[3]);
            let fields = match (before, self.environment) {
                (Some(before), Some(now)) => crate::key::record::differences(before, now),
                _ => vec!["environment".to_string()],
            };
            verdict.reasons.push(Reason::EnvironmentChanged(fields));
        }
        let mut options = Vec::new();
        if old[4] != parts[4] {
            options.push("options".to_string());
        }
        if old[5] != parts[5] {
            options.push("outputs".to_string());
        }
        if !options.is_empty() {
            verdict.reasons.push(Reason::OptionsChanged(options));
        }
        if old[6] != parts[6] {
            verdict.reasons.push(Reason::WorkerChanged);
        }
    }

    fn check_outputs(&self, unit: u32, key: Option<&UnitKey>, verdict: &mut Verdict) {
        let rows = self
            .results
            .outputs
            .get(&unit)
            .map_or(&[][..], Vec::as_slice);
        if let (Some(layout), Some(name)) = (self.layout, key.and_then(|k| k.compile_name.as_ref()))
            && let Some(missing) = layout
                .expected(name)
                .into_iter()
                .find(|file| !rows.iter().any(|row| row.path == file.dest))
        {
            verdict.reasons.push(Reason::OutputMissing(missing.dest));
            return;
        }
        for row in rows {
            match check(self.root, row) {
                OutputState::Current => {}
                OutputState::Restat(now) => verdict.restat.push((unit, now)),
                OutputState::Missing => {
                    verdict
                        .reasons
                        .push(Reason::OutputMissing(row.path.clone()));
                    return;
                }
                OutputState::Changed => {
                    verdict
                        .reasons
                        .push(Reason::OutputChanged(row.path.clone()));
                    return;
                }
            }
        }
    }
}
