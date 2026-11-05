use std::path::PathBuf;

use oxabl_pipeline::PipelineConfig;
use oxabl_workspace::ObservingFileSystem;
use rustc_hash::{FxHashMap, FxHashSet};

use super::analyse::{Analyser, Node};
use super::files::{Current, Files};
use super::intern::Interner;
use super::listing::Listings;
use super::snapshot::SnapshotFs;
use crate::error::BuildError;
use crate::kinds::ObservationKind;
use crate::paths::PathMap;
use crate::store::Analysis;

/// How often the working tree may change under a refresh before it gives up.
const SETTLE_ROUNDS: usize = 3;

/// The analyses of a refresh: the ones the store held, of which some are
/// stale, and the ones this refresh made.
#[derive(Default)]
pub struct Analyses {
    /// Every analysis the store held.
    pub old: FxHashMap<u32, Analysis>,
    /// The ids of `old` that no longer hold.
    stale: FxHashSet<u32>,
    pub made: FxHashMap<u32, Analysis>,
}

impl Analyses {
    pub fn new(old: FxHashMap<u32, Analysis>) -> Self {
        Analyses {
            old,
            stale: FxHashSet::default(),
            made: FxHashMap::default(),
        }
    }

    /// The analysis that holds for `id`.
    pub fn get(&self, id: u32) -> Option<&Analysis> {
        self.made
            .get(&id)
            .or_else(|| self.old.get(&id).filter(|_| !self.stale.contains(&id)))
    }

    /// The analyses that hold.
    pub fn iter(&self) -> impl Iterator<Item = (u32, &Analysis)> + Clone {
        self.made.iter().map(|(id, a)| (*id, a)).chain(
            self.old
                .iter()
                .filter(|(id, _)| !self.stale.contains(id) && !self.made.contains_key(id))
                .map(|(id, a)| (*id, a)),
        )
    }

    /// Declare the stored analysis of `id` stale and forget a made one.
    pub fn invalidate(&mut self, id: u32) {
        self.made.remove(&id);
        self.stale.insert(id);
    }
}

/// What the analysis loop works on.
pub struct Settle<'a> {
    pub config: &'a PipelineConfig,
    pub fs: &'a ObservingFileSystem<SnapshotFs>,
    pub paths: &'a PathMap,
    pub interner: &'a Interner,
    pub files: &'a mut Files,
    pub listings: &'a mut Listings,
    pub generation: u64,
}

impl Settle<'_> {
    fn host_path(&self, id: u32) -> PathBuf {
        let name = self.interner.name_of(id).unwrap_or_default();
        self.paths.absolute(&name)
    }

    /// Analyse the units in `pending`, and every class source that a result
    /// names and has no analysis, until nothing is left; then check that no
    /// file changed under the run, and analyse what read such a file again.
    pub fn run(&mut self, analyses: &mut Analyses, pending: Vec<u32>) -> Result<(), BuildError> {
        let mut round = 0;
        let mut pending = pending;
        loop {
            self.close(analyses, std::mem::take(&mut pending));
            let drifted = self.drifted(analyses);
            if drifted.is_empty() {
                return Ok(());
            }
            round += 1;
            if round >= SETTLE_ROUNDS {
                return Err(BuildError::Infrastructure(
                    "the workspace kept changing during the refresh".to_string(),
                ));
            }
            for id in &drifted {
                let path = self.host_path(*id);
                self.files.reprobe(*id, &path, self.fs.inner());
            }
            pending = analyses
                .iter()
                .filter(|(id, analysis)| {
                    drifted.contains(id)
                        || analysis
                            .observations
                            .iter()
                            .any(|o| drifted.contains(&o.path))
                })
                .map(|(id, _)| id)
                .collect();
            for id in &pending {
                analyses.invalidate(*id);
            }
        }
    }

    /// Analyse `batch`, then the class sources it makes reachable.
    fn close(&mut self, analyses: &mut Analyses, batch: Vec<u32>) {
        let mut queue = batch;
        let mut scanned = false;
        loop {
            if !scanned {
                scanned = true;
                queue.extend(missing_classes(analyses.iter(), analyses));
            }
            queue.sort_unstable();
            queue.dedup();
            queue.retain(|id| analyses.get(*id).is_none());
            if queue.is_empty() {
                return;
            }
            let nodes: Vec<Node> = queue
                .iter()
                .map(|id| Node {
                    id: *id,
                    path: self.host_path(*id),
                })
                .collect();
            let analyser = Analyser {
                config: self.config,
                fs: self.fs,
                paths: self.paths,
                interner: self.interner,
                generation: self.generation,
            };
            let results = analyser.analyse(&nodes);
            for (id, _) in &results {
                if !self.files.contains(*id) {
                    let path = self.host_path(*id);
                    self.files.discover(*id, &path, self.fs.inner());
                }
            }
            let batch_ids: Vec<u32> = results.iter().map(|(id, _)| *id).collect();
            for (id, analysis) in results {
                self.track(&analysis);
                analyses.made.insert(id, analysis);
            }
            let fresh = batch_ids
                .iter()
                .filter_map(|id| analyses.get(*id).map(|a| (*id, a)));
            queue = missing_classes(fresh, analyses);
        }
    }

    /// Track what `analysis` observed: each path it read or found absent, and
    /// each directory it listed.
    fn track(&mut self, analysis: &Analysis) {
        for observation in &analysis.observations {
            let path = self.host_path(observation.path);
            match observation.kind {
                ObservationKind::Listed => self.listings.discover(observation.path, &path),
                ObservationKind::Read | ObservationKind::Absent => {
                    if !self.files.contains(observation.path) {
                        self.files
                            .discover(observation.path, &path, self.fs.inner());
                    }
                }
            }
        }
    }

    /// The paths whose state under the run differs from the state the analyses
    /// saw: a file read with other bytes than the refresh hashed, a path found
    /// absent that exists, a path read that is gone.
    fn drifted(&mut self, analyses: &Analyses) -> FxHashSet<u32> {
        let mut drifted: FxHashSet<u32> = self
            .fs
            .inner()
            .take_drift()
            .iter()
            .filter_map(|path| self.interner.lookup(&self.paths.canonical(path)))
            .collect();
        for analysis in analyses.made.values() {
            for observation in &analysis.observations {
                let Some(tracked) = self.files.get(observation.path) else {
                    continue;
                };
                let inconsistent = matches!(
                    (observation.kind, tracked.current),
                    (ObservationKind::Absent, Current::Present { .. })
                        | (ObservationKind::Read, Current::Absent)
                );
                if inconsistent {
                    drifted.insert(observation.path);
                }
            }
        }
        drifted
    }
}

/// The class sources named by the class edges of `scan` that have no analysis.
fn missing_classes<'a>(
    scan: impl Iterator<Item = (u32, &'a Analysis)>,
    analyses: &Analyses,
) -> Vec<u32> {
    let mut missing = Vec::new();
    for (_, analysis) in scan {
        for edge in analysis.edges.iter().filter(|e| e.kind.is_class()) {
            if analyses.get(edge.target).is_none() {
                missing.push(edge.target);
            }
        }
    }
    missing
}
