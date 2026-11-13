//! V2: a set is released to the caches only after the databases were imaged
//! again, after the unit finished, with the schema identities of the probe.

use std::time::{Duration, Instant};

use rustc_hash::FxHashMap;

use crate::cache::ArtifactSetRef;
use crate::key::{SchemaImage, SchemaPart};

/// When held sets are checked against the databases.
#[derive(Clone, Copy, Debug)]
pub struct Settings {
    /// A check starts no sooner than this many times the duration of the
    /// last imaging after that imaging ended.
    pub budget: u32,
    /// The duration of the imaging of the probe, the first "last imaging".
    pub imaging: Duration,
}

/// A set that passed V1 and V3, with the unit it belongs to.
struct Candidate {
    path: String,
    set: ArtifactSetRef,
    done_at: Instant,
}

/// One imaging of the databases, split into parts that idle workers take.
struct Round {
    id: u64,
    started: Instant,
    shares: Vec<Option<Vec<SchemaPart>>>,
    failure: Option<String>,
}

/// What was not written, and why.
#[derive(Debug, Default)]
pub struct Dropped {
    /// A file changed between the key and the end of the compile.
    pub inputs: Vec<(String, String)>,
    /// The schema differed from the probe, or could not be imaged.
    pub schema: Vec<String>,
    pub schema_reason: Option<String>,
}

/// Holds verified sets until the schema check releases them.
pub struct Releaser {
    baseline: Vec<(String, blake3::Hash)>,
    settings: Settings,
    candidates: FxHashMap<u32, Candidate>,
    held: Vec<Candidate>,
    round: Option<Round>,
    next_round: u64,
    /// When the last check that matched the probe started.
    matched: Option<Instant>,
    /// When the last imaging ended, and how long it took.
    imaged: (Instant, Duration),
    /// Checks that ended, the final one not counted.
    pub rounds: usize,
    broken: bool,
    pub dropped: Dropped,
}

fn images_match(baseline: &[(String, blake3::Hash)], images: &[SchemaImage]) -> Result<(), String> {
    if images.len() != baseline.len() {
        return Err("the databases are not the ones of the probe".to_string());
    }
    for ((ldb, identity), image) in baseline.iter().zip(images) {
        if image.ldb != *ldb || image.identity() != *identity {
            return Err(format!("the schema of {ldb} differs from the probe"));
        }
    }
    Ok(())
}

impl Releaser {
    /// `baseline` is the schema identity of each database at the probe.
    pub fn new(baseline: Vec<(String, blake3::Hash)>, settings: Settings) -> Releaser {
        Releaser {
            baseline,
            settings,
            candidates: FxHashMap::default(),
            held: Vec::new(),
            round: None,
            next_round: 1,
            matched: None,
            imaged: (Instant::now(), settings.imaging),
            rounds: 0,
            broken: false,
            dropped: Dropped::default(),
        }
    }

    /// Whether there are databases to image.
    pub fn databases(&self) -> Vec<String> {
        self.baseline.iter().map(|(ldb, _)| ldb.clone()).collect()
    }

    pub fn is_broken(&self) -> bool {
        self.broken
    }

    pub fn holds(&self) -> bool {
        !self.held.is_empty()
    }

    /// A unit passed V3 and is not tainted: its set waits for V1.
    pub fn offer(&mut self, unit: u32, path: &str, set: ArtifactSetRef, done_at: Instant) {
        if self.broken {
            self.dropped.schema.push(path.to_string());
            return;
        }
        self.candidates.insert(
            unit,
            Candidate {
                path: path.to_string(),
                set,
                done_at,
            },
        );
    }

    /// V1 answered for `unit`: `Err` names the file that changed. Returns the
    /// sets that may be stored now.
    pub fn rehashed(&mut self, unit: u32, result: Result<(), String>) -> Vec<ArtifactSetRef> {
        let Some(candidate) = self.candidates.remove(&unit) else {
            return Vec::new();
        };
        if let Err(file) = result {
            self.dropped.inputs.push((candidate.path, file));
            return Vec::new();
        }
        if self.broken {
            self.dropped.schema.push(candidate.path);
            return Vec::new();
        }
        let checked = self.baseline.is_empty()
            || self
                .matched
                .is_some_and(|started| candidate.done_at < started);
        if checked {
            return vec![candidate.set];
        }
        self.held.push(candidate);
        Vec::new()
    }

    /// Whether a check of the databases is due and none is running. Every
    /// set that waits counts: the check must start after its unit finished,
    /// and the unit's inputs are hashed again meanwhile. Only the time since
    /// the last imaging decides, so the checks cost a fixed share of the
    /// build whatever the number of units.
    pub fn wants_round(&self, now: Instant) -> bool {
        if self.broken || self.round.is_some() || self.baseline.is_empty() {
            return false;
        }
        let (ended, took) = self.imaged;
        let wait = took.saturating_mul(self.settings.budget);
        let waiting = self.held.len() + self.candidates.len();
        waiting > 0 && now.saturating_duration_since(ended) >= wait
    }

    /// Starts a check split into `parts`; returns its id.
    pub fn begin(&mut self, parts: usize) -> u64 {
        let id = self.next_round;
        self.next_round += 1;
        self.round = Some(Round {
            id,
            started: Instant::now(),
            shares: vec![None; parts],
            failure: None,
        });
        id
    }

    /// Whether check `id` is the one that runs.
    pub fn running(&self, id: u64) -> bool {
        self.round.as_ref().is_some_and(|round| round.id == id)
    }

    /// One part of check `id` came back. When it was the last, returns the
    /// sets the check released.
    pub fn share(
        &mut self,
        id: u64,
        part: usize,
        result: Result<Vec<SchemaPart>, String>,
    ) -> Vec<ArtifactSetRef> {
        let Some(round) = self.round.as_mut().filter(|round| round.id == id) else {
            return Vec::new();
        };
        match result {
            Ok(shares) => round.shares[part] = Some(shares),
            Err(reason) => {
                round.failure.get_or_insert(reason);
                round.shares[part] = Some(Vec::new());
            }
        }
        if round.shares.iter().any(Option::is_none) {
            return Vec::new();
        }
        let round = self.round.take().expect("the round is running");
        self.imaged = (Instant::now(), round.started.elapsed());
        self.rounds += 1;
        let verdict = match round.failure {
            Some(reason) => Err(reason),
            None => self.combine(&round),
        };
        self.conclude(verdict, Some(round.started))
    }

    fn combine(&self, round: &Round) -> Result<(), String> {
        let mut images = Vec::new();
        for (at, (ldb, _)) in self.baseline.iter().enumerate() {
            let parts: Vec<SchemaPart> = round
                .shares
                .iter()
                .flatten()
                .filter_map(|share| share.get(at).cloned())
                .collect();
            images.push(SchemaImage::combine(ldb.clone(), parts)?);
        }
        images_match(&self.baseline, &images)
    }

    fn conclude(
        &mut self,
        verdict: Result<(), String>,
        started: Option<Instant>,
    ) -> Vec<ArtifactSetRef> {
        match verdict {
            Ok(()) => {
                if let Some(at) = started {
                    self.matched = Some(at);
                }
                let (released, kept): (Vec<_>, Vec<_>) = std::mem::take(&mut self.held)
                    .into_iter()
                    .partition(|held| started.is_none_or(|at| held.done_at < at));
                self.held = kept;
                released.into_iter().map(|held| held.set).collect()
            }
            Err(reason) => {
                self.broken = true;
                self.dropped.schema_reason.get_or_insert(reason);
                let held = std::mem::take(&mut self.held);
                self.dropped
                    .schema
                    .extend(held.into_iter().map(|held| held.path));
                Vec::new()
            }
        }
    }

    /// The check that ends the build, from the images of all databases now.
    pub fn conclude_with(
        &mut self,
        images: Result<Vec<SchemaImage>, String>,
    ) -> Vec<ArtifactSetRef> {
        self.round = None;
        let verdict = images.and_then(|images| images_match(&self.baseline, &images));
        self.conclude(verdict, None)
    }

    /// The pass ended: parts of a running check that no worker took are lost.
    pub fn abandon_round(&mut self) {
        self.round = None;
    }

    /// Drops what no check released, after a cancel or a failed pass.
    pub fn discard(&mut self) {
        self.round = None;
        self.held.clear();
        self.candidates.clear();
    }
}
